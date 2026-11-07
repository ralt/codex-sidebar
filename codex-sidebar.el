;;; codex-sidebar.el --- Project and chat navigation for Codex vterms -*- lexical-binding: t; -*-

;; Author: Florian Margaine
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (projectile "2.8.0") (vterm "0.0.2"))
;; Keywords: tools, terminals

;;; Commentary:
;; A project/chat sidebar backed by Codex app-server metadata.  Conversations
;; run in ordinary Codex CLI vterms.  No transcripts or credentials are copied.
;; Entry point: `codex-sidebar-open'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)
(require 'projectile)
(declare-function vterm-mode "vterm")
(declare-function vterm--sentinel "vterm")
(defvar vterm-shell)
(defvar vterm-exit-functions)
(defvar vterm-kill-buffer-on-exit)
(defvar vterm-buffer-name-string)

(defgroup codex-sidebar nil "Codex projects and saved chats." :group 'tools)
(defcustom codex-sidebar-executable "codex" "Codex CLI executable." :type 'string)
(defcustom codex-sidebar-server-arguments '("app-server" "proxy")
  "Arguments for the metadata process.  These never start an inference turn."
  :type '(repeat string))
(defcustom codex-sidebar-cli-arguments nil
  "Additional CLI arguments for interactive chats." :type '(repeat string))
(defcustom codex-sidebar-remote-endpoint "unix://"
  "Shared app-server endpoint used by vterm.  Nil uses the CLI's local resume path."
  :type '(choice (const nil) string))
(defcustom codex-sidebar-width 42 "Sidebar width in columns." :type 'integer)
(defcustom codex-sidebar-history-limit 500 "Maximum recent chats to fetch." :type 'integer)
(defcustom codex-sidebar-recent-count 15 "Chats shown in the Recent section." :type 'integer)
(defcustom codex-sidebar-request-timeout 30 "RPC timeout in seconds." :type 'number)
(defcustom codex-sidebar-state-file (locate-user-emacs-file "codex-sidebar-state.eld")
  "File holding selected/temporary projects and expanded groups, not chat history."
  :type 'file)

(defface codex-sidebar-project-face '((t :inherit font-lock-keyword-face :weight bold))
  "Project heading face.")
(defface codex-sidebar-live-face '((t :inherit success)) "Live terminal indicator.")

(defvar codex-sidebar--process nil)
(defvar codex-sidebar--ready nil)
(defvar codex-sidebar--pending (make-hash-table :test #'eql))
(defvar codex-sidebar--queue nil)
(defvar codex-sidebar--sequence 0)
(defvar codex-sidebar--threads nil)
(defvar codex-sidebar--drafts nil)
(defvar codex-sidebar--starting nil)
(defvar codex-sidebar--terminals (make-hash-table :test #'equal))
(defvar codex-sidebar--projects nil)
(defvar codex-sidebar--selected nil)
(defvar codex-sidebar--expanded nil)
(defvar codex-sidebar--state-loaded nil)
(defvar codex-sidebar--generation 0)
(defvar codex-sidebar--status "Not connected")
(defvar codex-sidebar--refreshing nil)
(defvar codex-sidebar--refresh-timer nil)
(defvar codex-sidebar--last-refresh 0)
(defvar-local codex-sidebar-thread-id nil)
(defvar-local codex-sidebar-project-root nil)

(defun codex-sidebar--get (object key)
  "Read KEY from a JSON OBJECT."
  (and (hash-table-p object) (gethash key object)))

(defun codex-sidebar--directory (directory)
  "Return a canonical local DIRECTORY, including a trailing slash."
  (when (file-remote-p directory) (user-error "Codex sidebar currently supports local directories"))
  (file-name-as-directory (file-truename (expand-file-name directory))))

(defun codex-sidebar--context ()
  "Return the current Projectile project, or a temporary directory project."
  (if (and (derived-mode-p 'codex-sidebar-mode) codex-sidebar--selected)
      (cl-find codex-sidebar--selected codex-sidebar--projects :key (lambda (p) (plist-get p :root)) :test #'equal)
    (let* ((root (ignore-errors
                   ;; Explicit navigation should notice newly initialized repos,
                   ;; even if Projectile cached a negative lookup earlier.
                   (let ((projectile-project-root-cache (make-hash-table :test #'equal)))
                     (projectile-project-root))))
           (directory (codex-sidebar--directory (or root default-directory))))
      (list :root directory :name (file-name-nondirectory (directory-file-name directory))
            :temporary (not root)))))

(defun codex-sidebar--load-state ()
  (unless codex-sidebar--state-loaded
    (setq codex-sidebar--state-loaded t)
    (when (file-readable-p codex-sidebar-state-file)
      (condition-case err
          (let ((state (with-temp-buffer
                         (insert-file-contents codex-sidebar-state-file)
                         (read (current-buffer)))))
            (when (eq (plist-get state :version) 1)
              (setq codex-sidebar--projects
                    (cl-remove-if-not
                     (lambda (p) (and (stringp (plist-get p :root))
                                      (file-name-absolute-p (plist-get p :root))
                                      (stringp (plist-get p :name))))
                     (plist-get state :projects))
                    codex-sidebar--expanded (cl-remove-if-not #'stringp (plist-get state :expanded)))))
        (error (display-warning 'codex-sidebar (error-message-string err)))))))

(defun codex-sidebar--save-state ()
  (make-directory (file-name-directory codex-sidebar-state-file) t)
  (let ((temporary (make-temp-file (concat codex-sidebar-state-file "."))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 (list :version 1 :projects codex-sidebar--projects
                           :expanded codex-sidebar--expanded) (current-buffer))))
          (set-file-modes temporary #o600)
          (rename-file temporary codex-sidebar-state-file t))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun codex-sidebar--remember (project)
  (setq codex-sidebar--projects
        (cons project (cl-remove (plist-get project :root) codex-sidebar--projects
                                :key (lambda (p) (plist-get p :root)) :test #'equal)))
  (codex-sidebar--save-state))

(defun codex-sidebar--error (message-text)
  (setq codex-sidebar--status message-text codex-sidebar--refreshing nil)
  (codex-sidebar--render)
  (message "Codex: %s" message-text))

(defun codex-sidebar--send (message-object)
  (let ((text (json-serialize message-object :null-object nil :false-object :false)))
    (if (process-get codex-sidebar--process 'websocket)
        (codex-sidebar--ws-send codex-sidebar--process 1 (encode-coding-string text 'utf-8))
      (process-send-string codex-sidebar--process (concat text "\n")))))

(defun codex-sidebar--bytes (number count)
  (apply #'unibyte-string
         (cl-loop for shift from (* 8 (1- count)) downto 0 by 8
                  collect (logand 255 (ash number (- shift))))))

(defun codex-sidebar--ws-send (process opcode payload)
  "Send a masked WebSocket PAYLOAD through the local Codex proxy PROCESS."
  (let* ((size (length payload))
         (mask (apply #'unibyte-string (cl-loop repeat 4 collect (random 256))))
         (masked (copy-sequence payload)))
    (dotimes (i size) (aset masked i (logxor (aref payload i) (aref mask (% i 4)))))
    (process-send-string
     process (concat (unibyte-string (logior 128 opcode))
                     (cond ((< size 126) (unibyte-string (logior 128 size)))
                           ((< size 65536) (concat (unibyte-string 254) (codex-sidebar--bytes size 2)))
                           (t (concat (unibyte-string 255) (codex-sidebar--bytes size 8))))
                     mask masked))))

(defun codex-sidebar--wire-filter (process chunk)
  "Handle JSONL or WebSocket framing without blocking on incomplete input."
  (if (not (process-get process 'websocket))
      (codex-sidebar--filter process chunk)
    (condition-case err
        (let ((data (concat (or (process-get process 'wire-input) "") chunk)))
          (unless (process-get process 'upgraded)
            (when-let ((end (string-match "\r\n\r\n" data)))
              (let* ((headers (substring data 0 end))
                     (accept (base64-encode-string
                              (secure-hash 'sha1 (concat (process-get process 'ws-key)
                                                       "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") nil nil t) t)))
                (unless (and (string-match-p "\\`HTTP/1.[01] 101" headers)
                             (let ((case-fold-search t))
                               (string-match-p (concat "Sec-WebSocket-Accept: *" (regexp-quote accept)) headers)))
                  (error "Codex WebSocket upgrade failed: %s" (car (split-string headers "\r\n"))))
                (setq data (substring data (+ end 4)))
                (process-put process 'upgraded t)
                (when-let ((timer (process-get process 'upgrade-timer))) (cancel-timer timer))
                (codex-sidebar--initialize))))
          (when (process-get process 'upgraded)
            (let ((continue t))
              (while (and continue (>= (length data) 2))
                (let* ((first (aref data 0)) (second (aref data 1))
                       (opcode (logand first 15)) (length-code (logand second 127))
                       (header-size (cond ((= length-code 126) 4) ((= length-code 127) 10) (t 2))))
                  (when (or (/= 0 (logand first 112)) (/= 0 (logand second 128)))
                    (error "Unsupported Codex WebSocket frame"))
                  (if (< (length data) header-size)
                      (setq continue nil)
                    (let ((size (if (< length-code 126) length-code
                                  (let ((n 0))
                                    (cl-loop for i from 2 below header-size
                                             do (setq n (+ (ash n 8) (aref data i)))) n))))
                      (when (> size (* 8 1024 1024)) (error "Codex metadata frame exceeds 8 MiB"))
                      (if (< (length data) (+ header-size size))
                          (setq continue nil)
                        (let ((payload (substring data header-size (+ header-size size))))
                          (setq data (substring data (+ header-size size)))
                          (pcase opcode
                            (8 (delete-process process) (setq continue nil))
                            (9 (codex-sidebar--ws-send process 10 payload))
                            (10 nil)
                            ((or 0 1)
                             (when (= opcode 0)
                               (unless (process-get process 'fragment) (error "Unexpected WebSocket continuation"))
                               (setq payload (concat (process-get process 'fragment) payload)))
                             (if (= 0 (logand first 128))
                                 (process-put process 'fragment payload)
                               (process-put process 'fragment nil)
                               (codex-sidebar--dispatch
                                (json-parse-string (decode-coding-string payload 'utf-8)
                                                   :object-type 'hash-table :array-type 'list
                                                   :null-object nil :false-object :false))))
                            (_ (error "Unsupported WebSocket opcode %s" opcode)))))))))))
          (process-put process 'wire-input data))
      (error
       (codex-sidebar--error (error-message-string err))
       (when (process-live-p process) (delete-process process))))))

(defun codex-sidebar--rpc-now (method params success failure)
  (let* ((id (cl-incf codex-sidebar--sequence))
         (timer (run-at-time
                 codex-sidebar-request-timeout nil
                 (lambda ()
                   (when-let ((pending (gethash id codex-sidebar--pending)))
                     (remhash id codex-sidebar--pending)
                     (funcall (nth 1 pending) (format "%s timed out; press g to retry" method)))))))
    (puthash id (list success failure timer) codex-sidebar--pending)
    (codex-sidebar--send `((id . ,id) (method . ,method) (params . ,params)))))

(defun codex-sidebar--dispatch (message-object)
  (let ((id (codex-sidebar--get message-object "id")))
    (cond
     (id
      (when-let ((pending (gethash id codex-sidebar--pending)))
        (remhash id codex-sidebar--pending)
        (cancel-timer (nth 2 pending))
        (if-let ((failure (codex-sidebar--get message-object "error")))
            (funcall (nth 1 pending) (or (codex-sidebar--get failure "message") "RPC error"))
          (funcall (car pending) (codex-sidebar--get message-object "result")))))
     ((member (codex-sidebar--get message-object "method")
              '("thread/started" "thread/archived" "thread/name/updated"))
      (when (equal (codex-sidebar--get message-object "method") "thread/started")
        (codex-sidebar--adopt-thread
         (codex-sidebar--get (codex-sidebar--get message-object "params") "thread")))
      (codex-sidebar--schedule-refresh)))))

(defun codex-sidebar--adopt-thread (thread)
  "Associate a fresh CLI terminal with its server-issued THREAD identifier."
  (let* ((root (codex-sidebar--thread-root thread))
         (draft (cl-find-if
                 (lambda (d)
                   (and (string-prefix-p "pending:" (gethash "id" d))
                        (equal root (codex-sidebar--thread-root d))))
                 codex-sidebar--drafts)))
    (when draft
      (let* ((old-id (gethash "id" draft)) (id (gethash "id" thread))
             (buffer (gethash old-id codex-sidebar--terminals)))
        (maphash (lambda (key value) (puthash key value draft)) thread)
        (remhash old-id codex-sidebar--terminals)
        (when (buffer-live-p buffer)
          (puthash id buffer codex-sidebar--terminals)
          (with-current-buffer buffer
            (setq codex-sidebar-thread-id id)
            (rename-buffer (format "*codex %s %s*"
                                   (file-name-nondirectory (directory-file-name root))
                                   (substring id 0 (min 8 (length id)))) t)))
        (setq codex-sidebar--starting (delete root codex-sidebar--starting))
        (codex-sidebar--render)))))

(defun codex-sidebar--filter (process text)
  "Decode complete JSON lines from PROCESS, preserving partial chunks in TEXT."
  (let* ((data (concat (or (process-get process 'partial) "") text))
         (start 0))
    (while (string-match "\n" data start)
      (let ((line (substring data start (match-beginning 0))))
        (setq start (match-end 0))
        (unless (string-empty-p line)
          (condition-case err
              (codex-sidebar--dispatch
               (json-parse-string line :object-type 'hash-table :array-type 'list
                                  :null-object nil :false-object :false))
            (error (codex-sidebar--error (error-message-string err)))))))
    (process-put process 'partial (substring data start))))

(defun codex-sidebar--sentinel (process event)
  (when-let ((timer (process-get process 'upgrade-timer))) (cancel-timer timer))
  (when (and (eq process codex-sidebar--process) (not (process-live-p process)))
    (setq codex-sidebar--ready nil codex-sidebar--process nil)
    (let ((pending codex-sidebar--pending) (queue codex-sidebar--queue))
      (setq codex-sidebar--pending (make-hash-table :test #'eql) codex-sidebar--queue nil)
      (maphash (lambda (_id request)
                 (cancel-timer (nth 2 request))
                 (funcall (nth 1 request) (concat "Metadata server stopped: " (string-trim event))))
               pending)
      (dolist (request queue) (funcall (nth 3 request) "Metadata server stopped; press g to reconnect")))))

(defun codex-sidebar--connect ()
  (unless (process-live-p codex-sidebar--process)
    (unless (executable-find codex-sidebar-executable)
      (user-error "Cannot find %s" codex-sidebar-executable))
    (setq codex-sidebar--ready nil)
    (let ((default-directory (expand-file-name "~/")))
      (setq codex-sidebar--process
            (make-process :name "codex-sidebar-server" :buffer nil
                          :stderr (get-buffer-create "*Codex sidebar server log*")
                          :command
                          (if (equal codex-sidebar-server-arguments '("app-server" "proxy"))
                              (list "/bin/sh" "-c"
                                    (format "%s app-server daemon start >&2 && exec %s app-server proxy"
                                            (shell-quote-argument codex-sidebar-executable)
                                            (shell-quote-argument codex-sidebar-executable)))
                            (cons codex-sidebar-executable codex-sidebar-server-arguments))
                          :coding (if (member "proxy" codex-sidebar-server-arguments) 'binary 'utf-8-unix)
                          :connection-type 'pipe :noquery t
                          :filter #'codex-sidebar--wire-filter :sentinel #'codex-sidebar--sentinel)))
    (if (not (member "proxy" codex-sidebar-server-arguments))
        (codex-sidebar--initialize)
      (process-put codex-sidebar--process 'websocket t)
      (let ((key (base64-encode-string
                  (apply #'unibyte-string (cl-loop repeat 16 collect (random 256))) t))
            (process codex-sidebar--process))
        (process-put process 'ws-key key)
        (process-put process 'upgrade-timer
                     (run-at-time codex-sidebar-request-timeout nil
                                  (lambda ()
                                    (unless (process-get process 'upgraded)
                                      (codex-sidebar--error "Codex WebSocket handshake timed out")
                                      (when (process-live-p process) (delete-process process))))))
        (process-send-string process
                             (concat "GET / HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\n"
                                     "Connection: Upgrade\r\nSec-WebSocket-Version: 13\r\n"
                                     "Sec-WebSocket-Key: " key "\r\n\r\n"))))))

(defun codex-sidebar--initialize ()
    (codex-sidebar--rpc-now
     "initialize" '((clientInfo . ((name . "codex_sidebar")
                                   (title . "Emacs Codex Sidebar") (version . "0.1.0"))))
     (lambda (_result)
       (codex-sidebar--send '((method . "initialized")))
       (setq codex-sidebar--ready t)
       (let ((queue codex-sidebar--queue))
         (setq codex-sidebar--queue nil)
         (dolist (request queue) (apply #'codex-sidebar--rpc-now request))))
     (lambda (error-text)
       (when (process-live-p codex-sidebar--process) (delete-process codex-sidebar--process))
       (codex-sidebar--error error-text))))

(defun codex-sidebar--request (method params success &optional failure)
  "Send an asynchronous metadata request; never block Emacs waiting for a reply."
  (let ((request (list method params success (or failure #'codex-sidebar--error))))
    (if (and codex-sidebar--ready (process-live-p codex-sidebar--process))
        (apply #'codex-sidebar--rpc-now request)
      (setq codex-sidebar--queue (append codex-sidebar--queue (list request)))
      (codex-sidebar--connect))))

(defun codex-sidebar-refresh ()
  "Refresh saved chats through app-server, retaining the current view on error."
  (interactive)
  (setq codex-sidebar--refreshing t codex-sidebar--status "Loading history…"
        codex-sidebar--last-refresh (float-time))
  (codex-sidebar--render)
  (let ((generation (cl-incf codex-sidebar--generation)))
    (codex-sidebar--fetch-page generation nil nil)))

(defun codex-sidebar--fetch-page (generation cursor accumulated)
  (codex-sidebar--request
   "thread/list"
   `((limit . ,(min 100 (max 1 (- codex-sidebar-history-limit (length accumulated)))))
     (sortKey . "updated_at") (cursor . ,cursor)
     (sourceKinds . ["cli" "vscode" "appServer" "unknown"])
     (modelProviders . []) (useStateDbOnly . t))
   (lambda (result)
     (when (= generation codex-sidebar--generation)
       (let ((threads (append accumulated (codex-sidebar--get result "data")))
             (next (codex-sidebar--get result "nextCursor")))
         (if (and next (< (length threads) codex-sidebar-history-limit))
             (codex-sidebar--fetch-page generation next threads)
           ;; Codex indexes a new chat only after its first user message.  Keep
           ;; empty tabs visible only while their terminal is still open.
           (setq codex-sidebar--drafts
                 (cl-remove-if
                  (lambda (draft)
                    (let ((id (codex-sidebar--get draft "id")))
                      (or (cl-find id threads
                                   :key (lambda (thread) (codex-sidebar--get thread "id")) :test #'equal)
                          (not (or (codex-sidebar--live-buffer id)
                                   (and (string-prefix-p "pending:" id)
                                        (member (codex-sidebar--thread-root draft) codex-sidebar--starting)))))))
                  codex-sidebar--drafts))
           (setq codex-sidebar--threads (append codex-sidebar--drafts threads) codex-sidebar--refreshing nil
                 codex-sidebar--status (format "%d chats%s" (length threads) (if next " (history limit reached)" "")))
           (codex-sidebar--render)))))
   (lambda (error-text)
     (when (= generation codex-sidebar--generation) (codex-sidebar--error error-text)))))

(defun codex-sidebar--schedule-refresh ()
  (unless (timerp codex-sidebar--refresh-timer)
    (setq codex-sidebar--refresh-timer
          (run-with-idle-timer .3 nil
           (lambda ()
             (setq codex-sidebar--refresh-timer nil)
             (when (and (get-buffer "*Codex*") (not codex-sidebar--refreshing))
               (codex-sidebar-refresh)))))))

(defun codex-sidebar--focus-refresh (_frame)
  (when (and (derived-mode-p 'codex-sidebar-mode) (not codex-sidebar--refreshing)
             (> (- (float-time) codex-sidebar--last-refresh) 2))
    (codex-sidebar--schedule-refresh)))

(defun codex-sidebar--thread-root (thread)
  (when-let ((cwd (codex-sidebar--get thread "cwd")))
    (condition-case nil (codex-sidebar--directory cwd) (error cwd))))

(defun codex-sidebar--title (thread)
  (let ((title (or (codex-sidebar--get thread "name") (codex-sidebar--get thread "preview"))))
    (if (and (stringp title) (not (string-empty-p (string-trim title))))
        (replace-regexp-in-string "[\n\r\t ]+" " " (string-trim title))
      "New chat")))

(defun codex-sidebar--live-buffer (id)
  (let ((buffer (gethash id codex-sidebar--terminals)))
    (and (buffer-live-p buffer) (process-live-p (get-buffer-process buffer)) buffer)))

(defun codex-sidebar--row (text entry &optional face)
  (insert (propertize (concat text "\n") 'codex-sidebar-entry entry
                      'mouse-face 'highlight 'face face
                      'help-echo (or (plist-get entry :root) text))))

(defun codex-sidebar--thread-row (thread &optional indent)
  (let* ((id (codex-sidebar--get thread "id")) (live (codex-sidebar--live-buffer id))
         (title (codex-sidebar--title thread)))
    (codex-sidebar--row
     (format "%s%s %s" (or indent "  ") (if live "●" "·") title)
     (list :kind 'thread :id id :thread thread :root (codex-sidebar--thread-root thread))
     (and live 'codex-sidebar-live-face))))

(defun codex-sidebar--all-projects ()
  (let ((projects (copy-sequence codex-sidebar--projects)))
    (dolist (root (append projectile-known-projects
                          (mapcar #'codex-sidebar--thread-root codex-sidebar--threads)))
      (when (and (stringp root) (not (file-remote-p root)) (file-directory-p root))
        (let ((directory (codex-sidebar--directory root)))
          (unless (cl-find directory projects :key (lambda (p) (plist-get p :root)) :test #'equal)
            (push (list :root directory :name (file-name-nondirectory (directory-file-name directory))) projects)))))
    (sort projects (lambda (a b)
                     (if (equal (plist-get a :root) codex-sidebar--selected) t
                       (and (not (equal (plist-get b :root) codex-sidebar--selected))
                            (string-lessp (plist-get a :root) (plist-get b :root))))))))

(defun codex-sidebar--entry () (get-text-property (line-beginning-position) 'codex-sidebar-entry))
(defun codex-sidebar--entry-key (entry)
  (cons (plist-get entry :kind) (or (plist-get entry :id) (plist-get entry :root))))

(defun codex-sidebar--project-rows (project)
  (let* ((root (plist-get project :root))
         (expanded (member root codex-sidebar--expanded))
         (selected (equal root codex-sidebar--selected)))
    (codex-sidebar--row
     (format "%s %s%s%s" (if expanded "▾" "▸") (if selected "▶ " "")
             (plist-get project :name) (if (plist-get project :temporary) " [temporary]" ""))
     (list :kind 'project :root root :project project) 'codex-sidebar-project-face)
    (when expanded
      (codex-sidebar--row "    + New chat" (list :kind 'new :root root))
      (dolist (thread codex-sidebar--threads)
        (when (equal root (codex-sidebar--thread-root thread))
          (codex-sidebar--thread-row thread "    "))))))

(defun codex-sidebar--render ()
  (when-let ((buffer (get-buffer "*Codex*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t) (old-key (codex-sidebar--entry-key (codex-sidebar--entry)))
            (position nil))
        (erase-buffer)
        (insert (propertize "Codex\n" 'face 'bold)
                (propertize (concat codex-sidebar--status "\n\n") 'face 'shadow))
        (insert (propertize "Current project\n" 'face 'bold))
        (dolist (project (codex-sidebar--all-projects))
          (when (equal (plist-get project :root) codex-sidebar--selected)
            (codex-sidebar--project-rows project)))
        (insert (propertize "\nRecent chats\n" 'face 'bold))
        (dolist (thread (seq-take codex-sidebar--threads codex-sidebar-recent-count))
          (codex-sidebar--thread-row thread))
        (insert (propertize "\nProjects\n" 'face 'bold))
        (dolist (project (codex-sidebar--all-projects))
          (unless (equal (plist-get project :root) codex-sidebar--selected)
            (codex-sidebar--project-rows project)))
        (insert (propertize "\nn new  RET open  TAB fold\ng refresh  / search  p project\nr rename  a archive  q hide\n" 'face 'shadow))
        (goto-char (point-min))
        (while (and (not position) (not (eobp)))
          (when (equal old-key (codex-sidebar--entry-key (codex-sidebar--entry))) (setq position (point)))
          (forward-line 1))
        (goto-char (or position (point-min)))))))

(defun codex-sidebar--show ()
  (let ((buffer (get-buffer-create "*Codex*")))
    (with-current-buffer buffer (unless (derived-mode-p 'codex-sidebar-mode) (codex-sidebar-mode)))
    (select-window
     (display-buffer-in-side-window buffer `((side . left) (slot . 0) (window-width . ,codex-sidebar-width))))
    (set-window-dedicated-p (selected-window) t)
    (codex-sidebar--render)))

;;;###autoload
(defun codex-sidebar-open ()
  "Open the chat sidebar with the current Projectile project selected.
Outside a Projectile project, use the current directory as a temporary project."
  (interactive)
  (let ((project (codex-sidebar--context)))
    (codex-sidebar--load-state)
    (codex-sidebar--remember project)
    (setq codex-sidebar--selected (plist-get project :root))
    (cl-pushnew codex-sidebar--selected codex-sidebar--expanded :test #'equal)
    (codex-sidebar--show)
    (codex-sidebar-refresh)
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (let ((entry (codex-sidebar--entry)))
          (when (and (eq (plist-get entry :kind) 'project)
                     (equal (plist-get entry :root) codex-sidebar--selected)) (setq found (point))))
        (unless found (forward-line 1)))
      (when found (goto-char found))))
  (add-hook 'window-selection-change-functions #'codex-sidebar--focus-refresh))

(defun codex-sidebar-select-project ()
  "Select a known Projectile project or a temporary directory."
  (interactive)
  (let* ((roots (delete-dups (append projectile-known-projects
                                    (mapcar (lambda (p) (plist-get p :root)) (codex-sidebar--all-projects)))))
         (root (completing-read "Project directory (or enter another directory): " roots nil nil))
         (default-directory (codex-sidebar--directory root)))
    (unless (file-directory-p default-directory) (user-error "Directory does not exist"))
    (with-temp-buffer (setq default-directory (codex-sidebar--directory root)) (codex-sidebar-open))))

(defun codex-sidebar-activate ()
  "Open the chat or select the project on the current line."
  (interactive)
  (let ((entry (codex-sidebar--entry)))
    (pcase (plist-get entry :kind)
      ('thread (codex-sidebar--open-thread (plist-get entry :thread)))
      ('new (codex-sidebar-new-chat (plist-get entry :root)))
      ('project
       (setq codex-sidebar--selected (plist-get entry :root))
       (cl-pushnew codex-sidebar--selected codex-sidebar--expanded :test #'equal)
       (codex-sidebar--remember (plist-get entry :project)) (codex-sidebar--render)))))

(defun codex-sidebar-toggle-project ()
  "Expand or collapse the project on the current line."
  (interactive)
  (let* ((entry (codex-sidebar--entry)) (root (plist-get entry :root)))
    (when (eq (plist-get entry :kind) 'project)
      (if (member root codex-sidebar--expanded)
          (setq codex-sidebar--expanded (delete root codex-sidebar--expanded))
        (push root codex-sidebar--expanded))
      (codex-sidebar--save-state) (codex-sidebar--render))))

(defun codex-sidebar--terminal-exit (buffer _event)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq codex-sidebar--starting (delete codex-sidebar-project-root codex-sidebar--starting))))
  (codex-sidebar--schedule-refresh))

(defun codex-sidebar--terminal-sentinel (process event)
  ;; vterm's sentinel reads buffer-local settings, but can be called with an
  ;; unrelated buffer current.  Keep exited terminals available for diagnostics.
  (when-let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (vterm--sentinel process event)))))

(defun codex-sidebar--open-thread (thread &optional fresh)
  (require 'vterm)
  (let* ((id (codex-sidebar--get thread "id"))
         (root (codex-sidebar--thread-root thread))
         (existing (codex-sidebar--live-buffer id)))
    (unless (and root (file-directory-p root)) (user-error "Chat directory no longer exists: %s" root))
    (if existing
        (pop-to-buffer existing '((display-buffer-use-some-window display-buffer-pop-up-window)))
      (when-let ((old (gethash id codex-sidebar--terminals)))
        (when (buffer-live-p old) (kill-buffer old)))
      (let* ((buffer (generate-new-buffer
                      (format "*codex %s %s*" (file-name-nondirectory (directory-file-name root)) (substring id 0 (min 8 (length id))))))
             (command (append (list codex-sidebar-executable)
                              (unless fresh '("resume")) (list "--cd" root)
                              (and codex-sidebar-remote-endpoint (list "--remote" codex-sidebar-remote-endpoint))
                              codex-sidebar-cli-arguments (unless fresh (list id))))
             (vterm-shell (mapconcat #'shell-quote-argument command " "))
             (vterm-exit-functions (cons #'codex-sidebar--terminal-exit vterm-exit-functions))
             (vterm-kill-buffer-on-exit nil) (vterm-buffer-name-string nil))
        (pop-to-buffer buffer '((display-buffer-use-some-window display-buffer-pop-up-window)))
        (setq default-directory root)
        (vterm-mode)
        (setq-local codex-sidebar-thread-id id codex-sidebar-project-root root
                    vterm-kill-buffer-on-exit nil vterm-buffer-name-string nil
                    vterm-exit-functions (cl-adjoin #'codex-sidebar--terminal-exit vterm-exit-functions))
        (set-process-sentinel (get-buffer-process buffer) #'codex-sidebar--terminal-sentinel)
        ;; A very fast server may announce the ID while vterm is initializing.
        (setq id (gethash "id" thread))
        (setq codex-sidebar-thread-id id)
        (puthash id buffer codex-sidebar--terminals)))
    (codex-sidebar--render)))

(defun codex-sidebar-new-chat (&optional root)
  "Start a fresh CLI chat in ROOT, then adopt its app-server thread ID."
  (interactive)
  (setq root (or root codex-sidebar--selected))
  (unless (and root (file-directory-p root)) (user-error "Select an existing project first"))
  (setq root (codex-sidebar--directory root))
  (when (member root codex-sidebar--starting) (user-error "Codex is still starting for this project"))
  (push root codex-sidebar--starting)
  (codex-sidebar--request
   ;; Ensure our subscription is initialized before the CLI announces its chat.
   "thread/loaded/list" (make-hash-table :test #'equal)
   (lambda (_result)
     (let ((thread (make-hash-table :test #'equal)))
       (puthash "cwd" root thread)
       (puthash "id" (format "pending:%s:%s" (float-time) (cl-incf codex-sidebar--sequence)) thread)
       (push thread codex-sidebar--drafts)
       (push thread codex-sidebar--threads)
       (codex-sidebar--open-thread thread t)))
   (lambda (error-text)
     (setq codex-sidebar--starting (delete root codex-sidebar--starting))
     (codex-sidebar--error error-text))))

(defun codex-sidebar-search ()
  "Find a saved chat by title and project using completion."
  (interactive)
  (let* ((candidates
          (mapcar (lambda (thread)
                    (cons (format "%s  [%s]  %s" (codex-sidebar--title thread)
                                  (or (codex-sidebar--thread-root thread) "unknown directory")
                                  (codex-sidebar--get thread "id")) thread))
                  codex-sidebar--threads))
         (choice (completing-read "Codex chat: " candidates nil t)))
    (when-let ((thread (cdr (assoc choice candidates)))) (codex-sidebar--open-thread thread))))

(defun codex-sidebar-rename-chat ()
  "Rename the saved chat on the current line."
  (interactive)
  (let* ((thread (plist-get (codex-sidebar--entry) :thread))
         (id (codex-sidebar--get thread "id")))
    (unless id (user-error "Choose a chat"))
    (let ((name (read-string "Chat title: " (codex-sidebar--title thread))))
      (when (string-empty-p (string-trim name)) (user-error "Title cannot be empty"))
      (codex-sidebar--request "thread/name/set" `((threadId . ,id) (name . ,name))
                             (lambda (_result) (puthash "name" name thread) (codex-sidebar--render))))))

(defun codex-sidebar-archive-chat ()
  "Archive the chat on the current line, asking first."
  (interactive)
  (let* ((thread (plist-get (codex-sidebar--entry) :thread)) (id (codex-sidebar--get thread "id")))
    (unless id (user-error "Choose a chat"))
    (when (codex-sidebar--live-buffer id) (user-error "Close this chat's terminal before archiving"))
    (when (yes-or-no-p (format "Archive %s? " (codex-sidebar--title thread)))
      (if (cl-find id codex-sidebar--drafts :key (lambda (draft) (gethash "id" draft)) :test #'equal)
          (progn
            (setq codex-sidebar--drafts
                  (cl-remove id codex-sidebar--drafts :key (lambda (draft) (gethash "id" draft)) :test #'equal))
            (codex-sidebar-refresh))
        (codex-sidebar--request "thread/archive" `((threadId . ,id))
                               (lambda (_result) (codex-sidebar-refresh)))))))

(defvar codex-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'codex-sidebar-activate)
    (define-key map (kbd "TAB") #'codex-sidebar-toggle-project)
    (define-key map (kbd "n") #'codex-sidebar-new-chat)
    (define-key map (kbd "g") #'codex-sidebar-refresh)
    (define-key map (kbd "p") #'codex-sidebar-select-project)
    (define-key map (kbd "/") #'codex-sidebar-search)
    (define-key map (kbd "r") #'codex-sidebar-rename-chat)
    (define-key map (kbd "a") #'codex-sidebar-archive-chat)
    (define-key map [mouse-1] (lambda (event) (interactive "e") (mouse-set-point event) (codex-sidebar-activate)))
    map))

(define-derived-mode codex-sidebar-mode special-mode "Codex"
  "Navigate saved Codex chats grouped by Projectile project."
  (setq-local truncate-lines t cursor-type 'box))

(provide 'codex-sidebar)
;;; codex-sidebar.el ends here
