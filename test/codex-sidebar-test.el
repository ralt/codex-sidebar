;;; codex-sidebar-test.el --- Regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'codex-sidebar)

(defun codex-sidebar-test--object (&rest pairs)
  (let ((object (make-hash-table :test #'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) object)) object))

(defun codex-sidebar-test--thread (id root &optional name)
  (codex-sidebar-test--object "id" id "cwd" root "name" name "preview" ""))

(defmacro codex-sidebar-test--isolated (&rest body)
  (declare (indent 0))
  `(let* ((directory (make-temp-file "codex-sidebar-test-" t))
          (codex-sidebar-state-file (expand-file-name "state.eld" directory))
          (codex-sidebar--process nil) (codex-sidebar--ready nil)
          (codex-sidebar--pending (make-hash-table :test #'eql))
          (codex-sidebar--terminals (make-hash-table :test #'equal))
          (codex-sidebar--queue nil) (codex-sidebar--sequence 0)
          (codex-sidebar--threads nil) (codex-sidebar--drafts nil) (codex-sidebar--projects nil)
          (codex-sidebar--starting nil)
          (codex-sidebar--expanded nil) (codex-sidebar--selected nil)
          (codex-sidebar--state-loaded nil) (codex-sidebar--refresh-timer nil)
          (codex-sidebar--refreshing nil) (codex-sidebar--generation 0)
          (projectile-known-projects nil))
     (unwind-protect (progn ,@body)
       (when (process-live-p codex-sidebar--process)
         (set-process-sentinel codex-sidebar--process #'ignore)
         (delete-process codex-sidebar--process))
       (maphash (lambda (_id pending) (cancel-timer (nth 2 pending))) codex-sidebar--pending)
       (when (timerp codex-sidebar--refresh-timer) (cancel-timer codex-sidebar--refresh-timer))
       (when (get-buffer "*Codex*") (kill-buffer "*Codex*"))
       (delete-directory directory t))))

(ert-deftest codex-sidebar-projectile-root-wins-over-subdirectory ()
  (codex-sidebar-test--isolated
    (let ((default-directory (file-name-as-directory directory)))
      (cl-letf (((symbol-function 'projectile-project-root) (lambda () directory)))
        (let ((project (codex-sidebar--context)))
          (should (equal (plist-get project :root) (file-name-as-directory directory)))
          (should-not (plist-get project :temporary)))))))

(ert-deftest codex-sidebar-outside-project-is-a-temporary-directory ()
  (codex-sidebar-test--isolated
    (let ((default-directory (file-name-as-directory directory)))
      (cl-letf (((symbol-function 'projectile-project-root) (lambda () nil)))
        (let ((project (codex-sidebar--context)))
          (should (plist-get project :temporary))
          (should (equal (plist-get project :name) (file-name-nondirectory directory)))
          (should (equal (plist-get project :root) default-directory)))))))

(ert-deftest codex-sidebar-new-git-repo-does-not-use-stale-projectile-negative-cache ()
  (codex-sidebar-test--isolated
    (let* ((default-directory (file-name-as-directory directory))
           (projectile-project-root-cache (make-hash-table :test #'equal)))
      (make-directory (expand-file-name ".git" directory))
      (puthash (cons 'none default-directory) 'none projectile-project-root-cache)
      (should-not (plist-get (codex-sidebar--context) :temporary)))))

(ert-deftest codex-sidebar-state-survives-restart-without-transcripts ()
  (codex-sidebar-test--isolated
    (let ((project (list :root directory :name "temporary" :temporary t)))
      (codex-sidebar--remember project)
      (setq codex-sidebar--projects nil)
      (codex-sidebar--load-state)
      (should (equal codex-sidebar--projects (list project)))
      (should (= (file-modes codex-sidebar-state-file) #o600)))))

(ert-deftest codex-sidebar-identically-named-projects-remain-distinct ()
  (codex-sidebar-test--isolated
    (let ((a (expand-file-name "a/repo" directory)) (b (expand-file-name "b/repo" directory)))
      (make-directory a t) (make-directory b t)
      (setq projectile-known-projects (list a b))
      (should (= 2 (length (codex-sidebar--all-projects)))))))

(ert-deftest codex-sidebar-does-not-connect-to-tramp-projects ()
  (codex-sidebar-test--isolated
    (setq projectile-known-projects '("/ssh:example.invalid:/repo"))
    (let ((original (symbol-function 'file-directory-p)))
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (path) (if (string-prefix-p "/ssh:" path)
                                    (ert-fail "Contacted TRAMP")
                                  (funcall original path)))))
        (should-not (codex-sidebar--all-projects))))))

(ert-deftest codex-sidebar-json-filter-handles-partial-and-multiple-lines ()
  (let ((process (make-pipe-process :name "codex-sidebar-filter-test" :noquery t)) messages)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-sidebar--dispatch) (lambda (message) (push message messages))))
          (codex-sidebar--filter process "{\"id\":1,\"res")
          (should-not messages)
          (codex-sidebar--filter process "ult\":{}}\n{\"id\":2,\"result\":{}}\n")
          (should (equal '(2 1) (mapcar (lambda (m) (gethash "id" m)) messages)))
          (should (equal "" (process-get process 'partial))))
      (delete-process process))))

(ert-deftest codex-sidebar-rpc-errors-reach-caller-and-release-request ()
  (codex-sidebar-test--isolated
    (let ((timer (run-at-time 100 nil #'ignore)) error-text)
      (puthash 7 (list (lambda (_) (ert-fail "Unexpected success"))
                       (lambda (message) (setq error-text message)) timer)
               codex-sidebar--pending)
      (codex-sidebar--dispatch
       (codex-sidebar-test--object "id" 7 "error" (codex-sidebar-test--object "message" "No such thread")))
      (should (equal "No such thread" error-text))
      (should-not (gethash 7 codex-sidebar--pending)))))

(ert-deftest codex-sidebar-pagination-keeps-both-pages-in-order ()
  (codex-sidebar-test--isolated
    (let ((codex-sidebar--generation 9) (codex-sidebar-history-limit 500) (calls 0))
      (cl-letf (((symbol-function 'codex-sidebar--request)
                 (lambda (_method params success &optional _failure)
                   (cl-incf calls)
                   (funcall success
                            (if (= calls 1)
                                (codex-sidebar-test--object "data" '(first) "nextCursor" "page2")
                              (progn (should (equal (alist-get 'cursor params) "page2"))
                                     (codex-sidebar-test--object "data" '(second))))))))
        (codex-sidebar--fetch-page 9 nil nil)
        (should (= calls 2))
        (should (equal codex-sidebar--threads '(first second)))))))

(ert-deftest codex-sidebar-stale-refresh-cannot-replace-newer-history ()
  (codex-sidebar-test--isolated
    (let ((codex-sidebar--generation 10) (codex-sidebar--threads '(newer)))
      (cl-letf (((symbol-function 'codex-sidebar--request)
                 (lambda (_method _params success &optional _failure)
                   (funcall success (codex-sidebar-test--object "data" '(older))))))
        (codex-sidebar--fetch-page 9 nil nil)
        (should (equal codex-sidebar--threads '(newer)))))))

(ert-deftest codex-sidebar-render-preserves-chat-selection-on-refresh ()
  (codex-sidebar-test--isolated
    (let ((root (file-name-as-directory directory)))
      (setq codex-sidebar--threads (list (codex-sidebar-test--thread "thread-1" root "A title")))
      (with-current-buffer (get-buffer-create "*Codex*")
        (codex-sidebar-mode)
        (codex-sidebar--render)
        (goto-char (point-min)) (search-forward "A title") (beginning-of-line)
        (codex-sidebar--render)
        (should (equal "thread-1" (plist-get (codex-sidebar--entry) :id)))))))

(ert-deftest codex-sidebar-running-chat-reuses-the-same-terminal ()
  (codex-sidebar-test--isolated
    (let* ((buffer (generate-new-buffer "*codex-reuse-test*"))
           (process (make-pipe-process :name "codex-reuse" :buffer buffer :noquery t)) opened)
      (unwind-protect
          (progn
            (puthash "same-id" buffer codex-sidebar--terminals)
            (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (setq opened b))))
              (codex-sidebar--open-thread (codex-sidebar-test--thread "same-id" directory))
              (should (eq opened buffer))))
        (delete-process process) (kill-buffer buffer)))))

(ert-deftest codex-sidebar-exiting-chat-kills-only-its-terminal-buffer ()
  (require 'vterm)
  (codex-sidebar-test--isolated
    (let ((vterm-kill-buffer-on-exit nil)
          (root (file-name-as-directory directory))
          buffer process refreshed)
      (unwind-protect
          (cl-letf (((symbol-function 'vterm-mode)
                     (lambda ()
                       (setq buffer (current-buffer)
                             process (make-pipe-process :name "codex-exit-test"
                                                        :buffer buffer :noquery t))))
                    ((symbol-function 'codex-sidebar--schedule-refresh)
                     (lambda () (setq refreshed t))))
            (save-window-excursion
              (codex-sidebar--open-thread (codex-sidebar-test--thread "exit-id" root))
              (should (buffer-local-value 'vterm-kill-buffer-on-exit buffer))
              (setq codex-sidebar--starting (list root))
              ;; Emacs can invoke the sentinel while an unrelated buffer is current.
              (with-temp-buffer
                (delete-process process)
                (should (buffer-live-p (current-buffer))))
              (should-not (buffer-live-p buffer))
              (should-not (codex-sidebar--live-buffer "exit-id"))
              (should-not codex-sidebar--starting)
              (should refreshed)
              (should-not vterm-kill-buffer-on-exit)))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest codex-sidebar-new-chat-does-not-start-a-model-turn ()
  (codex-sidebar-test--isolated
    (let (opened method-sent)
      (cl-letf (((symbol-function 'codex-sidebar--request)
                 (lambda (method params success &optional _failure)
                   (setq method-sent method)
                   (should (hash-table-p params))
                   (funcall success (codex-sidebar-test--object "thread" (codex-sidebar-test--thread "new-id" directory)))))
                ((symbol-function 'codex-sidebar--open-thread) (lambda (thread fresh) (should fresh) (setq opened thread)))
                ((symbol-function 'codex-sidebar--schedule-refresh) #'ignore))
        (codex-sidebar-new-chat directory)
        (should (equal "thread/loaded/list" method-sent))
        (should (string-prefix-p "pending:" (gethash "id" opened)))))))

(ert-deftest codex-sidebar-missing-directory-does-not-launch-a-terminal ()
  (codex-sidebar-test--isolated
    (should-error
     (codex-sidebar--open-thread (codex-sidebar-test--thread "gone" (expand-file-name "missing" directory)))
     :type 'user-error)))

(defun codex-sidebar-test--rpc (method params)
  "Wait for an RPC only inside the isolated integration test."
  (let (done result failure (deadline (+ (float-time) 15)))
    (codex-sidebar--request method params
                           (lambda (value) (setq result value done t))
                           (lambda (error-text) (setq failure error-text done t)))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output codex-sidebar--process .05))
    (should done)
    (should-not failure)
    result))

(ert-deftest codex-sidebar-real-server-shares-empty-chat-across-connections ()
  (skip-unless (executable-find "codex"))
  (codex-sidebar-test--isolated
    (let* ((process-environment (cons (concat "CODEX_HOME=" directory) process-environment))
           (socket (expand-file-name "server.sock" directory))
           (codex-sidebar-server-arguments (list "app-server" "proxy" "--sock" socket))
           (server (make-process :name "codex-sidebar-private-test-server" :noquery t
                                 :command (list codex-sidebar-executable "app-server" "--listen"
                                                (concat "unix://" socket))
                                 :buffer nil :stderr (get-buffer-create " *codex-test-server*"))))
      (unwind-protect
      (progn
      (let ((deadline (+ (float-time) 10)))
        (while (and (not (file-exists-p socket)) (process-live-p server) (< (float-time) deadline))
          (accept-process-output server .05)))
      (should (file-exists-p socket))
      (let* ((created (codex-sidebar-test--rpc "thread/start" `((cwd . ,directory))))
             (id (gethash "id" (gethash "thread" created))))
        (should id)
        (codex-sidebar-test--rpc "thread/name/set" `((threadId . ,id) (name . "Persistent test chat")))
        (should-not (gethash "data"
                            (codex-sidebar-test--rpc "thread/list"
                                                    '((cursor . nil) (modelProviders . [])
                                                      (sourceKinds . ["cli" "vscode" "appServer" "unknown"])
                                                      (useStateDbOnly . t)))))
        ;; Reconnect the metadata client while the private server keeps the session.
        (set-process-sentinel codex-sidebar--process #'ignore)
        (delete-process codex-sidebar--process)
        (setq codex-sidebar--process nil codex-sidebar--ready nil)
        (let* ((result (codex-sidebar-test--rpc "thread/read" `((threadId . ,id))))
               (thread (gethash "thread" result)))
          (should (equal id (gethash "id" thread)))
          (should (equal "Persistent test chat" (gethash "name" thread))))))
       (when (process-live-p codex-sidebar--process)
         (set-process-sentinel codex-sidebar--process #'ignore)
         (delete-process codex-sidebar--process))
       (when (process-live-p server) (delete-process server))))))

(ert-deftest codex-sidebar-empty-drafts-survive-history-refresh ()
  (codex-sidebar-test--isolated
    (let* ((draft (codex-sidebar-test--thread "draft" directory))
           (codex-sidebar--drafts (list draft)) (codex-sidebar--generation 1))
      (cl-letf (((symbol-function 'codex-sidebar--request)
                 (lambda (_method _params success &optional _failure)
                   (funcall success (codex-sidebar-test--object "data" nil))))
                ((symbol-function 'codex-sidebar--live-buffer) (lambda (_id) t)))
        (codex-sidebar--fetch-page 1 nil nil)
        (should (equal (list draft) codex-sidebar--threads))))))

(ert-deftest codex-sidebar-websocket-decodes-large-split-unicode-frame ()
  (let* ((process (make-pipe-process :name "codex-sidebar-ws-test" :noquery t))
         (text (concat "résumé " (make-string 70000 ?x)))
         (payload (encode-coding-string (json-serialize `((id . 42) (result . ((text . ,text))))) 'utf-8))
         (frame (concat (unibyte-string 129 127) (codex-sidebar--bytes (length payload) 8) payload))
         messages)
    (unwind-protect
        (cl-letf (((symbol-function 'codex-sidebar--dispatch) (lambda (m) (push m messages))))
          (process-put process 'websocket t) (process-put process 'upgraded t)
          (codex-sidebar--wire-filter process (substring frame 0 7))
          (codex-sidebar--wire-filter process (substring frame 7 1000))
          (should-not messages)
          (codex-sidebar--wire-filter process (substring frame 1000))
          (should (= 1 (length messages)))
          (should (equal text (gethash "text" (gethash "result" (car messages))))))
      (delete-process process))))

(ert-deftest codex-sidebar-rpc-timeout-cleans-pending-request ()
  (codex-sidebar-test--isolated
    (let ((codex-sidebar-request-timeout .01) failure)
      (cl-letf (((symbol-function 'codex-sidebar--send) #'ignore))
        (codex-sidebar--rpc-now "thread/list" nil #'ignore (lambda (message) (setq failure message)))
        (should-not failure)
        (sleep-for .03)
        (should (string-match-p "timed out" failure))
        (should (= 0 (hash-table-count codex-sidebar--pending)))))))

(ert-deftest codex-sidebar-closed-empty-chats-disappear-on-refresh ()
  (codex-sidebar-test--isolated
    (setq codex-sidebar--drafts (list (codex-sidebar-test--thread "empty" directory))
          codex-sidebar--generation 1)
    (cl-letf (((symbol-function 'codex-sidebar--request)
               (lambda (_method _params success &optional _failure)
                 (funcall success (codex-sidebar-test--object "data" nil)))))
      (codex-sidebar--fetch-page 1 nil nil)
      (should-not codex-sidebar--drafts)
      (should-not codex-sidebar--threads))))

(ert-deftest codex-sidebar-adopts-native-chat-id-without-duplicating-terminal ()
  (codex-sidebar-test--isolated
    (let* ((root (file-name-as-directory directory))
           (draft (codex-sidebar-test--thread "pending:1" root))
           (buffer (generate-new-buffer "*codex-adoption-test*")))
      (unwind-protect
          (progn
            (setq codex-sidebar--drafts (list draft) codex-sidebar--starting (list root))
            (puthash "pending:1" buffer codex-sidebar--terminals)
            (codex-sidebar--adopt-thread (codex-sidebar-test--thread "server-id" root))
            (should (equal "server-id" (gethash "id" draft)))
            (should-not (gethash "pending:1" codex-sidebar--terminals))
            (should (eq buffer (gethash "server-id" codex-sidebar--terminals)))
            (should (equal "server-id" (buffer-local-value 'codex-sidebar-thread-id buffer)))
            (should-not codex-sidebar--starting))
        (kill-buffer buffer)))))

;;; codex-sidebar-test.el ends here
