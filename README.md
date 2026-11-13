# codex-sidebar

A project and recent-chat sidebar for Codex CLI conversations in Emacs vterm.
Projectile selects the current repository; outside a project, the current
directory becomes a temporary project named after that directory.

Chat history stays in Codex's local store. A background client connects to Codex's
shared app-server daemon over its local WebSocket proxy. It reads metadata and
tracks chats started by a normal `codex` terminal. Saved chats reopen with
`codex resume` in vterm. No separate inference client, copied
transcripts, SQLite scraping, or API key is needed beyond normal Codex CLI login.

## Requirements

- Emacs 29.1 or newer, Projectile, and a working vterm module.
- Codex CLI on Emacs's `exec-path`, signed in using your normal setup.
- The app-server methods `thread/list`, `thread/loaded/list`, `thread/name/set`, and
  `thread/archive`. Developed against Codex CLI 0.167.0; this protocol can evolve.

## Install

Put this checkout on `load-path` and bind the entry point:

```elisp
(use-package codex-sidebar
  :load-path "~/Git/github.com/ralt/codex-sidebar"
  :commands codex-sidebar-open
  :bind ("C-x c" . codex-sidebar-open))
```

`M-x codex-sidebar-open` selects the current Projectile repository and opens its
group on the left. Press `n` for a new chat or Enter on an existing one. Selecting
a live chat reuses its buffer; selecting a closed chat resumes its saved ID in a
new vterm. Buffers are identified by chat ID, so multiple conversations in one
project are independent.

Opening a new, resumed, or already running chat automatically adds its enclosing
Git repository to Projectile, so it appears in your project switcher even if you
never open a file. Ordinary directories are left alone; Git worktrees are supported.

## Sidebar keys

| Key | Action |
|---|---|
| Enter / click | Select project or open/resume chat |
| TAB | Expand/collapse project |
| n | New chat in selected project |
| p | Select a Projectile project or enter another directory |
| g | Refresh history / reconnect after an error |
| / | Search saved chats using Emacs completion |
| r | Rename chat |
| a | Archive chat, with confirmation; close its terminal first |
| q | Hide sidebar |

`●` means an Emacs terminal for this chat is still running, not necessarily that
the model is working. Normal Codex permissions, approvals, model settings, and
authentication remain in the terminal. Exiting Codex automatically kills its
terminal buffer. Closing a vterm does not delete history.
Empty tabs stay visible while their terminal is running and disappear when it
closes. Codex indexes/persists a new
conversation after its first user message. No prompt is sent by New Chat itself.

History refreshes on opening/focusing the sidebar, terminal exit, applicable
metadata events, or `g`; there is no repeating polling timer. Titles/status from
another client may need a refresh. The default history limit is 500 chats.

## Configuration

- `codex-sidebar-executable`: CLI name or absolute executable path.
- `codex-sidebar-cli-arguments`: extra arguments passed to interactive Codex.
- `codex-sidebar-remote-endpoint`: shared server endpoint (default `unix://`).
- `codex-sidebar-width`: sidebar columns (default 42).
- `codex-sidebar-history-limit` and `codex-sidebar-recent-count`: history size.
- `codex-sidebar-request-timeout`: metadata RPC deadline.
- `codex-sidebar-project-open-hook`: functions called with the chat directory
  after opening a chat. Defaults to `codex-sidebar-register-project`; set it to
  `nil` to disable automatic Projectile registration.
- `codex-sidebar-state-file`: project/group state, defaulting to
  `~/.emacs.d/codex-sidebar-state.eld`. It does not contain transcripts or tokens.
- `codex-sidebar-server-arguments`: metadata server invocation, if your CLI
  requires a different transport setup.

Projects are grouped by canonical directory. Git worktrees remain separate
projects, and identical folder names in different locations stay distinct.
Deleted project directories cannot be resumed until restored. TRAMP projects are
not currently supported. Use the same `CODEX_HOME` environment in the app-server
and terminal to see the same saved chats.

Errors appear in the sidebar. Server stderr is available in
`*Codex sidebar server log*`. Requests are asynchronous and have deadlines;
failure does not erase the last successfully loaded chat list.

## Development

```sh
make check
```

This byte-compiles with warnings as errors and runs ERT regression checks. The
tests do not send prompts to a model. App-server documentation:
https://learn.chatgpt.com/docs/app-server
