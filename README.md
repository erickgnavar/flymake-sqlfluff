# flymake-sqlfluff

Flymake plugin to run a linter for SQL buffers using [sqlfluff](https://www.sqlfluff.com)

## Installation

### Cloning the repo

Clone this repo somewhere, and add this to your config:

```elisp
(add-to-list 'load-path "path where the repo was cloned")

(require 'flymake-sqlfluff)
(add-hook 'sql-mode-hook #'flymake-sqlfluff-load)
```

### Using straight.el

```emacs-lisp
(use-package flymake-sqlfluff
  :straight (flymake-sqlfluff
             :type git
             :host github
             :repo "erickgnavar/flymake-sqlfluff"))
```
