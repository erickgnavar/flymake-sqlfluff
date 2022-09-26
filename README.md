# flymake-sqlfluff

[![MELPA](https://melpa.org/packages/flymake-sqlfluff-badge.svg)](https://melpa.org/#/flymake-sqlfluff)

Flymake plugin to run a linter for SQL buffers using [sqlfluff](https://www.sqlfluff.com)

## Installation

### Cloning the repo

Clone this repo somewhere, and add this to your config:

```elisp
(add-to-list 'load-path "path where the repo was cloned")

(require 'flymake-sqlfluff)
(add-hook 'sql-mode-hook #'flymake-sqlfluff-load)
```

### Using use-package

```emacs-lisp
(use-package flymake-sqlfluff
  :ensure t)
```

### Using straight.el

```emacs-lisp
(use-package flymake-sqlfluff
  :straight (flymake-sqlfluff
             :type git
             :host github
             :repo "erickgnavar/flymake-sqlfluff"))
```
