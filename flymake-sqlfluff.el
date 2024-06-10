;;; flymake-sqlfluff.el --- A flymake plugin for SQL files using sqlfluff -*- lexical-binding: t -*-

;; Copyright © 2022 Erick Navarro
;; Author: Erick Navarro <erick@navarro.io>
;; URL: https://github.com/erickgnavar/flymake-sqlfluff
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Usage:
;;   (require 'flymake-sqlfluff)
;;   (add-hook 'sql-mode-hook #'flymake-sqlfluff-load)

;;; Code:

(defcustom flymake-sqlfluff-program "sqlfluff"
  "Path to program sqlfluff."
  :group 'flymake-sqlfluff
  :type 'string)

(defvar flymake-sqlfluff--dialect-options '("ansi" "athena" "bigquery" "clickhouse" "databricks" "db2" "exasol" "hive" "mysql" "oracle" "postgres" "redshift" "snowflake" "soql" "sparksql" "sqlite" "teradata" "tsql")
  "List of supported dialects.")

(defcustom flymake-sqlfluff-dialect "postgres"
  "List of possible dialect to be checked."
  :group 'flymake-sqlfluff
  :options flymake-sqlfluff--dialect-options
  :type 'list)

(defvar-local flymake-sqlfluff--flymake-proc nil)

;;;###autoload
(defun flymake-sqlfluff-change-dialect ()
  "Change sqlfluff dialect."
  (interactive)
  (setq flymake-sqlfluff-dialect (completing-read "Choose dialect: " flymake-sqlfluff--dialect-options)))

;;;###autoload
(defun flymake-sqlfluff-load ()
  "Load hook for the current buffer to tell flymake to run checker."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-sqlfluff--run-checker nil t))

(defun flymake-sqlfluff--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."

  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `flymake-sqlfluff--flymake-proc' to a different value
  ;;
  (when (process-live-p flymake-sqlfluff--flymake-proc)
    (kill-process flymake-sqlfluff--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `flymake-sqlfluff--flymake-proc' process to a new process
      ;; calling the sqlfluff.
      ;;
      (setq
       flymake-sqlfluff--flymake-proc
       (make-process
        :name "flymake-sqlfluff" :noquery t :connection-type 'pipe
        ;; Make output go to a temporary buffer.
        ;;
        :buffer (generate-new-buffer (concat "*" (make-temp-name "flymake-sqlfluff-") "*"))
        :command (list
                  (shell-quote-argument flymake-sqlfluff-program)
                  "lint"
                  "--dialect"
                  (shell-quote-argument flymake-sqlfluff-dialect)
                  "--format"
                  "json"
                  buffer-file-name)
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          ;;
          (if (memq (process-status proc) '(exit signal))
              (unwind-protect
                  ;; Only proceed if `proc' is the same as
                  ;; `flymake-sqlfluff--flymake-proc', which indicates that `proc'
                  ;; is not an obsolete process.
                  ;;
                  (if (with-current-buffer source (eq proc flymake-sqlfluff--flymake-proc))
                      (with-current-buffer source
                        ;; Parse the output buffer for diagnostic's messages and
                        ;; locations, collect them in a list of objects, and call
                        ;; `report-fn'.
                        ;;
                        (funcall report-fn
                                 (flatten-list
                                  (mapcar (lambda (node)
                                            (mapcar #'flymake-sqlfluff-process-item
                                                    (gethash "violations" node)))
                                          (json-parse-string
                                           (with-current-buffer (process-buffer proc)
                                             (buffer-substring-no-properties (point-min) (point-max)))))))))
                ;; Cleanup the temporary buffer used to hold the
                ;; check's output.
                ;;
                (kill-buffer (process-buffer proc)))))))
      ;; Send the buffer contents to the process's stdin, followed by
      ;; an EOF.
      ;;
      (process-send-region flymake-sqlfluff--flymake-proc (point-min) (point-max))
      (process-send-eof flymake-sqlfluff--flymake-proc))))

(defun flymake-sqlfluff-process-item (item)
  "Build a flymake diagnostic using ITEM data."
  (let* ((line (gethash "line_no" item))
         (column (gethash "line_pos" item))
         (code (gethash "code" item))
         (region (flymake-diag-region (current-buffer) line column))
         (description (gethash "description" item))
         (type (if (string= code "PRS") :error :warning)))
    (flymake-make-diagnostic (current-buffer) (car region) (cdr region) type (concat code ": " description))))

(provide 'flymake-sqlfluff)
;;; flymake-sqlfluff.el ends here
