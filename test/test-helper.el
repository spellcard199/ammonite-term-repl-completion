;;; test-helper.el --- Helpers for ammonite-term-repl-completion-test.el

(require 'ammonite-term-repl)

(defvar test/ammonite-term-repl-eval--proc-output "")

(defun test/ammonite-term-repl-eval--proc-filter (process str)
  (setq test/ammonite-term-repl-eval--proc-output
        (concat test/ammonite-term-repl-eval--proc-output
                (substring-no-properties
                 (ansi-color-filter-apply str))))
  (term-emulate-terminal process str))

(defun test/ammonite-term-repl-eval--proc-filter--get-kept ()
  test/ammonite-term-repl-eval--proc-output)

(defun test/ammonite-term-repl-eval--proc-filter--reset-kept ()
  (setq test/ammonite-term-repl-eval--proc-output ""))

(defun test/start-ammonite ()

  (add-to-list 'ammonite-term-repl-program-args "--no-remote-logging")
  (ammonite-term-repl)

  (let* ((amm-proc (get-buffer-process
                    ammonite-term-repl-buffer-name))
         (default-amm-proc-filter (process-filter amm-proc))
         (ready-comment "/*ready*/"))
    (unwind-protect
        (progn
          (set-process-filter amm-proc
                              'test/ammonite-term-repl-eval--proc-filter)
          ;; Sending custom string to detect when prompt is ready in a
          ;; "prompt-agnostic" way.
          (test/ammonite-term-repl-eval--proc-filter--reset-kept)
          (comint-send-string ammonite-term-repl-buffer-name
                              ready-comment)
          (while (not
                  (with-current-buffer ammonite-term-repl-buffer-name
                    ;; (print (test/ammonite-term-repl-eval--proc-filter--get-kept))
                    (string-suffix-p
                     (concat ready-comment " ")
                     (test/ammonite-term-repl-eval--proc-filter--get-kept))))
            (sleep-for 0.1))
          ;; Clear previous input, so we are ready to send other things.
          (comint-send-string ammonite-term-repl-buffer-name "\C-a\C-u\C-k")
          )

      ;; Restore default process filter
      (set-process-filter amm-proc
                          default-amm-proc-filter)
      )))

(message "Starting Ammonite...")
(test/start-ammonite)
(message "Ammonite started.")

;;; test-helper.el ends here
