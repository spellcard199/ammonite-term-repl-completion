(toggle-frame-maximized)
(tool-bar-mode -1)

(require 'ammonite-term-repl)
(require 'ammonite-term-repl-completion)
(require 'org)

(add-to-list 'ammonite-term-repl-program-args "--no-remote-logging")

(defun scamm/org-cycle-plus-src-completion (&optional arg)
  "1 function --> 2 possible org-mode behaviors"
  (interactive "P")
  (if (org-in-src-block-p)
      (ammonite-term-repl-complete-indented-block-at-point)
    (org-cycle arg)))

(define-key
  org-mode-map
  (kbd "<tab>")
  'scamm/org-cycle-plus-src-completion)

(find-file (concat (file-name-directory (or load-file-name (buffer-file-name)))
                   "quick-start.org"))

(with-current-buffer "quick-start.org"
  (org-mode)
  (visual-line-mode))

(ammonite-term-repl)
