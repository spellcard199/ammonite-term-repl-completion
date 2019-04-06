;;; ammonite-term-repl-term-clear.el --- Try to clear repl running in a term buffer, either deleting backward characters or getting a new prompr. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 spellcard199

;; Author: spellcard199 <spellcard199@protonmail.com>
;; URL: https://github.com/spellcard199/ammonite-term-repl-completion
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (ammonite-term-repl "0.1"))
;; Keywords: ammonite, term, repl, clear, scala

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'ammonite-term-repl)

;;;; Variables

(defvar term-clear--proc-output ""
  "Used by `term-clear--proc-filter' to stores process output.")
(defvar term-clear--proc-output-updated nil
  "Set to `t' by `term-clear--proc-filter' when it is called.")

;;;; Functions

(defun term-clear--proc-filter (process str)
  (setq term-clear--proc-output-updated t)
  (setq term-clear--proc-output
        (concat term-clear--proc-output
                (substring-no-properties
                 (ansi-color-filter-apply str))))
  (term-emulate-terminal process str))

(defun term-clear--proc-filter--reset-kept ()
  (setq term-clear--proc-output ""))

(defun term-clear--proc-filter--get-kept ()
  term-clear--proc-output)

(defun term-clear--proc-filter--output-updated-p ()
  term-clear--proc-output-updated)

(defun term-clear--proc-filter--output-updated-setnil ()
  (setq term-clear--proc-output-updated nil))

(defun term-clear--input-by-lines (term-buf)
  ;; TODO: with this (new) implementation `prompt' is not used and
  ;; needed. Think about if this is positive (generalizable) or
  ;; negative thing.
  (let* ((proc (get-buffer-process term-buf))
         (old-proc-filter (process-filter proc))
         (_ (term-clear--proc-filter--reset-kept))
         (timeout)
         (last-kept (term-clear--proc-filter--get-kept))
         (last-3-outputs '("0" "1" "2"))
         (last-output))

    (unwind-protect
        (progn
          (set-process-filter
           (get-buffer-process term-buf)
           #'term-clear--proc-filter)

          ;; Decide what the timeout should be basing on repl
          ;; responsiveness.
          (let ((start-time (current-time))
                (elapsed))
            (term-clear--proc-filter--output-updated-setnil)
            (comint-send-string term-buf " \b")
            (while (not (term-clear--proc-filter--output-updated-p))
              (sleep-for 0.000001))
            (setq elapsed (time-since start-time))
            ;; Set timeout to ~ 100 times response time in this test.
            (setq timeout (mapcar (lambda (x) (* 100 x))
                                  elapsed)))
          
          (with-current-buffer term-buf
            (while
                (or
                 (progn (setq last-3-outputs-debug last-3-outputs)
                        nil)
                 (not (string-equal (nth 0 last-3-outputs)
                                    (nth 1 last-3-outputs)))
                 (not (string-equal (nth 0 last-3-outputs)
                                    (nth 2 last-3-outputs))))

              (term-clear--proc-filter--output-updated-setnil)
              ;; Send many kill lines at a time, to avoid waiting for
              ;; each one.
              (comint-send-string
               (current-buffer)
               ;; 40 ^C-a^C-k ~= kill 20 non-empty lines.
               ;; Using
               ;; \C-a\C-k instead of \C-u because:
               ;; - in some repls, like kawa : \C-u doesn't go to
               ;;   previous line when current is empty
               ;; - in other repls, like ammonite : it's C-a that
               ;;   doesn't go to previous line when current is empty
               (concat
                "\C-k" ; if cursor is not at the end of line
                "\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a"
                "\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a"
                "\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a"
                "\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a\C-u\C-a"))

              ;; Wait either for term to print change or 1 second
              ;; without any update: the second case is because in
              ;; certain terminals, like sbt console, if deleting
              ;; doesn't actually delete something it just doesn't
              ;; output anything.
              ;; The reason timeout is so short is that we are
              ;; interested in when output _starts_ arriving, which
              ;; usually is pretty fast.
              ;; In other words : there are 2 ways to exit from this
              ;; loop:
              ;; - term produces some output: this becomes false:
              ;;   (not (term-clear--proc-filter--output-updated-p))
              ;; - timeout is reached: this becomes false:
              ;;   (time-less-p elapsed timeout)
              (let ((start-time (current-time)))
                (while (and
                        (not
                         (term-clear--proc-filter--output-updated-p))
                        (let ((elapsed (time-since start-time)))
                          (time-less-p elapsed timeout)))
                  (sleep-for 0.000001)))

              (setq last-output
                    (seq-subseq
                     (term-clear--proc-filter--get-kept)
                     (length last-kept)))
              (setq last-kept
                    (term-clear--proc-filter--get-kept))
              (setq last-3-outputs
                    (cons last-output
                          (butlast last-3-outputs))))))

      (set-process-filter proc old-proc-filter))))

(defun ammonite-term-repl--term-clear--input-by-lines ()

  ;; TODO: consider if it's better to clear previous input
  ;; line-by-line or if it's better to send an interrupt ("\C-c") to
  ;; get a new prompt, since ammonite can handle it.

  ;; TODO: `term-clear--input-by-lines' is a function that tries to
  ;; clear repls line by line in a "prompt-agnostic" way. However in
  ;; amonite wnen you try deleting things on a clean prompt it
  ;; re-prints the prompt : probably in this case an ammonite-specific
  ;; function should be preferred.
  ;; Something like:
  ;; (while (not (string-suffix-p
  ;;              (concat prompt " ")
  ;;              (term-clear--proc-filter--get-kept)))
  ;;  (sleep-for ...))
  (term-clear--input-by-lines
   ammonite-term-repl-buffer-name))

;;;; Footer

(provide 'ammonite-term-repl-term-clear)

;;; ammonite-term-repl-term-clear.el ends here
