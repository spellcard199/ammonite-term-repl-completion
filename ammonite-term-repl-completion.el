;;; ammonite-term-repl-completion.el --- Add completion support to ammonite-term-repl.el -*- lexical-binding: t; -*-

;; Copyright (C) 2019 spellcard199

;; Author: spellcard199 <spellcard199@protonmail.com>
;; URL: https://github.com/spellcard199/ammonite-term-repl-completion
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (ammonite-term-repl "0.1"))
;; Keywords: ammonite, term, repl, completion, scala

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package should use the term buffer used by
;; `ammonite-term-repl.el' to offer Ammonite completions in non-term
;; buffers.
;; How it works:
;; 1. Get a new clear prompt
;; 2. Send string to complete
;; 3. Send Tab
;; 4. Read and parse ammonite output:
;;   1. Trim lines after completions
;;   2. Trim lines before signatures/completions
;;   3. Get signatures if present
;;   4. Parse completion lines into a list of completion candidates
;; 7. Use resulting list as collection for `completing-read' or
;;    equivalent
;; Very, very alpha atm.

;; TODO: fix completion insertion logic to include other scala
;; separators.

;; TODO: decide what relation should this package have with
;; ammonite-term-repl.el. Should this develop into it's own mode or
;; should it use ammmonite-term-repl.el key map?

;; TODO: think about things that is reasonable to complete.

;; TODO: make it possible to choose completion backend.

;; TODO: think about key bindings.

;;;; Installation

;;;;; MELPA

;; TODO
;; Not yet.

;;;;; Manual

;; Install these required packages:

;; + ammonite-term-repl

;; Clone this repository:

;; git clone https://github.com/spellcard199/ammonite-term-repl-completion

;; Then add to load path the directory that contains this file:

;; (add-to-list 'load-path <path-of-dir-containing-this-file>)

;; And then put this in your init file:

;; (require 'ammonite-term-repl-completion)

;;;; Usage

;; Commands that are currently tought for interactive use are:
;; - `ammonite-term-repl-complete-indented-block-at-point'

;; Functions that are tought to be used for custom scripts are the
;; ones without double hyphens, so:
;; - `ammonite-term-repl-for-string-get': gets completions from repl
;;   calling internal parsing functions
;; - `ammonite-term-repl-for-string-choose': calls
;;   `ammonite-term-repl-for-string-get' and completes (TODO: actually
;;   work-in-progress) with the choice user picked

;;;; Tips

;; + You can custmize key binding for
;;   `ammonite-term-repl-complete-indented-block-at-point'
;;   since there is not a default yet

;;;; Credits

;; TODO

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
(require 'ammonite-term-repl-term-clear)

(require 'seq)
(require 'cl)
(require 'subr-x)

;;;; Variables

;;; UNUSED: if needed, it will used by
;;; `ammonite-term-repl-compl--get-completion-prefix'. Not commenting
;;; out so I don't forget to update
;;; `ammonite-term-repl-compl--proc-filter--reset-kept'
(defvar ammonite-term-repl-compl--proc-output-ansi "")

(defvar ammonite-term-repl-compl--proc-output
  "`ammonite-term-repl-compl--proc-filter' stores term output here")

(defvar ammonite-term-repl-compl-silent-repl nil
  "When t Ammonite completion output is filtered out and therefore hidden.")

;;;; Functions

;;; UNUSED
;; (defun ammonite-term-repl--mk-indented (scala-code)
;;   (replace-regexp-in-string "\n"
;;                             "\n  "
;;                             scala-code))

;;; UNUSED
;; (defun ammonite-term-repl--mk-indented (scala-code)
;;   (replace-regexp-in-string "\n"
;;                             "{\n  "
;; scala-code))

;;; UNUSED
;; (defun ammonite-term-repl-compl--parse--trim-first-n-lines
;;     (seq-of-lines n-of-lines-to-trim)
;;   (seq-subseq
;;    seq-of-lines
;;    n-of-lines-to-trim))

;;; UNUSED
(defun ammonite-term-repl-compl--parse--get-autocompleted
    (to-complete
     amm-output
     end-of-output-comment)
  "
`to-complete': code we are asking completion for.
`amm-output': ammonite output after sending Tab.
`end-of-output-comment': suffix that gets trimmed."

  (with-temp-buffer
    (insert (string-trim-right
             (string-trim amm-output)
             (regexp-quote end-of-output-comment)))
    (while (not (string-suffix-p to-complete
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point))))
      (backward-char))
    (buffer-substring-no-properties (point)
                                    (point-max))))

(defun ammonite-term-repl-compl--parse--trim-last-n-lines
    (seq-of-lines n-of-lines-to-trim)
  (seq-subseq
   seq-of-lines
   0
   (- (length seq-of-lines)
      n-of-lines-to-trim)))

;;; UNUSED
;; (defun ammonite-term-repl-compl--parse--trim-first-and-last-n-lines
;;     (seq-of-lines
;;      n-of-lines-to-trim-left
;;      n-of-lines-to-trim-right)
;;   (ammonite-term-repl-compl--parse--trim-last-n-lines
;;    (ammonite-term-repl-compl--parse--trim-first-n-lines
;;     seq-of-lines
;;     n-of-lines-to-trim-left)
;;    n-of-lines-to-trim-right))

(defun ammonite-term-repl-compl--parse--trim-indented-lines-left
    (seq-of-lines)
  (seq-drop-while (lambda (line)
                    (string-prefix-p " " line))
                  seq-of-lines))

(defun ammonite-term-repl-compl--parse--sig->name (sig)
  (let* (;; Remove prefix
         (no-prefix (thread-last sig
                         (string-remove-prefix "abstract " )
                         (string-remove-prefix "override " )
                         (string-remove-prefix "def " )
                         (string-remove-prefix "class " )
                         (string-remove-prefix "object " )
                         (string-remove-prefix "trait " )))
         ;; Remove suffix:
         ;; - for methods: name is separated from arglist by "(" and
         ;;   from return type by ":"
         ;; - for classes, etc... : name is separated by "extends" or
         ;;   other words by a white space
         (name (car (split-string no-prefix "[(: ]"))))
    name))

(defun ammonite-term-repl-compl--parse--separate-sig-from-completion-lines
    (sig-and-completion-lines)
  (let* ((maybe-sig-p
          (lambda (line)
            ;; Signatures must contain:
            ;; - no starting whitespaces
            ;; - no consecutive spaces
            ;; - at least 1 space
            ;; (false positive: ammonite echoing unindented input:
            ;; handled in
            ;; `ammonite-term-repl-compl--parse--amm-output->sig-cons-compl-lines'
            ;; at
            ;; `ammonite-term-repl-compl--parse--trim-last-n-lines').
            (and (not (string-prefix-p " " line))
                 (not (string-match-p "  " line))
                 (string-match-p " " line))))

         (maybe-sig-line-p
          (lambda (line)
            (let ((possible-sigs (split-string line "  " t)))
              (not
               (member nil
                       (mapcar (lambda (x) (funcall maybe-sig-p x))
                               possible-sigs))))))

         (maybe-compl-line-p
          ;; Completion Lines: contain either 0 or multiple
          ;; consecutive spaces ;; (false positive: ammonite echoing
          ;; indented input: handled in
          ;; `ammonite-term-repl-compl--parse--amm-output->sig-cons-compl-lines'
          ;; at
          ;; `ammonite-term-repl-compl--parse--trim-indented-lines-left').
          (lambda (line)
            (or (string-match-p "  " line)
                (not (string-match-p "[ ]" line)))))

         ;; Get signatures...
         (signature-lines-with-eol
          (seq-take-while
           (lambda (line)
             (funcall maybe-sig-line-p line))
           sig-and-completion-lines))

         ;; Completion lines are defined here as : non-signature lines
         ;; after signatures
         (completion-lines-with-eol
          (seq-drop-while
           (lambda (line)
             (member line signature-lines-with-eol))
           sig-and-completion-lines))

         ;; Remove trailing ""
         (signature-lines
          (mapcar (lambda (line)
                    (string-remove-suffix "" line))
                  signature-lines-with-eol))

         ;; Remove trailing ""
         (completion-lines
          (mapcar (lambda (line)
                    (string-remove-suffix "" line))
                  completion-lines-with-eol)))

    `(,signature-lines . ,completion-lines)))

(defun ammonite-term-repl-compl--parse--amm-output->sig-cons-compl-lines
    (amm-output to-complete)
  (let* ((lines (split-string amm-output "\n" t))

         (n-of-lines-of-to-complete
          (length
           (split-string to-complete "\n")))

         ;; Trim input lines before and after signatures and
         ;; completions.

         ;; Trim input lines after completions: The same input we
         ;; inserted is redisplayed by ammonite after completions, +/-
         ;; ammonite autocompletion (which wouldn't contain a
         ;; newline): so we know we have to trim the number of lines
         ;; of `to-complete'
         (lines-without-reinserted-input
          (ammonite-term-repl-compl--parse--trim-last-n-lines
           lines
           n-of-lines-of-to-complete))

         ;; Trim input lines before completions: depending on how you
         ;; implemented `ammonite-term-repl-compl-for-string-get' you
         ;; could get either:
         ;; - an empty line
         ;; - input you are asking completion for
         ;; To cover also for the second case here we are:
         ;; 1. Skipping the first line :
         ;;    - Surely it doesn't contain ammonite output: it's
         ;;      either blank or contains our input
         ;;    - Skipping makes identifying subsequent indented lines
         ;;      immediate: if present these are multi-line input:
         ;;      to be trimmed
         ;; 2. Skipping indented lines after the first: we know that
         ;;    both signature and completion lines are not indented, so
         ;;    these must be part of a multi-line input
         (signature-and-completion-lines
          (ammonite-term-repl-compl--parse--trim-indented-lines-left
           (cdr lines-without-reinserted-input)))

         ;; Split the mono-list of signature and completion lines in 2
         ;; separate lists
         (sig-cons-compl-lines
          (ammonite-term-repl-compl--parse--separate-sig-from-completion-lines
           signature-and-completion-lines)))

    sig-cons-compl-lines))

(defun ammonite-term-repl-compl--parse--completion-lines->sorted-candidates
    (completion-lines)
  (let* (;; `splitted-completion-lines' is a list of lists: inner lists
         ;; are the splitted lines printed by ammonite completion.
         ;; Therefore strings inside the inner lists are completion
         ;; candidates.
         (splitted-completion-lines
          (mapcar (lambda (line)
                    (split-string line "\s" t))
                  completion-lines))

         ;; Counting number of columns: used for sorting below at the
         ;; `transposed-list' let binding.  If we fail to recognize
         ;; the correct number of columns, candidates in the
         ;; un-counted columns are lost
         ;; TODO: giving an excess number of columns seems to work
         ;; anyway. Is it true? If yes, should we just give an
         ;; arbitrary, relatively large number instead of counting?
         (cols-per-line (mapcar 'length splitted-completion-lines))
         ;; The if on `cols-per-line' accounts for the case in which
         ;; there are no completions:
         ;; 1. completion-lines: is empty
         ;; 2. cols-per-line: is empty
         ;; 3. `max' applied to an empty list raises an error
         (n-of-cols (if cols-per-line
                        (apply 'max cols-per-line)
                      0))

         ;; `transposed-list' is a list of lists: inner lists are the
         ;; columns printed by ammonite completion.
         ;; (Note-for-self: How I found out this: starting with
         ;; splitted-completion-lines and wrapping successive results)
         (transposed-list (mapcar
                           (lambda (col-number)
                             (mapcar
                              (lambda (row-splitted)
                                (elt row-splitted col-number))
                              splitted-completion-lines))
                           (number-sequence 0 (- n-of-cols 1))))

         ;; Concatenate list of columns: now ammonite's alphabetical
         ;; order should be rebuilt.
         (sorted-candidates-plus-nils (apply 'append transposed-list))

         ;; Remove nils introduced during transposition.
         ;; Last column may not have the same length of the others:
         ;; during transposition nil values get introduced.
         (sorted-candidates (remove-if
                             (lambda (candidate)
                               (equalp candidate nil))
                             sorted-candidates-plus-nils)))
    sorted-candidates))

;; FIXME: Annotation
;; (ammonite-term-repl-compl--parse :: String -> String -> aList)

;; TODO : FIX parameters in tests (quantity)
(defun ammonite-term-repl-compl--parse
    (to-complete
     ;; amm-output-ansi ; Consider if we should use this to get better
     ;; prefix detection when completing?
     amm-output
     end-of-output-comment)
  "This function work is splitted in helper functions because it's
length would be an obstacle to reading the general flow of what's
happening, which is pretty obvious: takes ammonite output after a tab
completion and returns signatures (if present) and completions.

Helper function names start with the same prefix as this one.

The reason the whole work requires all these steps is that parsing
plain text in this way is a duct-taped solution waiting to be replaced
by a better one (maybe something like LSP or a scala nrepl).

`end-of-output-comment' is used only to get autocompletion that
Ammonite inserts if it detects that there is only one possibile
candidate.

Example: For this ammonite output...

    
    def scamm(s: String) : String
    def scamm(n: Integer): String
    Bar   Dog
    Cat   Foo
    @amm > complete.
             me.scamm

... this function should return:

    ((:signatures  . (\"def scamm(s:String): String\"
                      \"def scamm(n:Integer): String\"))
     (:completions . (\"Bar\" \"Cat\" \"Dog\" \"Foo\"))
"

  ;; Ammonite output for completion should be something like:
  ;; 1. (prompt)
  ;; 2. (input we want completion for)
  ;; 3. +/- *TYPE SIGNATURES* : we want these if present
  ;; 4. *COMPLETIONS*         : we want these
  ;; 5. prompt
  ;; 6. our input again: if the completion is unique ammonite
  ;; autocompletes it
  ;;
  ;; For some reason (I haven't understood yet) the way I have written
  ;; `ammonite-term-repl-compl-for-string-get' now omits the first 2 thing of
  ;; this list: this is something that has to be considered in
  ;; `ammonite-term-repl-compl--parse--amm-output->sig-cons-compl-lines',
  ;; at `n-of-lines-to-trim-left'.

  (let* (;; Currently UNUSED
         ;; notes/errors that user may be interested into
         (parsing-notes nil)

         ;; Fix for batch mode: for some reason a newline gets
         ;; inserted after prompt in batch mode (eg tests run with
         ;; ert-runner)
         (amm-output (replace-regexp-in-string
                      (regexp-quote
                       (concat "\n" to-complete))
                      (concat " " to-complete)
                      amm-output
                      nil
                      'literal))

         ;;; UNUSED
         ;; (autocompleted
         ;;  (ammonite-term-repl-compl--parse--get-autocompleted
         ;;   to-complete
         ;;   amm-output
         ;;   end-of-output-comment))

         (sig-cons-compl-lines
          (ammonite-term-repl-compl--parse--amm-output->sig-cons-compl-lines
           amm-output
           to-complete))

         (signature-lines  (car sig-cons-compl-lines))

         (signatures (apply
                      'append
                      (mapcar (lambda (line)
                                (split-string line "  " t))
                              signature-lines)))

         (completion-lines (cdr sig-cons-compl-lines))
         (already-matching
          (if signature-lines
              (list
               (ammonite-term-repl-compl--parse--sig->name
                (car signature-lines)))
            nil))

         (sorted-candidates
          (ammonite-term-repl-compl--parse--completion-lines->sorted-candidates
           completion-lines))

         ;; If `to-complete' already matches a definition we want it
         ;; included. already-matching gets appended completion candidates below, at
         ;; `sorted-candidates-plus-already-matching'
         (sorted-candidates-plus-already-matching
          (append already-matching
                  sorted-candidates)))

    (list
     `(:signatures    . ,signatures)
     `(:completions   . ,sorted-candidates-plus-already-matching)
     ;; `(:autocompleted . ,autocompleted) ; UNUSED
     `(:parsing-notes . ,parsing-notes))
    ))

(defun ammonite-term-repl-compl--proc-filter (process str)

  ;; UNUSED : TODO : consider if needed
  ;; (setq ammonite-term-repl-compl--proc-output-ansi
  ;;       (concat ammonite-term-repl-compl--proc-output-ansi
  ;;               (substring-no-properties str)))

  (setq ammonite-term-repl-compl--proc-output
        (concat ammonite-term-repl-compl--proc-output
                (substring-no-properties
                 (ansi-color-filter-apply str))))

  ;; Hide completions from repl when
  ;; `ammonite-term-repl-compl-silent-repl' is t
  (when (not ammonite-term-repl-compl-silent-repl)
    (term-emulate-terminal process str)))

;; There are only 2 things that is reasonable to do with
;; `ammonite-term-repl-compl--proc-output':
;; 1. reset
;; 2. read
;; Therefore avoid using it directly.
(defun ammonite-term-repl-compl--proc-filter--reset-kept ()
  (setq ammonite-term-repl-compl--proc-output-ansi "")
  (setq ammonite-term-repl-compl--proc-output ""))

(defun ammonite-term-repl-compl--proc-filter--get-kept-ansi ()
  ammonite-term-repl-compl--proc-output-ansi)

(defun ammonite-term-repl-compl--proc-filter--get-kept ()
  ammonite-term-repl-compl--proc-output)

(defun ammonite-term-repl-compl--get-completion-prefix
    (amm-output-with-ansi)
  ;;; UNUSED: try living without this and see how it goes.
  ;; Using the fact that Ammonite prints as blue the part that was
  ;; already part of the input. Compared to backward searching for
  ;; last dot this method would have:
  ;; - an advantage: more precise, since does not rely on the
  ;;   assumptions that the dot is the only completion separator (I
  ;;   don't know).
  ;; - a disadvantage: it depends on Ammonite colors: if colors change
  ;;   this stops working.
  (string-match
   "\\(^\\| \\)\\[34m\\([^\s]*?\\)\\[39m[^s]+"
   amm-output-with-ansi)
  (split-string (substring-no-properties amm-output-with-ansi
                                         (match-beginning 0)
                                         (match-end 0))
                "\\[[0-9][0-9]m"))

(defun ammonite-term-repl-compl-for-string-get (to-complete)

  ;; Stop if an `ammonite-term-repl' Ammonite buffer is not runninng.
  (ammonite-term-repl-check-process)

  ;; Both amm-proc and amm-proc-filter are used in the unwindforms, to
  ;; restore previous process filter
  (let* ((amm-proc (get-buffer-process
                    ammonite-term-repl-buffer-name))
         (amm-proc-filter (process-filter
                           amm-proc)))

    ;; After execution restore old process filter: even if errors
    ;; happen.
    (unwind-protect
        (progn

          ;; Delete lines until amm prompt is reached. On my laptop
          ;; deleting lines is slightly faster than sending an
          ;; interrupt, "\C-c".
          (ammonite-term-repl--term-clear--input-by-lines)

          ;; Keep output in `ammonite-term-repl-compl--proc-output' var.
          (set-process-filter amm-proc
                              #'ammonite-term-repl-compl--proc-filter)

          (ammonite-term-repl-compl--proc-filter--reset-kept)

          ;; Send string to complete + tab.
          (comint-send-string ammonite-term-repl-buffer-name
                              (concat to-complete "\t"))

          (let ((end-of-output-comment "/*amm-term-repl-compl-eoo*/"))
            ;; Send scala comment to make end of output easily
            ;; and reliably recognizable.
            (comint-send-string ammonite-term-repl-buffer-name
                                end-of-output-comment)
            ;; Wait for output.
            (while
                (not
                 (string-suffix-p
                  end-of-output-comment
                  (string-trim-right
                   (ammonite-term-repl-compl--proc-filter--get-kept))))
              (sit-for 0.01))

            ;; When `ammonite-term-repl-compl-silent-repl' is t we
            ;; don't want some input to remain in the prompt invisible
            ;; to the user. Clear input.
            (when ammonite-term-repl-compl-silent-repl
              (dotimes (x (1+ (length (split-string to-complete))))
                (comint-send-string ammonite-term-repl-buffer-name
                                    "\C-u\C-a\C-u\C-a\C-k")))

            ;; Parse output, return completions.
            (ammonite-term-repl-compl--parse
             to-complete
             ;; (ammonite-term-repl-compl--proc-filter--get-kept-ansi) ; consider if needed
             (ammonite-term-repl-compl--proc-filter--get-kept)
             end-of-output-comment)))

      ;; Re-set old process filter (`unwind-protect' returns bodyform,
      ;; not unwindforms, so this is just for side effects).
      (set-process-filter amm-proc
                          amm-proc-filter))))

(defun ammonite-term-repl-compl-for-string-choose (to-complete)
  (let* ((parsed-output (ammonite-term-repl-compl-for-string-get
                         to-complete))
         (signatures    (cdr (assq :signatures    parsed-output)))
         (candidates    (cdr (assq :completions   parsed-output)))
         ;; UNUSED
         ;; (autocompleted (cdr (assq :autocompleted parsed-output)))
         (completion-prompt
          (concat to-complete "[...]"
                  ;; Show signatures before completions.
                  (when signatures
                    (concat "\n"
                            (string-join signatures "\n")
                            "\n"))
                  " : "))
         ;; If `to-complete' does not end with a dot we don't want to
         ;; re-type what's after the dot in the completion backend.
         (text-from-last-dot
          (car (last (split-string to-complete "\\."))))

         (completion-initial-input
          (if (or (not (string-match-p "\\." to-complete)) ; deep completion
                  (string-suffix-p "." to-complete)
                  ;; If last dot is before the start of our thing to
                  ;; complete it's not useful to get an initial input.
                  (> (length text-from-last-dot)
                     (length to-complete)))
              ""
            text-from-last-dot))

         ;; Instead of waiting for user to press Tab: immediatly show
         ;; completions when minibuffer gets activated by
         ;; completing-read (below)
         (_ (run-at-time 0.1 nil
                         (lambda ()
                           (while (not (window-minibuffer-p))
                             (sleep-for 0.01))
                           (minibuffer-complete))))

         (choice (funcall
                  ;; TODO: offer choice for Helm/Ivy/Ido.
                  'completing-read
                  completion-prompt
                  candidates
                  nil
                  nil
                  completion-initial-input)))
    (cons (if (member choice candidates) t nil)
          choice)))

(defun ammonite-term-repl--bounds-of-thing-at-point-indented-block ()
  (let ((beg (save-excursion (re-search-backward "^[^\s]")))
        (end (point)))
    `(,beg . ,end)))

;; UNUSED
;; (defun ammonite-term-repl--thing-at-point-indented-block ()
;;   (let (bounds
;;         (ammonite-term-repl--bounds-of-thing-at-point-indented-block))
;;     (buffer-substring-no-properties (car bounds) (cdr bounds))))

;;;;; Commands

;;;###autoload
(defun ammonite-term-repl-complete-region (beg end)
  (interactive "r")
  (let* ((to-complete (buffer-substring-no-properties beg end))
         (completion-res (ammonite-term-repl-compl-for-string-choose
                          to-complete))
         (choice (cdr completion-res))
         (choice-in-candidates (car completion-res))
         (last-dot (save-excursion
                     (goto-char end)
                     (search-backward "." nil t))))

    ;; TODO: consider other separators. Look into ammonite code.
    ;; FIXME: what to do when `last-dot' (that should become
    ;; last-separator) is nil.
    (goto-char end)
    ;; if `last-dot' is before the beginning of `to-complete' it's not
    ;; a dot that we are completing on.
    (when (> (length to-complete) (- (point) last-dot))
      (kill-region (+ 1 last-dot) (point)))

    (when (and
           ;; There are 2 cases in which `choice' can contain a dot:
           ;; - Ammonite deep completion: in this case we want to
           ;;   replace all `to-complete'
           ;; - User has typed something that's not in candidates: In
           ;;   this case the he is expecting previous not text to be
           ;;   replaced.
           (string-match-p "[.]" choice)
           choice-in-candidates)
      (kill-region (- (point) (length to-complete))
                   (point)))

    (insert choice)))

;; TODO: complete different things at point.
;;;###autoload
(defun ammonite-term-repl-complete-indented-block-at-point ()
  (interactive)
  (let* ((bounds-of-indented-block
          (ammonite-term-repl--bounds-of-thing-at-point-indented-block))
         (beg-of-indented-block (car bounds-of-indented-block))
         (end-of-indented-block (cdr bounds-of-indented-block)))

    (ammonite-term-repl-complete-region
     beg-of-indented-block
     end-of-indented-block)))

;;;; Footer

(provide 'ammonite-term-repl-completion)

;;; ammonite-term-repl-completion.el ends here
