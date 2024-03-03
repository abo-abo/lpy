;;; lpy.el --- A lispy interface to Python -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lpy
;; Version: 0.1.0
;; Keywords: python, lisp
;; Package-Requires: ((emacs "25.1") (lispy "0.27.0"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an attempt to implement a variant of `lispy-mode'
;; (https://github.com/abo-abo/lispy) for Python.  Unfortunately,
;; Python isn't nearly as well-structured as LISP.  But Python is
;; ubiquitous, and the less powerful `lpy-mode' is better than nothing
;; at all.
;;
;; The basic idea of `lpy-mode' is to increase the editing efficiency
;; by binding useful navigation, refactoring and evaluation commands
;; to unprefixed keys, e.g. "j" or "e".  But only in certain point
;; positions, so that you are still able to use uprefixed keys to
;; insert themselves.
;;
;; Example, here "|" represents the point position:
;;
;;    print |("2+2=%d" % (2 + 2))
;;
;; Here, if you press the key "e", the whole line will be evaluated
;; and "2+2=4" will be printed in the Echo Area.  Note that if
;; `lpy-mode' was off, pressing "e" would instead result in:
;;
;;    print e|("2+2=%d" % (2 + 2))
;;
;; So inserting any key isn't actually useful with that point position
;; and e.g. the "e" can be used for evaluating the current statement.
;;
;; But, for instance, if you wanted to edit "print" into "printe", you
;; could do that in a straightforward way, just like you would with
;; `lpy-mode' off : with "C-b e".
;;
;;; Code:

(require 'lispy)
(require 'le-python)
(require 'lpy-soap)
(require 'flyspell nil t)
(require 'zoutline)

(defgroup lpy nil
  "List navigation and editing for Python."
  :group 'bindings
  :prefix "lpy-")

(defconst lpy-font-lock-keywords
  '(("^# ?\\(\\*\\|\\*[^*\n].*\\)$" 1 'org-level-1 prepend)
    ("^# ?\\(\\*\\{2\\}\\|\\*\\{2\\}[^*\n].*\\)$" 1 'org-level-2 prepend)
    ("^# ?\\(\\*\\{3\\}\\|\\*\\{3\\}[^*\n].*\\)$" 1 'org-level-3 prepend)
    ("^# ?\\(\\*\\{4\\}\\|\\*\\{4\\}[^*\n].*\\)$" 1 'org-level-4 prepend)
    ("^# ?\\(\\*\\{5\\}\\|\\*\\{5\\}[^*\n].*\\)$" 1 'org-level-5 prepend)
    (lpy-outline-comment-highlight 1 'default prepend)
    ;; ("^#  \\([^ ].*\\)$" 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(defun lpy-fill-forward-paragraph-function (&optional arg)
  "The setting for `fill-forward-paragraph-function'.
The argument ARG is passed to `forward-paragraph'."
  (let (bnd)
    (if (and (setq bnd (lispy--bounds-string))
             (string-match "\\`\"\"\"" (lispy--string-dwim bnd)))
        (goto-char (cdr bnd))
      (forward-paragraph arg))))

(defun lpy-fill-paragraph (&optional _justify)
  "The setting for `fill-paragraph-function'."
  (interactive)
  (let (bnd)
    (cond ((setq bnd (lispy--bounds-comment))
           (save-restriction
             (save-excursion
               (goto-char (car bnd))
               (while (looking-at outline-regexp)
                 (beginning-of-line 2)
                 (setcar bnd (point)))
               (narrow-to-region (car bnd) (cdr bnd))
               (let ((fill-paragraph-function nil))
                 (fill-paragraph)))))
          ((setq bnd (lispy--bounds-string))
           (let ((str (lispy--string-dwim bnd))
                 (pt (- (point) (car bnd))))
             (delete-region (car bnd) (cdr bnd))
             (insert
              (with-temp-buffer
                (insert str)
                (goto-char pt)
                (fill-paragraph)
                (buffer-string)))
             0))
          (t
           (let ((fill-paragraph-function nil))
             (fill-paragraph))))))

(defun lpy-outline-comment-highlight (limit)
  "Highlight outline comment up to LIMIT."
  (catch 'done
    (while (re-search-forward "^#\\(?:[^*\n]\\)" limit t)
      (let* ((pt (point))
             (success (save-excursion
                        (and (re-search-backward "^#\\*" nil t)
                             (null (re-search-forward "^[^#]" pt t))))))
        (when success
          (set-match-data (list (line-beginning-position) (line-end-position)
                                (point) (line-end-position)))
          (end-of-line)
          (throw 'done t))))))

(defun lpy-left-p ()
  "Return t if before `lispy-left'."
  (looking-at lispy-left))

(defun lpy-right-p ()
  "Return t if after `lispy-right'."
  (looking-back lispy-right
                (line-beginning-position)))

(defun lpy-outline-p ()
  "Return t when before `outline-regexp'."
  (and (looking-at outline-regexp)
       (looking-at lispy-outline-header)))

(defun lpy-space (arg)
  "Forward to `self-insert-command' ARG.
Use this to detect a space elsewhere."
  (interactive "p")
  (self-insert-command arg))

(defun lpy-line-left-p ()
  "Return t when in special position in Python."
  (or (and (bolp)
           (not (nth 3 (syntax-ppss)))  ; not IN multiline string
           (not (or
                 (eolp)
                 (looking-at " [^ ]")
                 (memq last-command '(lpy-space newline))
                 (looking-at "[ \n]*\\'"))))
      (and (looking-at " ")
           (looking-back "^ +" (line-beginning-position))
           (not (lispy--in-string-p))
           (not (eq last-command 'lpy-space)))))

(defun lpy-outline-add ()
  "Insert an outline below the current one."
  (interactive)
  (if (looking-at lispy-outline)
      (let ((lvl (lispy-outline-level)))
        (condition-case nil
            (outline-forward-same-level 1)
          (error (goto-char (point-max))))
        (while (eq (char-before) ?\n)
          (delete-char -1))
        (insert
         "\n\n"
         lispy-outline-header
         (make-string lvl ?\*)
         " \n")
        (beginning-of-line 0))
    (user-error "Not on an outline")))

(defun lpy-tab ()
  "Fold or unfold the current outline."
  (interactive)
  (cond ((or (lispy--in-comment-p)
             (looking-at lispy-outline))
         (let* ((bnd (zo-bnd-subtree))
                (eoh (car bnd))
                (eos (cdr bnd)))
           (if (and (get-char-property (1- eos) 'invisible)
                    (get-char-property (1+ eoh) 'invisible))
               (outline-flag-region eoh eos nil)
             (outline-flag-region eoh eos t))))
        ((region-active-p)
         (cond ((lpy-listp)
                (let ((beg (region-beginning))
                      (end (region-end)))
                  (if (and (save-excursion
                             (goto-char beg)
                             (lpy-arg-leftp))
                           (save-excursion
                             (goto-char end)
                             (lpy-arg-rightp)))
                      (when (save-excursion
                              (goto-char beg)
                              (re-search-forward "=" nil end))
                        (set-mark (1+ beg))
                        (goto-char (match-beginning 0))))))
               ((looking-at " \\(?:if\\|elif\\) \\([^\n]+\\):")
                (lispy--mark (cons (match-beginning 1)
                                   (match-end 1))))))
        ((looking-at "(")
         (let ((beg (point))
               (end (save-excursion (forward-list)))
               (col (1+ (current-column))))
           (move-beginning-of-line 2)
           (while (< (point) end)
             (skip-chars-forward " ")
             (indent-to col)
             (backward-delete-char (- (current-column) col))
             (move-beginning-of-line 2))
           (goto-char beg)))))

(defun lpy-contents ()
  "Toggle contents for the current outline."
  (interactive)
  (let ((bnd (zo-bnd-subtree)))
    (if (get-char-property (car bnd) 'invisible)
        (outline-show-subtree)
      (outline-show-subtree)
      (outline-hide-body))))

(defun lpy--insert-or-call (def)
  "Return a command to either self-insert or call DEF."
  `(lambda ()
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     (interactive)
     (unless (looking-at lispy-outline)
       (lispy--ensure-visible))
     (cond ((region-active-p)
            (call-interactively ',def))

           ((or (lispy--in-string-or-comment-p)
                (eq last-command 'python-indent-dedent-line-backspace))
            (call-interactively 'self-insert-command))

           ((or (lpy-line-left-p)
                (and (lispy-bolp)
                     (not (memq last-command '(self-insert-command
                                               lpy-soap-command
                                               newline)))
                     (or (looking-at lispy-outline-header)
                         (looking-at lispy-outline))))
            (call-interactively ',def))

           (t
            (setq this-command 'self-insert-command)
            (call-interactively
             'self-insert-command)))))

(defun lpy-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY DEF)."
  (declare (indent 3))
  (let ((func (defalias (intern (concat "pspecial-" (symbol-name def)))
                  (lpy--insert-or-call def))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (unless (memq func flyspell-delayed-commands)
      (add-to-list 'flyspell-delayed-commands func))
    (define-key keymap (kbd key) func)))

(defun lpy-line-p ()
  "Return t when a line is selected with a region."
  (and (region-active-p)
       (= (region-end) (line-end-position))
       (save-excursion
         (goto-char (region-beginning))
         (lispy-bolp))))

(defvar lpy-listp-last nil)

(defun lpy-listp ()
  "When on a list, return its bounds as a cons."
  (ignore-errors
    (unless (or (memq last-command '(python-indent-dedent-line-backspace
                                     lpy-parens
                                     self-insert-command))
                (and (eq ?\) (char-after))
                     (eq ?\( (char-before)))
                (lispy--in-string-or-comment-p))
      (let (end)
        (when (save-excursion
                (let ((forward-sexp-function nil))
                  (ignore-errors
                    (up-list 1)
                    (setq end (point))
                    (and (lispy-after-string-p ")")
                         (< (point) (point-max))))))
          (setq lpy-listp-last
                (cons
                 (save-excursion
                   (goto-char end)
                   (forward-list -1))
                 end)))))))

(defun lpy-arg-leftp ()
  "Test if on the left side of arg."
  (or (eq (point) (1+ (car lpy-listp-last)))
      (and (looking-at " ")
           (looking-back
            ",[ \n]*"
            (- (line-beginning-position) 2)))))

(defun lpy-arg-rightp ()
  "Test if on the right of arg."
  (or (and (eq (point) (1- (cdr lpy-listp-last)))
           (not (lispy-after-string-p " ")))
      (looking-at ",")))

(defun lpy-arg-forward ()
  "Forward by arg."
  (while (and (< (point) (point-max))
              (not (looking-at ",")))
    (forward-sexp 1)))

(defun lpy-bof-position ()
  "Return the start of the current function."
  (save-excursion
    (if (beginning-of-defun)
        (point)
      (line-beginning-position))))

(defun lpy-next-top-level-sexp ()
  "Go to next top level sexp."
  (let* ((outline-regexp "# ?\\*+")
         (end (lispy--outline-end)))
    (forward-char 1)
    (while (and (re-search-forward "^[^ \n]" end t)
                (lispy--in-string-or-comment-p))
      (python-nav-end-of-statement))
    (forward-char -1)))

(defun lpy-prev-top-level-sexp ()
  "Go to previous top level sexp."
  (let* ((outline-regexp "# ?\\*+")
         (beg (lispy--outline-beg)))
    (unless (eq beg 1)
      (cl-incf beg))
    (while (and (re-search-backward "^[^ \n]" beg t)
                (or (lispy--in-string-or-comment-p)
                    (looking-at "#"))))))

(defhydra hydra-lispy-move (:pre
                            (progn
                              (setq hydra-is-helpful nil)
                              (set-cursor-color "#e52b50"))
                            :before-exit
                            (progn
                              (setq hydra-is-helpful t)
                              (set-cursor-color "#ffffff")))
  ("h" lispy-outline-demote "left")
  ("j" lispy-move-down "down")
  ("k" lispy-move-up "up")
  ("l" lispy-outline-promote "right")
  ("v" lpy-outline-edit-below "edit outline" :exit t)
  ("n" lpy-outline-add "new" :exit t)
  ("q" nil "quit")
  ("c" nil "quit"))

(defun lpy-outline-edit-below ()
  "Edit the current outline below."
  (interactive)
  (beginning-of-line)
  (forward-line 1)
  (unless (bolp)
    (newline)))

(defun lpy-multiline-string-bnd ()
  "Return the bounds of the current triple quoted string."
  (let (bnd)
    (and (setq bnd (lispy--bounds-string))
         (> (count-lines (car bnd) (cdr bnd)) 1)
         bnd)))

(defun lpy-down (arg)
  "Move down ARG times inside the current sexp."
  (interactive "p")
  (lispy--remember)
  (lispy-dotimes arg
    (cond ((lpy-line-p)
           (let ((beg (region-beginning))
                 (end (region-end))
                 (leftp (= (point) (region-beginning)))
                 indent)
             (goto-char beg)
             (setq indent (concat
                           "^"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point))))
             (forward-char)
             (if (re-search-forward indent (line-end-position 2) t)
                 (progn
                   (while (and
                           (< (point) (point-max))
                           (= (point) (line-end-position)))
                     (forward-char))
                   (lispy--mark (cons (point) (line-end-position))))
               (lispy--mark (cons beg end)))
             (when leftp
               (exchange-point-and-mark))))
          ((region-active-p)
           (if (lpy-listp)
               (cond ((lpy-arg-rightp)
                      (unless (eq (point) (1- (cdr lpy-listp-last)))
                        (lpy-slurp)
                        (exchange-point-and-mark)
                        (lpy-barf)
                        (exchange-point-and-mark)))
                     ((lpy-arg-leftp)
                      (exchange-point-and-mark)
                      (if (eq (point) (1- (cdr lpy-listp-last)))
                          (exchange-point-and-mark)
                        (lpy-slurp)
                        (exchange-point-and-mark)
                        (lpy-barf)))
                     (t
                      nil))

             (lispy-down arg)))
          ((looking-at outline-regexp)
           (zo-down arg))
          ((lpy-line-left-p)
           (when (looking-at " ")
             (forward-char 1))
           (let (bnd)
             (if (setq bnd (lispy--bounds-comment))
                 (progn
                   (goto-char (cdr bnd))
                   (back-to-indentation)))
             (let* ((offset (current-column))
                    (bound (if (= offset 0)
                               (save-excursion
                                 (or (outline-next-heading)
                                     (point-max)))
                             (save-excursion
                               (or
                                (let ((match-found nil))
                                  (while (and (re-search-forward
                                               (format "^%s[^ \n]" (make-string (- offset lpy-offset) 32))
                                               nil t)
                                              (not (lispy-looking-back ")"))
                                              (setq match-found t)
                                              (lispy--in-string-or-comment-p)))
                                  (if match-found
                                      (point)))
                                (point-max)))))
                    (regex (format "^%s[^ \n)]" (buffer-substring-no-properties
                                                (line-beginning-position) (point))))
                    (old-point (point))
                    bnd)
               (forward-char 1)
               (unless (catch 'done
                         (while (re-search-forward regex bound t)
                           (goto-char (1- (match-end 0)))
                           (if (setq bnd (lpy-multiline-string-bnd))
                               (goto-char (cdr bnd))
                             (throw 'done t))))
                 (goto-char old-point))
               (unless (bolp)
                 (backward-char 1))))))))

(defun lpy-bounds-defun ()
  "Return the bounds of the current defun."
  (save-excursion
    (let (beg)
      (unless (looking-at "^")
        (beginning-of-defun))
      (setq beg (point))
      (end-of-defun)
      (cons beg (point)))))

(defun lpy-avy-symbol-action (pt)
  "Mark the symbol at PT."
  (goto-char pt)
  (cond ((looking-at "^"))
        ((looking-back "^ +" (line-beginning-position))
         (backward-char))
        (t
         (lpy-mark-symbol))))

(defun lpy-avy-symbol ()
  "Select a symbol with avy and mark it.
When on an outline, add an outline below."
  (interactive)
  (if (looking-at lispy-outline)
      (lpy-add-outline)
    (deactivate-mark)
    (let ((avy-action 'lpy-avy-symbol-action)
          (avy-handler-function (lambda (_) (throw 'done 'exit))))
      (lispy--avy-do
       "\\_<\\sw"
       (or
        (lispy-bounds-python-block)
        (lpy-bounds-defun))
       (lambda ()
         (not (save-excursion
                (forward-char -1)
                (lispy--in-string-or-comment-p))))
       'at-full))))

(defun lpy-avy ()
  "Select a line in the current function with avy."
  (interactive)
  (lispy--remember)
  (deactivate-mark)
  (let ((avy-keys lispy-avy-keys)
        (bnd (bounds-of-thing-at-point 'defun)))
    (avy-with lpy-avy
      (lispy--avy-do
       "^ *\\( \\)"
       bnd
       (lambda () (not (lispy--in-string-or-comment-p)))
       'at 1))
    (back-to-indentation)
    (backward-char)))

(defun lpy-add-outline ()
  "Add an outline below the current one."
  (let ((bnd (zo-bnd-subtree))
        (lvl (lispy-outline-level))
        (outline
         (when (looking-at (concat lispy-outline-header "\\*+ :$"))
           (concat (match-string-no-properties 0) "\n\n"))))
    (goto-char (cdr bnd))
    (cond
      ((<= (- (point-max) (point)) 1)
       (newline)
       (newline))
      ((looking-at "\n\n")
       (forward-char)
       (newline)
       (backward-char))
      (t
       (newline)))
    (if outline
        (progn
          (insert outline)
          (backward-char))
      (insert lispy-outline-header
              (make-string lvl ?\*)
              " "))))

(defun lpy-up (arg)
  "Move up ARG times inside the current sexp."
  (interactive "p")
  (lispy--remember)
  (lispy-dotimes arg
    (cond ((lpy-line-p)
           (let ((beg (region-beginning))
                 (end (region-end))
                 (leftp (= (point) (region-beginning)))
                 indent)
             (unless (= beg (point-min))
               (goto-char beg)
               (setq indent (concat
                             "^"
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (point))))
               (backward-char)
               (if (re-search-backward indent (lpy-bof-position) t)
                   (progn
                     (goto-char (match-end 0))
                     (while (and
                             (> (point) (point-min))
                             (= (point) (line-end-position)))
                       (forward-line -1))
                     (lispy--mark (cons (point) (line-end-position))))
                 (lispy--mark (cons beg end)))
               (when leftp
                 (exchange-point-and-mark)))))
          ((lpy-outline-p)
           (zo-up arg)
           (unless (= 0 (skip-chars-forward " "))
             (backward-char)))
          ((region-active-p)
           (if (lpy-listp)
               (cond ((lpy-arg-rightp)
                      (exchange-point-and-mark)
                      (if (= (point) (1+ (car lpy-listp-last)))
                          (exchange-point-and-mark)
                        (lpy-slurp)
                        (exchange-point-and-mark)
                        (lpy-barf)))
                     ((lpy-arg-leftp)
                      (lpy-slurp)
                      (exchange-point-and-mark)
                      (lpy-barf)))

             (lispy-up arg)))
          ((lpy-line-left-p)
           (when (looking-at " ")
             (forward-char 1))
           (let* ((offset (current-column))
                  (bound (if (= offset 0)
                             (condition-case nil
                                 (save-excursion
                                   (lpy-back-to-outline)
                                   (1+ (point)))
                               (error (point-min)))
                           (save-excursion
                             (while (and (re-search-backward
                                          (format "^%s[^ \n]" (make-string (- offset lpy-offset) 32)))
                                         (or (looking-at ")")
                                             (lispy--in-string-or-comment-p))))
                             (point))))
                  (regex (format "^%s[^ \n)]" (buffer-substring-no-properties
                                              (line-beginning-position) (point))))
                  bnd)
             (catch 'done
               (while (re-search-backward regex bound t)
                 (goto-char (- (match-end 0) 1))
                 ;; triple quoted strings
                 (if (setq bnd (lpy-multiline-string-bnd))
                     (goto-char (car bnd))
                   (throw 'done t))))
             (let (bnd)
               (when (setq bnd (lispy--bounds-comment))
                 (goto-char (car bnd))))
             (unless (bolp)
               (backward-char 1)))))))

(defun lpy-flow ()
  "Navigate to the next child item."
  (interactive)
  (cond ((looking-at lispy-outline)
         (lpy-next-top-level-sexp))
        ((lpy-line-left-p)
         (let ((bnd (lpy-bounds-defun))
               (old-pt (point)))
           (python-nav-forward-block)
           (if (> (point) (cdr bnd))
               (goto-char old-pt)
             (backward-char))))
        (t
         (self-insert-command 1))))

(defvar-local lpy-offset 4
  "Offset between indentation levels.")

(defun lpy-left (arg)
  "Go left ARG times."
  (interactive "p")
  (lispy--remember)
  (cond ((lpy-outline-p)
         (zo-left arg))
        ((lpy-line-left-p)
         (when (looking-at " ")
           (forward-char 1))
         (let ((offset (current-column)))
           (if (= offset 0)
               (when (eq major-mode 'python-mode)
                 (outline-back-to-heading))
             (while (and (progn (beginning-of-line 0)
                                (> (point) (point-min)))
                         (or (eolp)
                             (and (looking-at "^ +")
                                  (>= (- (match-end 0) (match-beginning 0))
                                      offset)))))
             (when (looking-at "^ +)")
               (beginning-of-defun))
             (back-to-indentation)
             (when (and (bolp) (eolp))
               (re-search-backward "^[^\n ]" nil t))
             (unless (bolp)
               (backward-char 1)))))
        (t
         (self-insert-command 1))))

(defun lpy-right (_arg)
  "Go right."
  (interactive "p")
  (lispy--remember)
  (cond ((lpy-outline-p)
         (unless (zo-right 1)
           (when (re-search-forward "^\\sw" (cdr (zo-bnd-subtree)) t)
             (backward-char))))
        ((lpy-line-left-p)
         (let* ((cur-offset (if (bolp) 0 (1+ (current-column))))
                (new-offset (+ cur-offset lpy-offset))
                (regex (format "^ \\{%d,\\}[^ \n]" new-offset))
                (pt (point))
                (end (save-excursion
                       (end-of-defun)
                       (if (<= (- pt (point)) 0)
                           (point-max)
                         (point))))
                success)
           (while (and (re-search-forward regex end t)
                       (if (lispy--in-comment-p)
                           t
                         (setq success t)
                         nil)))
           (if success
               (backward-char)
             (goto-char pt))
           (unless (or (bolp) (looking-at " "))
             (backward-char 1))))
        ((eolp))
        (t
         (self-insert-command 1))))

(defun lpy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((lpy-listp)
         (cond ((lpy-arg-leftp)
                (forward-sexp))
               ((lpy-arg-rightp)
                (backward-sexp))))
        ((or (lispy-left-p)
             (lispy-right-p))
         (lispy-different))
        ((looking-at lispy-outline)
         (goto-char (lispy--outline-end)))
        ((or (eobp)
             (looking-at "\n#"))
         (goto-char (lispy--outline-beg)))
        (t
         (user-error "Unexpected"))))

(defun lpy-mark-symbol ()
  "Mark the current symbol."
  (interactive)
  (lispy--remember)
  (let (bnd)
    (cond
      ;; Gradually mark e.g. self.foo.bar with "M-m M-m M-m"
      ;; Unmark with "b b b"
      ((region-active-p)
       (when (looking-at "\\.")
         (let ((end
                (save-excursion
                  (goto-char (region-beginning))
                  (with-syntax-table python-dotty-syntax-table
                    (+ (point) (length (symbol-name (symbol-at-point))))))))
           (forward-char)
           (or (prog1 (search-forward "." end t)
                 (backward-char))
               (forward-sexp 1)))))
      ((looking-at "(")
       (mark-sexp)
       (exchange-point-and-mark))
      ((and (looking-at " ?\\(\"\\)")
            (save-excursion
              (goto-char (match-beginning 1))
              (setq bnd (lispy--bounds-string))
              (= (car bnd) (point))))
       (lispy--mark bnd))
      ((looking-at " ")
       (skip-chars-forward " ")
       (lispy--mark (bounds-of-thing-at-point 'symbol)))
      (t
       (lispy--mark (bounds-of-thing-at-point 'symbol))))))

(defvar lpy-no-space t
  "When non-nil, don't insert a space before parens.")

(defun lpy-just-one-space ()
  "Make sure there is just one space around the point."
  (unless lpy-no-space
    (just-one-space)))

(defun lpy-parens (&optional arg)
  "Insert a pair of parens.
When ARG is non-nil, wrap the current item with parens."
  (interactive "P")
  (cond ((region-active-p)
         (lispy--surround-region
          "("
          ")")
         (backward-char 1)
         (lpy-just-one-space)
         (backward-char 1))
        (arg
         (let ((bnd (lispy--bounds-dwim)))
           (goto-char (cdr bnd))
           (insert ")")
           (save-excursion
             (goto-char (car bnd))
             (insert "("))))
        (t
         (cond ((lispy--in-string-p))
               ((lispy-after-string-p "("))
               ((lispy-after-string-p "["))
               ((lispy-after-string-p "*")
                (delete-char -1)
                (insert " * "))
               ((looking-back
                 "^ *"
                 (line-beginning-position)))
               (t
                (unless (eq major-mode 'julia-mode)
                  (lpy-just-one-space))))
         (insert "()")
         (backward-char))))

(defvar lpy-back-to-outline nil
  "Store the point for 'lpy-back-to-outline.")

(defun lpy-back-to-outline ()
  "Go to the current outline start.
Call this twice to go back."
  (interactive)
  (if (and (memq last-command '(lpy-back-to-outline))
           (looking-at lispy-outline))
      (lispy-pam-restore 'lpy-back-to-outline)
    (lispy-pam-store 'lpy-back-to-outline)
    (outline-back-to-heading)))

(defun lpy-back-to-special ()
  "Go back to special."
  (interactive)
  (if (lispy-bolp)
      (lispy-pam-restore 'lpy-back-to-special)
    (lispy-pam-store 'lpy-back-to-special)
    (back-to-indentation)
    (let (bnd)
      (when (setq bnd (lispy--bounds-string))
        (goto-char (car bnd))))
    (unless (bolp)
      (backward-char))))

(defun lpy-mark ()
  "Mark the current thing."
  (interactive)
  (lispy--remember)
  (let (bnd)
    (cond ((region-active-p)
           (deactivate-mark))
          ((and (looking-at " ?#")
                (save-excursion
                  (forward-char 1)
                  (setq bnd (lispy--bounds-comment))))
           (set-mark (cdr bnd)))
          ((setq bnd (lispy-bounds-python-block))
           (lispy--mark bnd)
           (exchange-point-and-mark)
           (back-to-indentation)
           (when (lispy-after-string-p " ")
             (backward-char)))
          ((lpy-listp)
           (cond ((lpy-arg-leftp)
                  (set-mark (point))
                  (forward-sexp))
                 ((lpy-arg-rightp)
                  (set-mark (point))
                  (backward-sexp))))
          (t
           (lispy-mark)))))

(defun lpy-slurp ()
  "Slurp in an item into the region."
  (interactive)
  (cond ((region-active-p)
         (cond ((eolp)
                (end-of-line 2))
               ((lpy-listp)
                (cond ((lpy-arg-rightp)
                       (forward-sexp))
                      ((lpy-arg-leftp)
                       (backward-sexp))))
               (t
                (let ((regex  "[[.]"))
                  (when (looking-at regex)
                    (forward-char))
                  (if (re-search-forward regex (line-end-position) t)
                      (backward-char)
                    (end-of-line))))))
        ((lispy--in-string-or-comment-p)
         (self-insert-command 1))
        (t
         (lpy-soap-command))))

(defun lpy-barf ()
  "Barf out an item from the region."
  (cond ((and (region-active-p)
              (lpy-listp))
         (cond ((lpy-arg-rightp)
                (let ((pt (point)))
                  (backward-sexp)
                  (if (= (point) (region-beginning))
                      (goto-char pt)
                    (skip-chars-backward ", \n"))))
               ((lpy-arg-leftp)
                (let ((pt (point)))
                  (forward-sexp)
                  (if (= (point) (region-end))
                      (goto-char pt))
                  (skip-chars-forward ", \n")
                  (backward-char)))))
        ((lispy--in-string-or-comment-p)
         (self-insert-command 1))
        (t
         (lpy-soap-command))))

(defun lpy-plus ()
  (interactive)
  (cond ((region-active-p)
         (er/expand-region 1))
        ((lispy--in-string-p)
         (self-insert-command 1))
        (t
         (lpy-soap-command))))

(defun lpy-minus ()
  (interactive)
  (cond ((region-active-p)
         (er/expand-region -1))
        ((eq major-mode 'yaml-mode)
         (self-insert-command 1))
        ((lispy--in-string-p)
         (self-insert-command 1))
        (t
         (lpy-soap-command))))

(defun lpy-open ()
  "Insert a newline after the current marked line."
  (interactive)
  (when (and (region-active-p)
             (eq (line-number-at-pos (region-beginning))
                 (line-number-at-pos (region-end))))
    (deactivate-mark)
    (end-of-line)
    (newline-and-indent)))

(defun lpy-teleport ()
  "Go to the end of the current outline."
  (interactive)
  (if (looking-at lispy-outline)
      (end-of-line)
    (self-insert-command 1)))

(defun lpy-meta-return ()
  "Insert a new heading."
  (interactive)
  (cond
   ((eq major-mode 'python-mode)
    (unless (bolp)
      (newline))
    (insert lispy-outline-header
            (make-string (max (lispy-outline-level) 1)
                         ?\*)
            " ")
    (end-of-line))

   ((eq major-mode 'yaml-mode)
    (let ((prefix
           (save-excursion
             (back-to-indentation)
             (buffer-substring-no-properties
              (line-beginning-position)
              (if (looking-at "- ")
                  (match-end 0)
                (point))))))
      (newline)
      (insert prefix)))))

(defun lpy-clean ()
  "Clean up the evaluation results in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^ *# =>" nil t)
      (let ((bnd (lispy--bounds-comment)))
        (delete-region (car bnd) (cdr bnd))
        (delete-region (1- (line-beginning-position)) (point))))
    (save-buffer)))

(defun lpy-view ()
  "Recenter the buffer around the current point."
  (interactive)
  (let ((window-line (count-lines (window-start) (point))))
    (if (or (= window-line 0)
            (and (not (bolp)) (= window-line (1+ scroll-margin))))
        (recenter (or (get 'lispy-recenter :line) 0))
      (put 'lispy-recenter :line window-line)
      (recenter 0))))

(defvar sd-force-reparse)
(defvar moo-jump-local-cache)
(declare-function moo-flatten-namepaces "ext:function-args")
(declare-function moo-format-tag-line "ext:function-args")
(declare-function moo-select-candidate "ext:function-args")
(declare-function moo-action-jump "ext:function-args")
(declare-function sd-fetch-tags "ext:function-args")
(declare-function semantic-tag-get-attribute "tag")
(declare-function semantic-tag-class "tag")
(declare-function semantic-tag-overlay "tag")
(declare-function compile-goto-error "compile")

(defun lpy-goto (arg)
  "Select a tag to jump to from tags defined in current buffer.
When ARG is 2, jump to tags in current dir."
  (interactive "p")
  (require 'semantic-directory)
  (require 'function-args)
  (let* ((file-list
          (if (> arg 1)
              (cl-remove-if
               (lambda (x)
                 (string-match "^\\.#" x))
               (append (file-expand-wildcards "*.py")))
            (list (buffer-file-name))))
         (sd-force-reparse (> arg 2))
         (ready-tags
          (or
           (let ((tags (moo-flatten-namepaces
                        (sd-fetch-tags file-list))))
             (when (memq major-mode '(python-mode))
               (setq tags
                     (delq nil
                           (mapcar
                            (lambda (x)
                              (let ((s (lpy-tag-name x)))
                                (when s
                                  (cons
                                   (moo-format-tag-line
                                    s (semantic-tag-get-attribute x :truefile))
                                   x))))
                            tags))))
             (puthash file-list tags moo-jump-local-cache)
             tags)))
         (preselect (python-info-current-defun)))
    (moo-select-candidate
     ready-tags
     #'moo-action-jump
     preselect)
    (unless (or (bolp) (looking-at " "))
      (backward-char 1))))

(defvar lpy-goto-history nil)

(defun lpy--current-symbol ()
  (cl-case major-mode
    (python-mode
     (python-info-current-defun))
    (yaml-mode
     (save-excursion
       (back-to-indentation)
       (thing-at-point 'symbol)))))

(defun lpy-goto ()
  (interactive)
  (let ((defs (lispy--py-to-el (format "lp.definitions('%s')" (buffer-file-name)))))
    (ivy-read "tag: " defs
              :action #'lpy-goto-action
              :preselect (lpy--current-symbol)
              :history 'lpy-goto-history
              :caller 'lpy-goto)))

(defun lpy-goto-action (x)
  (goto-char (point-min))
  (forward-line (1- (cadr x)))
  (when (looking-at " ")
    (skip-chars-forward " ")
    (backward-char 1)))

(defun lpy-tag-name (tag)
  "Return a pretty name for TAG."
  (let* ((class (semantic-tag-class tag))
         (str (cl-case class
                (function
                 (let ((args (semantic-tag-get-attribute tag :arguments)))
                   (format "%s %s (%s)"
                           (propertize "def" 'face 'font-lock-builtin-face)
                           (propertize (car tag) 'face 'font-lock-function-name-face)
                           (mapconcat #'car args ", "))))
                (variable
                 (car tag))
                (type
                 (propertize (car tag) 'face 'fa-face-type-definition))
                (include
                 (format "%s %s"
                         (propertize "import" 'face 'font-lock-preprocessor-face)
                         (car tag)))
                (code
                 (let* ((beg)
                        (end)
                        (ov (semantic-tag-overlay tag))
                        (buf (cond
                              ((and (overlayp ov)
                                    (bufferp (overlay-buffer ov)))
                               (setq beg (overlay-start ov))
                               (setq end (overlay-end ov))
                               (overlay-buffer ov))
                              ((arrayp ov)
                               (setq beg (aref ov 0))
                               (setq end (aref ov 1))
                               (current-buffer))))
                        str)
                   (when (and buf
                              (setq str
                                    (ignore-errors
                                      (with-current-buffer buf
                                        (buffer-substring-no-properties beg end))))
                              (string-match (format "^%s ?=" (car tag)) str))
                     (concat (car tag) "="))))
                (t
                 (error "Unknown class for tag: %S" tag)))))
    str))

(defun lpy-beautify-strings ()
  "Replace 'foo' with \"foo\"."
  (interactive)
  (goto-char (point-min))
  (let (bnd)
    (while (re-search-forward "'." nil t)
      (unless (looking-at "''")
        (when (setq bnd (lispy--bounds-string))
          (goto-char (car bnd))
          (delete-char 1)
          (insert "\"")
          (goto-char (cdr bnd))
          (delete-char -1)
          (insert "\""))))))

(defun lpy-beautify-commas ()
  "Place spaces after commas."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(,\\)[^ ]" nil t)
    (replace-match ", " t t nil 1)))

(defun lpy-beautify-parens ()
  "Place spaces before parens."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "[^ ]\\((\\)")
    (replace-match " (" t t nil 1)))

(defun lpy-quotes ()
  "Insert quotes."
  (interactive)
  (let (bnd)
    (cond ((region-active-p)
           (let ((beg (region-beginning))
                 (end (region-end)))
             (deactivate-mark)
             (goto-char beg)
             (insert "\"")
             (goto-char (1+ end))
             (insert "\"")))

          ((and (setq bnd (lispy--bounds-string))
                (> (point) (car bnd)))
           (cond
             ((eq (char-after (car bnd)) ?')
              (insert "\"\"")
              (backward-char))
             ((and (looking-back "\"" (line-beginning-position))
                   (looking-at "\""))
              (insert "\"\"\"\"")
              (backward-char 2))
             ((string= (buffer-substring (car bnd)
                                         (+ 3 (car bnd)))
                       "\"\"\"")
              (insert "\"\"")
              (backward-char 1))
             (t
              (insert "\\\"\\\"")
              (backward-char 2))))
          (t
           (unless (or (looking-back "^ *" (line-beginning-position))
                       (looking-back "\\s(" (line-beginning-position))
                       (looking-back "[,(\n] *\\(\\sw\\|\\s_\\)+=" (line-beginning-position 0))
                       (looking-back "\\bf" (line-beginning-position)))
             (just-one-space))
           (insert "\"\"")
           (backward-char)))))

(defun lpy-split ()
  "Split the current string or comment."
  (interactive)
  (let ((bnd (lispy--bounds-string)))
    (if bnd
        (let ((quote-str (string (char-after (car bnd)))))
          (when (looking-back " +" (line-beginning-position))
            (delete-region (match-beginning 0) (match-end 0)))
          (insert quote-str "\n" quote-str)
          (indent-for-tab-command))
      (indent-new-comment-line))))

(defun lpy-beginning-of-line ()
  "Go back to indentation or to the beginning of line."
  (interactive)
  (if (and (looking-at " ")
           (looking-back " +" (line-beginning-position)))
      (beginning-of-line)
    (back-to-indentation)
    (cond ((looking-back "^ +" (line-beginning-position))
           (backward-char))
          ((bolp))
          (t
           (let ((inhibit-field-text-motion t))
             (beginning-of-line))))))

(defun lpy-kill-line (&optional arg)
  "Kill the current line.
When in a string, kill until the end of the string.
When ARG is non-nil, kill until the beginning of the string."
  (interactive "P")
  (let (bnd)
    (cond
     ((and (setq bnd (lispy--bounds-string))
           (not (eq (point) (car bnd))))
      (if (member (buffer-substring-no-properties (- (cdr bnd) 3) (cdr bnd))
                  '("\"\"\"" "'''"))
          (if (> (cdr bnd) (line-end-position))
              (kill-region (point) (+ (line-end-position)
                                      (if (eolp) 1 0)))
            (kill-region (point) (- (cdr bnd) 3)))
        (if arg
            (kill-region
             (1+ (car bnd))
             (point))
          (kill-region
           (point)
           (1- (cdr bnd))))))
     ((bolp)
      (kill-line))
     ((eolp)
      (delete-region (line-beginning-position)
                     (1+ (point)))
      (back-to-indentation))
     (t
      (kill-line)))))

(defun lpy-delete ()
  "Delete the current item."
  (interactive)
  (cond
    ((region-active-p)
     (delete-active-region)
     (delete-region (line-beginning-position)
                    (1+ (line-end-position)))
     (back-to-indentation)
     (unless (bolp)
       (backward-char)))
    ((looking-at " *$")
     (let ((offset (mod (current-column) 4)))
       (delete-region (point) (1+ (match-end 0)))
       (when (looking-at " +")
         (delete-region (match-beginning 0)
                        (match-end 0)))
       (indent-for-tab-command)
       (when (= offset 3)
         (backward-char 1))))
    (t
     (delete-char 1))))

(defun lpy-import-from-markdown ()
  "Use after jupyter nbconvert --to markdown."
  (interactive)
  (unless (eq major-mode 'markdown-mode)
    (error "Please open a Markdown file"))
  (let* ((md-name (file-name-nondirectory (buffer-file-name)))
         (py-name (concat (file-name-sans-extension md-name)
                          ".py")))
    (find-file py-name)
    (erase-buffer)
    (insert-file-contents md-name)
    (goto-char (point-min))
    (let ((idx -1)
          (lvl 0))
      (while (re-search-forward "^\\(```\\|#+\\)" nil t)
        (if (string= (match-string 1) "```")
            (progn
              (delete-region (line-beginning-position)
                             (line-end-position))
              (insert "#*" (make-string lvl ?*)
                      " " (number-to-string (cl-incf idx)))
              (re-search-forward "^```")
              (delete-region (line-beginning-position)
                             (1+ (line-end-position))))
          (let ((len (length (match-string 1))))
            (setq lvl len)
            (replace-match (format "#%s" (make-string len ?*)) nil t)
            (setq idx -1)))))
    (save-buffer)))

(defun lpy-outline-level ()
  "Compute the outline level of the heading at point."
  (save-excursion
    (save-match-data
      (cond ((looking-at "^\\(class\\|def\\)")
             (if (re-search-backward "^#\\*+" nil t)
                 (1+ (lpy-outline-level))
               0))
            ((looking-at " +def")
             (if (re-search-backward "\\(?:^#\\*+\\)\\|^class" nil t)
                 (1+ (lpy-outline-level))
               0))
            (t
             (end-of-line)
             (if (re-search-backward outline-regexp nil t)
                 (max (cl-count ?* (match-string 0)) 1)
               0))))))

(defun lpy-occur-action (x)
  "Goto X."
  (goto-char lispy--occur-beg)
  (forward-line (read x))
  (when (re-search-forward (ivy--regex ivy-text) (line-end-position) t)
    (goto-char (match-beginning 0)))
  (when (lispy-bolp)
    (unless (bolp)
      (backward-char))))

(defun lpy-occur ()
  "Swipe the current defun."
  (interactive)
  (swiper--init)
  (unwind-protect
       (ivy-read "pattern: "
                 (lispy--occur-candidates
                  (save-excursion
                    (unless (bolp)
                      (re-search-backward "^[^ \n]" nil t))
                    (cons (point)
                          (progn
                            (end-of-defun)
                            (point)))))
                 :preselect (lispy--occur-preselect)
                 :require-match t
                 :update-fn (lambda ()
                              (lispy--occur-update-input
                               ivy-text
                               (if (boundp 'ivy--current)
                                   ivy--current
                                 (ivy-state-current ivy-last))))
                 :action #'lpy-occur-action
                 :caller 'lpy-occur)
    (swiper--cleanup)
    (when (null ivy-exit)
      (goto-char swiper--opoint))))

(defun lpy-follow-dbg-links-filter (output)
  "Filter OUTPUT.
Suitable for `comint-output-filter-functions'."
  (let ((regex "^  File \"\\([^\"]+\\)\", line [0-9]+"))
    (when (string-match regex output)
      ;; wait for `comint-output-filter' to insert the string into the buffer
      (run-at-time nil nil
                   (lambda ()
                     (save-excursion
                       (when (and (re-search-backward regex nil t)
                                  (file-exists-p (match-string 1)))
                         (save-selected-window
                           (compile-goto-error))))))))
  output)

(defun lpy-switch-to-shell ()
  "Switch to the current process buffer."
  (interactive)
  (let ((buffer (process-buffer (lispy--python-proc))))
    (if buffer
        (progn
          (pop-to-buffer buffer)
          (add-to-list 'completion-at-point-functions 'lispy-python-completion-at-point)
          (add-hook 'comint-output-filter-functions 'lpy-follow-dbg-links-filter)
          (when (equal (buffer-string) "")
            (comint-send-input)))
      (run-python "python")
      (pop-to-buffer "*Python*"))))

(defun lpy-yank ()
  "Yank while intelligently quoting strings."
  (interactive)
  (let (bnd)
    (if (and (setq bnd (lispy--bounds-string))
             (not (= (point) (car bnd))))
        (cond
          ((string= (buffer-substring (car bnd)
                                      (min (point-max)
                                           (+ 3 (car bnd)))) "\"\"\"")
           (insert (current-kill 0)))
          ((eq (char-after (car bnd)) ?\")
           (insert (replace-regexp-in-string "\"" "\\\\\"" (current-kill 0))))
          (t
           (insert (replace-regexp-in-string "'" "\\\\'" (current-kill 0)))))
      (yank))))

(defun lpy-eval-buffer ()
  "Eval the current buffer."
  (interactive)
  (condition-case err
      (if (process-live-p (lispy--python-proc))
          (let ((res
                 (progn
                   (lispy-python-middleware-reload)
                   (python-shell-send-string-no-output
                    (concat
                     ;; (format "lp.chfile('%s')\n" (buffer-file-name))
                     (buffer-substring-no-properties
                      (point-min)
                      (point-max)))
                    (lispy--python-proc)))))
            (setq res (concat res (lispy--eval
                                   (format "lp.reload_module('%s')" (buffer-file-name)))))
            (if (string= "" res)
                (lispy-message "(ok)")
              (lispy-message res)))
        (error "No process"))
    (eval-error
     (lispy-message (cdr err)))))

(declare-function iedit-mode "ext:iedit")

(defun lpy-iedit ()
  "Edit all occurences of the current symbol."
  (interactive)
  (require 'iedit)
  (if (or current-prefix-arg
          (let ((re (concat "\\_<" (ivy-thing-at-point) "\\_>"))
                (pt (point)))
            (save-excursion
              (save-restriction
                (narrow-to-defun)
                (if (/= (point) pt)
                    t
                  (goto-char (point-min))
                  (not (re-search-forward re nil t 2)))))))
      (iedit-mode)
    (iedit-mode 0)))

(defun lpy-import-last ()
  "Import the last symbol stored by `lispy-store-region-and-buffer'."
  (interactive)
  (let* ((buffer (get 'lispy-store-bounds 'buffer))
         (region (get 'lispy-store-bounds 'region))
         (code (with-current-buffer buffer
                 (lispy--string-dwim region)))
         (temp-file-name (python-shell--save-temp-file code)))
    (insert
     (lispy--eval-python-plain
      (format "lp.generate_import('%s','%s')"
              temp-file-name
              (expand-file-name (buffer-file-name buffer)))))))

(defun lpy-eval ()
  "Eval expression."
  (interactive)
  (let* ((str (lispy-eval-python-str))
         (res (lispy--eval-python str t)))
    (when lispy-eval-output
      (setq res (concat lispy-eval-output res)))
    (lispy-message res)))

(defvar lpy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-o") 'lpy-back-to-special)
    (define-key map (kbd "M-i") 'lpy-iedit)
    (define-key map (kbd "M-r i") 'lpy-import-last)
    (define-key map (kbd "C-y") 'lpy-yank)
    (define-key map (kbd "C-c C-z") 'lpy-switch-to-shell)
    (define-key map (kbd "C-c C-c") 'lispy-eval-current-outline)
    (define-key map (kbd "C-c C-l") 'lpy-eval-buffer)
    (define-key map (kbd "C-a") 'lpy-beginning-of-line)
    (define-key map (kbd "C-k") 'lpy-kill-line)
    (define-key map (kbd "C-d") 'lpy-delete)
    (define-key map (kbd "M-m") 'lpy-mark-symbol)
    (define-key map (kbd "M-RET") 'lpy-meta-return)
    (define-key map (kbd "<backtab>") 'lispy-shifttab)
    (define-key map (kbd "M-<left>") 'lispy-outline-demote)
    (define-key map (kbd "M-<right>") 'lispy-outline-promote)
    (define-key map (kbd "C-1") 'lispy-describe-inline)
    (define-key map (kbd "C-2") 'lispy-arglist-inline)
    (define-key map (kbd "M-.") 'lispy-goto-symbol)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "M-j") 'lpy-split)
    (define-key map (kbd "SPC") 'lpy-space)
    (define-key map "(" 'lpy-parens)
    (define-key map "φ" 'lpy-parens)
    (define-key map "\"" 'lpy-quotes)
    (lpy-define-key map "a" 'lpy-avy-symbol)
    (lpy-define-key map "b" 'lispy-back)
    (lpy-define-key map "c" 'hydra-lispy-move/body)
    (lpy-define-key map "d" 'lpy-different)
    (lpy-define-key map "e" 'lispy-eval)
    (lpy-define-key map "E" 'lispy-eval-and-insert)
    (lpy-define-key map "f" 'lpy-flow)
    (lpy-define-key map "g" 'lpy-goto)
    (lpy-define-key map "h" 'lpy-left)
    (lpy-define-key map "i" 'lpy-tab)
    (lpy-define-key map "/" 'lpy-contents)
    (lpy-define-key map "j" 'lpy-down)
    (lpy-define-key map "k" 'lpy-up)
    (lpy-define-key map "l" 'lpy-right)
    (lpy-define-key map "m" 'lpy-mark)
    (lpy-define-key map "n" 'lispy-new-copy)
    (lpy-define-key map "o" 'lpy-open)
    (lpy-define-key map "p" 'lpy-eval)
    (lpy-define-key map "q" 'lpy-avy)
    (lpy-define-key map "r" 'self-insert-command)
    (lpy-define-key map "s" 'self-insert-command)
    (lpy-define-key map "t" 'lpy-teleport)
    (lpy-define-key map "u" 'undo)
    (lpy-define-key map "v" 'lpy-view)
    (lpy-define-key map "w" 'self-insert-command)
    (lpy-define-key map "x" 'lispy-x)
    (lpy-define-key map "y" 'lpy-occur)
    (lpy-define-key map "z" 'self-insert-command)
    (lpy-define-key map "B" 'lispy-ediff-regions)
    (lpy-define-key map "C" 'lpy-clean)
    (lpy-define-key map "D" 'pop-tag-mark)
    (lpy-define-key map "F" 'lispy-goto-symbol)
    (lpy-define-key map "I" 'lispy-shifttab)
    (lpy-define-key map "J" 'lispy-outline-next)
    (lpy-define-key map "K" 'lispy-outline-prev)
    (lpy-define-key map "N" 'lispy-narrow)
    (lpy-define-key map "W" 'lispy-widen)
    (define-key map ">" 'lpy-slurp)
    (define-key map "<" 'lpy-soap-command)
    (define-key map "+" 'lpy-plus)
    (define-key map "-" 'lpy-minus)
    (dolist (x (number-sequence 0 9))
      (lpy-define-key map (format "%d" x) 'digit-argument))
    (dolist (x '("%" "&" "|" "=" ","))
      (define-key map x 'lpy-soap-command))
    map))

;;;###autoload
(define-minor-mode lpy-mode "Minor mode for navigating Python code similarly to LISP."
  :keymap lpy-mode-map
  :lighter " LPY"
  (if lpy-mode
      (progn
        (setq lispy-outline-header "#")
        (setq-local outline-regexp "# ?\\*+")
        (setq-local lispy-outline (concat "^" outline-regexp))
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-level 'lpy-outline-level)
        (setq-local fill-paragraph-function 'lpy-fill-paragraph)
        (setq-local fill-forward-paragraph-function 'lpy-fill-forward-paragraph-function)
        (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
        (setq-local lispy-no-space t)
        (font-lock-add-keywords major-mode lpy-font-lock-keywords)
        (setq-local lpy-offset
                    (if (memq major-mode '(js2-mode yaml-mode))
                        2
                      4)))
    (font-lock-remove-keywords major-mode lpy-font-lock-keywords)))

(provide 'lpy)

;;; lpy.el ends here
