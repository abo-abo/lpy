;;; soap.el --- Smart Operator a posteriori

;; Copyright (C) 2015-2020 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/lpy
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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
;; Insert spaces around operators, e.g. " += ".
;;
;; Based on smart-operator.el by William Xu.

;;; Code:

(defvar soap-alist
  '("\\+" "-" "\\*" "/" "%" "&" "|" "<" "=" ">"))

(defun soap-in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (or
   ;; proper
   (let ((beg (nth 8 (syntax-ppss))))
     (and beg
          (or (eq (char-after beg) ?\")
              (eq (char-after beg) ?\')
              (comment-only-p beg (point)))
          beg))
   ;; works for `matlab-mode'
   (eq (get-text-property (point) 'face)
       'font-lock-string-face)))

(defun soap-default-action (op)
  "Insert OP, possibly with spaces around it."
  (delete-horizontal-space t)
  (let ((op-p nil)
        (one-char-back nil))
    (unless (bolp)
      (backward-char)
      (setq one-char-back t))
    (setq op-p
          (catch 'return
            (dolist (front soap-alist)
              (when (looking-at front)
                (throw 'return t)))))
    (when (and (or op-p (not (and (bolp) (eolp))))
               one-char-back)
      (forward-char))
    (if (or op-p (bolp))
        (insert op)
      (insert " " op)))
  (delete-horizontal-space t)
  (unless (eq (char-after) ?\ )
    (insert " ")))

(defun soap-after-string-p (str)
  "Return t if the string before point is STR."
  (string=
   (buffer-substring
    (max
     (- (point) (length str))
     (point-min))
    (point))
   str))

(defun soap-op-> ()
  "Action for >."
  (interactive)
  (cond ((looking-back "\\(?:this\\)?\\(-\\| \\- \\)" (line-beginning-position))
         (delete-region (match-beginning 1)
                        (match-end 1))
         (insert "->"))
        ((eq major-mode 'sml-mode)
         (cond ((soap-after-string-p ":")
                (insert "> "))
               ((soap-after-string-p "- ")
                (delete-char -1)
                (insert "> "))
               (t
                (soap-default-action ">"))))
        ((looking-back "\\(?:this\\)?\\(-\\| \\- \\)" (line-beginning-position))
         (delete-region (match-beginning 1)
                        (match-end 1))
         (insert "->"))
        (t
         (soap-default-action ">"))))

(defun soap-command (&optional arg)
  "Similar to `self-insert-command' with ARG, except handles whitespace."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((op (this-command-keys)))
    (cond ((and (soap-in-string-or-comment-p)
                (member op (list "+" "-" "*" "/" "%" "&" "|" "=")))
           (self-insert-command arg))

          ((soap-after-string-p "(")
           (insert op))

          ((string= op "+")
           (cond
             ((looking-back " ?\\+ " (line-beginning-position))
              (if (eq major-mode 'haskell-mode)
                  (progn
                    (backward-delete-char 1)
                    (insert "+ "))
                (backward-delete-char 3)
                (insert "++")))
             ((looking-back "\\s-\\|=\\|\\+\\|\\([0-9.]+e\\)" (line-beginning-position))
              (insert "+"))
             (t (soap-default-action op))))

          ((string= op "-")
           (cond
             ((soap-after-string-p " - ")
              (backward-delete-char 3)
              (insert "--"))
             ((or (looking-at "[\\s-]*>") (bolp))
              (insert op))
             ((looking-back "\\s-\\|=\\|-\\|this\\|(\\|\\([0-9.]+e\\)" (line-beginning-position))
              (insert op))
             ((looking-back "[:[]" (line-beginning-position))
              (insert op))
             (t (soap-default-action op))))

          ((string= op "*")
           (cond ((soap-after-string-p " * ")
                  (delete-char -3)
                  (insert "**"))
                 ((or (looking-back "(\\|[\t :.]+" (line-beginning-position))
                      (looking-at ">")
                      (looking-back "^\\sw+" (line-beginning-position))
                      (looking-back "char\\|int\\|double\\|void" (line-beginning-position)))
                  (insert op))
                 ((memq major-mode '(java-mode))
                  (insert op))

                 (t
                  (soap-default-action op))))

          ((string= op "/")
           (cond ((or (soap-after-string-p ".")
                      (looking-back "^#.*" (line-beginning-position)))
                  (insert op))
                 ((and (looking-back "^ *" (line-beginning-position))
                       (memq major-mode '(c++-mode c-mode)))
                  (insert "// "))
                 (t
                  (soap-default-action op))))

          ((and (string= op "%")
                (looking-back "[ \t]+" (line-beginning-position)))
           (insert op))

          ((string= op "&")
           (cond ((soap-after-string-p "&")
                  (backward-delete-char 1)
                  (insert " && "))
                 (t
                  (insert "&"))))

          ((string= op ",")
           (if (and (looking-at "[^\n<]*>")
                    (looking-back "<[^\n>]+" (line-beginning-position)))
               (insert op)
             (insert ", ")))

          ((string= op ":")
           (insert ": "))

          ((string= op ">")
           (soap-op->))

          ((string= op "<")
           (soap-default-action op))

          ((string= op "=")
           (cond
             ((and (eq major-mode 'python-mode)
                   (looking-back "[,(][\n ]*\\(\\sw\\|\\s_\\)+" (line-beginning-position -1)))
              ;; keyword argument in Python
              (insert "="))
             ((soap-after-string-p "!")
              (backward-delete-char 1)
              (just-one-space)
              (insert "!= "))
             ((soap-after-string-p "<")
              (delete-char -1)
              (insert " <= "))
             ((looking-back "\\sw\\( ?\\+ ?\\)" (line-beginning-position))
              (delete-region (match-beginning 1)
                             (match-end 1))
              (insert " += "))
             ((soap-after-string-p "[")
              (insert op))
             (t
              (soap-default-action op))))
          (t
           (soap-default-action op)))))

(provide 'soap)

;;; soap.el ends here
