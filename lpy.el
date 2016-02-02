;;; lpy.el --- A lispy interface to Python -*- lexical-binding: t -*-

(require 'lispy)
(require 'ciao)
(require 'jedi)
(require 'soap)

(defgroup lpy nil
  "List navigation and editing for Python."
  :group 'bindings
  :prefix "lpy-")

(defvar lpy-mode-map (make-sparse-keymap))

(define-minor-mode lpy-mode
  "Minor mode for navigating Python code similarly to LISP."
  :keymap lpy-mode-map
  :lighter " LPY"
  (if lpy-mode
      (progn
        (setq-local lispy-outline "^#\\*+")
        (setq lispy-outline-header "#")
        (setq-local outline-regexp "#\\*+")
        (setq-local outline-heading-end-regexp "\n")
        (setq-local forward-sexp-function 'lpy-forward-sexp-function))
    (setq-local forward-sexp-function nil)))

(defun lpy-left-p ()
  (looking-at lispy-left))

(defun lpy-right-p ()
  (looking-back lispy-right
                (line-beginning-position)))

(defun lpy--insert-or-call (def)
  `(lambda ()
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     (interactive)
     (unless (looking-at lispy-outline)
       (lispy--ensure-visible))
     (cond ((region-active-p)
            (call-interactively ',def))

           ((lispy--in-string-or-comment-p)
            (call-interactively 'self-insert-command))

           ((or (lpy-left-p)
                (lpy-right-p)
                (and (lispy-bolp)
                     (or (looking-at lispy-outline-header)
                         (looking-at lispy-outline))))
            (call-interactively ',def))

           (t
            (setq this-command 'self-insert-command)
            (call-interactively
             'self-insert-command)))))

(defun lpy-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY FUNC)."
  (declare (indent 3))
  (let ((func (defalias (intern (concat "pspecial-" (symbol-name def)))
                  (lpy--insert-or-call def))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (unless (memq func company-no-begin-commands)
      (add-to-list 'company-begin-commands func))
    (define-key keymap (kbd key) func)))

(defun lpy-line-p ()
  (and (region-active-p)
       (= (region-end) (line-end-position))
       (save-excursion
         (goto-char (region-beginning))
         (lispy-bolp))))

(defun lpy-listp ()
  (let (end)
    (when (save-excursion
            (save-restriction
              (narrow-to-defun)
              (ignore-errors
                (up-list 1)
                (setq end (point))
                (< (point) (point-max)))))
      (cons
       (save-excursion
         (goto-char end)
         (forward-list -1))
       end))))

(defun lpy-forward-sexp-function (arg)
  (let* ((forward-sexp-function nil)
         (bnd
          (let ((forward-sexp-function nil))
            (lpy-listp))))
    ;; inside function arglist
    (cond (bnd
           (save-restriction
             (narrow-to-region (1+ (car bnd))
                               (1- (cdr bnd)))
             (if (> arg 0)
                 (lispy-dotimes arg
                   (cond ((= (point) (point-min))
                          (while (and (< (point) (point-max))
                                      (not (looking-at ",")))
                            (forward-sexp 1)))
                         ((or (looking-at ",")
                              (and (looking-at " ")
                                   (looking-back ",[ \n]*"
                                                 (- (line-beginning-position) 2))))
                          (forward-char 1)
                          (while (and (< (point) (point-max))
                                      (not (looking-at ",")))
                            (forward-sexp 1)))))
               (when (or (= (point) (point-max))
                         (or (looking-at ",")
                             (and (looking-at " ")
                                  (looking-back ",[ \n]*"
                                                (- (line-beginning-position) 2)))))
                 (forward-sexp -1)
                 (skip-chars-backward ", \n")
                 (while (and (> (point) (point-min))
                             (not (looking-at ",")))
                   (forward-sexp -1)
                   (skip-chars-backward ", \n"))
                 (skip-chars-forward ", \n")
                 (unless (bobp)
                   (backward-char))))))
          (t
           (forward-sexp arg)))))

(defun lpy-down (arg)
  "Move down ARG times inside the current sexp."
  (interactive "p")
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
         (lispy-down arg))
        ((looking-at outline-regexp)
         (lispy-down arg))))

(defun lpy-bof-position ()
  (save-excursion
    (if (beginning-of-defun)
        (point)
      (line-beginning-position))))

(defun lpy-up (arg)
  "Move up ARG times inside the current sexp."
  (interactive "p")
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
             (exchange-point-and-mark))))
        ((looking-at outline-regexp)
         (lispy-up arg))))

(defun lpy-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((or (lispy-left-p)
             (lispy-right-p))
         (lispy-different))
        (t
         (user-error "Unexpected"))))

(defun lpy-mark-symbol ()
  (interactive)
  (if (or (lispy-bolp) (region-active-p))
      (progn
        (back-to-indentation)
        (set-mark (point))
        (end-of-line))
    (lispy-mark-symbol)))

(defun lpy-parens (&optional arg)
  "Insert a pair of parens."
  (interactive "P")
  (cond ((region-active-p)
         (lispy--surround-region
          "("
          ")")
         (backward-char 1)
         (just-one-space)
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
               (t (just-one-space)))
         (insert "()")
         (backward-char))))

(defvar lpy-back-to-outline nil)

(defun lpy-back-to-outline ()
  (interactive)
  (if (and (memq last-command '(lpy-back-to-outline))
           (looking-at lispy-outline))
      (lispy-pam-restore 'lpy-back-to-outline)
    (lispy-pam-store 'lpy-back-to-outline)
    (outline-back-to-heading)))

(defun lpy-mark ()
  (interactive)
  (if (looking-at ",")
      (progn
        (set-mark (point))
        (backward-sexp))
    (lispy-mark)))

(let ((map lpy-mode-map))
  (define-key map (kbd "]") 'lispy-forward)
  (define-key map (kbd "[") 'lispy-backward)
  (define-key map (kbd "C-h") 'lpy-back-to-outline)
  (define-key map (kbd "M-m") 'lpy-mark-symbol)
  (define-key map (kbd "M-RET") 'lispy-meta-return)
  (define-key map (kbd "C-1") 'jedi:show-doc)
  (define-key map (kbd "M-.") 'lispy-goto-symbol)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map "(" 'lpy-parens)
  (define-key map "φ" 'lpy-parens)

  (lpy-define-key map "e" 'lispy-eval)
  (lpy-define-key map "j" 'lpy-down)
  (lpy-define-key map "k" 'lpy-up)
  (lpy-define-key map "d" 'lpy-different)
  (lpy-define-key map "i" 'lispy-tab)
  (lpy-define-key map "I" 'lispy-shifttab)
  (lpy-define-key map "x" 'lispy-x)
  (lpy-define-key map "m" 'lpy-mark)
  (dolist (x (number-sequence 0 9))
    (lpy-define-key map (format "%d" x) 'digit-argument))
  (dolist (x '("+" "-" "%" "&" "|" "<" "=" ">" ","))
    (lpy-define-key map x 'soap-command)))

(provide 'lpy)
