;;; lpy.el --- A lispy interface to Python -*- lexical-binding: t -*-

(require 'lispy)
(require 'jedi)
(require 'soap)
(require 'flyspell)

(defgroup lpy nil
  "List navigation and editing for Python."
  :group 'bindings
  :prefix "lpy-")
(require 'org)

(defconst lpy-font-lock-keywords
  '(("^#\\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^#\\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^#\\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^#\\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^#\\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    (lpy-outline-comment-highlight 1 'default prepend)
    ;; ("^#  \\([^ ].*\\)$" 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

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
        (setq-local outline-level 'lispy-outline-level)
        (setq-local forward-sexp-function 'lpy-forward-sexp-function)
        (font-lock-add-keywords major-mode lpy-font-lock-keywords))
    (font-lock-remove-keywords major-mode lpy-font-lock-keywords)
    (setq-local forward-sexp-function nil)))

(defun lpy-outline-comment-highlight (limit)
  (when (re-search-forward "^#\\(?:[^*]\\|$\\)" limit t)
    (let* ((pt (point))
           (success (save-excursion
                      (and (re-search-backward "^#\\*" nil t)
                           (null (re-search-forward "^[^#]" pt t))))))
      (when success
        (set-match-data (list (line-beginning-position) (line-end-position)
                              (point) (line-end-position)))
        (end-of-line)
        t))))

(defun lpy-left-p ()
  (looking-at lispy-left))

(defun lpy-right-p ()
  (looking-back lispy-right
                (line-beginning-position)))

(defun lpy-space (arg)
  (interactive "p")
  (cond
    ((lispy--in-string-or-comment-p)
     (self-insert-command arg))
    ((looking-back "^ *" (line-beginning-position))
     (skip-chars-forward " ")
     (when (eq (char-before) ?\ )
       (backward-char)))
    (t
     (self-insert-command arg))))

(defun lpy-line-left-p ()
  (or (and (bolp)
           (not (or
                 (looking-at " *$")
                 (memq last-command '(lpy-space newline))
                 (looking-at "[ \n]*\\'"))))
      (and (looking-at " ")
           (looking-back "^ +" (line-beginning-position)))))

(defun lpy-line-right-p ()
  (or ;; (eolp)
   (looking-at "#+$")))

(defun lpy-outline-add ()
  (interactive)
  (when (looking-at lispy-outline)
    (let ((lvl (lispy-outline-level)))
      (goto-char
       (lispy--outline-end))
      (unless (looking-back "\n\n")
        (newline))
      (insert lispy-outline-header
              (make-string lvl ?\*)
              " ")
      (beginning-of-line))))

(defun lpy-tab ()
  (interactive)
  (cond ((lispy--in-comment-p)
         (lispy-tab))
        ((region-active-p)
         (if (lpy-listp)
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
                     (goto-char (match-beginning 0)))))))
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
           (goto-char beg)))
        (t
         (let ((forward-sexp-function nil))
           (indent-sexp)))))

(defun lpy--insert-or-call (def)
  `(lambda ()
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     (interactive)
     (unless (looking-at lispy-outline)
       (lispy--ensure-visible))
     (cond ((or (region-active-p)
                (and (lpy-listp)
                     (or (lpy-arg-leftp)
                         (lpy-arg-rightp))))
            (call-interactively ',def))

           ((lispy--in-string-or-comment-p)
            (call-interactively 'self-insert-command))

           ((or (lpy-left-p)
                (lpy-right-p)
                (lpy-line-left-p)
                (lpy-line-right-p)
                (and (lispy-bolp)
                     (not (memq last-command '(self-insert-command
                                               soap-command
                                               newline)))
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
    (unless (memq func flyspell-delayed-commands)
      (add-to-list 'flyspell-delayed-commands func))
    (define-key keymap (kbd key) func)))

(defun lpy-line-p ()
  (and (region-active-p)
       (= (region-end) (line-end-position))
       (save-excursion
         (goto-char (region-beginning))
         (lispy-bolp))))

(defvar lpy-listp-last nil)

(defun lpy-listp ()
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
               end))))))

(defun lpy-arg-leftp ()
  (or (eq (point) (1+ (car lpy-listp-last)))
      (and (looking-at " ")
           (looking-back
            ",[ \n]*"
            (- (line-beginning-position) 2)))))

(defun lpy-arg-rightp ()
  (or (and (eq (point) (1- (cdr lpy-listp-last)))
           (not (lispy-after-string-p " ")))
      (looking-at ",")))

(defun lpy-arg-forward ()
  (while (and (< (point) (point-max))
              (not (looking-at ",")))
    (forward-sexp 1)))

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
                          (lpy-arg-forward))
                         ((or (lpy-arg-rightp)
                              (lpy-arg-leftp))
                          (forward-char 1)
                          (lpy-arg-forward))
                         (t
                          (forward-sexp arg))))
               (lispy-dotimes (- arg)
                 (if (or (= (point) (point-max))
                         (or (lpy-arg-rightp)
                             (lpy-arg-leftp)))
                     (progn
                       (forward-sexp -1)
                       (skip-chars-backward ", \n")
                       (while (and (> (point) (point-min))
                                   (not (looking-at ",")))
                         (forward-sexp -1)
                         (skip-chars-backward ", \n"))
                       (skip-chars-forward ", \n")
                       (unless (bobp)
                         (backward-char)))
                   (backward-sexp))))))
          ((looking-at " +\\*")
           (goto-char (match-end 0)))
          (t
           (forward-sexp arg)))))

(defun lpy-bof-position ()
  (save-excursion
    (if (beginning-of-defun)
        (point)
      (line-beginning-position))))

(defun lpy-next-top-level-sexp ()
  (forward-char 1)
  (re-search-forward "^[^ \n]" (lispy--outline-end) t)
  (forward-char -1))

(defhydra hydra-lispy-move (:pre
                            (progn
                              (setq hydra-is-helpful nil)
                              (set-cursor-color "#e52b50"))
                            :before-exit
                            (progn
                              (setq hydra-is-helpful t)
                              (set-cursor-color "#ffffff")))
  ("h" lispy-move-left "left")
  ("j" lispy-move-down "down")
  ("k" lispy-move-up "up")
  ("l" lispy-move-right "right")
  ("v" lpy-outline-edit-below "edit outline" :exit t)
  ("n" lpy-outline-add "new" :exit t)
  ("q" nil "quit")
  ("c" nil "quit"))

(defun lpy-outline-edit-below ()
  (interactive)
  (beginning-of-line)
  (forward-line 1)
  (unless (bolp)
    (newline)))

(defun lpy-down (arg)
  "Move down ARG times inside the current sexp."
  (interactive "p")
  (lispy--remember)
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
        ((lpy-listp)
         (cond ((lpy-arg-leftp)
                (let ((pt (point)))
                  (forward-sexp arg)
                  (if (forward-sexp 1)
                      (backward-sexp 1)
                    (goto-char pt)
                    nil)))
               ((lpy-arg-rightp)
                (forward-sexp arg))))
        ((looking-at outline-regexp)
         (lispy-down arg))
        ((lpy-line-left-p)
         (if (bolp)
             (lpy-next-top-level-sexp)
           (let ((indent (buffer-substring-no-properties
                          (line-beginning-position)
                          (1+ (point))))))))))

(defun lpy-avy-symbol ()
  (interactive)
  (lispy--avy-do
   avy-goto-word-0-regexp
   (lispy--bounds-outline)
   (lambda ()
     (not (save-excursion
            (forward-char -1)
            (lispy--in-string-or-comment-p))))
   'at-full))

(defun lpy-up (arg)
  "Move up ARG times inside the current sexp."
  (interactive "p")
  (lispy--remember)
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
         (lispy-up arg))
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

           (lispy-down arg)))
        ((lpy-listp)
         (cond ((lpy-arg-leftp)
                (backward-sexp arg))
               ((lpy-arg-rightp)
                (backward-sexp arg)
                (let ((pt (point)))
                  (if (backward-sexp)
                      (forward-sexp)
                    (goto-char pt)
                    nil)))))
        ((lpy-line-left-p)
         (if (bolp)
             (re-search-backward "^[^ \n]" (1+ (lispy--outline-beg)) t)))))

(defun lpy-flow ()
  (interactive)
  (cond ((looking-at lispy-outline)
         (lpy-next-top-level-sexp))
        ((lpy-line-left-p))
        (t
         (self-insert-command 1))))

(defun lpy-left ()
  (interactive)
  (cond ((looking-at lispy-outline)
         (lispy-outline-left))
        ((lpy-line-left-p)
         (lispy--remember)
         (if (bolp)
             (goto-char (lispy--outline-beg))))
        ((or (looking-at lispy-left)
             (looking-back lispy-right (line-beginning-position)))
         (lpy-backward))
        (t
         (self-insert-command 1))))

(defun lpy-lvl ()
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun lpy-right ()
  (interactive)
  (cond ((looking-at lispy-outline)
         (lispy-outline-right))
        ((eolp)
         (let ((lvl (lpy-lvl)))))
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
               (t
                (unless (eq major-mode 'julia-mode)
                  (just-one-space))))
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
  (cond ((region-active-p)
         (deactivate-mark))
        ((eq last-command 'move-end-of-line)
         (set-mark (point))
         (back-to-indentation))
        ((lpy-listp)
         (cond ((lpy-arg-leftp)
                (set-mark (point))
                (forward-sexp))
               ((lpy-arg-rightp)
                (set-mark (point))
                (backward-sexp))))
        ((bolp)
         (set-mark (point))
         (end-of-line))
        (t
         (lispy-mark))))

(defun lpy-slurp ()
  (interactive)
  (cond ((and (region-active-p)
              (lpy-listp))
         (cond ((lpy-arg-rightp)
                (forward-sexp))
               ((lpy-arg-leftp)
                (backward-sexp))))
        (t
         (soap-command))))

(defun lpy-barf ()
  (interactive)
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
        (t
         (soap-command))))

(defun lpy-open ()
  (interactive)
  (when (and (region-active-p)
             (eq (line-number-at-pos (region-beginning))
                 (line-number-at-pos (region-end))))
    (deactivate-mark)
    (end-of-line)
    (newline-and-indent)))

(defun lpy-teleport ()
  (interactive)
  (if (looking-at lispy-outline)
      (end-of-line)
    (self-insert-command 1)))

(defun lpy-meta-return ()
  "Insert a new heading."
  (interactive)
  (unless (bolp)
    (newline))
  (insert lispy-outline-header
          (make-string (max (lispy-outline-level) 1)
                       ?\*)
          " ")
  (beginning-of-line))

(defun lpy-backward ()
  (interactive)
  (let ((pt (point)))
    (python-nav-backward-up-list)
    (or (/= pt (point))
        (cond ((bolp)
               (outline-back-to-heading)
               nil)
              (t
               (python-nav-beginning-of-statement))))))

(let ((map lpy-mode-map))
  (define-key map (kbd "]") 'lispy-forward)
  (define-key map (kbd "[") 'lpy-backward)
  (define-key map (kbd "C-h") 'lpy-back-to-outline)
  (define-key map (kbd "M-m") 'lpy-mark-symbol)
  (define-key map (kbd "M-RET") 'lpy-meta-return)
  (define-key map (kbd "C-1") 'jedi:show-doc)
  (define-key map (kbd "M-.") 'lispy-goto-symbol)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "SPC") 'lpy-space)
  (define-key map "(" 'lpy-parens)
  (define-key map "φ" 'lpy-parens)
  (lpy-define-key map "a" 'lpy-avy-symbol)
  (lpy-define-key map "b" 'lispy-back)
  (lpy-define-key map "c" 'hydra-lispy-move/body)
  (lpy-define-key map "d" 'lpy-different)
  (lpy-define-key map "e" 'lispy-eval)
  (lpy-define-key map "f" 'lpy-flow)
  (lpy-define-key map "g" 'self-insert-command)
  (lpy-define-key map "h" 'lpy-left)
  (lpy-define-key map "i" 'lpy-tab)
  (lpy-define-key map "j" 'lpy-down)
  (lpy-define-key map "k" 'lpy-up)
  (lpy-define-key map "l" 'lpy-right)
  (lpy-define-key map "m" 'lpy-mark)
  (lpy-define-key map "n" 'lispy-new-copy)
  (lpy-define-key map "o" 'lpy-open)
  (lpy-define-key map "p" 'self-insert-command)
  (lpy-define-key map "q" 'self-insert-command)
  (lpy-define-key map "r" 'self-insert-command)
  (lpy-define-key map "s" 'self-insert-command)
  (lpy-define-key map "t" 'lpy-teleport)
  (lpy-define-key map "u" 'undo)
  (lpy-define-key map "v" 'lispy-view)
  (lpy-define-key map "w" 'self-insert-command)
  (lpy-define-key map "x" 'lispy-x)
  (lpy-define-key map "y" 'self-insert-command)
  (lpy-define-key map "z" 'self-insert-command)
  (lpy-define-key map "I" 'lispy-shifttab)
  (lpy-define-key map "J" 'lispy-outline-next)
  (lpy-define-key map "K" 'lispy-outline-prev)
  (lpy-define-key map "N" 'lispy-narrow)
  (lpy-define-key map "W" 'lispy-widen)
  (define-key map ">" 'lpy-slurp)
  (define-key map "<" 'lpy-barf)
  (dolist (x (number-sequence 0 9))
    (lpy-define-key map (format "%d" x) 'digit-argument))
  (dolist (x '("+" "-" "%" "&" "|" "=" ","))
    (define-key map x 'soap-command)))

(provide 'lpy)
