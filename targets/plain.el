(require 'lpy)

(use-package ivy
  :config (ivy-mode))

(use-package company-jedi)

(use-package jedi
  :config
  (setq jedi:setup-function nil)
  (setf (symbol-function #'jedi:handle-post-command) (lambda nil nil)))

(defun lpy-python-hook ()
  (lpy-mode)
  (company-mode)
  (jedi:setup)
  (setq-local company-backends '(company-jedi company-dabbrev-code company-keywords))
  (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
  (electric-indent-mode -1))

(add-hook 'python-mode-hook 'lpy-python-hook)
