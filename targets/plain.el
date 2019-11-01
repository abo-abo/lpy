(require 'lpy)

(use-package ivy
  :config (ivy-mode))

(use-package company-jedi)

(use-package python-environment
  :config
  (setq python-environment-virtualenv
        '("virtualenv" "--system-site-packages" "--quiet" "--python" "/usr/bin/python3")))

(use-package jedi
  :config
  (setq jedi:environment-root "jedi")
  (setq jedi:setup-function nil)
  (setq jedi:complete-on-dot nil)
  (setf (symbol-function #'jedi:handle-post-command) (lambda nil nil)))

(defun lpy-python-hook ()
  (lpy-mode)
  (company-mode)
  (jedi:setup)
  (setq jedi:environment-virtualenv python-environment-virtualenv)
  (setq-local company-backends '(company-jedi company-dabbrev-code company-keywords))
  (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
  (electric-indent-mode -1))

(add-hook 'python-mode-hook 'lpy-python-hook)
