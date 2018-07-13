;; Ensure tls is setup properly
(load-file (concat user-emacs-directory "tls-safe.el"))

;; Setup https package installation from repositories
(load-file (concat user-emacs-directory "package-setup.el"))

;; Load org-mode for org-babel
(use-package org
  :ensure t)

;; Load my main literate configuration
(org-babel-load-file (concat user-emacs-directory "config.el.org") nil)
