;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))

(package-initialize)

;;use-package setup
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
	:ensure t
	:config
	(which-key-mode))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode)))

(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)))

(use-package evil
  :ensure t
  :config
  (evil-mode))

(use-package exwm
  :ensure t
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (exwm-config-ido)
  (call-process-shell-command "~/data/scripts/startup.sh"))

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Ido fuzzy-search
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

;; Org
(setq org-refile-use-outline-path 'file)
(setq org-directory "~/data/org")
(setq org-agenda-files '("~/data/org"))
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq select-enable-primary t)
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-capture-templates '(("a" "" entry (file "~/data/org/inbox.org") "")))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; Set the font
(set-face-attribute 'default nil :height 130 :weight 'bold)

;; General settings
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(global-linum-mode 1)
(display-time)

;; Put all backup files into ~/tmp/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

