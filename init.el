(package-initialize)



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

(load-file "~/.emacs.d/exwm-config.el")

;; Packages
(setq package-archives
   (quote
    (("melpa" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
(setq package-selected-packages (quote (magit
					org
					melpa-upstream-visit)))
;; Theme
(load-theme 'zenburn t)

;; Ido fuzzy-search
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)

;; Smex (Ido M-x)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; Org
(setq org-refile-use-outline-path 'file)
(setq org-directory "~/data/org")
(setq org-agenda-files '("~/data/org"))
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq select-enable-primary t)
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; Org-capture
(setq org-capture-templates '(("a" "" entry (file "~/data/org/inbox.org") "")))
(define-key global-map "\C-cc"
  (lambda () (interactive) (org-capture nil "a")))

;; Put all backup files into ~/tmp/backups
(setq backup-directory-alist '(("." . "~/tmp/backups")))
(setq backup-by-copying t)
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(exwm-enable)

;; Set the font
(set-face-attribute 'default nil :height 130 :weight 'bold)
