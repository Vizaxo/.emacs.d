;; Properly configure TLS
;; This will throw an error at startup if the connection isn't secure
;; Requires `python`, `python -m certifi` and `gnutls-cli`
;; Install with:
;; python -m pip install --user certifi
;; apt install gnutls-bin (Debian-based)
;; dnf install gnutls-utils (Fedora)
;; Based on:
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
(setq tls-checktrust t)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

(require 'cl)
(let ((bad-hosts
       (loop for bad
	     in `("https://wrong.host.badssl.com/"
		  "https://self-signed.badssl.com/")
	     if (condition-case e
		    (url-retrieve
		     bad (lambda (retrieved) t))
		  (error nil))
	     collect bad)))
  (if bad-hosts
      (error (format (concat "TLS misconfigured; retrieved %s ok."
			     "To fix:"
			     "apt install python"
			     "dnf install python"
			     "python -m pip install --user certifi"
			     "apt install gnutls-bin (Debian-based)"
			     "dnf install gnutls-utils (Fedora)")
		     bad-hosts))
    (url-retrieve "https://badssl.com"
		  (lambda (retrieved) t))))

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
(setq org-capture-templates '(("i" "Capture to inbox" entry (file "~/data/org/inbox.org") "* %?")
			      ("f" "Capture to inbox with a link to the current file" entry (file "~/data/org/inbox.org") "* %?\n  %a")))
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

