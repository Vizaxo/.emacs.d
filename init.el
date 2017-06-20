;; Properly configure TLS
;; This will throw an error at startup if the connection isn't secure;
;; But the result will be cached on each machine, so Emacs doesn't
;; require an internet connection to start.
;; Requires `python`, `python -m certifi` and `gnutls-cli`
;; Install with:
;; python -m pip install --user certifi
;; apt install gnutls-bin (Debian-based)
;; dnf install gnutls-utils (Fedora)
;; Based on:
;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
(let ((tls-safe (concat user-emacs-directory "tls-safe")))
  (unless (file-exists-p tls-safe)
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
      (cond (bad-hosts
             (error (format (concat "TLS misconfigured; retrieved %s ok."
                                    "To fix:"
                                    "apt install python (Debian)"
                                    "dnf install python (Fedora)"
                                    "python -m pip install --user certifi"
                                    "apt install gnutls-bin (Debian)"
                                    "dnf install gnutls-utils (Fedora)")
                            bad-hosts))
             (url-retrieve "https://badssl.com"
                           (lambda (retrieved))))
            (t (with-temp-buffer (write-file tls-safe)))))))

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

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
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php?\\'"  . web-mode)
         ("\\.js?\\'"   . web-mode)
         ("\\.css?\\'"  . web-mode)))

(use-package magit
  :ensure t
  :config
  (defun my-magit-status ()
    (interactive)
    (if (string= (buffer-name) "jetbrains-idea-ce")
        (magit-status-internal "~/data/programming/Terasology/")
      (magit-status)))
  :bind
  (("C-c g" . my-magit-status)))

(use-package evil
  :ensure t
  :config
  (evil-mode))

(use-package exwm
  :ensure t
  :config
  (require 'exwm)
  (require 'exwm-config)

  ;; Set the initial workspace number.
  (setq exwm-workspace-number 0)
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  ;; Don't use evil-mode in exwm buffers
  (add-to-list 'evil-emacs-state-modes 'exwm-mode)

  ;; 's-w': Switch workspace
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
  ;; 's-N': Switch to certain workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
  ;; 's-r': Launch application
  (exwm-input-set-key (kbd "s-r")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

  ;; Better window management
  (exwm-input-set-key (kbd "s-h") 'windmove-left)
  (exwm-input-set-key (kbd "s-j") 'windmove-down)
  (exwm-input-set-key (kbd "s-k") 'windmove-up)
  (exwm-input-set-key (kbd "s-l") 'windmove-right)

  (exwm-input-set-key (kbd "s-s") 'split-window-right)
  (exwm-input-set-key (kbd "s-v") 'split-window-vertically)
  (exwm-input-set-key (kbd "s-d") 'delete-window)

  ;; Save my hands
  (exwm-input-set-key (kbd "s-f") 'find-file)
  (exwm-input-set-key (kbd "s-b") 'ido-switch-buffer)

  ;; Line-editing shortcuts
  (exwm-input-set-simulation-keys
   '(([?\C-b] . left)
     ([?\C-f] . right)
     ([?\M-f] . C-right)
     ([?\M-b] . C-left)
     ([?\C-y] . S-insert)
     ([?\C-p] . up)
     ([?\C-n] . down)
     ([?\C-a] . home)
     ([?\C-e] . end)
     ([?\M-v] . prior)
     ([?\C-v] . next)
     ([?\C-d] . delete)
     ([?\C-k] . (S-end delete))))
  ;; Configure Ido
  (exwm-config-ido)
  ;; Other configurations
  (exwm-config-misc)

  ;; Allow switching buffers between workspaces
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)

  (call-process-shell-command "~/data/scripts/startup.sh"))

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Lots of improvements from https://gist.github.com/belak/ca1c9ae75e53324ee16e2e5289a9c4bc
;; TODO: add moree, and check out other configs, starter packs, etc.
;; TODO: maybe configure fonts?
;; TODO: properly setup evil-mode bindings
(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always))

(use-package ido-ubiquitous
  :ensure t
  :config
  (ido-ubiquitous-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        ido-vertical-show-count t)
  (ido-vertical-mode 1))

(use-package flx-ido
  :config
  (flx-ido-mode 1))

(use-package org
  :ensure t
  :config
  (setq org-refile-use-outline-path 'file)
  (setq org-directory "~/data/org")
  (setq org-agenda-files '("~/data/org" "~/data/notes/"))
  (setq org-refile-targets '((org-agenda-files :level . 1)
			     (("~/data/notes") :level . 1)))
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-capture-templates
        '(("i" "Capture to inbox"
           entry (file "~/data/org/inbox.org") "* %?")
          ("f" "Capture to inbox with a link to the current file"
           entry (file "~/data/org/inbox.org") "* %?\n  %a")))
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)))

(use-package eclim
  :ensure t
  :config
  (setq eclim-executable "/home/mitch/programs/eclipse-neon/eclim")
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.01)
  (help-at-pt-set-timer)
  (add-hook 'java-mode-hook 'eclim-mode))

(use-package company-emacs-eclim
  :ensure t
  :config
  (company-emacs-eclim-setup)
  (global-company-mode t)
  (setq company-minimum-prefix-length 0)
  (setq company-idle-delay 0.01))

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
(setq select-enable-primary t)
(setq-default indent-tabs-mode nil)

;; Put all backup files into ~/tmp/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)

;; Set the font
(set-face-attribute 'default nil :height 130 :weight 'bold)
