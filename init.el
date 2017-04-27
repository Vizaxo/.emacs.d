(package-initialize)

(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"]))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 131 :width normal :foundry "DAMA" :family "Ubuntu Mono")))))

;; General settings
(setq blink-cursor-mode nil)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;; Theme
(load-theme 'deeper-blue t)

;; Packages
(setq package-archives
   (quote
    (("melpa" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
(setq package-selected-packages (quote (magit
					org
					melpa-upstream-visit)))

;; Ido fuzzy-search
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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
