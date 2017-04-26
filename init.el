(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(menu-bar-mode nil)
 '(nyan-wavy-trail t)
 '(package-archives
   (quote
    (("melpa" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages (quote (magit helm org melpa-upstream-visit)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 131 :width normal :foundry "DAMA" :family "Ubuntu Mono")))))

(setq org-refile-use-outline-path 'file)
(setq org-directory "~/data/org")
(setq org-agenda-files '("~/data/org"))
(setq org-refile-targets '((org-agenda-files :level . 1)))
(setq select-enable-primary t)
(setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;;Put all backup files into ~/tmp/backups
(setq backup-directory-alist '(("." . "~/tmp/backups")))
(setq backup-by-copying t)
(setq org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-capture-templates '(("a" "" entry (file "~/data/org/inbox.org") "")))
(define-key global-map "\C-cc"
  (lambda () (interactive) (org-capture nil "a")))
