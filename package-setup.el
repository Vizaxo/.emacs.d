;; Basic package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
