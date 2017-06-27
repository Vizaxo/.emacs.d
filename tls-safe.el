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
