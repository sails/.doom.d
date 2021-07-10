;;; lisp/init-cc.el -*- lexical-binding: t; -*-

(setq enable-local-variables t)
(defvar +ccls-initial-blacklist [])
(defvar +ccls-initial-whitelist [])
;; cc-mode设置
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (google-set-c-style)

            ;; firestarter
            (firestarter-mode)

            (setenv "GTAGSFORCECPP" "1")
            (add-hook 'c-mode-hook 'helm-gtags-mode)
            (add-hook 'c++-mode-hook 'helm-gtags-mode)
            (add-hook 'protobuf-mode-hook 'helm-gtags-mode)
            (with-eval-after-load 'helm-gtags
              (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
              (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag-from-here)
              (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
              (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

            (require 'ccls)
            (setq ccls-initialization-options
                  `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                              "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                              "-isystem/usr/local/include"]
                                  :resourceDir (cdr (doom-call-process "clang" "-print-resource-dir")))))


            (add-hook 'lsp-mode-hook (lambda ()
                      ;; 顶部目录、文件、方法breadcrumb
                      (setq lsp-headerline-breadcrumb-enable nil)
                      (setq lsp-modeline-diagnostics-enable nil)

                      (lsp-diagnostics-mode -1)
                      (flycheck-mode -1)
                      ))
            (add-hook 'hack-local-variables-hook
                      (lambda ()
                        (when (derived-mode-p 'c++-mode)
                          ;; +ccls-initial-blacklist +ccls-initial-whitelist在dir-locals.el重新中设置新值
                          (setq ccls-initialization-options
                                (append ccls-initialization-options
                                        `(:index (:threads 1 :initialBlacklist ,+ccls-initial-blacklist :initialWhitelist ,+ccls-initial-whitelist))))
                          ;; (print ccls-initialization-options)
                          (setq lsp-enable-file-watchers nil)
                          (setq lsp-diagnostics-provider :none)
                          (lsp)
                          )
                        ))

            ;; 可以很方便的在头文件与cpp文件中切换
            (setq cc-other-file-alist
                  '(("\\.cc\\'"  (".hh" ".h")) ("\\.hh\\'"  (".cc" ".C"))
                    ("\\.c\\'"   (".h")) ("\\.h\\'"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
                    ("\\.m\\'"    (".h")) ("\\.mm\\'"    (".h"))
                    ("\\.C\\'"   (".H"  ".hh" ".h")) ("\\.H\\'"   (".C"  ".CC"))
                    ("\\.CC\\'"  (".HH" ".H"  ".hh" ".h")) ("\\.HH\\'"  (".CC"))
                    ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++"))
                    ("\\.cpp\\'" (".hpp" ".hh" ".h" "_p.h")) ("\\.hpp\\'" (".cpp"))
                    ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx"))))
            (setq ff-search-directories '("." "../src" "../include"))

            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            ;; (local-set-key  (kbd "C-c C-c") 'ff-find-other-file)
            )
          )


(provide 'init-cc)

;;; init-cc.el ends here
