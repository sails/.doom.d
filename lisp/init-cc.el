;;; lisp/init-cc.el -*- lexical-binding: t; -*-

(setq enable-local-variables t)
;; 默认只reindex打开的文件
;; (defvar +ccls-initial-blacklist [".*"])
;; (defvar +ccls-initial-whitelist [])

;; cc-mode设置
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'c-mode-common-hook
          (lambda ()
            (google-set-c-style)

            ;; firestarter
            (firestarter-mode)

            (setenv "GTAGSFORCECPP" "1")

            ;; (add-hook 'c-mode-hook 'counsel-gtags-mode)
            ;; (add-hook 'c++-mode-hook 'counsel-gtags-mode)

            ;; (with-eval-after-load 'counsel-gtags
            ;;   (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-find-definition)
            ;;   (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
            ;;   (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
            ;;   (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))

            (add-hook 'c-mode-hook 'helm-gtags-mode)
            (add-hook 'c++-mode-hook 'helm-gtags-mode)
            (add-hook 'protobuf-mode-hook 'helm-gtags-mode)
            (with-eval-after-load 'helm-gtags
              (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
              (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag-from-here)
              (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
              (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

            (after! lsp-clangd
              (setq lsp-clients-clangd-args
                    '("-j=3"
                      "--background-index"
                      "--clang-tidy"
                      "--completion-style=detailed"
                      "--header-insertion=never"
                      "--enable-config"
                      "--header-insertion-decorators=0"))
              (set-lsp-priority! 'clangd 1)
              (lsp-diagnostics-mode -1)
              (flycheck-mode -1)
              (setq lsp-modeline-diagnostics-enable nil)
              (setq lsp-enable-file-watchers nil)
              (setq lsp-lens-enable nil)  ;; 导致cpu100%
              (setq lsp-diagnostics-provider :none)
              )

            ;; (lsp-modeline-diagnostics-mode -1)
            ;; ;; 在首次打开头文件时执行+format/region会卡住，但是当打开过cpp文件后正常
            ;; (setq +format-with-lsp nil)

            ;; (after! ccls
            ;;   (when IS-MAC
            ;;     (setq ccls-initialization-options
            ;;           (append ccls-initialization-options
            ;;                   `(:clang ,(list
            ;;                              :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
            ;;                                          "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
            ;;                                          "-isystem/usr/local/include"]
            ;;                              :resourceDir (cdr (doom-call-process "clang" "-print-resource-dir")))))))
            ;;   (when (or IS-MAC IS-LINUX)
            ;;     (setq ccls-initialization-options
            ;;           `(:index (:comments 2
            ;;                     :threads 1
            ;;                     :initialBlacklist ,+ccls-initial-blacklist
            ;;                     :initialWhitelist ,+ccls-initial-whitelist)
            ;;             :completion (:detailedLabel t))
            ;;           ))

            ;;   (lsp-diagnostics-mode -1)
            ;;   (flycheck-mode -1)
            ;;   (setq lsp-modeline-diagnostics-enable nil)
            ;;   (setq lsp-enable-file-watchers nil)
            ;;   (setq lsp-lens-enable nil)  ;; 导致cpu100%
            ;;   (setq lsp-diagnostics-provider :none)
            ;;   (set-lsp-priority! 'ccls 1)) ; optional as ccls is the default in Doom

            ;; (add-load-path! "~/.doom.d/lisp/lsp-bridge")
            ;; (require 'lsp-bridge)
            ;; (setq lsp-bridge-c-lsp-server "ccls")
            ;; (setq acm-enable-tabnine-helper nil)
            ;; (global-lsp-bridge-mode)


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

            (local-set-key  (kbd "C-x j") 'ff-find-other-file)
            ;; (local-set-key  (kbd "C-c C-c") 'ff-find-other-file)
            )

          (quickrun-add-command "c++/c11"
              '((:command . "g++")
                (:exec    . ("%c -std=c++11 %o -o %e %s"
                             "%e %a"))
                (:remove  . ("%e")))
              :default "c++")
          )


(provide 'init-cc)

;;; init-cc.el ends here
