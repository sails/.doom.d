;;; lisp/init-cc.el -*- lexical-binding: t; -*-

(setq enable-local-variables t)
;; 默认只reindex打开的文件
;; (defvar +ccls-initial-blacklist [".*"])
;; (defvar +ccls-initial-whitelist [])

;; cc-mode设置
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun my-c-mode-func ()
  (google-set-c-style)

  (setenv "GTAGSFORCECPP" "1")

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
          '("-j=1"
            "--background-index"
            "--clang-tidy"
            "--all-scopes-completion"
            "--header-insertion=iwyu"
            "--completion-style=detailed"
            "--enable-config"
            "--header-insertion-decorators=0"))

    (set-lsp-priority! 'clangd 1)
    ;; (lsp-diagnostics-mode -1)
    ;; (flycheck-mode -1)
    (setq lsp-modeline-diagnostics-enable nil)
    (setq lsp-enable-file-watchers nil)
    (setq lsp-lens-enable nil)  ;; 导致cpu100%
    (setq lsp-diagnostics-provider :none)
    (setq lsp-headerline-breadcrumb-segments '(symbols))
    )
  (after! eglot
    :config
    (set-eglot-client! 'c++-mode
                       '("clangd"
                         "-j=1"
                         "--background-index"
                         "--clang-tidy"
                         "--all-scopes-completion"
                         "--header-insertion=iwyu"
                         "--completion-style=detailed"
                         "--enable-config"
                         "--header-insertion-decorators=0"))

    ;; fix: 在if (){}回车会自动插入两行
    (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
    ;; 禁用flycheck-eglot-mode
    (use-package! flycheck-eglot
      :config
      (remove-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode)
      )
    (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
    )

  ;; 可以很方便的在头文件与cpp文件中切换
  (local-set-key
   (kbd "C-x j") 'projectile-find-other-file)
  (local-set-key
   (kbd "C-c o j") 'projectile-find-other-file)
  )

(use-package! quickrun
  :defer t
  :custom
  (quickrun-add-command "c++/c11"
    '((:command . "g++")
      (:exec    . ("%c -std=c++11 %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++")
)

(add-hook 'c-mode-common-hook 'my-c-mode-func)


(provide 'init-cc)

;;; init-cc.el ends here
