;;; lisp/init-shell.el -*- lexical-binding: t; -*-

;; ;; eshell-toggle
;; (use-package! eshell-toggle
;;   :ensure t
;;   :commands (eshell-toggle)
;;   :bind (("M-j" . eshell-toggle))
;;   )
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             ;;  关闭company，自动补全反而会让多次输入回车，影响速度
;;             (company-mode -1)
;;             ))

;; vterm-toggle
;; 使用vterm-toggle，而不是使用doom的vterm配置，因为它可以方便实现cd到当前目前文件目录
;; 不要打开doom中的vterm，以免设置冲突
(map! "M-j" #'vterm-toggle-cd)
(add-hook 'vterm-mode-hook
          (lambda ()
            (hide-mode-line-mode)
            (setq vterm-kill-buffer-on-exit t)
            ;; 当打开vterm后M-j替换eshell中的快捷键
            (define-key vterm-mode-map (kbd "M-j")        #'vterm-toggle-cd)
            ;; cd到当前buffer的目录
            (if (display-graphic-p)
                (progn
                  ;; if graphic
                  (define-key vterm-mode-map (kbd "M-RET")   #'vterm-toggle-insert-cd)
                  )
              ;; else (optional)
              (define-key vterm-mode-map (kbd "C-c j")   #'vterm-toggle-insert-cd)
              )
            ))

;; ;; doom +vterm/toggle
;; (map! "M-j" #'+vterm/toggle)
;; (add-hook 'vterm-mode-hook
;;           (lambda ()
;;             (define-key vterm-mode-map (kbd "M-j")        #'+vterm/toggle)
;;             )
;;           )

(provide 'init-shell)
;;; init-shell.el ends here
