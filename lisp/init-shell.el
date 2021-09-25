;;; lisp/init-shell.el -*- lexical-binding: t; -*-


;; eshell-toggle
(global-set-key "\M-j" 'eshell-toggle)
(require 'eshell-toggle)
;; (with-eval-after-load "esh-opt"
;;   (eshell-git-prompt-use-theme 'robbyrussell)
;;   )
(add-hook 'eshell-mode-hook
          (lambda ()
            ;;  关闭company，自动补全反而会让多次输入回车，影响速度
            (company-mode -1)
            ))

;; (map! "M-j" #'vterm-toggle-cd)
(add-hook 'vterm-mode-hook
          (lambda ()
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



            (define-key vterm-mode-map (kbd "C-c C-y") 'vterm-yank)
            ;; 在buffer底部显示
            (setq vterm-toggle-fullscreen-p nil)
            (add-to-list 'display-buffer-alist
                         '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                           (display-buffer-reuse-window display-buffer-at-bottom)
                           ;;(display-buffer-reuse-window display-buffer-in-direction)
                           ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                           ;;(direction . bottom)
                           ;;(dedicated . t) ;dedicated is supported in emacs27
                           (reusable-frames . visible)
                           (window-height . 0.3)))
            ))


(provide 'init-shell)
;;; init-shell.el ends here
