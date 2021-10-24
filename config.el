;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "sailsxu"
      user-mail-address "sailsxu@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one-light)
;; Disable doom-theme by commenting 'doom' in the ui section
;; 当禁用doom-theme时，改变fire字体
;; (setq doom-theme 'sails)
(if (display-graphic-p)
    (progn
      ;; if graphic
      (set-face-attribute 'default nil :font "Fira Code-12")
      )
  )


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
(when IS-MAC
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

(when (display-graphic-p)
  (setq doom-modeline-height 2)
  (setq doom-modeline-icon nil)
)
;; 当使用vertico时，补全列表中默认没有开启recent files
(recentf-mode 1)

;; 设置窗口位置
(when (display-graphic-p)
  (defun my/set-initial-frame ()
    (let* ((width-factor 0.40)
           (height-factor 0.70)
           (a-width (* (display-pixel-width) width-factor))
           (a-height (* (display-pixel-height) height-factor))
           (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
           ;; (a-top (truncate (/ (- (display-pixel-height) a-height) 2)))
           )
      ;; (set-frame-position (selected-frame) a-left a-top)
      (set-frame-position (selected-frame) a-left 0)
      (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
  (setq frame-resize-pixelwise t)
  (my/set-initial-frame)
)


;; 高效的选中region
(global-set-key (kbd "C-x m") 'er/expand-region)

;; 复制当前buffer name
(defun copy-file-name(choice)
  "Copy the `buffer-file-name` to the `kill-ring` as CHOICE."
  (interactive "cCopy Buffer Name (f) full, (d) directory, (n) name, (r) relative")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?r)
           (setq new-kill-string  (file-relative-name buffer-file-name (projectile-project-root))))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string)
      )
    )
  )


;; 代码折叠
;; (global-set-key (kbd "C-=") 'hs-show-block)
;; (global-set-key (kbd "C--") 'hs-hide-block)
(map! "C-=" #'hs-show-block
      "C--" #'hs-hide-block)


;; 自动折行
(setq-default truncate-lines nil)

;; 保存时自动格式化
(setq +format-on-save-enabled-modes
      '(go-mode
        sql-mode))

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x b") 'consult-buffer)
;; :completion ivy
;; (after! ivy
;;   (global-set-key (kbd "C-x b") '+ivy/switch-buffer)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-virtual-abbreviate 'abbreviate))

;; flyecheck默认关闭
(flycheck-mode -1)

; Default doom threshold of 400 is too low in my experience.
(after! so-long (setq so-long-threshold 1000))

(setq ns-use-proxy-icon nil)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   ;; (abbreviate-file-name (buffer-file-name))
                   ;; (file-relative-name buffer-file-name (projectile-project-root))
                   (buffer-name)
                 "%b"))))
;; 光亮当前行
;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(add-load-path! "lisp")
(require 'init-convert)
(require 'init-cc)
(require 'init-shell)

;;doom uses it to highlight incorrect indentation in buffers and activates it default
(global-whitespace-mode nil)

;; snails
(when (display-graphic-p)
  (use-package! snails
    :defer t
    :custom (snails-use-exec-path-from-shell nil)
    :load-path  "lisp/snails"
     :custom-face
     ;; (snails-content-buffer-face ((t (:background "#111" :height 110))))
     ;; (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
     ;; (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
    :commands snails
    :config
    (setq snails-show-with-frame nil)
    (map!
     (:map snails-mode-map
      :nvi "C-g" #'snails-quit
      :nvi "ESC ESC ESC" #'snail-quit
      :nvi "C-n" #'snails-select-next-item
      :nvi "C-p" #'snails-select-prev-item
      :nvi "C-v" #'snails-select-next-backend
      :nvi "M-v" #'snails-select-prev-backend
      :nvi "RET" #'snails-candidate-do
      :nvi "C-RET" #'snails-candiate-alternate-do))
    )
  )
