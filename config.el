;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "sailsxu"
      user-mail-address "sailsxu@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'sails-light)
(setq sails-light-brighter-comments t)
;; (setq doom-theme 'doom-one-light)
;;(setq doom-font (font-spec :family "JetBrains Mono" :size 12))
(setq doom-font (font-spec :family "Fira code" :size 12))
;;(setq doom-theme 'doom-solarized-dark-high-contrast)
;; (use-package ef-themes
;;   :init
;;   (ef-themes-select 'ef-duo-light)
;;   )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(when IS-MAC
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; (when (display-graphic-p)
;;   (setq doom-modeline-height 2)
;;   (setq doom-modeline-icon nil)
;; )

;; 当使用vertico时，补全列表中默认没有开启recent files
(recentf-mode 1)

;; 高效的选中region
(global-set-key (kbd "C-x m") 'er/expand-region)

;; (pushnew! initial-frame-alist '(width . 100) '(height . 55))
;; 设置窗口位置
(defun my/frame-recenter (&optional frame)
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                            frame)
                      (selected-frame)))
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))

           (frame-w (truncate (* monitor-w 0.5)))
           (frame-h (truncate (* monitor-h 0.9)))


           (a-left (truncate (/ (- monitor-w frame-w) 2))))

           (set-frame-position (selected-frame) a-left 0)
           (set-frame-size (selected-frame) (truncate frame-w)  (truncate frame-h) t)
      )))

(add-hook 'after-init-hook #'my/frame-recenter)
(add-hook 'after-make-frame-functions #'my/frame-recenter)

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

(map! "C-c p f" #'projectile-find-file)
(map! "C-x c i" #'helm-imenu)


;; 自动折行
(setq-default truncate-lines nil)

;; 保存时自动格式化
(setq +format-on-save-enabled-modes
      '(go-mode
        rustic-mode
        sql-mode))

;; (add-hook 'rustic-mode-hook #'format-all-mode)

;;;; Mouse scrolling in terminal emacs
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  )

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

(global-set-key (kbd "C-x b") 'consult-buffer)
;; (global-set-key (kbd "C-s") 'consult-line)
isearch-forward
;; completion ivy
(after! ivy
  (global-set-key (kbd "C-x b") '+ivy/switch-buffer)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate))

;; flyecheck默认关闭
(setq flycheck-checker-error-threshold 5000)
(add-hook 'prog-mode-hook
          (lambda ()
            (flycheck-mode -1)
            ))


; Default doom threshold of 400 is too low in my experience.
(after! so-long (setq so-long-threshold 1000))

;; (setq ns-use-proxy-icon nil)
;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (file-relative-name buffer-file-name (projectile-project-root))
;;                    ;;(buffer-name)
;;                  "%b"))))
;; 光亮当前行
;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(add-load-path! "~/.doom.d/lisp")
(require 'init-convert)
(require 'init-cc)
(require 'init-shell)
(require 'init-exec-path)

;;doom uses it to highlight incorrect indentation in buffers and activates it default
;; (global-whitespace-mode nil)

;; 大小写M-u,M-l
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(when (display-graphic-p)
  (use-package! leetcode
    :defer t
    ;; :load-path  "lisp/leetcode"
    :config
    (setq leetcode-save-solutions t)
    (setq leetcode-prefer-language "cpp")
    (setq leetcode-directory "~/.leetcode")
    ;; :commands leetcode
    )
  )

;; 设置cache文件数50w,如果太小，会在重启后清理，导致每次启动后运行projectile都需要重建
(setq doom-projectile-cache-limit 500000)

(add-to-list 'doom-large-file-excluded-modes 'c++-mode)

;; (use-package hide-mode-line
;;   :ensure t
;;   :config
;;   ;; (add-hook 'prog-mode #'hide-mode-line-mode)
;;   ;; (add-hook 'go-mode-hook #'hide-mode-line-mode)
;;   (global-hide-mode-line-mode)
;;   )

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; 光标
;; (blink-cursor-mode t)

;; fringe-mode(左侧边缘宽度，有几种设置)
;; fringe-mode和vi-tilde-fringe-mode打开时,默认buffer尾部空白处会有波浪线
(remove-hook 'prog-mode-hook 'vi-tilde-fringe-mode)
(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)

;; 平滑滚动，但cpu占用很高
;;(pixel-scroll-precision-mode 1)
(setq scroll-margin 4)


;; 自动识别文件编码
 (unicad-mode 1)

;; (use-package! magit
;;   :config
;;   ;;(setq magit-refresh-verbose 1)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
;;   ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
;;   )

;; 透明
;; (set-frame-parameter (selected-frame) 'alpha '(85 100))
;; (add-to-list 'default-frame-alist '(alpha 97 100))


(setq enable-remote-dir-locals t)

;; (map! "M-n" #'forward-paragraph)
;; (map! "M-p" #'backward-paragraph)
(global-set-key (kbd "M-n")
    (lambda () (interactive) (forward-line  10)))
(global-set-key (kbd "M-p")
    (lambda () (interactive) (forward-line -10)))

;; eglot提示在minibuffer会占用多行，让页面跳动，如果不占用多行，显示的内容又没有意义，所以关闭eldoc-mode
;; (setq eldoc-echo-area-use-multiline-p nil) ;; 不占用多行
;; (eldoc-mode nil)

;; 关闭一些告警
(setq byte-compile-warnings
      '(not
        ;; free-vars
        ;; unresolved
        ;; callargs
        ;; redefine
        ;; obsolete
        ;; noruntime
        ;; interactive-only
        ;; lexical
        ;; lexical-dynamic
        ;; make-local
        ;; mapcar
        ;; not-unused
        ;; constants
        docstrings
        ;; docstrings-non-ascii-quotes
        ;; suspicious
        ))


;; 分配一定量的内存后会触发一次gc
;; 在超过1000行的c++文件上滚动时，会发现偶尔卡住，可以通过下garbage-collection-messages来查看是否发起了gc回收
;; (setq garbage-collection-messages t)
;; 可以通过调整回收阀值来优化滚动速度，但也不能太高，否则回收时会明显感受卡顿
;; https://github.com/doomemacs/doomemacs/issues/3108
(after! gcmh
  (setq gcmh-high-cons-threshold (* 64 1024 1024)))


;; 加快与外部进程交互速度(eg: eshell)
(setq process-adaptive-read-buffering nil)

;;
(setq ag-highlight-search t)

;; (use-package aichat
;;   :ensure nil
;;   :load-path "~/.doom.d/lisp/aichat")

(use-package blink-search
  :ensure nil
  :load-path "~/.doom.d/lisp/blink-search")
