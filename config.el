;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-one-brighter-comments t)
(setq doom-one-comment-bg nil)
;;(setq doom-theme 'doom-one)

;; (setq doom-theme 'sails-light)
;; (setq sails-light-brighter-comments t)
(setq sails-light2-brighter-comments t)
(setq doom-theme 'sails-light2)
;; (setq doom-theme 'sails-light3)


;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'regular)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; (setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 12))

(when IS-MAC
  ;; 启用细线平滑
  (setq ns-use-thin-smoothing t)
  )
;; 中文字体配置
(defun init-cjk-fonts()
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset (font-spec :family "PingFang SC" :size 12))))
(add-hook 'doom-init-ui-hook 'init-cjk-fonts)

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
(setq doom-modeline-buffer-file-name-style 'buffer-mame)
;; (setq doom-modeline-major-mode-icon t)

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

           (frame-w (truncate (* monitor-w 0.58)))
           (frame-h (truncate (* monitor-h 0.85)))


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

(map! "M-c" #'kill-ring-save)
(map! "M-v" #'yank)

;; 自动折行
(setq-default truncate-lines nil)

;; 保存时自动格式化
;; 目前format-on-save有bug，先不打开+onsave选项
;; (setq +format-on-save-enabled-modes
;;       '(go-mode
;;         rustic-mode
;;         sql-mode))
;; (setq +format-on-save-disabled-modes
;;       '(c-mode
;;         cc-mode
;;         c++-mode
;;         emacs-lisp-mode  ; elisp's mechanisms are good enough
;;         sql-mode         ; sqlformat is currently broken
;;         tex-mode         ; latexindent is broken
;;         latex-mode))


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


(require 'consult)
(after! consult
  (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; (global-set-key (kbd "C-s") 'consult-line)
  ;; 关闭consult-buffer的preview，在切换buffer时总是会先显示排在第一的文件内容，影响注意力
  (consult-customize consult-buffer :preview-key nil))

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


;; 单行长文本
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; 在doom中默认titlebar与编辑框是相同颜色，但显得头轻脚重，这里设置成不同颜色
;; (defun my-ns-transparent-titlebar-advice (&rest _args)
;;   (set-frame-parameter nil 'ns-transparent-titlebar nil))
;; (advice-add 'ns-auto-titlebar-set-frame :after 'my-ns-transparent-titlebar-advice)
;;(setq ns-use-proxy-icon nil)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (file-relative-name buffer-file-name (projectile-project-root))
                   ;; (buffer-name)
                 "%b"))))
;;  高亮当前行
;; (remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(add-load-path! "~/.config/doom/lisp")
(require 'init-convert)
(require 'init-cc)
(require 'init-shell)
(require 'init-exec-path)

;;doom uses it to highlight incorrect indentation in buffers and activates it default
;; (global-whitespace-mode nil)
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

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

(use-package! hide-mode-line
  :config
  (add-hook! '(text-mode-hook prog-mode-hook) #'hide-mode-line-mode)
  (global-hide-mode-line-mode)
  (setq hide-mode-line-excluded-modes '())  ;; 默认设置fundamental-mode会被排除，这里让它也可以隐藏
  (defun my-delayed-hide-mode-setup ()
    (run-at-time "0.1 sec" nil
                 (lambda ()
                   (hide-mode-line-mode)
                   )))
  (add-hook! '(magit-status-mode-hook magit-log-mode-hook helm-gtags-mode-hook) 'my-delayed-hide-mode-setup)
  (defun my/hide-mode-line-after-helm-gtags-pop-stack (&rest _)
    (hide-mode-line-mode 1))
  (advice-add 'helm-gtags-pop-stack :after #'my/hide-mode-line-after-helm-gtags-pop-stack)
  )

(use-package! anzu
  :after-call isearch-mode
  :config
  (global-anzu-mode 1))

(use-package! awesome-tray
  :init
  (defface awesome-tray-green-face
    '((((background light)) :foreground "#00a400" :bold nil)
      (t :foreground "green3" :bold nil))
    "Awesome tray green."
    :group 'awesome-tray)
  (defface awesome-tray-orange-face
    '((((background light)) :foreground "#cc7700" :bold nil)
      (t :foreground "#ff9500" :bold nil))
    "Awesome tray orange."
    :group 'awesome-tray)
  (defface awesome-tray-red-face
    '((((background light)) :foreground "#cc2444" :bold nil)
      (t :foreground "#ff2d55" :bold nil))
    "Awesome tray red."
    :group 'awesome-tray)
  (global-anzu-mode 1)
  (awesome-tray-mode 1)
  :config
  (setq awesome-tray-active-modules '("anzu" "buffer-name" "location" "mode-name" "belong"))
  ;;(setq awesome-tray-active-modules '("anzu" "buffer-name" "location"))
  (setq awesome-tray-buffer-name-max-length 30)
  (setq awesome-tray-file-path-show-filename nil)
  (setq awesome-tray-file-path-truncated-name-length 5) ;; default 1
  (setq awesome-tray-location-format "(%l:%c)")
  (setq awesome-tray-git-format "%s")
  )

;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;; 光标
(blink-cursor-mode t)
;; 样式
;;(setq-default cursor-type 'bar)
;;(add-hook 'eshell-mode-hook '(lambda () (setq cursor-type 't)))
;;(add-hook 'minibuffer-setup-hook '(lambda () (setq cursor-type 't)))
;; 设置不在text区域的鼠标样式 (‘arrow’, ‘text’, ‘hand’, ‘vdrag’, ‘hdrag’, ‘nhdrag’, ‘modeline’, ‘hourglass’)
(setq void-text-area-pointer 'text)


;; fringe-mode(左侧边缘宽度，有几种设置)
;; fringe-mode和vi-tilde-fringe-mode打开时,默认buffer尾部空白处会有波浪线
(remove-hook 'prog-mode-hook 'vi-tilde-fringe-mode)
(remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)

;; 平滑滚动，但cpu占用很高
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
;; 鼠标滚轮平滑滚动
(defalias 'scroll-up-command 'pixel-scroll-interpolate-up)
(defalias 'scroll-down-command 'pixel-scroll-interpolate-down)
;; when search words at the bottom of the screen, It's not easy to notice
;; 防止搜索到的数据太靠边框
(defadvice isearch-update (before my-isearch-update activate)
  (sit-for 0)
  (if (and
       ;; not the scrolling command
       (not (eq this-command 'isearch-other-control-char))
       ;; not the empty string
       (> (length isearch-string) 0)
       ;; not the first key (to lazy highlight all matches w/o recenter)
       (> (length isearch-cmds) 2)
       ;; the point in within the given window boundaries
       (let ((line (count-screen-lines (point) (window-start))))
         (or (> line (* (/ (window-height) 4) 3))
             (< line (* (/ (window-height) 9) 1)))))
      (let ((recenter-position 0.3))
        (recenter '(4)))))
;; scroll-margin lines of margin at the top and bottom of a window, default:0.
(setq scroll-margin 0
      scroll-conservatively 101)

;; only for emacs-mac
;; (use-package ultra-scroll-mac
;;   :if (eq window-system 'mac)
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   :config
;;   (ultra-scroll-mac-mode 1))

;; 自动识别文件编码
(unicad-mode 1)

;; 透明
;; (set-frame-parameter (selected-frame) 'alpha '(95 100))
;; (add-to-list 'default-frame-alist '(alpha 97 100))


(setq enable-remote-dir-locals t)

(defun my-move-up ()
  "Move up 10 lines."
  (interactive)
  (previous-line 10))

(defun my-move-down ()
  "Move down 10 lines."
  (interactive)
  (next-line 10))

(global-set-key (kbd "M-p") 'my-move-up)
(global-set-key (kbd "M-n") 'my-move-down)
;; markdown中M-n绑定到了其它命令
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "M-p") 'my-move-up)
            (define-key markdown-mode-map (kbd "M-n") 'my-move-down)))

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
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))


;; 加快与外部进程交互速度(eg: eshell)
(setq process-adaptive-read-buffering nil)

;;
(setq ag-highlight-search t)

;; (use-package lsp-bridge
;;   :load-path "~/.config/doom/lisp/lsp-bridge"
;;   :hook (prog-mode . global-lsp-bridge-mode)
;;   :config
;;   (setq lsp-bridge-enable-with-tramp 1
;;         lsp-bridge-remote-start-automatically t
;;         lsp-bridge-enable-log 1
;;         lsp-bridge-python-command "/usr/bin/python3"
;;         lsp-bridge-user-ssh-private-key "~/.ssh/dev_rsa"
;;         )
;;   ;; need add lsp_bridge.py in remote server execute PATH.
;;   ;; if server not support rsa-sha2-512 rsa-sha2-256 when sshd version too old
;;   ;; can set disabled_algorithms=dict(pubkeys=["rsa-sha2-512", "rsa-sha2-256"]
;;   ;; in remote_file.py when call connect.
;;   )


;; (setq mouse-wheel-progressive-speed t)

;; 需要在init中开启vertico posframe选项
(use-package! vertico-posframe
 :after vertico
 :config
 (vertico-posframe-mode 1)
 (setq vertico-posframe-border-width 1)
 (add-hook 'doom-after-reload-hook #'posframe-delete-all)
 ;; (setq vertico-posframe-poshandler #'posframe-poshandler-frame-top-center)  ;; 默认在中间
 (setq vertico-posframe-parameters '((left-fringe . 8)
                                     (right-fringe . 8)))
 (setq vertico-posframe-width 120)
 )

;; tramp
;; if remote server use zsh, need setting this at the top of .zshrc
;; [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
;; (setq tramp-default-method "rsync")
 ;; 增加压缩传输的文件起始大小
(setq tramp-inline-compress-start-size (* 1024 8))
 ;; 当文件大小超过 tramp-copy-size-limit 时，用 external methods(如 scp）来传输，从而大大提高拷贝效率。
(setq tramp-copy-size-limit (* 1024 10))


;; 性能问题
;; 用'(center repeated)会导致在滚动时cpu异常高
;; 224的二进制是11100000表示3，共有18个224表示 3x18 的位图
;; (setq bmp-middle-vector (make-vector 18 224))
;; (after! diff-hl
;;   (defadvice! +vc-gutter-define-thin-bitmaps-a (&rest args)
;;     :override #'diff-hl-define-bitmaps
;;     (define-fringe-bitmap 'diff-hl-bmp-middle bmp-middle-vector nil nil 'center)
;;   ))

;; (require 'topsy)
;; (add-hook 'c++-mode-hook #'topsy-mode)

;; emacs-plus
(use-package! indent-bars
  :config
  ;; NOTE: emacs-plus on mac doens't support :stipple face
  ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/622
  (setq
   ;; indent-bars-prefer-character t
   indent-bars-no-stipple-char ?┊  ;; | ⎸
   indent-bars-width-frac 0.1
   ;; indent-bars-starting-column 0
   indent-bars-color '("DimGray" :face-bg t :blend 1)
   indent-bars-highlight-current-depth '(:face default :blend 0.2) ;; 改变当前列颜色
   )
  )

;; (use-package treesit-auto
;;  :custom
;;  (treesit-auto-install 'prompt)
;;  :config
;;  (treesit-auto-add-to-auto-mode-alist 'all)
;;  (global-treesit-auto-mode))

;; emacs-mac bug https://github.com/railwaycat/homebrew-emacsmacport/issues/362
;; (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

;; show function in modeline
;; 当使用lsp +eglot时which-function-mode会影响打开文件的速度，像是需要等eglot解析完成才能打开
;; 有时还会报which-func-ff-hook error ... jsonrpc-error-message . "Timed out"，才能打开
;; 但在lsp-mode中不会,应该是没有用lsp-mode的数据，lsp-mode还没有解析完，函数已经能正常显示
;; (which-function-mode 1)

(setq ssh-deploy-verbose 0)

;; (after! grip-mode
;;   ;; first need brew install grip, and then build emacs with --with-xwidgets
;;   (setq grip-preview-use-webkit t))

(defun my-lsp-format-region-advice (&rest _args)
  "Advice function to run after `lsp-format-region` to deactivate the mark."
  ;; 取消区域标记
  (deactivate-mark))

;; 添加 advice 到 lsp-format-region 的 :after 钩子
(advice-add 'lsp-format-region :after #'my-lsp-format-region-advice)

;; (use-package holo-layer
;;   :load-path "~/.config/doom/lisp/holo-layer"
;;   :config
;;   (setq holo-layer-enable-cursor-animation t)
;;   ;; (setq holo-layer-enable-indent-rainbow t)
;;   (setq holo-layer-python-command "/usr/bin/python3")
;;   (holo-layer-enable)
;;   )
