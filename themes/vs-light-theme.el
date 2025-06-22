;;; vs-light-theme.el --- VSCode Light+ style Doom theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: ChatGPT
;; Inspired by VSCode Light+ and Doom themes
;;
;;; Commentary:
;;  A light theme for Doom Emacs, closely mimicking VSCode Light+.
;;
;;; Code:

(require 'doom-themes)

(defgroup vs-light-theme nil
  "Options for the `sails-light' theme."
  :group 'doom-themes)

(defcustom sails-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'vs-light-theme
  :type '(choice integer boolean))

(def-doom-theme sails-light
  "A light theme inspired by VSCode Light+."
  :background-mode 'light

  ((bg         '("#ffffff" "white"   "white"))
   (fg         '("#333333" "#333333" "black"))
   (bg-alt     '("#f3f3f3" "white"   "white"))
   (fg-alt     '("#2d2d2d" "#2d2d2d" "brightblack"))
   (base0      '("#ffffff" "#ffffff" "white"))
   (base1      '("#f3f3f3" "#f3f3f3" "brightblack"))
   (base2      '("#e5e5e5" "#e5e5e5" "brightblack"))
   (base3      '("#d4d4d4" "#d4d4d4" "brightblack"))
   (base4      '("#bfbfbf" "#bfbfbf" "brightblack"))
   (base5      '("#333333" "#333333" "brightblack"))
   (base6      '("#2d2d2d" "#2d2d2d" "brightblack"))
   (base7      '("#1e1e1e" "#1e1e1e" "brightblack"))
   (base8      '("#000000" "black"   "black"))
   (grey       base4)
   (red        '("#e51400" "#e51400" "red"))
   (orange     '("#b89500" "#b89500" "brightred"))
   (green      '("#008000" "#008000" "green"))
   (teal       '("#008080" "#008080" "brightgreen"))
   (yellow     '("#b89500" "#b89500" "yellow"))
   (blue       '("#0000ff" "#0000ff" "brightblue"))
   (dark-blue  '("#001080" "#001080" "blue"))
   (magenta    '("#af00db" "#af00db" "magenta"))
   (violet     '("#68217a" "#68217a" "brightmagenta"))
   (cyan       '("#267f99" "#267f99" "brightcyan"))
   (dark-cyan  '("#0451a5" "#0451a5" "cyan"))
   (highlight      blue)
   (vertical-bar   base2)
   (selection      '("#ADD6FF" "#ADD6FF" "lightblue"))
   (builtin        blue)
   (comments       green)
   (doc-comments   (doom-lighten green 0.2))
   (constants      '("#098658" "#098658" "green"))
   (functions      '("#795E26" "#795E26" "brown"))
   (keywords       blue)
   (methods        '("#795E26" "#795E26" "brown"))
   (operators      blue)
   (type           cyan)
   (strings        '("#A31515" "#A31515" "red"))
   (variables      base5)
   (numbers        '("#098658" "#098658" "green"))
   (region         '("#ADD6FF" "#ADD6FF" "lightblue"))
   (error          red)
   (warning        orange)
   (success        '("#098658" "#098658" "green"))
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   (modeline-fg              fg)
   (modeline-fg-alt          base4)
   (modeline-bg              base2)
   (modeline-bg-alt          base1)
   (modeline-bg-inactive     base1)
   (modeline-bg-alt-inactive base0)
   (-modeline-pad
    (when sails-light-padded-modeline
      (if (integerp sails-light-padded-modeline) sails-light-padded-modeline 4))))

  (
   ;; 基础
   (default :background "#ffffff" :foreground "#333333")
   (fringe :background "#ffffff")
   (region :background "#ADD6FF")
   (hl-line :background "#f3f3f3")
   (cursor :background "#000000")
   (shadow :foreground "#bfbfbf")
   (minibuffer-prompt :foreground "#0000FF" :weight 'bold)

   ;; 行号
   (line-number :foreground "#237893" :background "#ffffff")
   (line-number-current-line :foreground "#000000" :background "#e5f3ff" :weight 'normal)

   ;; modeline
   (mode-line :background "#E5E5E5" :foreground "#333333"
              :box (if -modeline-pad `(:line-width ,-modeline-pad :color "#E5E5E5")))
   (mode-line-inactive :background "#F3F3F3" :foreground "#bfbfbf"
                       :box (if -modeline-pad `(:line-width ,-modeline-pad :color "#F3F3F3")))

   ;; 语法高亮
   (font-lock-comment-face :foreground "#008000" :slant 'italic)
   (font-lock-doc-face :foreground "#008000" :slant 'italic)
   (font-lock-keyword-face :foreground "#0000FF" :weight 'normal)
   (font-lock-builtin-face :foreground "#A31515") ; for <iostream> etc
   (font-lock-function-name-face :foreground "#795E26" :weight 'normal)
   (font-lock-type-face :foreground "#267f99" :weight 'normal)
   (font-lock-string-face :foreground "#A31515")
   (font-lock-constant-face :foreground "#098658")
   (font-lock-number-face :foreground "#098658")
   (font-lock-variable-name-face :foreground "#333333")
   (font-lock-preprocessor-face :foreground "#AF00DB" :weight 'normal)
   (font-lock-warning-face :foreground "#b89500" :weight 'bold)

   ;; 选区和高亮
   (isearch :background "#FFFACD" :foreground "#333333")
   (lazy-highlight :background "#E5E5E5" :foreground "#333333")
   (highlight :background "#ADD6FF" :foreground "#333333")

   ;; 其他
   (vertical-border :foreground "#E5E5E5")
   (tooltip :background "#F3F3F3" :foreground "#333333")
   (success :foreground "#098658" :weight 'bold)
   (error :foreground "#e51400" :weight 'bold)
   (warning :foreground "#b89500" :weight 'bold)

   ;; Org/Markdown/Outline
   (org-block :background "#F3F3F3")
   (org-block-begin-line :foreground "#237893" :slant 'italic)
   (org-ellipsis :foreground "#0000FF" :underline nil)
   (markdown-code-face :background "#F3F3F3")
   (outline-1 :foreground "#0000FF" :weight 'bold)
   (outline-2 :foreground "#795E26")
   (outline-3 :foreground "#267f99")
   (outline-4 :foreground "#A31515")
   (outline-5 :foreground "#098658")
   (outline-6 :foreground "#AF00DB")
   (outline-7 :foreground "#b89500")
   (outline-8 :foreground "#008000")
   )
  ()
  )

(font-lock-add-keywords
 'c++-mode
 '(("\\b[0-9]+\\b" . 'my/vscode-number-face)))
(defface my/vscode-number-face
  '((t :foreground "#098658" :weight normal))
  "VSCode Light+ style number face.")

;;; vs-light-theme.el ends here
