;;; sails-light-theme.el --- inspired by Atom One Light -*- lexical-binding: t; no-byte-compile: t; -*-
;;

(require 'doom-themes)


;;
;;; Variables

(defgroup sails-light-theme nil
  "Options for the `sails-light' theme."
  :group 'doom-themes)

(defcustom sails-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'sails-light-theme
  :type 'boolean)

(defcustom sails-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'sails-light-theme
  :type 'boolean)

(defcustom sails-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'sails-light-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme sails-light
  "A light theme inspired by Atom One Light."
  :family 'doom-one
  :background-mode 'light

  ;; name        default256       16
  ((bg         '("#fafafa" "white"   "white"        ))
   (fg         '("#2d2d2d" "#2d2d2d" "black"))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   ;; close solaire-mode will better looking
   (bg-alt     '("#f3f3f3" "white"   "white"))
   (fg-alt     '("#2d2d2d" "#2d2d2d" "brightblack"))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#f0f0f0" "#f0f0f0" "white"        ))
   ;; (base0      '("#ffffff" "#ffffff" "white"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#6a737d" "#6a737d" "brightblack"  ))
   (base5      '("#222222" "#222222" "brightblack"  ))
   (base6      '("#202328" "#2e2e2e" "brightblack"  ))
   (base7      '("#1c1f24" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))
   (base9      '("#7E848D" "7E848D"   "brightblack" ))

   (grey       base4)
   (red        '("#e45649" "#e45649" "red"          ))
   (dark-red        '("#d12f1b" "#d12f1b" "red"          ))
   (orange     '("#c25d1e" "#c25d1e" "brightred"    ))
   (green      '("#22863a" "#22863a" "green"        ))
   (green2      '("#008000" "#008000" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#b87700" "#b87700" "yellow"       ))
   (blue       '("#4078f2" "#4078f2" "brightblue"   ))
   (dark-blue  '("#a0bcf8" "#a0bcf8" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#a626a4" "#a626a4" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base2 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   ;;(comments       (if sails-light-brighter-comments cyan base4))
   (comments       (if sails-light-brighter-comments green2 base4))
   (doc-comments   (doom-darken comments 0.15))
   (constants      violet)
   (functions      magenta)
   (keywords       dark-red)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-darken magenta 0.36))
   (numbers        orange)
   (region         '("#add6ff" "#add6ff" "lightblue"))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          (doom-blend
                              violet base4
                              (if sails-light-brighter-modeline 0.5 0.2)))
   (modeline-bg              (if sails-light-brighter-modeline
                                 (doom-darken base2 0.05)
                               base1))
   (modeline-bg-alt          (if sails-light-brighter-modeline
                                 (doom-darken base2 0.1)
                               base2))
   (modeline-bg-inactive     (doom-darken bg 0.1))
   (modeline-bg-alt-inactive `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))

   (-modeline-pad
    (when sails-light-padded-modeline
      (if (integerp sails-light-padded-modeline) sails-light-padded-modeline 4))))

  ;;;; Base theme face overrides
  (
   ;; ((font-lock-comment-face &override)
   ;;  :background (if sails-light-brighter-comments base0))
   ((font-lock-doc-face &override) :slant 'italic)
   ;; ((line-number &override) :foreground (doom-lighten base4 0.15) :slant 'normal)
   ((line-number &override) :foreground base9 :slant 'normal)
   ((line-number-current-line &override) :foreground base8 :slant 'normal)
   ;; ((line-number-current-line &override) :foreground blue :slant 'normal)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if sails-light-brighter-modeline base8 highlight))
   (shadow :foreground base4)
   (tooltip :background base1 :foreground fg)

   ;;;; centaur-tabs
   (centaur-tabs-unselected :background bg-alt :foreground base4)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if sails-light-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :weight 'normal)
   (doom-modeline-buffer-modified :weight 'normal)
   (doom-modeline-buffer-major-mode :weight 'normal)
   (doom-modeline-vcs-default :weight 'normal)
   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))
   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)
   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   ;;;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)
   ;;;; markdown-mode
   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   ((markdown-code-face &override)       :background base1)
   (mmm-default-submode-face :background base1)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground red)
   ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
   ((org-block &override) :background base1)
   ((org-block-begin-line &override) :foreground fg :slant 'italic)
   (org-ellipsis :underline nil :background bg     :foreground red)
   ((org-quote &override) :background base1)
   ;;;; posframe
   (ivy-posframe               :background base0)
   ;;;; selectrum
   (selectrum-current-candidate :background base2)
   ;;;; vertico
   (vertico-current :background base2)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-alt-inactive
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt-inactive)))
   ;;;; web-mode
   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)
   ;;;; wgrep <built-in>
   (wgrep-face :background base1)
   ;;;; whitespace
   ((whitespace-tab &override)         :background (if (not (default-value 'indent-tabs-mode)) base0 'unspecified))
   ((whitespace-indentation &override) :background (if (default-value 'indent-tabs-mode) base0 'unspecified))

   (nerd-icons-completion-dir-face :foreground "#459CFE")
   (all-the-icons-dired-dir-face    :foreground "#459CFE")
  
   )

  ;;;; Base theme variable overrides-
  ()
  )

;;; sails-light-theme.el ends here
