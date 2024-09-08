;;; sails-light3-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Commentary:
;;; Code:

(deftheme sails-light3
  "My custom theme")

(custom-theme-set-faces
 'sails-light3

 ;; 左右边框颜色
 '(fringe  ((t :background "#FFFFFF")))
 ;; mode-line 颜色
 ;; `(mode-line          ((t :background "#F8F8F8" :foreground "dimgray" :box ( :line-width -1 :style released-button))))
 ;; `(mode-line-inactive ((t :background "#F8F8F8" :foreground "dimgray" :box ( :line-width -1 :style released-button))))
 ;; 选中区域
 '(region ((t (:background "#ADD6FF" ))))
 ;; 高亮，比如高亮buffer中与当前光标所在单词相同的内容
 '(highlight ((t (:background "#ADD6FF"))))
 ;; 设置高亮行的背景颜色
 '(hl-line ((t (:background "#F5F5F5"))))

 ;; git gutter
 '(diff-hl-insert ((t (:foreground "#529E4E"))))
 '(diff-hl-change ((t (:foreground "#DA8548"))))
 '(diff-hl-delete ((t (:foreground "#E35749"))))

 ;; '(git-gutter-fr:added ((t :foreground "#529E4E" )))
 ;; '(git-gutter-fr:deleted ((t :foreground "#b81a1f" )))
 ;; '(git-gutter-fr:modified ((t :foreground "#dbba3f")))
 )

(provide-theme 'sails-light3)

;;; sails-light3-theme.el ends here
