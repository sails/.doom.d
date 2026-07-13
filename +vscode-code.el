;;; +vscode-code.el --- 让 C/C++/Go 代码 buffer 呈现 VSCode Light+ 配色 -*- lexical-binding: t; -*-
;;
;; 设计说明（方案A）：
;; 全局主题保持 `sails-light2'（dashboard / org / modeline / 补全菜单 / buffer 列表
;; 等所有基础界面与以前完全一致）。本文件只在 C/C++/Go 代码 buffer 里，用 buffer-local
;; 的 `face-remap-add-relative' 把 font-lock-*-face 与 lsp-face-semhl-* 重映射成
;; VSCode Light+ 配色。
;;
;; 为什么必须走 buffer-local remap 而不是主题 override：
;; font-lock-*-face 是全局共享 face，dashboard / org / which-key / vertico 等 UI 都在
;; 用同一批。若在主题里 &override 它们（旧 sails-vscode-light 的做法），代码是 VSCode 色了，
;; 但这些 UI 也会跟着变色（菜单标题变紫、快捷键变蓝等）。buffer-local remap 只染当前
;; 代码 buffer，天然不波及任何 UI。
;;
;; 色值来源：VSCode Light+ 官方语法配色（原 sails-vscode-light 主题的 vsc-* 调色板），
;; 集中定义在下方 `+sails/vsc-colors' 里，单点维护。

;;; --- VSCode Light+ 语法配色表（单点维护）---
(defvar +sails/vsc-colors
  '((purple     . "#af00db")   ;; keywords / preprocessor
    (brown      . "#745f30")   ;; functions / methods
    (teal-type  . "#267f99")   ;; types / class / namespace
    (dark-blue  . "#001080")   ;; variables / members / params
    (comment    . "#008000")   ;; comments
    (number     . "#098658")   ;; numbers
    (string     . "#a31515")   ;; strings
    (operator   . "#0451a5")   ;; operators
    (const-blue . "#0070c1")   ;; constants / enum member / readonly
    (macro      . "#0000ff")   ;; macros / preprocessor semantic
    (blue-kw    . "#1111f6"))  ;; storage kw & builtin types: static/const/int/void
  "VSCode Light+ 语法配色表，供代码 buffer face-remap 单点维护。")

(defun +sails/vsc (name)
  "取 VSCode 语法色 NAME(symbol) 的十六进制值。"
  (cdr (assq name +sails/vsc-colors)))

;;; --- 代码 buffer 的 face 重映射（只影响本 buffer）---
(defvar-local +sails/vsc-remapped nil
  "Non-nil once this buffer's faces have been remapped to VSCode colors.")

(defun +sails/enable-vscode-code-colors ()
  "在 C/C++/Go 代码 buffer 里把语法高亮 face 重映射成 VSCode Light+ 配色。
只影响当前 buffer（buffer-local），不影响 dashboard/org 等任何 UI；幂等，重复进入
同一 buffer 不会叠加。"
  (unless +sails/vsc-remapped
    (setq +sails/vsc-remapped t)
    (let ((purple    (+sails/vsc 'purple))
          (brown     (+sails/vsc 'brown))
          (teal      (+sails/vsc 'teal-type))
          (dkblue    (+sails/vsc 'dark-blue))
          (comment   (+sails/vsc 'comment))
          (number    (+sails/vsc 'number))
          (string    (+sails/vsc 'string))
          (operator  (+sails/vsc 'operator))
          (constblue (+sails/vsc 'const-blue))
          (macro     (+sails/vsc 'macro))
          (bluekw    (+sails/vsc 'blue-kw)))
      (dolist (spec
               `(;; --- 传统 font-lock（cc-mode / 未开 lsp 时也生效）---
                 (font-lock-keyword-face           :foreground ,purple)
                 (font-lock-builtin-face           :foreground ,bluekw)   ;; int/bool/void + cpp-recolor 存储类关键字
                 (font-lock-namespace-face         :foreground ,teal :weight normal)
                 (font-lock-constant-face          :foreground ,constblue)
                 (font-lock-type-face              :foreground ,teal)
                 (font-lock-storage-modifier-face  :foreground ,bluekw)
                 (font-lock-keyword-storage-face   :foreground ,bluekw)
                 (font-lock-preprocessor-face      :foreground ,purple)
                 (font-lock-function-name-face     :foreground ,brown)
                 (font-lock-function-call-face     :foreground ,brown)   ;; cpp-recolor 函数调用引用此 face
                 (font-lock-method-call-face       :foreground ,brown)
                 (font-lock-variable-name-face     :foreground ,dkblue)
                 (font-lock-variable-use-face      :foreground ,dkblue)
                 (font-lock-property-name-face     :foreground ,dkblue)
                 (font-lock-property-use-face      :foreground ,dkblue)
                 (font-lock-string-face            :foreground ,string)
                 (font-lock-number-face            :foreground ,number)
                (font-lock-comment-face           :foreground ,comment :slant normal)
                (font-lock-comment-delimiter-face :foreground ,comment :slant normal)
                (font-lock-doc-face               :slant normal)
                 ;; --- lsp-mode 语义高亮（clangd/gopls semantic tokens）---
                 (lsp-face-semhl-keyword           :foreground ,purple)
                 (lsp-face-semhl-namespace         :foreground ,teal :weight normal)
                 (lsp-face-semhl-type              :foreground ,teal)
                 (lsp-face-semhl-class             :foreground ,teal)
                 (lsp-face-semhl-struct            :foreground ,teal)
                 (lsp-face-semhl-enum              :foreground ,teal)
                 (lsp-face-semhl-interface         :foreground ,teal)
                 (lsp-face-semhl-type-parameter    :foreground ,teal)
                 (lsp-face-semhl-function          :foreground ,brown)
                 (lsp-face-semhl-method            :foreground ,brown)
                 (lsp-face-semhl-member            :foreground ,dkblue)
                 (lsp-face-semhl-variable          :foreground ,dkblue)
                 (lsp-face-semhl-parameter         :foreground ,dkblue)
                 (lsp-face-semhl-property          :foreground ,dkblue)
                 (lsp-face-semhl-field             :foreground ,dkblue)
                 (lsp-face-semhl-constant          :foreground ,constblue)
                 (lsp-face-semhl-enum-member       :foreground ,constblue)
                 (lsp-face-semhl-macro             :foreground ,macro)
                 (lsp-face-semhl-preprocessor      :foreground ,macro)
                 (lsp-face-semhl-number            :foreground ,number)
                 (lsp-face-semhl-string            :foreground ,string)
                 (lsp-face-semhl-comment           :foreground ,comment :slant normal)
                 (lsp-face-semhl-operator          :foreground ,operator)
                 (lsp-face-semhl-static            :foreground ,bluekw)))
        (apply #'face-remap-add-relative (car spec) (cdr spec))))))

;;; --- lsp 语义高亮开关（让 Emacs 像 VSCode 一样给函数调用/成员/参数上色）---
;; clangd/gopls 支持 semantic tokens，默认关闭；这是 Emacs 代码看起来比 VSCode "灰、平"
;; 的根本原因。开启后配合上面的 lsp-face-semhl-* remap 呈现 VSCode 的丰富着色。
(after! lsp-mode
  (setq lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t
        ;; 保留文档链接(可点击/M-RET 跳转、带下划线)，颜色统一交给下面的 face-remap 处理。
        lsp-enable-links t))

;;; --- 统一 lsp document-link 颜色 ---
;; lsp(clangd/gopls/…)把"能解析到的头文件/import 包"通过 documentLink 做成
;; default-button(继承自 button→link)，link face 默认带蓝色前景 + 下划线，导致这些
;; 链接与解析不到的(纯 font-lock-string-face)颜色不一致；鼠标悬停时 mouse-face=highlight
;; 又套一层深蓝背景。这里把 link/button/default-button 的前景改成字符串色、只保留下划线，
;; 并用 set-base 把 highlight 整体替换(去掉深蓝背景)，于是无论 C++ 的 #include 还是 Go 的
;; import，颜色都统一、悬停也不变色，可点击的仍有下划线(即"文件存在/可跳转"标记)。
;; 挂在 lsp-mode-hook 上做 buffer 局部改动，对所有 lsp 语言生效，也不影响 magit 等其它按钮。
(defvar-local +sails/lsp-link-color-unified nil
  "Non-nil once link/highlight faces have been remapped in this buffer, to avoid stacking.")
(defun +sails/lsp-unify-link-color ()
  "Make lsp document-link foreground match strings (keep underline) and neutralize
the mouse-hover face, so links look consistent and hovering does not change color."
  (unless +sails/lsp-link-color-unified
    (setq +sails/lsp-link-color-unified t)
    (dolist (f '(link button default-button))
      (face-remap-add-relative f '(:inherit font-lock-string-face :underline t)))
    ;; face-remap-add-relative 是"叠加"，去不掉 highlight 自带的背景色；
    ;; 用 face-remap-set-base 把本 buffer 的 highlight 整体替换成字符串色+下划线、不带背景。
    (face-remap-set-base 'highlight '(:inherit font-lock-string-face :underline t :background unspecified))))
(add-hook 'lsp-mode-hook #'+sails/lsp-unify-link-color)

;;; --- 让 lsp 语义高亮的 declaration/definition 修饰符不抢色 ---
;; clangd 给"函数/方法/变量的声明与定义"打上 declaration/definition 修饰符，
;; lsp-mode 默认把 declaration 映射成 lsp-face-semhl-interface(青色)。由于修饰符
;; face 在 face 列表里排在类型 face 前面、优先级更高，导致方法定义名(如 GetRuleInfo)
;; 被染成青色而非金色。VSCode 并不会因为"是声明"就给标识符换色，所以这里把
;; declaration/definition 两个修饰符置空(无前景色)，让底层类型色(method=金/variable=蓝)透出。
(defface +sails/semhl-noop '((t nil))
  "Neutral face that contributes no attributes, used to disable certain lsp semantic-token modifiers."
  :group 'lsp-faces)
(after! lsp-semantic-tokens
  (dolist (m '("declaration" "definition"))
    (let ((cell (assoc m lsp-semantic-token-modifier-faces)))
      (if cell
          (setcdr cell '+sails/semhl-noop)
        (push (cons m '+sails/semhl-noop) lsp-semantic-token-modifier-faces)))))

;;; --- C/C++ 存储类关键字 / 内置类型 / 函数调用重着色 ---
;; VSCode Light+ 里 static/const/extern 这类"存储类关键字"是蓝色(#1111F6)，
;; 而 if/return/for 这类"控制流关键字"才是紫色。但 cc-mode 的 font-lock 把
;; 两者都染成 font-lock-keyword-face(紫)，且 clangd 不给关键字发语义 token，
;; 所以必须在这里把存储类关键字单独重染成蓝色的 font-lock-builtin-face
;; (上面的 remap 已把本 buffer 的 font-lock-builtin-face 指向 VSCode 蓝)。
(defun +sails/cpp-recolor-storage-keywords ()
  "Recolor C/C++ storage-class keywords to `font-lock-builtin-face' (blue),
and color function/method CALLS to VSCode function gold via `font-lock-function-call-face'."
  (font-lock-add-keywords
   nil
   '(;; 存储类关键字 → 蓝 (override t：强制覆盖 keyword-face)
     ("\\_<\\(static\\|extern\\|auto\\|register\\|typedef\\|mutable\\|friend\\|constexpr\\|consteval\\|constinit\\|explicit\\|virtual\\|inline\\|override\\|final\\|noexcept\\|thread_local\\|volatile\\|const\\|using\\|namespace\\|class\\|struct\\|union\\|enum\\|public\\|private\\|protected\\|template\\|typename\\|operator\\|this\\|nullptr\\|true\\|false\\)\\_>"
      0 'font-lock-builtin-face t)
     ;; 基础(内置)类型 → 蓝 (override t：VSCode 里 int/char/void 等是蓝色，和存储类一致；
     ;; 只有自定义类/类型才保持青色，所以这里只列内置类型，不动 font-lock-type-face 的其它命中)
     ("\\_<\\(void\\|bool\\|char\\|char8_t\\|char16_t\\|char32_t\\|wchar_t\\|short\\|int\\|long\\|float\\|double\\|signed\\|unsigned\\)\\_>"
      0 'font-lock-builtin-face t)
     ;; 函数/方法调用：标识符紧跟 "(" → font-lock-function-call-face(金)，
     ;; 色值由本 buffer 的 remap 单点维护。(override nil：不覆盖已有的关键字/类型着色，
     ;; 所以 if(/for(/while(/return(/sizeof( 这些控制流关键字保持原色)
     ("\\_<\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*("
      1 'font-lock-function-call-face))
   'append))

;;; --- hook 注册 ---
;; 代码色 remap：C/C++/Go 代码 buffer 都启用（font-lock + lsp semantic tokens）
(add-hook 'c-mode-hook      #'+sails/enable-vscode-code-colors)
(add-hook 'c++-mode-hook    #'+sails/enable-vscode-code-colors)
(add-hook 'go-mode-hook     #'+sails/enable-vscode-code-colors)
(add-hook 'go-ts-mode-hook  #'+sails/enable-vscode-code-colors)
;; 存储类关键字/内置类型/函数调用重着色：仅 C/C++（Go 靠 gopls semantic tokens 上色）
(add-hook 'c-mode-hook   #'+sails/cpp-recolor-storage-keywords)
(add-hook 'c++-mode-hook #'+sails/cpp-recolor-storage-keywords)

(provide '+vscode-code)
;;; +vscode-code.el ends here
