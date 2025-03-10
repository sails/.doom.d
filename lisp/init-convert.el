;;; lisp/init-convert.el -*- lexical-binding: t; -*-

;;; init-convert.el --- convert -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun convert:replace-region (start end func)
  "Replace-region START to END with FUNC result."
  (let ((input-string (buffer-substring start end))
        (marker (point-marker)))
    (goto-char start)
    (delete-region start end)
    (insert (funcall func input-string))
    (goto-char marker))
  )

;; 时间转换
(defun convert:time-conv (time)
  "Time-conv TIME."
  (if (> (string-to-number time) 100000)
      (progn
        (let (ts)
          (setq ts (string-to-number time))
          (format-time-string "%Y-%m-%d %T" (seconds-to-time ts))
          )
        )
    (progn
      (if (string-equal time "now")
          (progn
            (format-time-string "%s")
            )
        (progn
          (format-time-string "%s" (date-to-time time))
          )
        )
      )
    ))

(defun convert:time-region (start end)
  "Convert time between timestramp and date for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert:time-conv)
  )

;; Unicode转换
(defun convert:unibyte-to-string (text)
  "Converts a region from Unicode escape sequences to Utf8 characters."
  (decode-coding-string (string-as-unibyte (read (concat "\"" text "\""))) 'utf-8)
  )
(defun convert:unibyte-decode-region (start end)
  "Convert decode unibyte for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert:unibyte-to-string)
  )

(defun convert::string-to-hex-escape (text)
  "Convert a string to hexadecimal escape sequences."
  (let* ((utf8-bytes (encode-coding-string text 'utf-8))
         (hex-escape (mapconcat (lambda (char) (format "\\x%02x" char)) utf8-bytes "")))
    hex-escape)
  )
(defun convert:unibyte-encode-region (start end)
  "Convert encode unibyte for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert::string-to-hex-escape)
  )

;; url转换
(defun convert:url-decode-string (url)
  "Convert decode URL."
  (decode-coding-string (url-unhex-string (string-to-unibyte url)) 'utf-8)
  )

(defun convert:url-decode-region (start end)
  "Convert decode URL for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert:url-decode-string)
  )

(defun convert:url-encode-string (url)
  "Convert encode URL."
  (url-hexify-string (encode-coding-string url 'utf-8))
  )

(defun convert:url-encode-region (start end)
  "Convert encode URL for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert:url-encode-string)
  )

;; md5
(defun convert:md5-encode-region (start end)
  "Convert md5 for region START and END."
  (interactive "r")
  (convert:replace-region start end 'md5)
  )

;; base64
(defun convert:base64-encode-region (start end)
  "Convert base64 for region START and END."
  (interactive "r")
  (base64-encode-region start end)
  )

(defun convert:base64-decode-region (start end)
  "Convert base64 for region START and END."
  (interactive "r")
  (base64-decode-region start end)
  )

;; decimal-to-binary
(defun convert::decimal-to-binary (str)
  "Convert a DECIMAL number to its binary representation as a string."
  (let ((binary "") (decimal (string-to-number str)))
    (while (> decimal 0)
      (setq binary (concat (number-to-string (% decimal 2)) binary))
      (setq decimal (/ decimal 2)))
    (if (string= binary "")
        "0"
      binary)))

(defun convert:decimal-to-binary-region (start end)
  "Convert decimal for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert::decimal-to-binary)
  )

;; binary-to-decimal
(defun convert::binary-to-decimal (binary-str)
  "Convert a binary string BINARY-STR to its decimal representation.
If BINARY-STR is not a valid binary string, signal an error."
  (if (string-match-p "\\`[01]+\\'" binary-str)
      (let ((decimal 0)
            (len (length binary-str)))
        (dotimes (i len)
          (let ((bit (substring binary-str (- len i 1) (- len i))))
            (if (string= bit "1")
                (setq decimal (+ decimal (expt 2 i))))))
        (number-to-string decimal))
    (error "Invalid binary string: %s" binary-str)))

(defun convert:binary-to-decimal-region (start end)
  "Convert binary for region START and END."
  (interactive "r")
  (convert:replace-region start end 'convert::binary-to-decimal)
  )


(defun convert::json-compact (json-str)
  "使用 jq 将 JSON 字符串压缩为单行"
  (with-temp-buffer
    (insert json-str)
    (let ((exit-code (shell-command-on-region
                      (point-min) (point-max)
                      "jq -c ."  ; -c 表示压缩输出
                      (current-buffer) t)))
      (if (eq exit-code 0)
          (buffer-string)  ; 返回压缩后的 JSON
        (error "JSON 压缩失败: 请检查 JSON 语法或 jq 是否安装")))))

(defun convert::json-compact-region (start end)
  "使用 jq 将选中的 JSON 区域压缩为单行"
  (interactive "r")
  (convert:replace-region start end 'convert::json-compact)
  )

(defun convert::json-pretty-print (json-str)
  "将 JSON 字符串美化格式化"
  (require 'json)  ; 确保加载 JSON 库
  (condition-case err
      (let ((json-encoding-pretty-print t)    ; 启用美化输出
            (json-encoding-default-indentation "  "))  ; 缩进 2 空格
        (with-temp-buffer
          (insert json-str)
          (goto-char (point-min))
          (let ((json-data (json-read)))      ; 解析 JSON
            (json-encode json-data))))        ; 重新编码（带美化）
    (error (error "JSON 美化失败: 无效的 JSON 语法 (%s)" (error-message-string err)))))

(defun convert::json-pretty-print-region (start end)
  "美化选中的 JSON 区域"
  (interactive "r")
  (convert:replace-region start end 'convert::json-pretty-print)
  )

(provide 'init-convert)

;;; init-convert.el ends here
