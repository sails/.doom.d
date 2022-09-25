;;; lisp/init-exec-path.el --- Set up exec-path to help Emacs find programs -*- lexical-binding: t; -*-


(require 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "GOPATH"))
    (add-to-list 'exec-path-from-shell-variables var)))


(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  ;; 设成nil 则不从 .zshrc 读 只从 .zshenv读
  ;; （可以加快速度，但是需要你将环境变量相关的都放到 .zshenv 中，而非 .zshrc 中）
  (setq exec-path-from-shell-check-startup-files nil) ;
  (setq exec-path-from-shell-arguments '("-l" )) ;remove -i read form .zshenv
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
