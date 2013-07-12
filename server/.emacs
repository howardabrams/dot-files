(custom-set-variables
 '(menu-bar-mode nil)
 '(save-place t nil (saveplace))
 '(tool-bar-mode nil))

(custom-set-faces
 )

(add-to-list 'load-path "~/.emacs.d/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/emamux-20130331.2353")

(if (getenv "TMUX")
  (progn
    (require 'emamux)
    (global-set-key (kbd "M-!") 'emamux:run-command)
    (global-set-key (kbd "C-x !") 'emamux:send-command)
    (global-set-key (kbd "C-x ~") 'emamux:emamux:close-runner-pane)))

(server-start)
