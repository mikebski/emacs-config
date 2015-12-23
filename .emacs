(cd "~/")
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq mac-command-modifier 'control)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(require 'paren)
(show-paren-mode 1)
(global-set-key "%" 'match-paren)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key "\C-cc" 'iwb)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-f" 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key "\C-xe" 'end-of-buffer)
(global-set-key "\C-xa" 'beginning-of-buffer)
(global-set-key "\C-xm" 'call-last-kbd-macro)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-z" 'shell)

(global-set-key "\C-ck" 'copy-line)
(global-set-key "\M-k" 'kill-other-buffer)
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(define-key text-mode-map (kbd "RET") 'newline)

(global-unset-key "\C-u")
(define-prefix-command `C-u-prefix)
(global-set-key "\C-uk" 'copy-line)
(global-set-key "\C-ug" 'igrep-visited-files)

;; Set esc-o to switch buffer.
(global-set-key [(meta o)]
                (lambda ()
                  (interactive)
                  (switch-to-buffer (other-buffer))))

;; Set the font for strings
(custom-set-variables
 '(font-lock-maximum-size 524288))
(custom-set-faces
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Black")))))

;; These are written by Mike Baranski.
(defun kill-other-buffer ()
  "Kill other buffer, if more than one is showing.  If only one is showing, it'll kill that one."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window 1)
  (delete-other-windows)
  )

;; Set up the copy line function
(defun copy-line (n)
  "Copy N lines at point to the kill-ring."
  (interactive "nEnter number of lines to copy: ")
  (beginning-of-line)
  (let ((beg (point)))
    (forward-line n)
    (copy-region-as-kill beg (point))
    (previous-line n)
    ))

(defun clear-shell-buffer ()
  "Clear the contents of the current buffer"
  (interactive)
  (erase-buffer)
  ;;  (insert "/usr/games/fortune -a")
  (comint-send-input)
  )

(defun indent-file ()
  "Indent the whole file"
  (interactive)
  (mark-whole-buffer)
  (indent-region)
  )

(defun copy-next-word ()
  "Copys the next word to the clipboard"
  (interactive)
  (kill-word)
  (yank)
  )

;; End of Mike's Functions!

(defun load-files (file-list)
  "Loads all files in the list with load-file"
  (if file-list
      (load-file (car file-list))
    (load-files (cdr file-list))
    ))

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

(put 'erase-buffer 'disabled nil)
(eval-after-load 'shell
  '(define-key shell-mode-map [(?\C-z)] 'clear-shell-buffer))

;;;;; End my custom functions
(load-files (directory-files "site-lisp" 1 ".*el$"))

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
