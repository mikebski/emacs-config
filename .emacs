;; ____________________________________________________________________________
;; Aquamacs custom-file warning:
;; Warning: After loading this .emacs file, Aquamacs will also load
;; customizations from `custom-file' (customizations.el). Any settings there
;; will override those made here.
;; Consider moving your startup settings to the Preferences.el file, which
;; is loaded after `custom-file':
;; ~/Library/Preferences/Aquamacs Emacs/Preferences
;; _____________________________________________________________________________
(cd "~/")
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq mac-command-modifier 'control)

;; Home and End functions which let you jump back to the stack
(setq mwb-point-stack ()) ;; Initial global stack is empty
(make-variable-buffer-local 'mwb-point-stack) ;; Set so each buffer gets its own stack

(defun copy-whole-buffer ()
  "Copy all the text in the buffer to the clipboard"
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard")
  )

;;
;; Goto last point on the buffer-local stack
;;
(defun goto-point-on-stack ()
  "Go to the first point on the stack"
  (interactive)
  (if (> (length mwb-point-stack) 0)
      (progn
        (goto-char (car mwb-point-stack))
        (setq mwb-point-stack (cdr mwb-point-stack))
        (recenter-top-bottom)
        )
    (message "No saved points found")
    )
  )

;;
;; Save current cursor location to the buffer-local stack
;;
(defun goto-save-point-on-stack ()
  "Save the current point to the stack so that goto-point-on-stack can use it."
  (interactive)
  (setq mwb-point-stack (cons (point) (bound-and-true-p mwb-point-stack)))
  (message "Point saved")
  )

;;
;; GOTO END
;;
(defun goto-end ()
  "Go to the end of the buffer and push the current location on to the stack, if the current location is not the beginning or end of the buffer"
  (interactive)
  (if (or (eq (point) (point-max)) (eq (point) (point-min))) (message "Not saving position")
    (goto-save-point-on-stack)
    )
  (goto-char (point-max))
  )

;;
;; GOTO BEGINNING
;;
(defun goto-beginning ()
  "Got to the beginning of the buffer and push the current location on to the stack, if the current location is not the beginning or end of the buffer"
  (interactive)
  (if (or (eq (point) (point-max)) (eq (point) (point-min))) (message "Not saving position")
    (goto-save-point-on-stack)
    )
  (goto-char (point-min))
  )

;; Map error keys:
(global-set-key (kbd "\C-x <down>") 'goto-end)
(global-set-key (kbd "\C-x <up>") 'goto-beginning)
(global-set-key (kbd "\C-x <left>") 'goto-point-on-stack)
(global-set-key (kbd "\C-x <right>") 'goto-save-point-on-stack)
;; End home and end functions

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

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

(require 'paren)
(show-paren-mode 1)
(global-set-key "%" 'match-paren)

(global-set-key "\C-cc" 'iwb)
(global-set-key (kbd "\C-z") 'shell)
(global-set-key "\C-cw" 'copy-whole-buffer)
(when (featurep 'aquamacs)
  (global-set-key "\C-\M-y" 'clipboard-yank)
  )
(global-set-key [(meta o)]
                (lambda ()
                  (interactive)
                  (switch-to-buffer (other-buffer))))

                                        ; try to improve slow performance on windows. ;
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

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (yaml-mode anaconda-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
