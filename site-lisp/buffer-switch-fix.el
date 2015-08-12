;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER SWITCHING FIX
;;
;; This changes the behaviour of the switch-to-buffer completion functions so
;; that the current buffer is NOT in the completion list.
;;
;; i.e. say you're working in "temp.c", and you want to visit "temp.h"; so you
;; type "C-xb", then "t<TAB>" which then presents you with a completion list of
;; temp.c and temp.h, so you then must type "h<RET>".  This is annoying since 
;; why would you want to switch back to the buffer you're in?!?
;; Using this fix would remove "temp.c" from the completion lits so that when 
;; you had typed "t<TAB>" the name would be completed as "temp.h" as desired.
;;
;; Steve Dodd 
;; March 1998
;;
;; Mike Baranski
;; Feb, 2002
;; 
;; I'm also going to remove *shell* from the list.  I've got the C-z bound
;; to start a shell, so I don't need that showing up in the switch to list.
;; I might also remove *scratch* and *messages*, but not sure yet.  Maybe
;; those could be config vars, too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun s-minibuffer-complete ()
  "A shell around minibuffer-complete which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete))

(defun s-minibuffer-complete-word ()
  "A shell around minibuffer-complete-word which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete-word)
  )

(defun s-minibuffer-complete-and-exit ()
  "A shell around minibuffer-complete-and-exit which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (if s-remove-first-completion
      (progn (setq s-remove-first-completion nil)
             (if (consp minibuffer-completion-table)
                 (setq  minibuffer-completion-table 
                        (cdr minibuffer-completion-table)) ()))
    ())
  (minibuffer-complete-and-exit))


(defun s-switch-to-buffer ()
  "A shell around switch-to-buffer which removes the name of the current buffer from the buffer completion list.  The default behaviour doesn't make sense since there is no reason to ask to switch to the buffer you are already in!"
  (interactive)
  (setq s-remove-first-completion 't)
  (switch-to-buffer (read-buffer "Switch to buffer: " (other-buffer (get-buffer "*shell*")))))

(setq s-remove-first-completion 'nil)

(define-key minibuffer-local-completion-map "\040" 's-minibuffer-word)
(define-key minibuffer-local-completion-map "\t" 's-minibuffer-complete)
(define-key minibuffer-local-must-match-map [return] 's-minibuffer-complete-and-exit)
(global-set-key "\C-xb" 's-switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF BUFFER SWITCHING FIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
