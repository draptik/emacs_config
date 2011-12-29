;; ===================================================================
;; ORG MODE ==========================================================
;; ===================================================================

;; Using the git repository...
(setq load-path (cons (concat (getenv "EMACS_PACKAGES") "org-mode/lisp") load-path))
(setq load-path (cons (concat (getenv "EMACS_PACKAGES") "org-mode/contrib/lisp") load-path))

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Default todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELLED" "DELEGATED")))

;; TODO keywords faces (see M-x list-colors-display for available colors)
(setq org-todo-keyword-faces
      '(("TODO"      . org-warning)
		("CANCELLED"  . shadow)
		("WAITING"  . (:foreground "magenta" :weight bold))
		("STARTED"  . (:foreground "orange" :weight bold))
		("READY_FOR_DEPLOY"  . (:foreground "spring green" :weight bold))))


;; ;; Put the following line in a lisp (cond ...) to easily switch
;; ;; between locations.
;; (setenv "ORG_DIR" (expand-file-name "~/org-stuff/"))
;; (setenv "WORK_DIR" (expand-file-name "~/work/redheads/siemens/healthcare/healthcare-events/"))

;; (setq org-agenda-files
;;       (list (concat (getenv "ORG_DIR") "computerstuff.org")
;; 	    (concat (getenv "ORG_DIR") "redheads.org")
;; 	    (concat (getenv "WORK_DIR") "healthcare-events.org")
;; 	    (concat (getenv "ORG_DIR") "privat.org")))


;; Include diary in Agenda View (these settings are connected to diary section!)
;; 
;; 2009-11-28 Change interpretation of dates for EMACS >=23.1:
;; calendar-date-style (used to be european-calendar-style)
;;
;; 2009-11-28 Don't port diary to org mode (german holidays are
;; calculated in file "german-holidays.el"; org mode only takes sexp).
(setq org-agenda-include-diary t)

;; Highlight the current line in org's agenda view:
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Sending Org's agenda to myself by mail ======================
;;;
;;; See script on PC "golem2": `~/skripte/diary-mail.sh'
;;;
;;; Content of mail script (be sure to change paths!):
;;;
;; $EMACS --batch \
;;     --load $HOME/.emacs.d/init.el \
;;     --load $HOME/.emacs.d/.gnus \
;;     --funcall org-mail >& $LOGFILE

;;;
;;; The above script requires a GNUS setup (Emacs Gnus==Mail/Newsgroup
;;; setup)
;;;
;; (defun org-mail ()
;;   "Send mail of agenda to myself."
;;   (org-batch-agenda "a")
;;   (let ((str (buffer-string)))
;;     (compose-mail diary-mail-addr "Agenda")
;;     (insert str)
;;     (call-interactively (get mail-user-agent 'sendfunc))))
