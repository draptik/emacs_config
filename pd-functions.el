;; ===================================================================
;; NEW: INDENT-BUFFER ================================================
;; ===================================================================
(defun indent-buffer ()
  (interactive)
  (save-excursion (indent-region (point-min) (point-max) nil))
  )
(global-set-key [f2] 'indent-buffer)

;; ===================================================================
;; COMMENT ===========================================================
;; ===================================================================

;; TODO: there is also "indent-according-to-mode"
;; TODO: Comments like /* comment */ (ie CSS) don't work
(defun pd-insert-empty-comment-line (x)
  "Inserts an empty comment line filled with string-to-char of X.
The line is indented according to the current mode."
  (indent-for-tab-command)
  (insert (make-string (- (- fill-column 
							 (current-column)) 3) 
					   (string-to-char x)))
  (comment-region (point-at-bol) (point-at-eol))
  (newline))

(defun pd-insert-comment-line (comment sym)
  "Inserts a COMMENT. The remaining line is filled with SYM.
The line is indented according to the current mode. TODO:
Currently does not yet work with comments spanning more than a
single line."
  (indent-for-tab-command)
  (insert comment " " (make-string (- fill-column 
									  (length comment) 
									  (current-column) 4) 
								   (string-to-char sym)))
  (comment-region (point-at-bol) (point-at-eol))
  (newline))

(defun pd-insert-comment-heading (comment sym)
  "Insert COMMENT, followed by SYM. SYM defaults to \"=\".  The
line will be commented based on the current mode. An optional
prefix (`C-u') adds another empty comment line filled with SYM
above and below the actual comment. The line is indented
according to the current mode.

USAGE EXAMPLES: 
1. M-x pd-insert-comment-heading RET yourComment RET RET 
   -> ;; yourComment =========
2. M-x pd-insert-comment-heading RET yourComment RET * RET 
   -> ;; yourComment *********
3. C-u M-x pd-insert-comment-heading RET yourComment RET RET 
   -> ;; =====================
   -> ;; yourComment =========
   -> ;; =====================
4. C-u M-x pd-insert-comment-heading RET yourComment RET * RET 
   -> ;; *********************
   -> ;; yourComment *********
   -> ;; *********************
"
  (interactive 
   ;; Use read-string because we want a default value for second arg
   (list
    (read-string "Comment: ")
    (read-string "Symbol (default: \"=\"): " nil nil "=")))

  ;; prefix present: add empty comment line above
  (if (not (eq current-prefix-arg nil))
      (pd-insert-empty-comment-line sym))
  
  ;; check if COMMENT string is empty
  (if (string-equal comment "")
      (pd-insert-empty-comment-line sym)
    (pd-insert-comment-line comment sym))

  ;; prefix present: add empty comment line below
  (if (not (eq current-prefix-arg nil))
      (pd-insert-empty-comment-line sym)))

(global-set-key [f7] 'pd-insert-comment-heading)


;; ===================================================================
;; Kill + Copy current line ==========================================
;; ===================================================================
;; C-w: now kills current line
;; M-w: now copies current line
;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile) 
  "When called interactively with no active region, copy a single
line instead."
  (interactive 
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line") 
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
		   (line-beginning-position 2)))))

;; ===================================================================
;; COPY AND DUP ======================================================
;; ===================================================================
(defun pd-comment-and-dup ()
  "duplicate line at point, and comment the current one"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark)
    (forward-line 1)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (comment-region (region-beginning) (region-end))
      (insert-string str)))
  (next-line))

;; ===================================================================
;; Testing ===========================================================
;; ===================================================================

(defun djcb-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
			   (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
							 (if icon (concat "-i " icon) "")
							 " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))


;; the appointment notification facility
(setq
 appt-message-warning-time 1 ;; warn 15 min in advance

 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little façade-function for djcb-popup
(defun djcb-appt-display (min-to-app new-time msg)
  (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg 
			  "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
			  "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))
(setq appt-disp-window-function (function djcb-appt-display))

;; (djcb-popup "Warning" "The end is near"
;;    "/usr/share/icons/gnome/32x32/status/appointment-soon.png" 
;;    "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg")


(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
		 (buffer (car list)))
	(while buffer
	  (when (buffer-file-name buffer)
		(set-buffer buffer)
		(revert-buffer t t t))
	  (setq list (cdr list))
	  (setq buffer (car list))))
  (message "Refreshing open files"))
