;; Note for Windows ==================================================
;; 
;; Under Windows7 Emacs home (~) is located at
;;
;; C:\Users\<username>\AppData\Roaming
;;
;; The default init file is located at
;;
;; C:\Users\<username>\AppData\Roaming\.emacs
;;
;; Place the following lines in ~/.emacs so that this file is loaded:
;; 
;; (setenv "EMACS_DIR" (expand-file-name "~/emacs_conf/"))
;; (load (concat (getenv "EMACS_DIR") "init.el"))


;; ===================================================================
;; EMACS DIRS ========================================================
;; ===================================================================

;; Put the following line in a lisp (cond ...) to easily switch
;; between locations.
(cond ((eq system-type 'windows-nt) 
	   (setenv "EMACS_DIR" (expand-file-name "~/emacs_conf/")))
	  ((eq system-type 'gnu/linux)
	   (setenv "EMACS_DIR" (expand-file-name "~/.emacs_conf/"))))

(cond ((eq system-type 'windows-nt) 
	   (setenv "EMACS_PACKAGES" 
			   (expand-file-name 
				(concat (getenv "EMACS_DIR") "emacs_packages/"))))
	  ((eq system-type 'gnu/linux)
	   (setenv "EMACS_PACKAGES" 
			   (expand-file-name
				(concat (getenv "EMACS_DIR") "emacs_packages/")))))

;; Usage: (concat (getenv "EMACS_DIR") "yourFileName")

;; ===================================================================
;; GNUS ==============================================================
;; ===================================================================
;; (setq gnus-init-file "~/.emacs_config/gnus.el")
(cond ((eq system-type 'gnu/linux)
	   (setq gnus-select-method 
			 '(nntp "Gmane" 
					(nntp-address "news.gmane.org"))) ))

;; ===================================================================
;; SCRATCH BUFFER ====================================================
;; ===================================================================
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; ===================================================================
;; ENABLE EMACS SERVER ===============================================
;; ===================================================================
;;(server-start)

;; ===================================================================
;; DISABLE CTRL-Z MINIMIZATION/SUSPENSION OF EMACS ===================
;; ===================================================================
(global-set-key [(control z)] nil)

;; ===================================================================
;; REPLACE MARKED REGION WITH NEWLY TYPED TEXT =======================
;; ===================================================================
(delete-selection-mode 1)

;; ===================================================================
;; ANSWER Y INSTEAD OF YES ON PROMPTS ================================
;; ===================================================================
(defalias 'yes-or-no-p 'y-or-n-p)

;; ===================================================================
;; FRAME TITLE =======================================================
;; ===================================================================
;; Display filename in title
;; (setq frame-title-formatlist "%b")

;; Display complete path + filename in title (ie
;; ~/dirA/dirB/fileX.txt)
(setq frame-title-format
      '("%S" (buffer-file-name "%f"
							   (dired-directory dired-directory "%b"))))

;; ===================================================================
;; TAB-WIDTH =========================================================
;; ===================================================================
(setq-default tab-width 4)

;; ===================================================================
;; SKELETON PAIR MODE ================================================
;; ===================================================================
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;; ===================================================================
;; IBUFFER ===========================================================
;; ===================================================================
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)

;; ===================================================================
;; IDO (Buffer Management) ===========================================
;; ===================================================================
(require 'ido)
(ido-mode t)

;; ===================================================================
;; CSS ===============================================================
;; ===================================================================
(setq cssm-indent-level 4)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)

;; ===================================================================
;; NXHTML ============================================================
;; ===================================================================
;; Includes MuMaMo (Multiple Major Modes)
;; (load (concat (getenv "EMACS_PACKAGES") "nxhtml/autostart.el"))

;; ===================================================================
;; PRIVATE FUNCTIONS =================================================
;; ===================================================================
(load (concat (getenv "EMACS_DIR") "pd-functions.el"))

;; ===================================================================
;; CALENDAR / DIARY: =================================================
;; ===================================================================
;; default diary file:
(setq diary-file (concat (getenv "EMACS_DIR") "diary"))
;; Send diary entries by mail:
(setq diary-mail-addr "yourname@foo.bar")

;; Number of days the diary-mail-entries function should include:
(setq diary-mail-days 14)

;;; activate fancy diary display (taken from the manual):
(add-hook 'diary-display-hook 'fancy-diary-display)
;; sort diary entries (needs the above fancy display; taken from the manual):
(add-hook 'list-diary-entries-hook 'sort-diary-entries t)
(load (concat (getenv "EMACS_DIR") "german-holidays.el"))

;; ===================================================================
;; AUTO-COMPLETE =====================================================
;; ===================================================================
(add-to-list 'load-path 
			 (concat (getenv "EMACS_PACKAGES") "auto-complete/"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
			 (concat (getenv "EMACS_PACKAGES") "auto-complete/ac-dict"))
(ac-config-default)

;; FIXME =============================================================
(load-file (concat (getenv "EMACS_PACKAGES") "fixme-mode.el"))
(require 'fixme-mode)

;; ===================================================================
;; ORG MODE ==========================================================
;; ===================================================================
(load-file (concat (getenv "EMACS_DIR") "org-stuff.el"))

;; ===================================================================
;; AUCTEX ============================================================
;; ===================================================================
;; only load when system is linux
(cond ((eq system-type 'gnu/linux)
	   (load-file (concat (getenv "EMACS_DIR") "tex-stuff.el"))))

;; ===================================================================
;; ECB/CEDET =========================================================
;; ===================================================================
;; only load when system is linux
(cond ((eq system-type 'gnu/linux)
	   (load-file (concat (getenv "EMACS_DIR") "ecb-cedet-stuff.el"))))

;; ===================================================================
;; RUBY ON RAILS =====================================================
;; ===================================================================
; only load when system is linux
(cond ((eq system-type 'gnu/linux)
	   (load (concat (getenv "EMACS_DIR") "rails-stuff.el"))))

;; ===================================================================
;; CUSTOMIZE SECTION =================================================
;; ===================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style (quote european))
 '(column-number-mode t)
 '(desktop-restore-eager 5)
 '(desktop-save-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(fancy-splash-image "nil")
 '(global-auto-revert-mode t)
 '(ido-enable-flex-matching t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)
