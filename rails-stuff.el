1;; ===================================================================
;; RUBY ON RAILS STUFF ===============================================
;; ===================================================================

;; Requires NXHTML mode:
;; Ruby On Rails =====================================================
;; (require 'mumamo-fun)
;; (setq mumamo-chunk-coloring 'submode-colored)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))

;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)
;; (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))

;; RINARI ============================================================

;; Rinari is not a Ruby IDE

;; IDO is recommended (see above)     

;; Rinari
(add-to-list 'load-path (concat (getenv "EMACS_PACKAGES") "/rinari"))

;; 2010-04-27 
;; 
;; rinari.el: See defvar rinari-minor-mode-prefixes
;;
;; Why? All the default prefixes need a modifier like shift or alt on
;; a German keyboard.
;;
;; This patch must appear before the line "(require 'rinari)"!
(defvar rinari-minor-mode-prefixes
  (list ";" "'" ",") ;; PDCHANGE 2010-04-27 Added comma to prefix list
  "List of characters, each of which will be bound (with C-c) as a rinari-minor-mode keymap prefix.")

(require 'rinari)

(setq rinari-tags-file-name "TAGS")


;; RSPEC =============================================================
(load-file (concat (getenv "EMACS_PACKAGES") "el-mock.el"))
(load-file (concat (getenv "EMACS_PACKAGES") "el-expectations.el"))
(load-file (concat (getenv "EMACS_PACKAGES") "rspec-mode/rspec-mode.el"))
(load-file (concat (getenv "EMACS_PACKAGES") "rspec-mode/rspec-mode-expectations.el"))

;; HAML ==============================================================
(add-to-list 'load-path "/var/lib/gems/1.8/gems/haml-3.0.18/extra/")
(require 'haml-mode)

;; RUBY ==============================================================
;; Automatically close Ruby blocks
;; https://github.com/rejeep/ruby-end
(add-to-list 'load-path (concat (getenv "EMACS_PACKAGES") "ruby-end/"))
(require 'ruby-end)

;; RSENSE ============================================================
(setq rsense-home (concat (getenv "EMACS_PACKAGES") "rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)
