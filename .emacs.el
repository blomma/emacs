;;;-----------------------------------------------------------------------------
;;; Set the load path
;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/psgml-1.2.5") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/template/lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/ljupdate") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/muse-3.01.91") load-path))

;;; Add for ecb
;; (setq load-path (cons (expand-file-name "~/.emacs.d/lisp/speedbar-0.14beta4") load-path))
;; (setq load-path (cons (expand-file-name "~/.emacs.d/lisp/eieio-0.17") load-path))
;; (setq load-path (cons (expand-file-name "~/.emacs.d/lisp/semantic-1.4.4") load-path))
;; (setq load-path (cons (expand-file-name "~/.emacs.d/lisp/ecb-2.31") load-path))


;;;-----------------------------------------------------------------------------
;;; Misc
;;;

(setq darwin(eq system-type 'darwin))
(setq linux(eq system-type 'gnu/linux))
(setq windows(eq system-type 'windows-nt))

;; Start upp a server under linux
(if (not windows)
    (server-start))

;;; Language settings
(unless window-system
  (set-terminal-coding-system 'iso-latin-1)
  (set-keyboard-coding-system 'iso-latin-1))

(set-input-mode nil nil 0)
(set-input-mode t nil 0)
(set-language-environment "Latin-1")
(set-terminal-coding-system 'iso-8859-1)
(set-keyboard-coding-system 'iso-8859-1)

(prefer-coding-system 'iso-8859-1)

(when linux
  (set-default-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1"))

;; Mac specifik confs
(when darwin
  (create-fontset-from-fontset-spec
   "-apple-monaco-medium-r-normal--14-*-*-*-*-*-fontset-monaco, ascii:-apple-monaco-medium-r-normal--14-*-*-*-m-*-mac-roman,latin-iso8859-1:-apple-monaco-medium-r-normal--14-*-*-*-m-*-mac-roman")
  (set-frame-font "-apple-monaco-medium-r-normal--14-*-*-*-*-*-fontset-monaco" 'keep)
  (add-to-list 'default-frame-alist '(font . "-apple-monaco-medium-r-normal--14-*-*-*-*-*-fontset-monaco"))

  ;; Set startup frame width
  (setq default-frame-alist
		'((top . 1) (left . 1) (width . 126) (height . 42)))

  ;; Antialias the text
;;   (setq mac-allow-anti-aliasing 1)
  )


;;;-----------------------------------------------------------------------------
;;; Internal emacs variables
;;;

;; Turn off cursor blinking
(blink-cursor-mode -1)

;; Turn off toolbar
(tool-bar-mode -1)

;; Turn off tooltips
(tooltip-mode -1)

;; Mouse focus follow
(setq mouse-autoselect-window t)
(setq x-mouse-click-focus-ignore-position t)

;; Disable the scrollbar
(scroll-bar-mode -1)

;; put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

;; enable syntax-highlighting
(global-font-lock-mode 1 t)

;; change default major mode to text from fundamental
(setq default-major-mode 'text-mode)

;; Set to insert tabs instead of space
(setq-default indent-tabs-mode t)

;; don't automatically add new lines when scrolling down at the bottom
;; of a buffer
(setq next-line-add-newlines nil)

;; scroll just one line when hitting the bottom of the window
(setq scroll-step 1)

;; format the title-bar to always include the buffer name
(setq-default frame-title-format (list "Emacs: %f"))
(setq-default icon-title-format "Emacs - %b")

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; show a menu only when running within X (save real estate when
;; in console)
(menu-bar-mode (if window-system 1 -1))

;; flash instead of that annoying bell
(setq visible-bell t)

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

;; (add-hook 'write-file-hooks 'copyright-update)
;; (setq copyright-update-active t)

;; Set tab length
(setq default-tab-width 4)

;; highlight during query
(setq query-replace-highlight t)        

;; Automagically read compressed files
(auto-compression-mode 1)

;; Access system clipboard
(setq x-select-enable-clipboard t)

;; Set default spell shecker
(setq-default ispell-program-name "/Users/blomma/Opt/bin/aspell")

(setq completion-auto-help t)
(setq completion-auto-exit t)
(setq inhibit-startup-message t)
(setq garbage-collection-messages t)

;; No optimizations please, unless running from home...
(if window-system
    (setq baud-rate 1000000)
  (setq baud-rate 2400))

;;;---------------------------------------------------------------------
;;; Mark handling.  The following two lines makes the highlighted
;;; region visible, but I'm still able to use all region-commands even
;;; if the region has been turned off (just like in the good ol'
;;; days!)
;;;

(if (boundp 'transient-mark-mode)
    (setq transient-mark-mode t))
(setq mark-even-if-inactive t)

;;;---------------------------------------------------------------------
;;; Search
;;;

(setq-default search-highlight t)
(define-key isearch-mode-map "\C-^" 'isearch-edit-string)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(copy-face 'highlight 'isearch)

;;;---------------------------------------------------------------------
;;; Autosave and Backup
;;;

(setq auto-save-default t)
(setq auto-save-visited-file-name nil)
(setq make-backup-files nil)
(setq backup-by-copying-when-linked t)  ; Preserve links!

;;;---------------------------------------------------------------------
;;; The mode line and the frame header.
;;;
;;; The following section adds the line number to the mode line, and
;;; the time and date to the frame header line.  The date is displayed
;;; in standard european 24 hour format, the format americans tends to
;;; refer to as "military" time...

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
(line-number-mode t)
(column-number-mode t)

;;;-----------------------------------------------------------------------------
;;; Muse mode
;;;

;; Initialize
(require 'outline)       ; I like outline-style faces
(require 'muse)          ; load generic module
(require 'muse-colors)   ; load coloring/font-lock module
(require 'muse-mode)     ; load authoring mode
(require 'muse-html)     ; load (X)HTML publishing style
(require 'muse-journal)
(require 'muse-project)
(require 'muse-wiki)

(setq muse-project-alist
	  '(("Website"                 ; my various writings
		 ("~/Projects/blog" :default "index")
		 (:base "html" :path "~/Projects/blog/html"))))


;;;-----------------------------------------------------------------------------
;;; Livejournal mode
;;;

(require 'ljupdate)

;;;-----------------------------------------------------------------------------
;;; Highligth parenteses
;;;

(when (or (string-match "XEmacs\\|Lucid" emacs-version) window-system)
  (require 'mic-paren) ; loading
  (paren-activate)     ; activating
  )

;;;-----------------------------------------------------------------------------
;;; Lua mode
;;;

(autoload 'lua-mode "lua-mode" "Lua Mode." t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(setq lua-indent-level 4)

;;;-----------------------------------------------------------------------------
;;; Todo mode
;;;

(autoload 'todoo "todoo" "TODO Mode" t)
(add-to-list 'auto-mode-alist '("TODO$" . todoo-mode))

(defun todoo-or-close-todoo()
  (interactive)
  (if (equal mode-name "Todoo")
      (call-interactively 'todoo-save-and-exit)
    (call-interactively 'todoo)))


;; (load "/Users/blomma/Applications/acl62_trial/eli/fi-site-init")
;; (setq fi:lisp-evals-always-compile nil)
;; (add-to-list 'auto-mode-alist '("\\.cl$" . common-lisp-mode))
;; (setq fi:common-lisp-buffer-name "*common-lisp*")
;; (setq fi:common-lisp-directory (expand-file-name "/Users/blomma/")) 
;; (setq fi:common-lisp-image-name "/Users/blomma/Applications/acl62_trial/alisp")
;; ;; (setq fi:common-lisp-host (system-name))
;; (setq fi:common-lisp-host "localhost")

;; (defun run-allegro ()
;;   (interactive)
;;   (split-window)
;;   (find-file "/Users/blomma/rename-me.cl")
;;   (other-window 1)
;;   (fi:common-lisp
;;    fi:common-lisp-buffer-name
;;    fi:common-lisp-directory
;;    fi:common-lisp-image-name
;;    fi:common-lisp-image-arguments
;;    fi:common-lisp-host))

;;; Tone down parens in lisp modes

(defvar paren-face 'paren-face)

(defface paren-face
  '((((class color))
     (:foreground "DimGray")))
  "Face for displaying a paren."
  :group 'faces)

(defmacro paren-face-add-support (keywords)
  "Generate a lambda expression for use in a hook."
  `(lambda ()
     (let* ((regexp "(\\|)")
            (match (assoc regexp ,keywords)))
       (unless (eq (cdr match) paren-face)
         (setq ,keywords (append (list (cons regexp paren-face)) ,keywords))))))

;; Keep the compiler quiet.
(eval-when-compile
  (defvar scheme-font-lock-keywords-2 nil)
  (defvar lisp-font-lock-keywords-2 nil))

(add-hook 'scheme-mode-hook           (paren-face-add-support scheme-font-lock-keywords-2))
(add-hook 'lisp-mode-hook             (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'emacs-lisp-mode-hook       (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'lisp-interaction-mode-hook (paren-face-add-support lisp-font-lock-keywords-2))
(add-hook 'common-lisp-mode-hook      (paren-face-add-support lisp-font-lock-keywords-2))

;;;-----------------------------------------------------------------------------
;;; Python mode
;;;

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))


;;;-----------------------------------------------------------------------------
;;; Ecb mode
;;;

;; (setq semantic-load-turn-everything-on t)
;; (require 'semantic-load)
;; (global-semantic-show-dirty-mode -1)
;; (global-semantic-show-unmatched-syntax-mode -1)

;; (require 'ecb)

;;;-----------------------------------------------------------------------------
;;; File-log mode
;;;

(require 'file-log)

(define-key ctl-x-map "l" 'flog-add-entry)
(define-key ctl-x-4-map "l" 'flog-add-entry-other-window)
(define-key ctl-x-5-map "l" 'flog-add-entry-other-frame)

(define-key menu-bar-tools-menu [separator-print]
  '("--"))
(define-key menu-bar-tools-menu [flog-add-entry]
  '("Add file log entry" . flog-add-entry))

;;;-----------------------------------------------------------------------------
;;; css mode
;;;

(autoload 'css-mode "css-mode")
(setq auto-mode-alist       
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))

(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)

;;;-----------------------------------------------------------------------------
;;; Template mode
;;;

(require 'template)
(template-initialize)

;;;-----------------------------------------------------------------------------
;;; Filladapt
;;;

(require 'filladapt)

;;;-----------------------------------------------------------------------------
;;; Php mode
;;;

(require 'php-mode)
(font-lock-add-keywords 'php-mode '(("\\<FIXME:" 0 font-lock-warning-face t)))

;;;-----------------------------------------------------------------------------
;;; Ruby mode
;;;

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files")
(setq auto-mode-alist
	  (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
									 interpreter-mode-alist))
(setq ruby-indent-level 4)

(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
		  '(lambda ()
			 (inf-ruby-keys)
			 ))

;;;-----------------------------------------------------------------------------
;;; Psgml mode
;;; use psgml-mode instead of emacs native sgml-mode
;;;

(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t )
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(setq auto-mode-alist
	  (append
	   (list
	    '("\\.sgm$" . sgml-mode)
		'("\\.sgml$" . sgml-mode)
		'("\\.xml$" . sgml-mode)
		)
	   auto-mode-alist))

;; set some psgml variables
(setq sgml-auto-activate-dtd t)
(setq sgml-omittag-transparent t)
(setq sgml-balanced-tag-edit t)
(setq sgml-auto-insert-required-elements t)
(setq sgml-live-element-indicator t)
(setq sgml-indent-step 4)
(setq sgml-indent-data t)
(setq sgml-insert-end-tag-on-new-line t)

;; create faces to assign to markup categories				  
(make-face 'sgml-comment-face)
(make-face 'sgml-start-tag-face)
(make-face 'sgml-end-tag-face)
(make-face 'sgml-entity-face)
(make-face 'sgml-doctype-face) ; DOCTYPE data
(make-face 'sgml-ignored-face) ; data ignored by PSGML
(make-face 'sgml-ms-start-face) ; marked sections start
(make-face 'sgml-ms-end-face) ; end of marked section
(make-face 'sgml-pi-face) ; processing instructions
(make-face 'sgml-sgml-face) ; the SGML declaration
(make-face 'sgml-shortref-face) ; short references


;; assign faces to markup categories
(setq sgml-markup-faces '
	  (
	   (comment . sgml-comment-face)
	   (start-tag . sgml-start-tag-face)
	   (end-tag . sgml-end-tag-face)
	   (entity . sgml-entity-face)
	   (doctype . sgml-doctype-face)
	   (ignored . sgml-ignored-face)
	   (ms-start . sgml-ms-start-face)
	   (ms-end . sgml-ms-end-face)
	   (pi . sgml-pi-face)
	   (sgml . sgml-sgml-face)
	   (shortref . sgml-shortref-face)
	   ))

;; tell PSGML to pay attention to face settings
(setq sgml-set-face t)

;;;-----------------------------------------------------------------------------
;;; Info mode
;;;

(defvar info-font-lock-keywords
  (list
   '("^\\* [^:]+:+" . font-lock-function-name-face)
   '("\\*[Nn]ote\\b[^:]+:+" . font-lock-reference-face)
   '("  \\(Next\\|Prev\\|Up\\):" . font-lock-reference-face))
  "Additional expressions to highlight in Info mode")

(add-hook 'Info-mode-hook
          (lambda ()
            (make-local-variable 'font-lock-defaults)
            (define-key Info-mode-map [down-mouse-1] 
              'Info-mouse-follow-nearest-node)
            (setq font-lock-defaults '(info-font-lock-keywords nil t))))

(setq Info-directory-list
      '(
        "/usr/local/info/" 
		"/home/child/opt/info"))

;;;-----------------------------------------------------------------------------
;;; CPerl mode
;;;

(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq cperl-hairy t)
(setq cperl-tab-always-indent t)
(setq cperl-indent-left-aligned-comments t)
(setq cperl-use-syntax-table-text-property t)
(setq cperl-pod-here-scan t)
(setq cperl-electric-keywords 'null)
(setq cperl-indent-level 4)
(setq cperl-brace-offset -4)
(setq cperl-continued-brace-offset 0)
(setq cperl-label-offset -4)
(setq cperl-continued-statement-offset 4)
(setq cperl-merge-trailing-else nil)
(setq cperl-extra-newline-before-brace t)

(setq auto-mode-alist
      (append '(("\\.\\([pP][Llm]\\|al\\)$" . perl-mode))  auto-mode-alist ))

(setq auto-mode-alist (append (list (cons "\\.cgi\\'" 'perl-mode))
                              auto-mode-alist))

;;;-----------------------------------------------------------------------------
;;; Html-helper mode
;;;

(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)

;; use html-helper-mode when editing .html files
(setq auto-mode-alist (cons '("\\.htm$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.template$" . html-helper-mode)  auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tpl$" . html-helper-mode)  auto-mode-alist))

;; don't wrap lines when editing web pages
(add-hook 'html-helper-mode-hook 'turn-on-hscroll)

(setq html-helper-item-continue-indent 4)
(setq html-helper-basic-offset 4)
(setq html-helper-do-write-file-hooks t)
(setq html-helper-build-new-buffer t)
(setq html-helper-address-string 
      "<a href=\"mailto:blomma@artsoftheinsane.com\">Mikael Hultgren &lt;blomma@artsoftheinsane.com&gt;</a>")

;; add some color
(make-face 'html-tag-face)

;;;-----------------------------------------------------------------------------
;;; Diff mode
;;;
(require 'diff-mode-)
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;;;----------------------------------------------------------------------
;;;  C/C++ mode
;;;

(defconst my-c-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . (
							   (substatement-open before after)
							   (brace-list-open before after)
                               (defun-open)
                               (defun-close)
							   ))
    (c-offsets-alist . (
						(topmost-intro        . 0)
                        (topmost-intro-cont   . *)
                        (substatement         . +)
                        (substatement-open    . 0)
                        (case-label           . +)
                        (access-label         . /)
                        (inclass              . +)
                        (inline-open          . 0)
                        (arglist-close        . 0)
                        ))
	(setq c-echo-syntactic-information-p t)
	)
  "My C/C++ Programming Style")

(defun my-c-mode-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  
  ;; Define some stuff to font lock
  (font-lock-add-keywords 'c-mode '(("\\<FIXME:" 0 font-lock-warning-face t)))
  (font-lock-add-keywords 'c++-mode '(("\\<FIXME:" 0 font-lock-warning-face t)))

  ;; Turn on filladapt
  (c-setup-filladapt)
  (setq filladapt-mode 1)
  (setq indent-tabs-mode t)

  ;; we like hungry-delete
  ;;(c-toggle-hungry-state 1)

  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;----------------------------------------------------------------------
;;; C# Mode support
;;;

(autoload 'csharp-mode "csharp-mode")

(c-add-style "myC#Style"
             '(
               (c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . (
                                          (substatement-open before after)
                                          (brace-list-open before after)
                                          (defun-open)
                                          (defun-close)
                                          ))

               (c-offsets-alist . (
                                   (topmost-intro        . 0)
                                   (topmost-intro-cont   . *)
                                   (substatement         . +)
                                   (substatement-open    . 0)
                                   (case-label           . +)
                                   (access-label         . 0)
                                   (inclass              . +)
                                   (inline-open          . 0)
                                   (arglist-close        . 0)
                                   (innamespace          . +)
                                   ))
               ))

(defun my-csharp-mode-hook ()
  (cond (window-system
         (turn-on-font-lock)
         (c-set-style "myC#Style")
         )))

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(setq auto-mode-alist
      (append '(
                ("\\.cs$" . csharp-mode)
                ) auto-mode-alist ))

;;;------------------------------------------------------------
;;; Folding
;;;

;;(add-hook 'folding-mode-hook 'my-folding-mode-hook)

;;(defun my-folding-mode-hook ()
;;  (interactive)
;;  (setq fold-behave-table
;;		'((close 	fold-hide)
;;		  (open   	fold-enter)
;;		  (up		fold-exit)
;;		  (other	fold-mouse-call-original))))

;;; I like the keys the way they used to be...
;;(setq fold-default-keys-function 'fold-bind-backward-compatible-keys)

;;(load "folding.el" 'nomessage 'noerror)
;;(folding-mode-add-find-file-hook)

;;(folding-add-to-marks-list 'csharp-mode "#startregion"  "#endregion"  nil t)

;;;-----------------------------------------------------------------------------
;;; Key bindings
;;;

(global-set-key [f1] 'goto-line) 
(global-set-key [(shift f1)] 'view-emacs-FAQ)
(global-set-key [f2] 'comment-region) 
(global-set-key [(shift f2)] 'uncomment-region)
(global-set-key [f3] 'shell) 
(global-set-key [f4] 'indent-region)
(global-set-key [(shift f4)] 'wrap-all-lines) 
(global-set-key [f5] 'bury-buffer)
(global-set-key [(shift f5)] 'list-colors-display)
(global-set-key [f7] 'imenu)
(global-set-key [(shift f7)] 'insert-perl-die)
(global-set-key [f8] 'run-perl) 
(global-set-key [(shift f8)] 'debug-perl) 
(global-set-key [f9] 'split-window-vertically)
(global-set-key [f10] 'todoo-or-close-todoo)
(global-set-key [f11] 'query-replace)

;; Goto a specific line is really needed!
(global-set-key "\M-g" 'goto-line)

(global-set-key "\C-xs" 'save-buffer)

(define-key global-map [M-S-down-mouse-3] 'imenu)

;; Scroll Bar gets dragged by mouse butn 1
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;; Dabbrev
;;(global-set-key (kbd "C-<tab>") 'dabbrev-expand)
;;(define-key minibuffer-local-map (kbd "C-<tab>") 'dabbrev-expand)

;; Revert buffer
(global-set-key [(control c) r] 'revert-buffer)

;;
;; Rebind mouse-2 events to mouse-1 in various places:
;; Completion list
(add-hook 'completion-list-mode-hook
		  '(lambda() (define-key completion-list-mode-map [down-mouse-1] 
					   'mouse-choose-completion)))
;; Buffer Menu
(add-hook 'buffer-menu-mode-hook
		  '(lambda() (define-key Buffer-menu-mode-map [down-mouse-1] 
					   'Buffer-menu-mouse-select)))

(pc-bindings-mode)
(pc-selection-mode)
(delete-selection-mode nil)

;;;----------------------------------------------------------------------
;;;  Custom var
;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ada-broken-decl-indent 1)
 '(ada-indent 4)
 '(ada-prj-default-comp-cmd "/Users/blomma/Opt/Stow/gcc-4.0/bin/gnatmake -u -c ${gnatmake_opt} ${full_current} -cargs ${comp_opt}")
 '(ada-prj-default-gnatmake-opt "-g -gnato -gnatwu")
 '(ada-prj-default-make-cmd "/Users/blomma/Opt/Stow/gcc-4.0/bin/gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}")
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(cperl-brace-imaginary-offset 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-brace-offset 0 t)
 '(cperl-highlight-variables-indiscriminately nil)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face nil)
 '(cperl-under-as-char t)
 '(cua-mode t nil (cua-base))
 '(delete-selection-mode t nil (delsel))
 '(ecb-layout-window-sizes (quote (("blomma" (0.2608695652173913 . 0.225) (0.2608695652173913 . 0.325) (0.2608695652173913 . 0.425)))))
 '(ecb-options-version "2.26")
 '(global-font-lock-mode t nil (font-core))
 '(html-helper-mode-uses-KG-style t nil (html-helper-mode))
 '(lj-allow-comments "no")
 '(lj-default-username "artsoftheinsane")
 '(paren-ding-unmatched nil)
 '(paren-display-message (quote only))
 '(paren-dont-load-timer nil)
 '(paren-sexp-mode nil)
 '(semanticdb-default-save-directory "~/.emacs.d/semanticdb.cache/")
 '(sgml-basic-offset 4)
 '(show-paren-mode nil)
 '(swbuff-clear-delay 1)
 '(template-auto-insert t)
 '(template-initialize t)
 '(template-subdirectories (quote ("~/.emacs.d/templates/" "Templates/")))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(user-mail-address "Mikael Hultgren <blomma@gmail.com>")
 '(vc-path (quote ("/sw/bin")))
 '(vc-stay-local nil))

;;;----------------------------------------------------------------------
;;;  Font lock
;;;

(defun color-theme-blomma ()
  "Color theme for blomma's funk"
  (interactive)
  (color-theme-install
   '(color-theme-blomma
     ((background-color . "#01010e")
      (background-mode . dark)
      (mouse-color . "Grey")
	  (cursor-color . "light goldenrod")
      (foreground-color . "light goldenrod"))

     ;;; Standard faces
     (default ((t (nil))))
     (font-lock-builtin-face ((t (:bold t :foreground "#777"))))
     (font-lock-comment-face ((t (:foreground "NavajoWhite4"))))
     (font-lock-constant-face ((t (:foreground "lightsteelblue3"))))
     ;;(font-lock-doc-string-face ((t (:foreground "#777"))))
     ;;(font-lock-doc-face ((t (:foreground "#777"))))
     (font-lock-function-name-face ((t (:foreground "lightsteelblue3"))))
     (font-lock-keyword-face ((t (:foreground "#8080b3"))))
     ;;(font-lock-preprocessor-face ((t (:foreground "#777"))))
     ;;(font-lock-reference-face ((t (:foreground "#777"))))
     (font-lock-string-face ((t (:foreground "goldenrod3"))))
     (font-lock-type-face ((t (:foreground "lightsteelblue3"))))
     (font-lock-variable-name-face ((t (:foreground "lightsteelblue3"))))
     (font-lock-warning-face ((t (:bold t :foreground "#999"))))
     
     ;;; Miscellaneous
     (isearch ((t (:foreground "black" :background "paleturquoise"))))
     (show-paren-match-face ((t (:foreground "black" :background "yellow"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "red"))))
     ;;(paren-face ((t (:background "black" :foreground "darkgreen"))))
     (paren-face-match ((t (:background "lightsteelblue3"))))
     (paren-match ((t (:background "darkseagreen4"))))
     (region ((t (:background "#434343"))))

     ;;; GUI elements
     (menu ((t (:foreground "#c0c0c0" :background "#5a5c6f"))))
     (modeline ((t (:background "lightsteelblue" :foreground "#2c0e76"))))

     ;;; Cperl faces
     (cperl-array-face ((t (:foreground "lightsteelblue3"))))
     (cperl-hash-face ((t (:foreground "lightsteelblue3"))))
     (cperl-nonoverridable-face ((t (:foreground "#ff8080"))))
     (cperl-invalid-face ((t (:foreground "#c0c0c0"))))

     ;;; SGML faces
     (sgml-start-tag-face ((t (:foreground "lightsteelblue3"))))
     (sgml-ignored-face ((t (:foreground "#ff8080"))))
     (sgml-doctype-face ((t (:foreground "lavender"))))
     (sgml-sgml-face ((t (:foreground "#c0c0c0"))))
     (sgml-end-tag-face ((t (:foreground "lightsteelblue3"))))
     (sgml-entity-face ((t (:foreground "lightsteelblue3"))))
     (sgml-comment-face ((t (:foreground "#b38080"))))
     (sgml-ms-start-face ((t (:foreground "#c0c0c0"))))
     (sgml-ms-end-face ((t (:foreground "#c0c0c0"))))
     (sgml-pi-face-face ((t (:foreground "#c0c0c0"))))
     (sgml-shortref-face ((t (:foreground "#c0c0c0"))))
 
     ;;; Html helper
     (html-helper-bold-face ((t (:bold t))))
     (html-helper-bold-italic-face ((t (nil))))
     (html-helper-builtin-face ((t (:underline t :foreground "blue3"))))
     (html-helper-italic-face ((t (:foreground "medium sea green"))))
     (html-helper-underline-face ((t (:underline t))))
     (html-tag-face ((t (:foreground "#8080b3"))))
	 (todoo-item-assigned-header-face ((t (:foreground "NavajoWhite4" :weight bold))))
	 (todoo-item-header-face ((t (:foreground "lightsteelblue3" :weight bold))))

)))

(require 'color-theme)
(color-theme-blomma)

;;;---------------------------------------------------------------------
;;; Misc functions
;;;

;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer;
print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; insert date into buffer
(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %e, %Y %l:%M %p")))

;; convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

;; vice versa
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))

;; Call fortune!
(defun fortune ()
  (interactive)
  (pop-to-buffer "*Fortune*")
  (insert (shell-command-to-string "/usr/games/fortune")))

;; Ange FTP detection. (from Mike West <mikewest@hotmail.com>)
(defconst remote-file-regexp "^/[^/:]*:" 
  "Regexp to match for remote filename")

(defun is-remote-file (filename)
  "Returns t if filename is a valid remote file name for ange-ftp"
  (cond ((and filename
              (string-match remote-file-regexp filename)) t)))

(defun is-ange-ftp ()
  (interactive)
  (if (is-remote-file (buffer-file-name))
      (message "File is from remote")
    (message "File is local")))

;; This method, when bound to C-x C-c, allows you to close an emacs frame the 
;; same way, whether it's the sole window you have open, or whether it's
;; a "child" frame of a "parent" frame.  If you're like me, and use emacs in
;; a windowing environment, you probably have lots of frames open at any given
;; time.  Well, it's a pain to remember to do Ctrl-x 5 0 to dispose of a child
;; frame, and to remember to do C-x C-x to close the main frame (and if you're
;; not careful, doing so will take all the child frames away with it).  This
;; is my solution to that: an intelligent close-frame operation that works in 
;; all cases (even in an emacs -nw session).
(defun intelligent-close ()
  "quit a frame the same way no matter what kind of frame you are on"
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      ;;for parent/master frame...
      (if (> (length (visible-frame-list)) 1)
          ;;close a parent with children present
          (delete-frame (selected-frame))
        ;;close a parent with no children present
        (save-buffers-kill-emacs))
    ;;close a child frame
    (delete-frame (selected-frame))))

;;compute the length of the marked region 
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

;; Kill other window and and enlarge current buffer
(defun kill-buffer-other-window (arg)
  "Kill the buffer in the other window, and make the current buffer full size. If no
      other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
               (other-window arg)
               (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)))

;; Insert // header
(defun insert-header ()
  "Insert a // header for the current file"
  (interactive)
  (insert (concat
		   "// File: " (file-name-nondirectory (buffer-file-name)) "\n//\n"
		   "// Created: " (format-time-string "%Y-%m-%d") "\n"
		   "// Time-stamp: <>\n"
		   "// Copyright (C) " (substring( current-time-string) -4 )
		   " by " (user-full-name) "\n//\n"
		   "// Author: " (user-full-name) "\n//\n"
		   )))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added-face ((t (:foreground "DarkGreen"))) t)
 '(diff-changed-face ((t (:foreground "MediumBlue"))) t)
 '(diff-context-face ((t (:foreground "Black"))) t)
 '(diff-file-header-face ((t (:foreground "Red" :background "White"))) t)
 '(diff-header-face ((t (:foreground "Red"))) t)
 '(diff-hunk-header-face ((t (:foreground "White" :background "Salmon"))) t)
 '(diff-index-face ((t (:foreground "Green"))) t)
 '(diff-nonexistent-face ((t (:foreground "DarkBlue"))) t)
 '(diff-removed-face ((t (:foreground "DarkMagenta"))) t))

;;;-----------------------------------------------------------------------------
;;; Buffer switching
;;;

(require 'swbuff-y);
(setq swbuff-display-intermediate-buffers t)
(swbuff-y-mode t)

(global-set-key [(control right)] 'swbuff-switch-to-next-buffer)
(global-set-key [(control left)] 'swbuff-switch-to-previous-buffer)
