;;;-----------------------------------------------------------------------------
;;; Set the load path
;;;

(setq load-path (cons (expand-file-name "~/.emacs.d/lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lisp/template/lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/themes") load-path))

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
;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(when linux
  (set-default-font "-misc-fixed-medium-r-normal--15-140-75-75-c-90-iso8859-1"))

;; Mac specifik confs
(when darwin
  (set-face-font 'default "Menlo-12")

  ;; Set startup frame width
  (setq default-frame-alist
		'((top . 1) (left . 1) (width . 126) (height . 42)))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser
		delete-by-moving-to-trash t)
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

;; Disable the scrollbar
(scroll-bar-mode -1)

;; Mouse focus follow
(setq mouse-autoselect-window t)
(setq x-mouse-click-focus-ignore-position t)

;; put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

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

;; flash instead of that annoying bell
(setq visible-bell t)

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

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

(setq auto-save-default nil)
(setq auto-save-visited-file-name nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
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
;;; Markdown mode
;;;
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
	  (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;;-----------------------------------------------------------------------------
;;; Whitespace mode
;;;

(autoload 'nuke-trailing-whitespace "whitespace" nil t)
(add-hook 'write-file-functions 'nuke-trailing-whitespace)

;;;-----------------------------------------------------------------------------
;;; Line numbering mode
;;;

(require 'linum)
(setq linum-format " %d ")

;;;-----------------------------------------------------------------------------
;;; Textmate mode
;;;

(require 'textmate)
(textmate-mode)

;;;-----------------------------------------------------------------------------
;;; Highligth parenteses
;;;

(require 'mic-paren) ; loading
(paren-activate)     ; activating

;;;-----------------------------------------------------------------------------
;;; VB.net mode
;;;

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
								 vbnet-mode)) auto-mode-alist))

(defun my-vbnet-mode-fn ()
  "My hook for VB.NET mode"
  (interactive)
  ;; This is an example only.
  ;; These statements are not required to use VB.NET, but
  ;; you might like them.
  (turn-on-font-lock)
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  )

(add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)

;;;-----------------------------------------------------------------------------
;;; Lua mode
;;;

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

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

;;;-----------------------------------------------------------------------------
;;; Key bindings
;;;

(global-set-key [f1] 'goto-line)
(global-set-key [(shift f1)] 'view-emacs-FAQ)
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
(global-set-key [f11] 'query-replace)

;; Goto a specific line is really needed!
(global-set-key "\C-l" 'goto-line)

(global-set-key "\C-xs" 'save-buffer)

;; Revert buffer
(global-set-key [(control c) r] 'revert-buffer)

;;;----------------------------------------------------------------------
;;;  Custom var
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(cperl-brace-imaginary-offset 0)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-brace-offset 0 t)
 '(cperl-highlight-variables-indiscriminately nil)
 '(cperl-indent-parens-as-block t)
 '(cperl-invalid-face nil)
 '(cperl-under-as-char t)
 '(delete-selection-mode t nil (delsel))
 '(global-linum-mode t)
 '(paren-ding-unmatched nil)
 '(paren-display-message (quote only))
 '(paren-dont-load-timer nil)
 '(paren-sexp-mode nil)
 '(show-paren-mode nil)
 '(template-auto-insert t)
 '(template-initialize t)
 '(template-subdirectories (quote ("~/.emacs.d/templates/" "Templates/")))
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode t)
 '(user-mail-address "Mikael Hultgren <blomma@gmail.com>")
)

;;;----------------------------------------------------------------------
;;;  Color Theme
;;;

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/color-theme-twilight.el")
(color-theme-twilight)

;;;---------------------------------------------------------------------
;;; Misc functions
;;;

;; narrower window, better line wrapping for prose
(defun write-words ()
  (interactive)
  (set-frame-width nil 90)
  (global-visual-line-mode t)
  (setq mode-line-format nil)
  (show-paren-mode nil))

;; widescreen, no line-wrap
(defun write-code ()
  (interactive)
  ;;(set-frame-width nil 320)
  (set-frame-height nil 95)
  (global-visual-line-mode 0)
  (show-paren-mode)
  (setq mode-line-format
		(list "-"
			  'mode-line-mule-info
			  'mode-line-modified
			  'mode-line-frame-identification
			  'mode-line-buffer-identification
			  "   "
			  'mode-line-position
			  '(vc-mode vc-mode)
			  "   "
			  'mode-line-modes
			  '(which-func-mode ("" which-func-format))
			  '(global-mode-string (global-mode-string))
			  )))

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
  (insert (shell-command-to-string "/Users/blomma/Opt/homebrew/bin/fortune")))

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

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
