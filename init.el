;;;-----------------------------------------------------------------------------
;;; Set the load path
;;;

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/themes")

(defun add-subfolders-to-load-path (parent-dir) ;; from bbatsov
  "Adds all first level `parent-dir' subdirs to the Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(add-subfolders-to-load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Executables might be somewhere else
(add-to-list 'exec-path "~/Opt/homebrew/bin")

;;;-----------------------------------------------------------------------------
;;; Misc
;;;

(setq darwin(eq system-type 'darwin))
(setq linux(eq system-type 'gnu/linux))
(setq windows(eq system-type 'windows-nt))

;; Start upp a server
(if (not windows)
    (server-start))

;;; Language settings
;; use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Mac specifik confs
(when darwin
  (setq default-input-method "MacOSX")

  (setq
   mac-option-modifier t
   mac-allow-anti-aliasing t
   mac-command-key-is-meta t
   )

  (set-face-font 'default "Menlo-12")

  ;; Set startup frame width
  (setq default-frame-alist
        '((top . 90) (left . 90) (width . 126) (height . 42)))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash t)
  )

;;;-----------------------------------------------------------------------------
;;; Internal emacs variables
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs nil)
 '(current-language-environment "UTF-8")
 '(delete-selection-mode t nil (delsel))
 '(paren-ding-unmatched nil)
 '(paren-display-message (quote only))
 '(paren-dont-load-timer nil)
 '(paren-sexp-mode nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(user-mail-address "Mikael Hultgren <blomma@gmail.com>"))

;; put as much syntax highlighting into documents as possible
(setq font-lock-maximum-decoration t)

;; change default major mode to text from fundamental
(setq default-major-mode 'text-mode)

;; Set to insert spaces instead of tabs
(setq-default indent-tabs-mode nil)

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

;; Set tab length
(setq default-tab-width 4)

;; highlight during query
(setq query-replace-highlight t)

(setq completion-auto-help t)
(setq completion-auto-exit t)
(setq inhibit-startup-message t)

;; Make buffer names unique.
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")

;;;---------------------------------------------------------------------
;;; Mark handling.  The following two lines makes the highlighted
;;; region visible, but I'm still able to use all region-commands even
;;; if the region has been turned off (just like in the good ol'
;;; days!)
;;;

;; Make the region act like common text selection.
(transient-mark-mode 1)

;; <Enter> should be smart. (DWIM)
(global-set-key (kbd "RET") 'newline-and-indent)

;;;---------------------------------------------------------------------
;;; Search
;;;

(setq-default search-highlight t)
(define-key isearch-mode-map "\C-^" 'isearch-edit-string)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(copy-face 'highlight 'isearch)

;; Display "Don't Panic" in large, friendly, letters
(setq initial-scratch-message
      (propertize "Don't\nPanic\n"
                  'font-lock-face '(:height 10.0 :inherit variable-pitch))
      inhibit-startup-screen t)

;;;---------------------------------------------------------------------
;;; Autosave and Backup
;;;

(setq auto-save-default nil)
(setq auto-save-visited-file-name nil)
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq backup-by-copying-when-linked t)

;;;---------------------------------------------------------------------
;;; The mode line and the frame header.
;;;

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

(setq markdown-command "multimarkdown")

;;;-----------------------------------------------------------------------------
;;; Javascript beautify
;;;

(require 'js-beautify)

;;;-----------------------------------------------------------------------------
;;; Autocomplete
;;;

(require 'auto-complete-config)

; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete.el/dict")

(ac-config-default)

(global-auto-complete-mode t)

; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)

; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;;;-----------------------------------------------------------------------------
;;; Yasnippets
;;;

(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory '("~/.emacs.d/snippets"
                           "~/.emacs.d/vendor/yasnippet.el/snippets"))
(mapc 'yas/load-directory yas/root-directory)

;; (add-to-list 'ac-sources 'ac-source-yasnippet)

;;;-----------------------------------------------------------------------------
;;; Flymake mode
;;;

(require 'flymake-cursor)

;;;-----------------------------------------------------------------------------
;;; Whitespace mode
;;;

(add-hook 'before-save-hook 'whitespace-cleanup)

;;;-----------------------------------------------------------------------------
;;; Textmate mode
;;;

(require 'textmate)
(textmate-mode)

;;;-----------------------------------------------------------------------------
;;; Highligth parenteses
;;;

(require 'mic-paren)
(paren-activate)

;;;-----------------------------------------------------------------------------
;;; Lisp modes
;;;

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
;;; Filladapt
;;;

(require 'filladapt)

;;;-----------------------------------------------------------------------------
;;; Buffer switching
;;;

(require 'ido)
(ido-mode t)

(defun my-ido-ignore-buffers (name)
  (with-current-buffer name
    (string-match "-template-indent-buffer$" name)))

(setq ido-ignore-buffers '(my-ido-ignore-buffers))

;; fuzzy matching is a must have, says rmm5t
(setq ido-enable-flex-matching t)

;; Get rid of the annoying .ido.last file
;; (http://stackoverflow.com/questions/1371076)
(setq
 ido-enable-last-directory-history nil
 ido-record-commands nil
 ido-max-work-directory-list 0
 ido-max-work-file-list 0)

(global-set-key (kbd "C-;") 'ido-switch-buffer)

;; Display IDO results vertically, rather than horizontally
;; (from timcharper, jpkotta via EmacsWiki)
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
              " [No match]" " [Matched]" " [Not readable]"
              " [Too big]" " [Confirm]")))

(defun ido-disable-line-trucation ()
  (set (make-local-variable 'truncate-lines) nil))

(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;;;----------------------------------------------------------------------
;;;  Color Theme
;;;

(require 'color-theme-twilight)
(color-theme-twilight)

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

  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;-----------------------------------------------------------------------------
;;; Key bindings
;;;

(global-set-key [(shift f1)] 'view-emacs-FAQ)
(global-set-key [f3] 'shell)
(global-set-key [f4] 'indent-region)
(global-set-key [(shift f4)] 'wrap-all-lines)
(global-set-key [f5] 'bury-buffer)
(global-set-key [(shift f5)] 'list-colors-display)
(global-set-key [f7] 'imenu)
(global-set-key [f9] 'split-window-vertically)
(global-set-key [f11] 'query-replace)

;; Goto a specific line is really needed!
;; (global-set-key "\C-l" 'goto-line)

;; (global-set-key "\C-xs" 'save-buffer)

;; Revert buffer
(global-set-key [(control c) r] 'revert-buffer)

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
  (set-frame-width nil 150)
  (set-frame-height nil 40)
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

;;compute the length of the marked region
(defun region-length ()
  "length of a region"
  (interactive)
  (message (format "%d" (- (region-end) (region-beginning)))))

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
 )

;; --------------------------------------------------------
;; nice little alternative visual bell; Miles Bader <miles /at/ gnu.org>
(defcustom echo-area-bell-string "*DING* " ;"♪"
  "Message displayed in mode-line by `echo-area-bell' function."
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :group 'user)
;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)
(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.
    The string displayed is the value of `echo-area-bell-string',
    with a red background; the background highlighting extends to the
    right margin.  The string is displayed for `echo-area-bell-delay'
    seconds.
    This function is intended to be used as a value of `ring-bell-function'."
  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
            echo-area-bell-string)
           'face '(:background "red")))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))
(setq ring-bell-function 'echo-area-bell)
