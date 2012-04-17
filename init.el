;;;-----------------------------------------------------------------------------
;;; Set the load path
;;;

(defun add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

        (add-subfolders-to-load-path "~/.emacs.d/vendor")

        (defun add-subfolders-to-theme-load-path (parent-dir)
          "Adds all first level `parent-dir' subdirs to the Emacs theme load path."
          (dolist (f (directory-files parent-dir))
            (let ((name (concat parent-dir "/" f)))
              (when (and (file-directory-p name)
                         (not (equal f ".."))
                         (not (equal f ".")))
                (add-to-list 'custom-theme-load-path name)))))

(add-subfolders-to-theme-load-path "~/.emacs.d/theme")

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Executables might be somewhere else
(add-to-list 'exec-path "~/Opt/homebrew/bin")

;;;-----------------------------------------------------------------------------
;;; General settings
;;;

(setq darwin(eq system-type 'darwin))
(setq linux(eq system-type 'gnu/linux))
(setq windows(eq system-type 'windows-nt))

;; Start upp a server
(if (not windows)
    (server-start))

;; key board / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal

;; Mac specifik confs
(when darwin
  (setq default-input-method "MacOSX")

  (setq
   mac-option-modifier t
   mac-allow-anti-aliasing t
   mac-command-key-is-meta t
   )

  (set-face-font 'default "Menlo-14")

  ;; Set startup frame width
  (setq default-frame-alist
        '((top . 90) (left . 90) (width . 126) (height . 42)))

  (setq browse-url-browser-function 'browse-url-default-macosx-browser
        delete-by-moving-to-trash t)
  )

(setq user-mail-address "Mikael Hultgren <blomma@gmail.com>")

(setq blink-cursor-mode nil)

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

;; scrollbar
(scroll-bar-mode -1)

;; delete seleciton
(delete-selection-mode 1)

;; goodbye toolbar
(tool-bar-mode -1)

;; Create a reasonable titlebar for emacs, which works
;; on both windows and unix.
;; Note: assumes HOSTNAME is exported.
(defun create_title_format (user host)
  "Creates a window title string"
  (interactive)
  (list (getenv user) "@" (getenv host) ":"
        '(:eval
          (if buffer-file-name
              (replace-regexp-in-string
               (getenv "HOME")
               "~"
               (buffer-file-name))
            (buffer-name))))
  )

;; Set window and icon title.
(setq frame-title-format
      (create_title_format "USER" "HOSTNAME"))

;; Display "Don't Panic" in large, friendly, letters
(setq initial-scratch-message
      (propertize "Don't\nPanic\n"
                  'font-lock-face '(:height 10.0 :inherit variable-pitch))
      inhibit-startup-screen t)


;;;---------------------------------------------------------------------
;;; Rainbow delimiters
;;;

(require 'rainbow-delimiters)

(global-rainbow-delimiters-mode)

;;;---------------------------------------------------------------------
;;; Taskpaper
;;;

(require 'taskpaper)

(setq taskpaper-dir (expand-file-name "~/Dropbox/Bucket/"))

(defun taskpaper ()
  (interactive)
  (let ((filename (concat taskpaper-dir
                          (format-time-string "%Y-%m-%d.taskpaper"))))
    (find-file-other-frame filename)))

;;;---------------------------------------------------------------------
;;; Deft
;;;

(require 'deft)

(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/Bucket/Notes")
(setq deft-text-mode 'markdown-mode)
(setq deft-use-filename-as-title t)

;;;---------------------------------------------------------------------
;;; Auto pair
;;;

(require 'autopair)

(autopair-global-mode)

;;;---------------------------------------------------------------------
;;; Save place
;;;

(require 'saveplace)

(setq-default save-place t)

;;;---------------------------------------------------------------------
;;; Make buffer names unique.
;;;

(require 'uniquify)

(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

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

(setq-default mode-line-format
              (list
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "] "


               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat ","  (propertize "Mod"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               minor-mode-alist  ;; list of minor modes
               "%-" ;; fill with '-'
               ))

;;;-----------------------------------------------------------------------------
;;; Markdown mode
;;;

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(setq markdown-command "multimarkdown")

;;;-----------------------------------------------------------------------------
;;; Autocomplete
;;;

(require 'auto-complete-config)

;; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/vendor/auto-complete.el/dict")

(ac-config-default)
(global-auto-complete-mode t)

;; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)

;; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;;;-----------------------------------------------------------------------------
;;; Yasnippets
;;;

(require 'yasnippet)

(yas/initialize)
(setq yas/root-directory
      '("~/.emacs.d/snippets"
        "~/.emacs.d/vendor/yasnippet.el/snippets"))

(mapc 'yas/load-directory yas/root-directory)
(setq yes/wrap-around-region t)
(setq yes/prompt-functions
      '(yas/x-prompt yas/ido-prompt))
(yas/global-mode 1)
(add-to-list 'auto-mode-alist '("yas/.*" . snippet-mode))

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
;;; Buffer switching
;;;

(require 'ido)

(ido-mode 'both)

;; fuzzy matching is a must have, says rmm5t
(setq ido-enable-flex-matching t)

(setq
 ido-ignore-buffers ;; ignore these guys
 '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold  t                 ;; be case-insensitive
 ido-enable-last-directory-history t ;; remember last used dirs
 ido-max-work-directory-list 30   ;; should be enough
 ido-max-work-file-list      50   ;; remember many
 ido-use-filename-at-point nil    ;; don't use filename at point (annoying)
 ido-use-url-at-point nil         ;; don't use url at point (annoying)
 ido-enable-flex-matching t       ;; fuzzy matching is a must have, says rmm5t
 ido-max-prospects 8              ;; don't spam my minibuffer
 ido-confirm-unique-completion t) ;; wait for RET, even with unique completion

;; Get rid of the annoying .ido.last file
;; (http://stackoverflow.com/questions/1371076)
(setq
 ido-enable-last-directory-history nil
 ido-record-commands nil
 ido-max-work-directory-list 0
 ido-max-work-file-list 0)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

(global-set-key (kbd "C-;") 'ido-switch-buffer)

;; Display IDO results vertically, rather than horizontally
;; (from timcharper, jpkotta via EmacsWiki)
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
              " [No match]" " [Matched]" " [Not readable]"
              " [Too big]" " [Confirm]")))

(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (set (make-local-variable 'truncate-lines) nil))))

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook
          (function
           (lambda ()
             (make-local-variable 'resize-minibuffer-window-max-height)
             (setq resize-minibuffer-window-max-height 1))))

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

(global-set-key [f8] 'deft)

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
  )

;; widescreen, no line-wrap
(defun write-code ()
  (interactive)
  (set-frame-width nil 150)
  (set-frame-height nil 40)
  (global-visual-line-mode 0)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(background-color "#042028")
 '(background-mode dark)
 '(cursor-color "#708183")
 '(custom-enabled-themes (quote (twilight)))
 '(custom-safe-themes (quote ("f2de89c68dd88605a1a22467c77375b4f3b9530d6d866b1b2f1b05378a20242c" "c2e946f6382ad9476ed8f45a2971f46d1f291e10747090ee8cbe6537e0a34347" "da93340745b198a67a0daa41d72ec713c893e7cef9b8fe2d00f346d5755d3f37" "ecb58d3d27238e077bc48e013c9f5f09e2cab5442863339eb0c6ada97767bab7" "69a85c90b5ed858ccbb4426e836b522fc6c9ec99415a41e402fa059e5ddb2efd" "3602d01868d70a6ee600bb7d2cde42f9dba207d2ea77ed48409cc1bf27f3b860" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "870bd363bb2770316775ffa6e5938d73bee3adaba1f4d5b7b129533b3e0fed41" "0c8ffb272e65e2ffc49b0d5755d7db137931c5e3ed47890d7a6de07717eaa2e8" "7511ae742ae5e87bc096db346ab4694c1042a4a6035d7d15f4b86b4f2213c8d8" "b70e5b325e9c1b5672675343b35407a3d64b055b6fca5846f55232127693cb2c" "3f43263bd8540fcba50c16b0e21f788a4a73fb06cbfc8fb61afeab625ff61ec2" "edb0e9dce76acf08243762d30683293812c838773f0e9f41b7e6baf904776d6c" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#708183"))
