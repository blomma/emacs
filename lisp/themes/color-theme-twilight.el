;;; color-theme-twilight.el --- Twilight Color Theme for Emacs.

;; Copyright (C) 2008 Marcus Crafter <crafterm@redartisan.com>

;; Author: Marcus Crafter
;; Adapted-By: Yesudeep Mangalapilly
;; Keywords: textmate twilight color theme
;; URL: https://github.com/crafterm/twilight-emacs
;; Version: 0.2
;; Package-Requires: ((color-theme "6.6.1"))

;;; Code:

;; requires
(require 'color-theme)

(defun color-theme-twilight ()
  "TextMate Twilight theme for GNU Emacs."
  (interactive)
  (color-theme-install
   '(color-theme-twilight

     ;;; color-theme mapping
     ((background-color . "#141414")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8"))

     ;;; basic coloring
     (default ((t (:foreground "#CACACA"))))
     (blue ((t (:foreground "blue"))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
     (highlight ((t (:background "#111111"))))
     (highline-face ((t (:background "SeaGreen"))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "blue"))))
     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (region ((t (:background "#27292A"))))

     ;;; font lock
     (font-lock-builtin-face ((t (:foreground "#CACACA"))))
     (font-lock-comment-delimiter-face ((t (:foreground  "#5F5A60"))))
     (font-lock-comment-face ((t (:foreground "#5F5A60"))))
     (font-lock-constant-face ((t (:foreground "#CF6A4C"))))
     (font-lock-doc-face ((t (:foreground "DarkOrange"))))
     (font-lock-doc-string-face ((t (:foreground "#94bff3"))))
     (font-lock-function-name-face ((t (:foreground "#9B703F"))))
     (font-lock-keyword-face ((t (:foreground "#CDA869"))))
     (font-lock-negation-char-face ((t (:foreground "#dcdccc"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#8F9D6A"))))
     (font-lock-type-face ((t (:foreground "#9B703F"))))
     (font-lock-variable-name-face ((t (:foreground "#7587A6"))))
     (font-lock-warning-face ((t (:background "#EE799F" :foreground "red"))))

     ;; show-paren
     (show-paren-mismatch ((t (:foreground "#9c6363" :weight bold))))
     (show-paren-match ((t (:foreground "#7cb8bb" :weight bold))))

     ;; Enhanced-Ruby-Mode
     (ruby-string-delimiter-face  ((t (:foreground "#5A6340"))))
     (ruby-regexp-delimiter-face ((t (:foreground "orange"))))
     (ruby-heredoc-delimiter-face ((t (:foreground "#9B859D"))))
     (ruby-op-face ((t (:foreground "#CDA869"))))

     ;; ido-mode
     (ido-subdir ((t (:foreground "#CF6A4C"))))
     (ido-first-match ((t (:foreground "#8F9D6A"))))
     (ido-only-match ((t (:foreground "#8F9D6A"))))

     ;; (mumamo-background-chunk-submode ((t (:background "#222222"))))

     (linum ((t (:background "#141314" :foreground "#2D2B2E"))))

     (hl-line ((t (:background "#212121"))))

     (yas/field-highlight-face ((t (:background "#27292A"))))


     ;; mode line
     (isearch ((t (:foreground "#f0dfaf" :background "#2b2b2b"))))
     (isearch-fail ((t (:foreground "#dcdccc" :background "#8c5353"))))
     (lazy-highlight ((t (:foreground "#f0dfaf" :background "#5f5f5f"))))

     (minibuffer-prompt ((t (:foreground "#5F5A60"))))

     (mode-line
      ((t (:foreground "#8fb28f"  :background "#2b2b2b"))))
     (mode-line-buffer-id ((t (:foreground "#f0dfaf" :weight bold))))
     (mode-line-inactive
      ((t (:foreground  "#5f7f5f"  :background  "#2b2b2b")))))))

(add-to-list 'color-themes '(color-theme-twilight
                             "Twilight"
                             "Mikael Hultgren <blomma@gmail.com"))

(provide 'color-theme-twilight)
