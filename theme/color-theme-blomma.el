;;; color-theme-blomma.el --- 

;; Copyright 2011 Mikael Hultgren
;;
;; Author: Mikael Hultgren <blomma@gmail.com>
;; Version: $Id: color-theme-blomma.el,v 0.0 2011/11/12 16:23:57 blomma Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'color-theme)

(defun color-theme-blomma ()
  "Color theme for blomma's funk"
  (interactive)
  (color-theme-install
   '(color-theme-blomma
     ((background-color . "#01010e")
      (background-mode . dark)
	  (cursor-color . "light goldenrod")
      (foreground-color . "light goldenrod"))

     ;;; Standard faces
     (default ((t (nil))))
     (font-lock-builtin-face ((t (:bold t :foreground "#777"))))
     (font-lock-comment-face ((t (:foreground "NavajoWhite4"))))
     (font-lock-constant-face ((t (:foreground "lightsteelblue3"))))
     (font-lock-doc-string-face ((t (:foreground "#777"))))
     (font-lock-doc-face ((t (:foreground "#777"))))
     (font-lock-function-name-face ((t (:foreground "lightsteelblue3"))))
     (font-lock-keyword-face ((t (:foreground "#8080b3"))))
     (font-lock-preprocessor-face ((t (:foreground "#777"))))
     (font-lock-reference-face ((t (:foreground "#777"))))
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

(provide 'color-theme-blomma)

;;; color-theme-blomma.el ends here
