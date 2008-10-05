;;; diff-mode-.el --- Extensions to `diff-mode.el'.
;; 
;; Filename: diff-mode-.el
;; Description: Extensions to `diff-mode.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004, Drew Adams, all rights reserved.
;; Created: Mon Nov 08 16:36:09 2004
;; Version: 1.0
;; Last-Updated: Tue Nov 16 14:56:59 2004
;;           By: dradams
;;     Update #: 571
;; Keywords: data, matching, tools, unix, local, font, face
;; Compatibility: GNU Emacs 21.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Extensions to `diff-mode.el'.
;;
;;  "*Diff*" buffer is highlighted differently.
;;
;;  NOTE: The faces defined here look best on a medium-dark
;;        background, because some are light and some are dark.
;;        Try, for example, setting the background to "LightSteelBlue"
;;        in your `~/.emacs' file: You can do this is via
;;        `special-display-buffer-names':
;;
;;         (setq special-display-buffer-names
;;               (cons '("*Diff*" (background-color . "LightSteelBlue"))
;;                     special-display-buffer-names))
;;
;;        You can alternatively change the background value of
;;        `special-display-frame-alist' and set
;;        `special-display-regexps' to something matching "*info*":
;;
;;         (setq special-display-frame-alist 
;;               (cons '(background-color . "LightSteelBlue")
;;                     special-display-frame-alist))
;;         (setq special-display-regexps '("[ ]?[*][^*]+[*]"))
;;
;;
;;  New user options defined here:
;;
;;    `diff-change-indicator-face', `diff-file1-hunk-header-face',
;;    `diff-file2-hunk-header-face', `diff-insertion-indicator-face',
;;    `diff-removal-indicator-face'.
;;
;;
;;  ***** NOTE: The following faces defined in `diff-mode.el' have
;;              been REDEFINED HERE:
;;
;;    `diff-added-face', `diff-changed-face', `diff-context-face',
;;    `diff-file-header-face', `diff-header-face',
;;    `diff-hunk-header-face', `diff-index-face',
;;    `diff-nonexistent-face', `diff-removed-face'.
;;
;;
;;  ***** NOTE: The following variable defined in `diff-mode.el' has
;;              been REDEFINED HERE:
;;
;;    `diff-font-lock-keywords'.
;;
;;
;;  This library should be loaded *before* library `diff-mode.el'.
;;  Put this in your initialization file, `~/.emacs':
;;    (require 'diff-mode-)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

;;;;;;;;;;;;;;;;;;;;;;;;


;;; Define some additional faces.
(defface diff-change-indicator-face
  '((t (:foreground "PaleGoldenrod" :background "MediumBlue")))
  "*Face used to highlight the line-start indicator of a modified line.")
(defvar diff-change-indicator-face 'diff-change-indicator-face)

(defface diff-file1-hunk-header-face
  '((t (:foreground "Blue" :background "DarkSeaGreen1")))
  "Face used to highlight a diff hunk for the first `diff' argument.")
(defvar diff-file1-hunk-header-face 'diff-file1-hunk-header-face)

(defface diff-file2-hunk-header-face
  '((t (:foreground "Red" :background "PaleGoldenrod")))
  "Face used to highlight a diff hunk for the second `diff' argument.")
(defvar diff-file2-hunk-header-face 'diff-file2-hunk-header-face)

(defface diff-insertion-indicator-face
  '((t (:foreground "PaleGoldenrod" :background "DarkGreen")))
  "*Face used to highlight the line-start indicator of an inserted line.")
(defvar diff-insertion-indicator-face 'diff-insertion-indicator-face)

(defface diff-removal-indicator-face
  '((t (:foreground "PaleGoldenrod" :background "DarkMagenta")))
  "*Face used to highlight the line-start indicator of a removed line.")
(defvar diff-removal-indicator-face 'diff-removal-indicator-face)


;;; Change existing `diff-mode' faces too.
(custom-set-faces
 '(diff-added-face ((t (:foreground "DarkGreen"))) 'now)
 '(diff-changed-face ((t (:foreground "MediumBlue"))) 'now)
 '(diff-context-face ((t (:foreground "Black"))) 'now)
 '(diff-file-header-face ((t (:foreground "Red" :background "White"))) 'now)
 ;; '(diff-function-face ((t (:foreground "Orange"))) 'now)
 '(diff-header-face ((t (:foreground "Red"))) 'now)
 '(diff-hunk-header-face ((t (:foreground "White" :background "Salmon"))) 'now)
 '(diff-index-face ((t (:foreground "Green"))) 'now)
 '(diff-nonexistent-face ((t (:foreground "DarkBlue"))) 'now)
 '(diff-removed-face ((t (:foreground "DarkMagenta"))) 'now)
 )

;;; Change the highlighting regexps.
(defvar diff-font-lock-keywords
  `(
    ("^\\(@@ -[0-9,]+ \\+[0-9,]+ @@\\)\\(.*\\)$" ;unified
     (1 diff-hunk-header-face)
     (2 diff-function-face))
    ("^\\(\\*\\{15\\}\\)\\(.*\\)$"      ;context
     (1 diff-hunk-header-face)
     (2 diff-function-face))
    ("^--- .+ ----$" . diff-file2-hunk-header-face)               ;context
    ("^\\*\\*\\* .+ \\*\\*\\*\\*". diff-file1-hunk-header-face)   ;context
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\(\\S-+\\)\\(.*[^*-]\\)?\n"
     (0 diff-header-face) (2 diff-file-header-face prepend))
    ("^[0-9,]+[acd][0-9,]+$" . diff-hunk-header-face)
    ("^\\(!\\)\\(.*\\)\n" (1 diff-change-indicator-face)(2 diff-changed-face))
    ("^\\([+>]\\)\\(.*\\)\n" (1 diff-insertion-indicator-face)(2 diff-added-face))
    ("^\\([-<]\\)\\(.*\\)\n" (1 diff-removal-indicator-face)(2 diff-removed-face))
    ("^Index: \\(.+\\).*\n" (0 diff-header-face) (1 diff-index-face prepend))
    ("^Only in .*\n" . diff-nonexistent-face)
    ("^#.*" . font-lock-string-face)
    ("^[^-=+*!<>].*\n" (0 diff-context-face))
    ))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'diff-mode-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diff-mode-.el ends here
