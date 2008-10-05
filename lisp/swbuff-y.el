;;; swbuff-y.el --- Modifications to David Ponce's swbuff

;; Copyright (C) 2001, 2002, 2003  Free Software Foundation, Inc.

;; Author: Kahlil (Kal) HODGSON <dorge@tpg.com.au>
;; Rewritten as swbuff-y.el by: Lennart Borgman <lennart dot borgman dot 073 at student dot lu dot se>
;; Keywords: files, convenience
;; X-URL: http://www.ourcomments.org/Emacs/DL/more/swbuff-y.el
;; Version: 0.56

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.



;;; History:
;;
;; 2005-06-19: Modified bad test for xemacs.
;;             Changed default face color for special buffers.
;; 2005-07-01: First version of swbuff-y.
;;
;; 2005-07-17: 0.53, Bug fixes: Mixed read-char and define-key by mistaked.
;;
;; 2005-07-30: 0.54, Fixed a bug introduced above. All not defined keys where not ignored.
;;             Buffer ordering after 'n' was incorrect. Fixed.
;;             Various smaller bug fixes.
;;             Added C-g to keymap.


;;; Commentary:

;; This is (maybe) the successor of the excellent swbuff-x.el
;; package.  Most of the code is just inherited (stolen?) from there.
;; New symbols have been named swbuff-y-.*.
;;
;; The following have been changed and added (from a user perspektive):
;;
;; - Checks for if buffers are visible in other windows.  If so then
;;   the buffer name is underlined and typing RET will display the
;;   corresponding frame.
;; 
;; - "n" means display selected buffer in new frame.
;;
;; - Buffer switching is bound to C-Tab/C-S-Tab. This can be turned
;;   on/off with the minor mode swbuff-y-mode.
;;
;; - You can jump out of buffer switching with C-g.
;;
;; - Keyboard commands during buffer switching are also available in a
;;   popup menu (assigned to <apps>).
;;
;; - Typing a key will not carry out the associated command.  It just
;;   switches to the selected buffer.  (Then you can type things like
;;   "C-Tab Tab" to switch buffer.)
;;
;; - The kill command is not available by default.  (But can be added
;;   to the keymap.)
;;
;; - Changing to default values, including faces.



;;; Commentary from swbuff-x.el
;; X-URL: http://www.emacswiki.org/elisp/swbuff-x.el
;; Time-stamp: <2003-11-16 17:23:10 kahlil>

;; A few modifications to David Ponce's most excellent swbuff package
;;
;; (1) Fix the display timer so that it doesn't interfere with other
;; packages e.g speedbar and Ispell.
;;
;; (2) Maintain buffer list ordering so that only the first and last
;; buffer in a sequence are effected.
;;
;; (3) Exclude buffers whose major mode matches
;; `swbuff-exclude-mode-regexp' from the buffer list but include any
;; buffers that match `swbuff-include-buffer-regexps' (a couterpoint
;; to `swbuff-exclude-buffer-regexps'). Also If
;; `swbuff-this-frame-only' is non-nil exclude buffers displayed in
;; other visible frames.
;;
;; (4) New hook `swbuff-pre-switch-hook' for things you may want to do
;; before switching buffers.
;;
;; (5) New function `swbuff-kill-this-buffer' which useful for
;; selectively cleaning out your buffer list.
;;
;; (6) If `swbuff-start-with-current-centered' is non-nil buffer list
;; display starts with the current buffer roughly in the middle of the
;; display ordering.  This encourages the use of
;; `swbuff-previous-buffer' to get to buffers which would otherwise
;; appear at the end of the list.
;;
;; (7) New variables `swbuff-left' and `swbuff-right' as an
;; alternative to `swbuff-header', `swbuff-trailer' and
;; `swbuff-separator'.  This allows you to place brackets around the
;; buffer name.

;; (8) Display buffer name matching `swbuff-special-buffers-re' using
;; `swbuff-special-buffers-face'.

;; (9) Added variable `swbuff-modeline-format' to make the modeline of the
;; status window configurable (Thanks to Matthias Wedel).
;;

;;; Thanks:

;; Matthias Wedel for spotting some bugs and with XEmacs
;; compatibility.

;;; Code:

;;(unless (fboundp 'cua-mode) (error "swbuff-y.el requires Emacs 22.0.50 or later"))

(require 'swbuff)

;;; Change some defaults:
(setq swbuff-clear-delay 9)
(setq  swbuff-delay-switch t)



;;; User Variables

(defcustom swbuff-this-frame-only nil
  "If non-nil, buffers displayed in other frames are skipped.
This is a convient way of temprorily excluding a particluar
buffer from your cycle list."
  :type 'boolean
  :group 'swbuff
  )

(defvar swbuff-exclude-mode-regexp ""
  "Regular expression matching major modes to skip when cycling.")

(defvar swbuff-include-buffer-regexps '("")
  "For excluding buffers from list.
List of regular expressions matching buffer names to include in
the `swbuf-buffer-list'.")

(defvar swbuff-pre-switch-hook nil
"Standard hook containing functions to be called before a switch.
You may make this buffer local.  This may be useful for handling modes that
use more than one window for display.  For example, VM use one (small)
window for it Summary buffer and the remaining frame for the Presentation
buffer.  Switching buffers and retaining the window configuration doesn't
make sense (at least to me) in this context, so I set the following hooks
to delete these extra windows before switching:

\(defun my-vm-mode-hook () \"Delete other windows before a switch.\"
  (make-local-hook 'swbuff-pre-switch-hook)
  (add-hook 'swbuff-pre-switch-hook #'delete-other-windows t t))

\(add-hook 'vm-mode-hook              #'my-vm-mode-hook)
\(add-hook 'vm-summary-mode-hook      #'my-vm-mode-hook)
\(add-hook 'vm-presentation-mode-hook #'my-vm-mode-hook)"
)

(defcustom swbuff-start-with-current-centered nil
  "If non-nil center current buffer when entering switching if."
  :type 'boolean
  :group 'swbuff)

(defcustom swbuff-delay-switch nil
  "When non-nil do not switch buffer when entering switching.
If t the first call to `swbuff-next-buffer' or
`swbuff-previous-buffer' simply displays the buffer list rather
than switching."
  :type 'boolean
  :group 'swbuff)

(defcustom swbuff-display-intermediate-buffers nil
  "If non-nil display intermediate buffers.
If t each call to `swbuff-next-buffer' or
`swbuff-previous-buffer' in a sequence causes a new buffer to be
displayed.  If nil only the last buffer in the sequence is
actually displayed."
  :type 'boolean
  :group 'swbuff)

;; alternative to head tail and sep
(defcustom swbuff-left ""
  "String placed immediately before a buffer name in the status line.
For example, try \"(\"."
  :type 'string
  :group 'swbuff)
(defcustom swbuff-right ""
  "String placed immediately after a buffer name in the status line.
For example, try \")\"."
  :type 'string
  :group 'swbuff)

(defcustom swbuff-special-buffers-re "^\\*"
  "Regular expression for matching special buffers.
Regular expression matching buffers that should receive special
highlighting in the buffer display."
  :type 'string
  :group 'swbuff)

(defface swbuff-y-special-buffers-face
  '((t (:foreground "gray" :bold nil :underline nil)))
  "Face for special buffers in swbuff display." )

(defface swbuff-y-special-buffers-other-face
  '((t (:inherit 'swbuff-y-special-buffers-face :underline t)))
  "Face for special buffers shown in other frames." )

(defface swbuff-y-current-buffer-face
  '((t (:inherit 'highlight)))
  "Face for highlighting current buffer in swbuff display." )

(defface swbuff-y-current-buffer-other-face
  '((t (:inherit 'swbuff-y-current-buffer-face :underline t)))
  "Face for highlighting current buffer shown in other frames." )

(defface swbuff-y-in-other-frame-face
  '((t :underline t))
  "Face for windows currently shown in other windows." )

;; Respect different Emacsen naming conventions, otherwise interactuve
;; help will not work as expected.

(let ((fmt " Type a key or wait. 'n' new frame, RET switch frame (underlined only), APPS menu."))
  (if (featurep 'xemacs)
      (defvar swbuff-modeline-format fmt
        "Adjust modeline of the status window.
See `mode-line-format' for a detailed format description.")
    (defvar swbuff-mode-line-format fmt
      "Adjust modeline of the status window.
See `mode-line-format' for a detailed format description.")))
  
;; quiet compilers for both Emacsen
(defvar modeline-format)
(defvar mode-line-format)




;;; Local Variables

(defvar swbuff-y-current-marker nil "For positioning popup menu.")

;; Keymap to use during switching
(defconst swbuff-y-keymap (make-sparse-keymap))

(defconst swbuff-y-keymap-alist (list (cons 'swbuff-y-keymap swbuff-y-keymap)))
(unless (fboundp 'cua-mode)
  (setq swbuff-y-keymap-alist (car swbuff-y-keymap-alist)))
(define-key swbuff-y-keymap [(control g)] 'swbuff-y-quit)
(define-key swbuff-y-keymap "\r"      'swbuff-y-maybe-display-other-frame)
(define-key swbuff-y-keymap [(?f)]    'swbuff-y-maybe-display-other-frame)
(define-key swbuff-y-keymap [(?n)]    'swbuff-y-show-in-new-frame)
(define-key swbuff-y-keymap [(apps)]  'swbuff-y-show-popup)
(define-key swbuff-y-keymap [(left)]  'swbuff-switch-to-previous-buffer)
(define-key swbuff-y-keymap [(right)] 'swbuff-switch-to-next-buffer)
(define-key swbuff-y-keymap [(control ?p)]  'swbuff-switch-to-previous-buffer)
(define-key swbuff-y-keymap [(control ?n)]  'swbuff-switch-to-next-buffer)
(define-key swbuff-y-keymap [(control shift tab)] 'swbuff-switch-to-previous-buffer)
(define-key swbuff-y-keymap [(control tab)]       'swbuff-switch-to-next-buffer)
;;(setcdr (cdr swbuff-y-keymap) (cons (cons t 'ignore) (cdr (cdr swbuff-y-keymap)) ))  

(defvar swbuff-y-jump-to-window nil
  "Used for jumping to a window on another frame.")
(defvar swbuff-y-new-frame nil
  "Used for showing buffer in a new frame.")

;; Store the initial buffer-list, buffer, window, and frame at the
;; time the switch sequence was called.
(defvar swbuff-initial-buffer-list nil)
(defvar swbuff-initial-buffer nil)
(defvar swbuff-initial-window nil)
(defvar swbuff-initial-frame nil)

;; Current buffer being displayed by swbuff sequence.
(defvar swbuff-current-buffer nil)

;; Save the status buffer window, in case any external code that runs on a
;; timer changes the current window.
(defvar swbuff-status-window nil)




(defun swbuff-y-check-status-window-active()
  (unless swbuff-status-window
    ;;(message "emul=%s" emulation-mode-map-alists)
    (error "You can not call %s unless Swbuff status window is active" this-command)))



;;; Frame handling helpers

(defun swbuff-y-in-other-frame-p (buffer)
  "Return window in other frame where BUFFER is displayed.
Useful if we want to skip buffers displayed in other frames \(see
variable `swbuff-buffer-list')."
  (let ((found-in-other-frame nil)
	(window nil)
	(window-list (get-buffer-window-list buffer nil 0)))
    (while (and (setq window (car window-list))
		(not found-in-other-frame))
      (unless (eq (window-frame window) swbuff-initial-frame)
	;;(setq found-in-other-frame t))
        ;; Return window so that we can use this
	(setq found-in-other-frame window))
      (pop window-list))
    found-in-other-frame
    ))

(defun swbuff-y-maybe-display-other-frame()
  (interactive)
  (swbuff-y-check-status-window-active)
  (setq swbuff-y-jump-to-window (swbuff-y-in-other-frame-p swbuff-current-buffer))
  (swbuff-discard-status-window))
(defun swbuff-y-show-in-new-frame()
  (interactive)
  (swbuff-y-check-status-window-active)
  (setq swbuff-y-new-frame t)
  (swbuff-discard-status-window))
(defun swbuff-y-display-other-frame()
  (interactive)
  (swbuff-y-check-status-window-active)
  (swbuff-discard-status-window))




;;; Popup menu

(defun swbuff-y-pop-coord(point)
  (interactive)
  (if (boundp 'posn-at-point)
      (let* ((pn (posn-at-point point swbuff-status-window))
             (x-y (posn-x-y pn))
             (x (car x-y))
             (y (cdr x-y))
             (window swbuff-status-window)
             (pos (list (list x (+ y 20)) window))
             (ans)
             )
        pos)
    (list '(0 0) swbuff-status-window)))

(defun swbuff-y-show-popup ()
  "Show popup menu."
  (interactive)
  (swbuff-y-check-status-window-active)
  (let ((menulst nil)
        (menu nil)
        (window (if swbuff-status-window swbuff-status-window (selected-window)))
        (choice))
    (setq swbuff-y-jump-to-window (swbuff-y-in-other-frame-p swbuff-current-buffer))
    (when swbuff-y-jump-to-window
      (add-to-list 'menulst '("Display other &frame showing selected buffer" . this-other-frame)))
    (add-to-list 'menulst '("Show selected buffer in &current window" . this))
    (add-to-list 'menulst '("Show selected buffer in &new frame" . this-new-frame))
    (setq menu (list "Choose" (cons "Choose" menulst)))
    (setq choice (x-popup-menu (swbuff-y-pop-coord swbuff-y-current-marker) menu))
    (cond ((not choice)
           (swbuff-show-status-window))
          ((eq choice 'this)
           (swbuff-discard-status-window))
          ((eq choice 'this-new-frame)
           (swbuff-y-show-in-new-frame))
          ((eq choice 'this-other-frame)
           (swbuff-y-display-other-frame))
          (t
           (error "No match???")))))
;; (swbuff-y-show-popup)





(defun swbuff-initialize ()
  "Initialize swbuff variables prior to a switch sequence."
  (setq swbuff-buffer-list    (swbuff-buffer-list)
	swbuff-initial-buffer-list swbuff-buffer-list
	swbuff-initial-buffer (car swbuff-initial-buffer-list)
        swbuff-current-buffer swbuff-initial-buffer
	swbuff-initial-window (selected-window)
	swbuff-initial-frame  (selected-frame)))

(defun swbuff-kill-this-buffer ()
  "Kill the current buffer but retain the status window.

I bind

  (control right)  to `swbuff-switch-to-next-buffer',
  (control left)   to `swbuff-switch-to-previous-buffer' and
  (delete)         to `swbuff-kill-this-buffer'

which I find useful for cycling through and culling superfluous buffers."
  (interactive)

  (let ((dead-buffer (current-buffer)))
    (if (condition-case nil (kill-buffer dead-buffer))
	(progn
	  (if swbuff-initial-buffer
	      (setq swbuff-buffer-list
		    (delq dead-buffer swbuff-buffer-list)
		    swbuff-initial-buffer-list
		    (delq dead-buffer swbuff-initial-buffer-list))
	    (swbuff-initialize))
	  (if (car swbuff-buffer-list)
	      (progn (switch-to-buffer (car swbuff-buffer-list))
		     (swbuff-show-status-window))
	    (swbuff-discard-status-window)))
      (swbuff-discard-status-window))))

;; rename
(defvar swbuff-display-timer nil)

(defun swbuff-y-quit()
  (interactive)
  (setq swbuff-current-buffer swbuff-initial-buffer)
  (swbuff-discard-status-window)
  (keyboard-quit))

(defun swbuff-pre-command-hook ()
  "`pre-command-hook' used to track successive calls to switch commands."
  (when (eq (selected-frame) swbuff-initial-frame)
    ;;(message "pre this-command=%s, mmma=%s" this-command  minor-mode-map-alist)
    (remove-hook 'pre-command-hook 'swbuff-pre-command-hook)
    (if (timerp swbuff-display-timer)
	(cancel-timer swbuff-display-timer))
    (setq swbuff-display-timer nil)
    (condition-case err
        (let ((kmlst (cdr swbuff-y-keymap)))
          (unless (and (rassq this-command kmlst)
                       (not (eq this-command 'ignore)))
            ;; discard the command that ends the sequence
            (setq this-command 'ignore)
            (swbuff-discard-status-window)))
      (error (swbuff-discard-status-window)))))



(defun swbuff-discard-status-window ()
  "Discard the status window.
Called by both `sit-for' in `swbuff-show-status-window' and
`swbuff-post-command-hook'"

  ;;(message "discard")(sit-for 4)
  (if (boundp 'emulation-mode-map-alists)
      (progn
        ;;(message "Removing from emul")
        (setq emulation-mode-map-alists (delq 'swbuff-y-keymap-alist emulation-mode-map-alists)))
    (setq minor-mode-map-alist (delete swbuff-y-keymap-alist minor-mode-map-alist)))
  (let ((buffer (get-buffer swbuff-status-buffer-name))
	(buffer-list (nreverse swbuff-initial-buffer-list)))

    (if (window-live-p swbuff-status-window)
	(delete-window swbuff-status-window))

    (if buffer (kill-buffer buffer))

    (unwind-protect
	(when (and swbuff-initial-buffer swbuff-current-buffer)
	  (save-window-excursion

	    ;; Because this may be called from a timer we have to be real
	    ;; careful that we are in the right frame, window and buffer
	    ;; at that time --- other timers (eg those called by
	    ;; speedbar) may put us elsewhere:-)

	    (select-frame swbuff-initial-frame)
	    (select-window swbuff-initial-window)

	    ;; reset visit order to what it was before the sequence began
	    (while (setq buffer (car buffer-list))
	      (switch-to-buffer buffer)
	      (setq buffer-list (cdr buffer-list)))
	    )
          (cond (swbuff-y-jump-to-window
                 (select-frame-set-input-focus (window-frame swbuff-y-jump-to-window))
                 (select-window swbuff-y-jump-to-window)
                 (message "Swbuff-y: Switched to other frame already displaying selected buffer!")
                 )
                (swbuff-y-new-frame
                 (set-window-buffer (select-window (frame-first-window (new-frame)))
                                    swbuff-current-buffer)
                 (let ((bl (buffer-list))
                       (cb))
                   (while bl
                     (setq cb (car bl))
                     (unless (eq cb swbuff-current-buffer)
                       (bury-buffer cb))
                     (setq bl (cdr bl))))
                 (message "Swbuff-y: Created new frame to display selected buffer!")
                 )
                (t
                 ;; then switch between the first and last buffers in the sequence
                 (and swbuff-initial-buffer
                      (switch-to-buffer swbuff-initial-buffer))
                 (and swbuff-current-buffer
                      (switch-to-buffer swbuff-current-buffer))))
	  )
      ;; protect forms
      (setq swbuff-initial-buffer	 nil
	    swbuff-initial-buffer-list   nil
	    swbuff-current-buffer	 nil
	    swbuff-initial-frame	 nil
	    swbuff-initial-window	 nil
	    swbuff-status-window	 nil))
    ))


(defun swbuff-layout-status-line (window bcurr)
  "Layout a status line in WINDOW current buffer.
BCURR is the buffer name to highlight."
  (let* ((blist swbuff-initial-buffer-list)
	 (head  (or swbuff-header    "" ))
	 (separ (or swbuff-separator " "))
	 (trail (or swbuff-trailer   "" ))
	 (left  (or swbuff-left     "" ))
	 (right (or swbuff-right     "" ))
	 (width (window-width window))
	 (lines 0)
	 (adjust (or (eq swbuff-status-window-layout 'adjust)
		     (swbuff-one-window-p window)))
	 ;; okay, its crazy logic but it works:-)
	 (half-way (1- (/
			(if (= (% (length blist) 2) 0) ;; if even ...
			    (length blist)
			  (1+ (length blist))) ;; make it even
			2)))

	 start end buffer bname fillr)

    (when swbuff-start-with-current-centered
      ;; rearrange blist so that the first elt is in the middle
      (setq blist (append (last blist half-way)      ;; last half
			  (butlast blist half-way)))) ;; first half

    (save-selected-window
      (select-window window)

      (setq header-line-format nil) ;; Hide Emacs 21 header line.
      (if (featurep 'xemacs)
	  (setq modeline-format swbuff-modeline-format)
	(setq mode-line-format swbuff-mode-line-format))

      (erase-buffer)
      (setq start (point))

      (insert head)
      (if (> (point) start)
	  (set-text-properties
	   start (point) '(face swbuff-separator-face)))

      (while blist
	(setq buffer (car blist)
	      blist  (cdr blist))
	(when (buffer-live-p buffer)
	  (setq bname (buffer-name buffer)
		start (point)
		fillr (if blist separ trail))

	  ;; add a newline if we will run out of space
	  (when (and adjust
		     (> (- (+ start (length bname)
			      (length (concat left fillr right)))
			   (* lines width))
			width))
	    (newline)
	    (setq start (point)
		  lines (1+ lines)))

	  (insert left)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))

	  (setq start (point))
	  (insert bname)

	  ;; highlight it if it is the current one
	  (cond
	   (  (string-equal bname bcurr)
              (setq end (point))
              (setq swbuff-y-current-marker (copy-marker (point)))
              (unless (markerp swbuff-y-current-marker)
                (error "Not a marker"))
              (set-text-properties
               start end
               (if (swbuff-y-in-other-frame-p buffer)
                   '(face swbuff-y-current-buffer-other-face)
                 '(face swbuff-y-current-buffer-face)))
              )
           ;;start end '(face highlight)))
	   (  (string-match  swbuff-special-buffers-re bname)
              (set-text-properties
               start (point)
               (if (swbuff-y-in-other-frame-p buffer)
                   '(face swbuff-y-special-buffers-other-face)
                 '(face swbuff-y-special-buffers-face)))
              )
           (  (swbuff-y-in-other-frame-p buffer)
              (set-text-properties
               start (point) '(face swbuff-y-in-other-frame-face))
              )
	   (   t
               (set-text-properties
                start (point) '(face swbuff-default-face))))

	  (setq start (point))
	  (insert right)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))

	  (setq start (point))
	  (insert fillr)
	  (if (> (point) start)
	      (set-text-properties
	       start (point) '(face swbuff-separator-face)))))
      (if adjust
	  (swbuff-adjust-window)
	(swbuff-adjust-window 1)
	(swbuff-scroll-window end)))))


(defun swbuff-show-status-window ()
  "Pop-up a status window at the bottom of the selected window.
The status window shows the list of switchable buffers where the
switched one is hilighted using `swbuff-current-buffer-face'.  It
is automatically discarded after any command is executed or after
the delay specified by `swbuff-clear-delay'."

  (if (boundp 'emulation-mode-map-alists)
      (unless (memq 'swbuff-y-keymap-alist emulation-mode-map-alists)
        (setq  emulation-mode-map-alists (cons 'swbuff-y-keymap-alist emulation-mode-map-alists)))
    (unless (memq 'swbuff-y-keymap minor-mode-map-alist)
      (setq  minor-mode-map-alist (cons swbuff-y-keymap-alist minor-mode-map-alist))))
  (setq swbuff-y-jump-to-window nil)
  (setq swbuff-y-new-frame nil)
  (if swbuff-initial-buffer-list
      (let ((buffer-name (buffer-name swbuff-current-buffer))
	    (window-min-height 1)
	    (cursor-in-non-selected-windows nil))
	(with-current-buffer (get-buffer-create swbuff-status-buffer-name)
	  (let ((window (or (get-buffer-window swbuff-status-buffer-name)
			    (split-window-vertically -2))))

            ;; if we forget this we may end up with multiple status
            ;; windows (kal)
            (setq swbuff-status-window window)

	    (set-window-buffer window (current-buffer))
	    (swbuff-layout-status-line window buffer-name)
	    (add-hook 'pre-command-hook 'swbuff-pre-command-hook)

	    ;; use a timer that we can cancel rather than sit-for
            (if t
                (progn
                  (if (timerp swbuff-display-timer)
                      (cancel-timer swbuff-display-timer))
                  (setq swbuff-display-timer
                        (run-with-timer swbuff-clear-delay nil
                                        'swbuff-discard-status-window))
                  )
;;               (message "setting up with-timeout")
;;               (with-timeout (swbuff-clear-delay (swbuff-discard-status-window))
;;                 (let* ((key (read-key-sequence nil))
;;                        (cmd (key-binding key t)))
;;                   (message "key=%s, cmd=%s, sts-win=%s" key cmd swbuff-status-window)
;;                   (if (not swbuff-status-window)
;;                       (message "no status window")
;;                     (cond
;;                      (  (member cmd '(swbuff-switch-to-next-buffer
;;                                       swbuff-switch-to-previous-buffer))
;;                         (run-with-timer 0.02 nil cmd)
;;                         )
;;                      (  (member cmd '(swbuff-y-show-popup))
;;                         (funcall cmd))
;;                      (  (eq cmd 'keyboard-quit)
;;                         (setq swbuff-current-buffer nil)
;;                         (swbuff-discard-status-window)
;;                         )
;;                      (  t
;;                         (error "in t")
;;                         (cond ((equal key "\r")
;;                                (setq swbuff-y-jump-to-window (swbuff-y-in-other-frame-p swbuff-current-buffer)))
;;                               ((equal key "n")
;;                                (setq swbuff-y-new-frame t))
;;                               )
;;                         (swbuff-discard-status-window))))))
              ))))
    (swbuff-discard-status-window)
    (message "No buffers eligible for switching.")
    ))


(defun swbuff-exclude-mode-p (buffer)
  "Return non-nil if BUFFER should be excluded by major mode.
Return non-nil iff the major mode of BUFFER matches
`swbuff-exclude-mode-regexps'."
  (unless (string-equal "" swbuff-exclude-mode-regexp)
    (save-excursion
      (set-buffer buffer)
      (string-match swbuff-exclude-mode-regexp
		    (symbol-name major-mode)))))

(defun swbuff-exclude-p (name)
  "Return non-nil if buffer NAME matched exclude pattern.
Return non-nil if buffer NAME matches one of the
`swbuff-exclude-buffer-regexps'."
  (let ((rl (cons (regexp-quote swbuff-status-buffer-name)
		  (delete "" swbuff-exclude-buffer-regexps))))
    (while (and rl (car rl) (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (not (null rl))))

(defun swbuff-include-p (name)
  "Return non-nil if buffer NAME match include pattern.
Return non-nil if buffer NAME matches one of the
`swbuff-include-buffer-regexps'."
  (let ((rl (delete "" swbuff-include-buffer-regexps)))
    (while (and rl (car rl) (not (string-match (car rl) name)))
      (setq rl (cdr rl)))
    (not (null rl))))

(defvar swbuff-buffer-list nil
  "Stores the current list of switchable buffers.
This way we only have to call `swbuff-buffer-list' once.")

(defun swbuff-buffer-list ()
  "Return the list of switchable buffers.
That is without the ones whose name matches
`swbuff-exclude-buffer-regexps'.  If `swbuff-this-frame-only' is
non-nil, buffer that are currently displayed in other visble or
iconified frames are also excluded."
  (let ((blist
	 (delq nil (mapcar
		    (lambda (buf)
		      (and (or (swbuff-include-p (buffer-name buf))
			       (not
				(or
				 (swbuff-exclude-mode-p buf)
				 (swbuff-exclude-p (buffer-name buf)))))
			   (if swbuff-this-frame-only
			       (not (swbuff-y-in-other-frame-p buf))
			     t)
			   buf))
		    (buffer-list)))))
    (when blist
      ;; add the current buffer if it would normally be skipped
      (unless (memq (current-buffer) blist)
	(setq blist (cons (current-buffer) blist))))
    blist))

(defun swbuff-next-buffer ()
  "Display and activate the next buffer in the buffer list."
  (let ((buf (car swbuff-buffer-list)))
    (when buf
      (setq swbuff-buffer-list (cdr swbuff-buffer-list))
      (setq swbuff-buffer-list (append swbuff-buffer-list (list buf)))
      (setq swbuff-current-buffer (car swbuff-buffer-list))
      (when swbuff-display-intermediate-buffers
	(switch-to-buffer (car swbuff-buffer-list) t)) ;; no record
      )))

(defun swbuff-previous-buffer ()
  "Display and activate the buffer at the end of the buffer list."
  (let ((buf (car (last swbuff-buffer-list))))
    (when buf
      (when swbuff-display-intermediate-buffers
	(switch-to-buffer buf t))
      (setq swbuff-current-buffer buf)
      (setq swbuff-buffer-list (butlast swbuff-buffer-list))
      (setq swbuff-buffer-list (cons buf swbuff-buffer-list)))))

(defun swbuff-switch-to-previous-buffer ()
  "Switch to the previous buffer in the buffer list.
Bound to key \\[swbuff-switch-to-previous-buffer]."
  (interactive)
  (run-hooks 'swbuff-pre-switch-hook)
  (if swbuff-initial-buffer
      (and swbuff-delay-switch (swbuff-previous-buffer))
    (swbuff-initialize))
  (or swbuff-delay-switch (swbuff-previous-buffer))
  (swbuff-show-status-window))

(defun swbuff-switch-to-next-buffer ()
  "Switch to the next buffer in the buffer list.
Bound to key \\[swbuff-switch-to-next-buffer]."
  (interactive)
  (run-hooks 'swbuff-pre-switch-hook)

  (if swbuff-initial-buffer
      (swbuff-next-buffer)
    ;; first call in the sequence
    (swbuff-initialize)
    (unless swbuff-delay-switch
      (swbuff-next-buffer)))

  (swbuff-show-status-window))


;;; Try to be cooperative:
(when (eq 'swbuff-switch-to-next-buffer (lookup-key global-map [(control f6)]))
  (global-set-key [(control f6)] nil))
(when (eq 'swbuff-switch-to-previous-buffer (lookup-key global-map [(control shift f6)]))
  (global-set-key [(control shift f6)] nil))

(define-minor-mode swbuff-y-mode
  "Turns on a keyboard keys for buffer switching.
By default this is bound to C-tab and C-S-Tab."
  nil ; off by default
  nil ; no lighter
  '(([(control tab)]       . swbuff-switch-to-next-buffer)
    ([(control shift tab)] . swbuff-switch-to-previous-buffer))
  :global t
  :group 'swbuff)

;; if the user loads this file it is to use it, so enable the minor mode:
;;(swbuff-y-mode t)

(provide 'swbuff-y)

;;; swbuff-y.el ends here

