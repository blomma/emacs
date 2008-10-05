;; mysql-mode.el - MySQL interface
;;
;;
;; Modified for MySQL by: David Hollman. This code is a conversion of
;;                        SQL*Plus mode for the MySQL client.
;; Original Author      : Jim Lange, Oracle Corporation (jlange@us.oracle.com)
;; Bugs to              : dah8@cornell.edu
;;
;; 
;; Revision History:
;;   19-APR-90 (jlange) - Trap EXIT/QUIT and terminate session.
;;                      - Trap EDIT, but just print message.
;;                      - Allow multiple sqlplus sessions by renaming current
;;                        buffer to new name and executing sqlplus again.
;;                      - Set left-margin to 5 in sqlplus-mode so that
;;                        newline-and-indent (C-j) will indent.
;;                      - Added (accept-process-output process) in 
;;                        sqlplus-kill-command to prevent occasional scrambled
;;                        lines when command is executed.
;;                      - Added sqlplus-reset-buffer.
;;   25-APR-90 (jlange) - Treat GET like LIST and RUN--ignore in 
;;                        sqlplus-get-command.
;;                      - Add sqlplus-drop-old-lines.
;;   04-MAY-90 (jlange) - Add sqlplus-copy-word (C-c C-w).
;;                      - Enhance sqlplus-kill-command to delete command or
;;                        command output, depending on location of point.
;;   11-MAY-90 (jlange) - In sql-send-region, detect imbedded SQL statement 
;;                        format (used in SQL*FORMS, SQL*REPORT, PRO*C, etc.) 
;;                        and convert to standard SQL before executing (remove
;;                        INTO clause and convert :WORD to &WORD).
;;                      - Automatically load and save session history based
;;                        on the variable sqlplus-keep-history.
;;   05-JUN-90 (jlange) - Delete ~/sqlplus.buf when exiting.
;;                      - In sqlplus-send-region, when performing substitutions
;;                        in statements, add "/" at end if not present.
;;   12-JUN-90 (jlange) - In sqlplus-send-region, look for :text.text and convert
;;                        to &text_text (used in SQL*Forms for block.field).
;;   07-SEP-90 (jlange) - Removed process argument from accept-process-output in
;;                        sql-send-line to prevent apparent lockup after issuing
;;                        complex SQL statement.
;;                      - Trap EDIT command and open new emacs buffer containing
;;                        command text.  (new revision 1.2)
;;   08-MAY-91 (jlange) - In sql-send-region, truncate &-variables to 30 characters
;;                        before executing.
;;
;;   12-AUG-2001 (Hollman) - Modifications for MySQL client.
;;
;;---------------------------------------------------------------------------------

;; Copyright (C) 1990 Free Software Foundation, Inc., and Jim Lange.
;;
;; This file is derived from Emacs 18.55's shell.el.
;;
;; MySQL-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless s/he says so in writing.  Refer to the GNU General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; MySQL-mode, but only under the conditions described in the
;; GNU General Public License.   A copy of this license is
;; supposed to have been given to you along with MySQL-mode so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;----------------------------------------------------------------------

;; This file contains function definitions for two new Emacs major modes,
;; sql-mode and mysql-mode.  Sql-mode is for editing SQL statements in
;; a standard text buffer.  MySQL-mode is used in conjunction with the 
;; mysql function which runs the MySQL client as an inferior process (similar to 
;; shell-mode).
;;
;; Sql-mode is provided primarily as a convenience so that SQL statements 
;; may be sent to MySQL running in another buffer.
;;
;; Both modes turn on abbrev-mode and share a mode-specific abbreviation table
;; with some predefined abbreviations.  Users may add to these or load in their
;; own.  Abbrev-mode may be turned off in a user defined hook routine.
;;
;; Place this file in your Emacs lisp path (eg. site-lisp)
;; and add to your .emacs file:
;; (load-library "mysql-mode")
;;
;; Enhancements/Bugs:
;;   - Specify default username, password, database in .emacs file
;;   - Specify additional MySQL client command line options in .emacs
;;   - Translate ^G that shows up in the output to an actual bell, or to nothing
;;   - Trap '\e' as well as 'edit'
;;   - Trap '\q' as well as 'exit' or 'quit'
;;   - Assess (and maybe remove) the colon/ampersand processing stuff for MySQL
;;
;;   - Suggestions to dah8@cornell.edu
;;

(provide 'sql-mode)

;; Define variables
(defvar mysql-startup-message
  (concat 
   "Emacs MySQL Interpreter\n"
   "-----------------------\n"
   "Maintained by David Hollman (dah8@cornell.edu)\n"
   "Derived from SQL*plus mode by Jim Lange (jlange@us.oracle.com)\n\n"
   "Version: 0.1"
   "\n\n"
   "--------------------------------------------------------------\n")
  "Message displayed when \\[mysql] is executed.")

(defvar last-output-start nil
  "In a mysql-mode buffer, marker for beginning of last batch of output.")
(defvar sql-prompt nil
  "In a mysql-mode buffer, string containing prompt text.")
(defvar sql-continue-pattern nil
  "In a mysql-mode buffer, regular expression for continuation line prompt.")

;; Modifications DH for MySQL username, password, database
(defvar mysql-username ""
  "The username to use when starting MySQL.")
(defvar mysql-password ""
  "The password to use when starting MySQL.")
(defvar mysql-database ""
  "The database to use when starting MySQL.")


(defvar mysql-stack-pointer 0
  "Current command recalled from history of commands.")
(defvar mysql-keep-history nil
  "If non-nil, save current session in file .sqlhist when exiting.")
(defvar mysql-lines-to-keep 1000
  "Number of lines to keep in a MySQL buffer when \\[mysql-drop-old-lines] is executed.")
(defvar mysql-mode-map nil)
(defvar sql-mode-map nil)
(defvar sql-mode-syntax-table nil
  "Syntax table used while in SQL and MySQL modes.")
(defvar sql-mode-abbrev-table nil
  "Abbrev table used in SQL and MySQL modes.")

;; initialize syntax table
(if sql-mode-syntax-table
    ()
  (setq sql-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14" sql-mode-syntax-table)   ; comment start
  (modify-syntax-entry ?* ". 23" sql-mode-syntax-table)
  (modify-syntax-entry ?+ "." sql-mode-syntax-table)
  (modify-syntax-entry ?- "." sql-mode-syntax-table)
  (modify-syntax-entry ?= "." sql-mode-syntax-table)
  (modify-syntax-entry ?% "w" sql-mode-syntax-table)
  (modify-syntax-entry ?< "." sql-mode-syntax-table)
  (modify-syntax-entry ?> "." sql-mode-syntax-table)
  (modify-syntax-entry ?& "w" sql-mode-syntax-table)
  (modify-syntax-entry ?| "." sql-mode-syntax-table)
  (modify-syntax-entry ?_ "w" sql-mode-syntax-table)     ; make _ part of words
  (modify-syntax-entry ?\' "\"" sql-mode-syntax-table))

;; initialize abbreviations
(if sql-mode-abbrev-table
    nil
  (define-abbrev-table 'sql-mode-abbrev-table ())
  (let ((abbrevs-changed nil))
    (define-abbrev sql-mode-abbrev-table  "d"   "DESCRIBE" nil)
    (define-abbrev sql-mode-abbrev-table  "s"   "SELECT"   nil)
    (define-abbrev sql-mode-abbrev-table  "f"   "FROM"     nil)
    (define-abbrev sql-mode-abbrev-table  "w"   "WHERE"    nil)
    (define-abbrev sql-mode-abbrev-table  "o"   "ORDER BY" nil)
    (define-abbrev sql-mode-abbrev-table  "l"   "LIKE"     nil)
    (define-abbrev sql-mode-abbrev-table  "i"   "INSERT"   nil)
    (define-abbrev sql-mode-abbrev-table  "g"   "GROUP BY" nil)
    (define-abbrev sql-mode-abbrev-table  "h"   "HAVING"   nil)
    (define-abbrev sql-mode-abbrev-table  "a"   "AND"      nil)
    (define-abbrev sql-mode-abbrev-table  "n"   "NOT"      nil)
    (define-abbrev sql-mode-abbrev-table  "v"   "VALUES"   nil)
    (define-abbrev sql-mode-abbrev-table  "u"   "UPDATE"   nil)
    (define-abbrev sql-mode-abbrev-table  "t"   "TRUNCATE" nil)
    (define-abbrev sql-mode-abbrev-table  "q"   "Quit"     nil)
  )
)

;;-----------------------------

(defun sql-mode ()
  "Major mode for editing MySQL batch files.
\\{sql-mode-map}
sql-send-buffer and sql-send-region are commands that will send MySQL
commands defined in the current buffer to MySQL to be executed.  Output
is displayed in the *MySQL* buffer (which will open as separate window
if it does not already exist).  (use '\\[describe-mode]' while in *MySQL*
buffer for information on mysql-mode.)

Entry to this mode calls the value of mysql-mode-hook with no args,
if that value is non-nil.  Abbrev-mode is also enabled with the following
abbreviations available by default:

        s  ->  SELECT
        f  ->  FROM
        w  ->  WHERE
        o  ->  ORDER BY

Use \\[list-abbrevs] for a full list.

If the SQL statements to be executed contain variables prefixed with colons
or INTO clauses, the colons are converted into ampersands and the INTO clauses
are removed before being sent to MySQL.  This provides compatibility with
Pro*C, SQL*Report, and SQL*Forms (.inp files).  For example,

     SELECT SYSDATE + :days_added INTO :variable FROM SYSTEM.DUAL

is converted to

     SELECT SYSDATE + &days_added FROM SYSTEM.DUAL

and the user is prompted to enter the value of days_added."

  (interactive)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (setq require-final-newline t)
  (run-hooks 'sql-mode-hook)
)

(if sql-mode-map
    nil
  (setq sql-mode-map (make-sparse-keymap))
  (define-key sql-mode-map "\C-c\C-x" 'sql-send-buffer)
  (define-key sql-mode-map "\C-c\C-r" 'sql-send-region)
)

(defun mysql-mode ()
  "Major mode for interacting with Oracle MySQL.
Return at end of buffer sends line as input.
Return not at end copies SQL statement to end and executes it.  
\\{mysql-mode-map}
This mode is normally invoked by 'M-x mysql' (not 'M-x mysql-mode').
You will be prompted to enter a username/password combination
to access the Oracle database.  This can be prevented by setting the 
variable mysql-username-password in your .emacs file as follows:

     (setq mysql-username-password \"myname/mypassword\")

There are two ways of editing and re-executing prior commands.
'\\[mysql-back-command]' and '\\[mysql-forward-command]' will move to the location 
in the buffer of the previous or next SQL statement, respectively
(based on the command prompt).  The command can then be edited
normally and re-executed by pressing Return.  To insert a newline,
you may press '\\[newline-and-indent]'.  '\\[mysql-next-command]' and '\\[mysql-previous-command]' 
are similar except the next or previous SQL statement is inserted at
the end of the buffer.  Repeating these commands will clear the
current statement and recall the next or previous statement from the
stack.  For additional information on command execution use 
'\\[describe-key] RTN'.

'\\[show-mysql-output]' will move to the beginning of the last ouput
generated by MySQL.  This is useful for reviewing the results of 
the last statement.  '\\[mysql-end-of-buffer]' moves to the end of the buffer, 
but unlike '\\[end-of-buffer]' (end-of-buffer), it does not set the mark.

'\\[mysql-kill-command]' deletes either the current command being entered or
the last output generated by MySQL, depending on when it is used.  
If executed while the cursor is within a SQL statement, the statement and 
any text after it are deleted.  If the cursor is within or at the end of
output generated by MySQL, the output is deleted and the cursor is 
positioned at the end of the SQL statement that generated the ouput.  
'\\[mysql-kill-command]' can be used like an undo command to alternately
delete commands and output from the end of the buffer.

The commands sql-send-region and sql-send-buffer can be executed from
another buffer to execute the SQL statements defined by the current
region or entire buffer, respectively.  Output from these commands is
displayed in the *MySQL* buffer.  The major mode called sql-mode has
these functions bound to key sequences.

Entry to this mode calls the value of mysql-mode-hook with no args,
if that value is non-nil.  Abbrev-mode is also enabled with the following
abbreviations available by default:

        s  ->  Select
        f  ->  From
        w  ->  Where
        o  ->  Order By

Use '\\[list-abbrevs]' for a full list.  

If the variable mysql-keep-history is non-nil, the current session is
saved in the file ~/.sqlhist and recalled automatically the next time
mysql is executed.  The session will only be saved if the EXIT or QUIT
command is used to terminate the MySQL session.  The maximum number of 
lines saved can be set with the variable mysql-lines-to-keep which defaults
to 1000.  The command '\\[mysql-drop-old-lines]' will truncate the buffer 
to this length at any time.  mysql-keep-history and mysql-lines-to-keep
should be set in your .emacs file.

If the *MySQL* buffer is killed with '\\[kill-buffer]', the MySQL
process will automatically be terminated, but the session will not be saved,
even if mysql-keep-history is non-nil.

'\\[mysql-reset-buffer]' will delete all ouput lines from the buffer, leaving
only commands.  This will significantly shrink the buffer, but retain a full 
history of commands for re-execution."

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mysql-mode)
  (setq mode-name "MySQL")
  (setq mode-line-process '(": %s"))
  (use-local-map mysql-mode-map)
  (set-syntax-table sql-mode-syntax-table)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (make-local-variable 'last-output-start)
  (setq last-output-start (make-marker))
  (make-local-variable 'sql-prompt)
  (setq sql-prompt "^\\(mysql> \\)+")     ;* allows "SQL> SQL> ..."
  (make-local-variable 'sql-continue-pattern)
  (setq sql-continue-pattern "^[ 0-9][ 0-9][0-9][* \t][ \t]\\|     ")
  (setq indent-tabs-mode nil)
  (setq left-margin 5)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (run-hooks 'mysql-mode-hook)
)

(if mysql-mode-map
    nil
  (setq mysql-mode-map (make-sparse-keymap))
  (define-key mysql-mode-map "\C-m"     'mysql-execute-command)      
  (define-key mysql-mode-map "\t"       'indent-relative)                
  (define-key mysql-mode-map "\C-c\C-c" 'interrupt-mysql-subjob) 
  (define-key mysql-mode-map "\C-c\C-r" 'show-mysql-output)      
  (define-key mysql-mode-map "\C-c\C-p" 'mysql-previous-command) 
  (define-key mysql-mode-map "\C-c\C-n" 'mysql-next-command)     
  (define-key mysql-mode-map "\C-up"    'mysql-previous-command) 
  (define-key mysql-mode-map "\C-down"  'mysql-next-command)     
  (define-key mysql-mode-map "\C-c\C-e" 'mysql-end-of-buffer)    
  (define-key mysql-mode-map "\C-c\C-b" 'mysql-back-command)     
  (define-key mysql-mode-map "\C-c\C-f" 'mysql-forward-command)  
  (define-key mysql-mode-map "\C-c\C-k" 'mysql-kill-command)     
  (define-key mysql-mode-map "\C-c\C-x" 'mysql-reset-buffer)     
  (define-key mysql-mode-map "\C-c\C-d" 'mysql-drop-old-lines)   
  (define-key mysql-mode-map "\C-c\C-w" 'mysql-copy-word)        
  (define-key mysql-mode-map "\C-c\C-s" 'mysql-save-session)     
)

;;-----------------------------

(defun mysql ()
  "Start up an interactive MySQL session in a new buffer.
If an active MySQL process already exists, will switch to that
buffer."
  (interactive)
  (let ((process (mysql-start)))
    (switch-to-buffer "*MySQL*")
    (if (and mysql-keep-history
	     (file-readable-p (expand-file-name "~/.sqlhist")))
	(progn
	  (sit-for 1)
	  (while (accept-process-output) (sleep-for 1)) 
	  (insert-file-contents (expand-file-name "~/.sqlhist") nil)
	  (goto-char (point-max))
	  (set-marker (process-mark process) (point))
	  (message ".sqlhist loaded")
        )
    );endif
  )
)

(defun mysql-start ()
  "Start up an interactive MySQL session in a new buffer."
  (let ((mysql-buffer (get-buffer-create "*MySQL*")) process)
    (setq process			; set process
	  (or				; to the first that is non-nil:
	   (get-buffer-process mysql-buffer)    ; current process
	   (progn			          ; or new process
	     (set-buffer mysql-buffer)
	     (insert mysql-startup-message)
	     (start-process "MySQL"
			    mysql-buffer
			    "mysql"
			    (concat "--user=" (read-string "Username: "))
			    (concat "--password=" (read-string "Password: "))
			    (read-string "Database: ")
			    )
	     )
	   )
	  )
    (set-buffer mysql-buffer)
    (goto-char (point-max))
    (set-marker (process-mark process) (point))
    (mysql-mode)
    process             ; return process
    )
  )

(defun mysql-execute-command ()
"When executed at end of buffer, sends text entered since last 
output from MySQL.  When executed while positioned within another
valid command in the buffer, copies command to end of buffer and 
re-executes it.  If point is within a multi-line statement at the end
of the buffer (such as after '\\[mysql-previous-command]'), the entire
statement will be cleared and re-entered one line at a time.

Multi-line statements are recognized by the continuation prompt displayed
by MySQL.  This is controlled by the variable mysql-continue-pattern
which defaults to recognize either a right-justified number padded to four 
characters followed by a space or asterisk, or simply five spaces.  A line
ending with \";\" or \" /\" is also considered the end of a statement.
A new line inserted into a prior statement must be indented at least five
spaces to be included when the statement is re-executed. 

The output from a List command is also recognized as a valid MySQL
statement; the 'List' command itself is stripped out (as are 'Get' and 'Run').

When a complex SQL statement is executed, it may take a long time before
the output is generated.  Emacs may appear to be hung since no keystrokes
are accepted until the first character of output arrives.  In this situation
'\\[keyboard-quit]' may be used to force emacs to stop waiting for output.
You may then switch to another buffer to perform other work while you wait
or press '\\[interrupt-mysql-subjob]' to cancel the current SQL command."
  (interactive)
  (let ((process (get-buffer-process (current-buffer))))
    (if (not process) 
	(error "Current buffer has no process.  Use 'M-x mysql' to start MySQL process.")
    )

    (cond
; last line of buffer and only one input line
     ((and (save-excursion (end-of-line) (eobp)) 
	   (<= (count-lines (process-mark process) (point)) 1))
	(end-of-line)
        (mysql-send-line process)
     )

; within last multi-line command of buffer 
     ((not (save-excursion (re-search-forward sql-prompt (point-max) t)))
	(let ((command-lines (mysql-get-command)))
	  (mysql-kill-command t)        ; clear existing command lines
	  (while command-lines            ; send command lines
	    (insert (car command-lines))
	    (mysql-send-line process)
	    (setq command-lines (cdr command-lines))
	  )
	)
      )
; otherwise - prior command in buffer
     (t                                 
	(or (save-excursion
	      (beginning-of-line)
	      (looking-at (concat sql-prompt "\\|" sql-continue-pattern)))
	    (error "This is not a valid MySQL command."))
	(let ((command-lines (mysql-get-command)))
	     (goto-char (point-max))
	     (mysql-kill-command t)     ; clear pending command (if any)
	     (while command-lines
		(insert (car command-lines))
		(mysql-send-line process)
		(setq command-lines (cdr command-lines))
	     )
	)
    )
  ) ;end cond
 ) ;end let
 (setq mysql-stack-pointer 0)
)					; end defun

(defun mysql-send-line (process)      ; called from mysql-execute-command
  (insert ?\n)
  (let ((command (buffer-substring (process-mark process) (point)))
	(temp-file (expand-file-name "~/mysql.buf")))
    (move-marker last-output-start (point))
; trap EDIT command - must be the only word on the line
    (if (string-match "^ *edit\\s-*\\(\\w*\\)[ ;]*$" command) 
      (let (command-lines 
	    (edit-file-name (save-excursion (and 
					     (re-search-backward "edit\\s-+\\([^ \t\n;]+\\)" 
								 (process-mark process) t)
					     (buffer-substring (match-beginning 1) (match-end 1))
					    )))

	   )
	(sit-for 0)
	(set-marker (process-mark process) (point))
	(process-send-string process "LIST\n")
	(accept-process-output process)	; wait for process to respond
	(sleep-for 1)
	(forward-line -1)
	(setq command-lines (mysql-get-command)) ; capture command
	(delete-region last-output-start (point))  ; delete listed lines
	(goto-char (point-max))
	(switch-to-buffer-other-window (get-buffer-create (or edit-file-name "*mysql-temp*")))
	(if edit-file-name 
	    (setq buffer-offer-save t)
	)
	(delete-region (point-min) (point-max))    ; clear buffer
	(while command-lines		           ;insert command lines
	  (insert (car command-lines) "\n")
	  (setq command-lines (cdr command-lines))
	)
	(insert ";\n")
	(goto-char (point-min))
	(sql-mode)			;turn on sql-mode
      )
;   else
; execute command line
      (process-send-string process command)
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (sit-for 0)			; force display update
      (accept-process-output)		; wait for process to respond
    )
; trap QUIT command
    (if (string-match "^ *\\(exit\\|quit\\)[ ;]*$" command)
	(progn
	  (if mysql-keep-history
	      (let ((lines-to-keep (or mysql-lines-to-keep 1000)))
		(and (> (count-lines (point-min) (point-max)) lines-to-keep)
		     (y-or-n-p 
		      (format "Current session is longer than %d lines.  Ok to truncate? " lines-to-keep))
		     (mysql-drop-old-lines lines-to-keep)
		)
		(mysql-save-session "~/.sqlhist")
	      )
	  )
	  (while (get-buffer-process (current-buffer)) 
	    (sit-for 1))		 ; wait for process to die
	  (kill-buffer (current-buffer))
	  (and (file-exists-p temp-file) ; if mysql.buf exists, delete it
	       (delete-file temp-file))
	);end progn
    );end if
  );end let
)

(defun mysql-kill-command (command-only-flag)
  "Delete the current SQL command or output generated by last SQL command.
When used at the end of the buffer, serves as an undo command.

If point is within a valid SQL statement, delete region from SQL> prompt 
before point to end of buffer, otherwise delete all text between the end 
of the prior SQL statement and the end of the buffer."
  (interactive "P")
  (let ((process (get-buffer-process (current-buffer))))
    (if (or command-only-flag
	    (save-excursion
	      (beginning-of-line)
	      (looking-at (concat sql-prompt ".+\\|" sql-continue-pattern))
	    )
	 )
    ;then - delete command and everything beyond
	(progn
	  (delete-region (progn 
			   (re-search-backward sql-prompt (point-min) t) 
			   (point))
			 (point-max))
	  (process-send-string process "\n")	; generate new SQL> prompt
	  (goto-char (point-max))
	  (set-marker (process-mark process) (point))
	  (sit-for 0)				; update display
	  (accept-process-output process)	; wait for prompt
	)
    ;else - delete output from prior command, leaving cursor at end of command
      (beginning-of-line)
      (or (re-search-backward sql-prompt (point-min) t)
	  (error "Nothing to kill"))
      (set-marker (process-mark process) (match-end 0))
      (mysql-get-command)		; convenient way to find end of command
      (forward-char -1)			; back up one character
      (delete-region (point) (point-max))
    );end if
  )
)

(defun mysql-get-command ()
  (interactive)
  (let ((line "") command-lines)
    (end-of-line)
    (or (re-search-backward sql-prompt (point-min) t)
	(error "Unable to execute this command"))
    (goto-char (match-end 0))       ; skip past prompt
    (setq command-lines             ; initialize command-lines list
        (if (looking-at "l$\\|list$\\|r$\\|run$\\|get .*\\|edit") ;ignore LIST,RUN,GET,EDIT
	    nil
	  (list (setq line 
  	            (buffer-substring (point) (progn (end-of-line) (point)))))
	)
    )
    (forward-line)
    (while (and                                      ; while previous line 
	    (not (string-match "^\\(.*;$\\| */\\)$" line)) ; does not end in / or ;
	    (looking-at sql-continue-pattern))       ; and this is a cont. line
         (goto-char (match-end 0))                   ; skip over prompt
	 (setq line (buffer-substring (point) (progn (end-of-line) (point))))
	 (setq command-lines (append command-lines (list line)))
	 (forward-line)
    )
    command-lines          ; return command-lines as value of function
))

(defun interrupt-mysql-subjob ()
  "Interrupt this shell's current subjob."
  (interactive)
  (interrupt-process nil t))

(defun show-mysql-output ()
  "Display most recent batch of output at top of window.
Also put cursor there."
  (interactive)
  (goto-char last-output-start)
)

(defun sql-send-buffer (prefix-arg)
  "Execute all MySQL commands defined in current buffer.
Output is displayed in the *MySQL* buffer."
  (interactive "P")
  (sql-send-region (point-min) (point-max) prefix-arg)
)

(defun sql-send-region (start end prefix-arg)
  "Execute all MySQL commands defined between point and mark.
Output is displayed in the *sqlpus* buffer."
  (interactive "r\nP")
  (let (process this-buffer temp-file-name imbedded-variables (temp-buffer nil))
   (setq this-buffer (current-buffer))
   (or (setq process (get-buffer-process "*MySQL*")) ; look for process
       (setq process (mysql-start)) ; or create process
       (error "Unable to create MySQL session.")) 
;    (setq temp-file-name (format "/tmp/sqlbuf.%d" (process-id process)))
    (setq temp-file-name (expand-file-name "~/mysql.buf"))
    (set-buffer this-buffer) 
    (if (and (null prefix-arg)		; if no prefix argument
	     (save-excursion		; look for 'INTO :' or ':variable'
	       (goto-char start)
	       (re-search-forward "\\binto\\s-+:\\|\\s-:\\w+" end t)))
	(progn
	  (setq temp-buffer (get-buffer-create "*sql-temp*"))
	  (set-buffer temp-buffer)
	  (set-syntax-table sql-mode-syntax-table) ; important for regular expressions
	  (erase-buffer)
	  (insert-buffer-substring this-buffer start end) ; insert region
	  (skip-chars-backward "\n\t ")	; trim trailing whitespace
	  (if (save-excursion
		(forward-char -1)	; back up one
		(not (looking-at ";\\|/")) ; last character is not ; or /
	      )
	    (insert "\n/\n")		; add "/" so statement is executed
	  )
	  (goto-char (point-min))	; delete INTO clause
	  (if (re-search-forward "\\binto\\s-+:" (point-max) t)
		(delete-region (match-beginning 0) ; delete between INTO & FROM
			       (progn
				 (re-search-forward "\\bfrom\\b" (point-max) t)
				 (match-beginning 0)
			       )
		)
	  ) ;endif
	  (goto-char (point-min))	; convert all ":block.field" to "&block_field"
	  (replace-regexp ":\\(\\w+\\)\\." "&\\1_" nil)
	  (goto-char (point-min))	; convert all remaining ":" to "&"
	  (replace-string ":" "&" nil)
	  (goto-char (point-min))
	  (while (re-search-forward "&\\w+" (point-max) t)
	    (let ( (wbeg (match-beginning 0)) (wend (match-end 0)) )
		 (if (> (- wend wbeg) 30)	; if word > 30 characters
		     (delete-region (+ wbeg 1) (- wend 30)) ; truncate to 30
		 )
	    )
	  )
	  (setq start (point-min))	; reset start & end for write-region
	  (setq end (point-max))
	) ;end progn
    ) ;endif 
    (setq imbedded-variables (save-excursion     ; look for &, accept, acc
			       (goto-char start)
			       (re-search-forward "&\\|\\bacc\\(ept\\)?\\b" end t)))
    (write-region start end temp-file-name nil 0) ; write temp file
    (switch-to-buffer-other-window "*MySQL*")
    (goto-char (point-max))
    (recenter 0)
    (insert (format "\nOutput from buffer '%s':\n" 
		    (buffer-name this-buffer)))
    (set-marker (process-mark process) (point))
    (sit-for 0)				; update display
    (process-send-string process (concat "\\. " temp-file-name "\n"))
    (if temp-buffer (kill-buffer temp-buffer))
    (if imbedded-variables		; stay in *MySQL* buffer to
	(goto-char (point-max))		; allow entry of variables
      (switch-to-buffer-other-window this-buffer)
    )
  )
)

(defun mysql-back-command (arg)
  "Move to the MySQL command before current position.
With prefix argument, move to ARG'th previous command."
  (interactive "p")
  (if (save-excursion 
	(beginning-of-line)
	(re-search-backward sql-prompt (point-min) t arg)
      )
      (goto-char (match-end 0))
    (error "No previous MySQL command.")
  )
)
  
(defun mysql-forward-command (arg)
  "Move to the MySQL command after current position.
With prefix argument, move to ARG'th previous command."
  (interactive "p")
  (if (re-search-forward sql-prompt (point-max) t arg)
      nil
    (error "No next MySQL command.")
  )
)

(defun mysql-previous-command (arg)
  "Recall the previous MySQL command from the command stack.
With prefix argument, recall the command ARG commands before the current
stack pointer."
  (interactive "p")
; - clear current pending command
  (goto-char (process-mark (get-buffer-process (current-buffer))))
  (delete-region (point) (point-max))

; - increment stack pointer by arg
  (setq mysql-stack-pointer (+ mysql-stack-pointer arg))
  (if (< mysql-stack-pointer 0)
      (progn (setq mysql-stack-pointer 0)
	     (error "At last command."))
  )
  ;if there is a prior command    
  (if (re-search-backward (concat sql-prompt ".+")  ; skip empty prompts
			  (point-min) t mysql-stack-pointer)
  ;then
      (let ((command-lines (mysql-get-command)) col)
	(goto-char (point-max))
	(setq col (current-column))
	(while command-lines
	  (indent-to col)
	  (insert (car command-lines))
	  (setq command-lines (cdr command-lines))
	  (if command-lines (insert ?\n))
	)
        (message (if (> mysql-stack-pointer 0)
		     (format "#%d" mysql-stack-pointer)
		   ""))
      )
  ;else
    (setq mysql-stack-pointer (- mysql-stack-pointer arg)) ; reset
    (error "No previous MySQL command.")
  )
)

(defun mysql-next-command (arg)
  "Recall the next MySQL command from the command stack.
With prefix argument, recall the command ARG commands after the current
stack pointer."
  (interactive "p")
  (mysql-previous-command (- arg))
)

(defun mysql-end-of-buffer ()
  "Move cursor to end of buffer."
  (interactive)
  (goto-char (point-max))
)

(defun mysql-reset-buffer ()
  "Reset MySQL buffer to contain only command history, not output.
Commands of one or fewer characters (/, l, r, etc.) are not retained."
  (interactive)
  (and (y-or-n-p 
	(format "Delete all output lines from buffer %s? " (buffer-name)))
       (let ((line "") (process (get-buffer-process (current-buffer))) start)
	 (message "Deleting output lines...")
	 (goto-char (point-min))
	 (setq start (point))
	 (while (re-search-forward (concat sql-prompt "..+") (point-max) t)
	   (goto-char (match-end 1))
	   (setq line (buffer-substring (point) (progn (end-of-line) (point))))
	   (beginning-of-line)
	   (delete-region start (point))
	   (forward-line)
	   (while (and			; skip past SQL statement
		   (not (string-match "^\\(.*;$\\| */\\)$" line))
		   (looking-at sql-continue-pattern)) ; and this is a cont. line
	     (goto-char (match-end 0))                   ; skip over prompt
	     (setq line (buffer-substring (point) (progn (end-of-line) (point))))
	     (forward-line)
	   )
	   (setq start (point))
	 )
	 (goto-char (point-max))
	 (delete-region start (point))
	 (process-send-string process "\n")	; generate new prompt
	 (goto-char (point-max))
	 (set-marker (process-mark process) (point))
	 (sit-for 0)				; update display
	 (accept-process-output)		; wait for prompt
	 (message "Deleting output lines...Done.")
       )
  )
)

(defun mysql-drop-old-lines (lines-to-keep)
  "Delete old lines from current buffer.
Number of lines to keep is determined by the variable mysql-lines-to-keep.
With prefix argument, keep the last ARG lines."
  (interactive "P")
  (delete-region (save-excursion
		   (goto-char (point-min))
		   (point))
		 (save-excursion
		   (goto-char (point-max))
		   (forward-line (- (or lines-to-keep 
					mysql-lines-to-keep 
					1000)))
		   (point)))
)

(defun mysql-save-session (filename)
"Save current MySQL session to FILENAME."
  (interactive "File to save session to: ")	    
  (save-excursion
    (if (or (null filename) (string= filename ""))
	(setq filename "~/.sqlhist"))
    (message (format "Saving %s..." filename))
    (write-region (progn
		    (goto-char (point-min))
		    (re-search-forward sql-prompt (point-max) t)
		    (match-end 0))
		  (progn
		    (goto-char (point-max))
		    (re-search-backward sql-prompt (point-min) t)
		    (match-end 0))
		  (expand-file-name filename) nil 0)
    (message (format "Saving %s...done" filename))
  )
)

(defun mysql-copy-word ()
  "Copy current word to the end of buffer, inserting SELECT keyword 
or commas if appropriate."
  (interactive)
  (let (word preceding-word)
    (save-excursion
      (setq word (buffer-substring	; extract word
		  (progn (forward-char 1) (backward-word 1) (point))
		  (progn (forward-word 1) (point))))
      (goto-char (point-max))		; goto end of buffer
      (cond
       ; sitting at empty command line
       ((save-excursion (beginning-of-line) 
			(looking-at (concat sql-prompt "$")))
	(insert "SELECT ")
       )
       ; on same line as SELECT or ORDER BY, but other words already inserted
       ((save-excursion (re-search-backward " select .+\\| order by .+" 
			  (save-excursion (beginning-of-line) (point)) t))
	(insert ", ")
       )
       ; Otherwise
       (t
	(if (eq (preceding-char) ? )	; if preceding character = space
	    nil
	  (insert " ")
	)
       )
      );end case
      (insert word)
      (message (format "\"%s\" copied" word))
    )
  )
)
