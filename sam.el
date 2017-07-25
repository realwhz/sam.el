;;; sam.el -- emulate the sam text editor                    -*- Emacs-Lisp -*-
;;; Copyright (C) 1993 Rick Sladkey <jrs@world.std.com>
;;; Copyright (C) 2017 Hongzheng Wang <wanghz@gmail.com>

;; This file is not part of Emacs but is distributed under
;; the same conditions as Emacs.

;; Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;; LCD Archive Entry:
;; sam|Rick Sladkey|jrs@world.std.com|
;; Emulate the sam text editor Using Emacs|
;; 11-Dec-1993|0.5|~/modes/sam.el.Z|

(defconst sam-version "sam v0.6 - 07-24-2017")

;; Problems or Omissions:

;; can insert-buffer-substring be useful?
;; command grouping
;; missing simultaneous undo in multiple buffers
;; current directory for shell commands
;; buffer commands should use buffer-file-name
;; no support for a named pipe for commands
;; none of the special mouse features are implemented
;; multiple windows into a file don't really have separate dots
;; gracefully handle more syntax errs (e.g. "s")
;; gracefully handle empty or missing editing buffer
;; syntax errors cause unclearable in progress commands

;; Recent changes by Hongzheng Wang:

;; Removed the dedicated sam command buffer and instead use it in Acme
;; way, i.e., provide a `Edit' command to issue Sam commands.


;;; Variables and keymaps.

(defvar sam-command-assoc
  '(;; Text commands
    ("a" . sam-append)
    ("c" . sam-change)
    ("i" . sam-insert)
    ("d" . sam-delete)
    ("s" . sam-substitute)
    ("m" . sam-move)
    ("t" . sam-copy)
    ;; Display commands
    ("p" . sam-print)
    ("=" . sam-value)
    ;; File commands
    ("b" . sam-switch-buffer)
    ("B" . sam-visit-files)
    ("n" . sam-buffer-menu)
    ("D" . sam-kill-buffers)
    ;; I/O commands
    ("e" . sam-edit)
    ("r" . sam-read)
    ("w" . sam-write)
    ("f" . sam-file)
    ("<" . sam-pipe-in)
    (">" . sam-pipe-out)
    ("|" . sam-pipe-thru)
    ("!" . sam-shell)
    ("cd" . sam-cd)
    ;; Loops and conditionals
    ("x" . sam-for-each)
    ("y" . sam-except-each)
    ("X" . sam-for-each-buffer)
    ("Y" . sam-except-each-buffer)
    ("g" . sam-when)
    ("v" . sam-unless)
    ;; Misc commands
    ("k" . sam-set-reference)
    ("q" . sam-quit)
    ("u" . sam-undo)
    ("{" . sam-compound)
    ("" . sam-default))
  "Association list used to look up sam commands.")

(defvar sam-operator-assoc
  '(("+" . sam-plus)
    ("-" . sam-minus)
    ("," . sam-comma)
    (";" . sam-semi))
  "Association list used to look up sam address operators.")

(defvar sam-precedence-assoc
  '((sam-plus . 2)
    (sam-minus . 2)
    (sam-comma . 1)
    (sam-semi . 1))
  "Association list used to look up the precedence of sam address operators.")

(defvar sam-edit-buffer nil
  "The name of the buffer currently being edited by sam.")

(defvar sam-last-command nil
  "Last command issued from the sam command window.")

(defvar sam-last-regexp nil
  "Last sam regexp, used when a regexp is empty.")

(defvar sam-last-shell-command nil
  "Last sam shell command, used when a command is empty.")

(defvar sam-reference nil
  "The address mark for each sam editing buffer.")
(make-variable-buffer-local 'sam-reference)

(defvar sam-current-dot nil
  "The dot in current buffer.")
(make-variable-buffer-local 'sam-current-dot)

(defvar sam-please-go-away nil
  "Non-nil means sam is not wanted anymore.")

(defvar sam-affected-buffers nil
  "List of buffers affected since the last top-level sam command.")

(defvar sam-edit-mode nil
  "Non-nil if the buffer is being edited by sam.")
(or (assq 'sam-edit-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(sam-edit-mode " Sam-Edit"))))
(put 'sam-edit-mode 'permanent-local t)

(defvar sam-edit-buffer-name nil
  "Name of buffer currently being editing by sam.")
(or (assq 'sam-edit-buffer-name minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(sam-edit-buffer-name (" Editing: " sam-edit-buffer-name)))))

(defvar sam-command-in-progress nil
  "String used to build up multi-line commands.")

;;; User-visible.

(defvar sam-edit-mode-hook nil
  "*Hooks to run when sam edit mode is started.")

(defvar sam-case-fold-search nil
  "*Non-nil if searches should ignore case.")

(defvar sam-emacs-style-regexps nil
  "*Non-nil if sam should use emacs-style regexps instead of egrep-style.")


;;; Buffer mode support functions.

;;;###autoload

(define-minor-mode sam-edit-mode ()
  "Make the current buffer be the buffer affected by sam commands."
  nil nil nil
  (and sam-edit-buffer
       (sam-leave-edit-mode))
  (setq sam-edit-buffer (current-buffer))
  (set (make-local-variable 'sam-edit-mode) t)
  (save-excursion
    (set (make-local-variable 'sam-edit-buffer-name)
	 (buffer-name sam-edit-buffer)))
  (run-hooks 'sam-edit-mode-hook))

(defun sam-leave-edit-mode ()
  (save-excursion
    (set (make-local-variable 'sam-edit-buffer-name) nil))
  (save-excursion
    (and sam-edit-buffer
	 (buffer-name sam-edit-buffer)
	 (progn
	   (set-buffer sam-edit-buffer)
	   (set (make-local-variable 'sam-edit-mode) nil))))
  (setq sam-edit-buffer nil))


;;; Address structure constructors and accessors.

(defmacro sam-make-addr (buffer beg end)
  (` (cons (, buffer) (cons (, beg) (, end)))))

(defmacro sam-addr-buffer (addr)
  (` (car (, addr))))

(defmacro sam-addr-beg (addr)
  (` (car (cdr (, addr)))))

(defmacro sam-addr-end (addr)
  (` (cdr (cdr (, addr)))))


;;; Command run-time support functions.

(defmacro sam-command (addr &rest body)
  (` (progn
       (set-buffer (sam-addr-buffer (, addr)))
       (or (memq (current-buffer)
		 sam-affected-buffers)
	   (setq sam-affected-buffers (cons (current-buffer)
					    sam-affected-buffers)))
       (,@ body))))
(put 'sam-command 'lisp-indent-function 1)

(defmacro sam-get-dot ()
  '(if (use-region-p)
       (sam-set-dot (region-beginning) (region-end))
     (sam-set-dot)))

(defmacro sam-set-dot (&optional beg end)
  (or beg
      (setq beg '(point)))
  (or end
      (setq end '(point)))
  (` (progn
       (set-mark (, beg))
       (goto-char (, end))
       (setq sam-current-dot (cons (, beg) (, end))))))

(defmacro sam-highlight-dot ()
  '(setq mark-active (not (eq (mark) (point)))))


;;; Text commands.

(defun sam-append (addr str)
  (sam-command addr)
  (goto-char (sam-addr-end addr))
  (insert-before-markers str)
  (sam-set-dot (sam-addr-end addr))
  (sam-highlight-dot))

(defun sam-change (addr str)
  (sam-command addr)
  (kill-region (sam-addr-beg addr) (sam-addr-end addr))
  (goto-char (sam-addr-beg addr))
  (insert-before-markers str)
  (sam-set-dot (sam-addr-beg addr))
  (sam-highlight-dot))

(defun sam-insert (addr str)
  (sam-command addr)
  (goto-char (sam-addr-beg addr))
  (insert-before-markers str)
  (sam-set-dot (sam-addr-beg addr))
  (sam-highlight-dot))

(defun sam-delete (addr)
  (sam-command addr)
  (kill-region (sam-addr-beg addr) (sam-addr-end addr))
  (sam-set-dot))

(defun sam-substitute (addr n regexp replac global)
  (sam-command addr)
  (let ((limit (copy-marker (sam-addr-end addr)))
	(case-fold-search sam-case-fold-search)
	(substituted-something nil)
	(continuing t))
    (goto-char (sam-addr-beg addr))
    (if global
	(while (and continuing
		    (re-search-forward regexp limit t n))
	  (setq substituted-something t)
	  (setq continuing (< (point) limit))
	  (replace-match replac)
	  (and (> (point) limit)
	       (set-marker limit (point)))
	  (and (= (match-beginning 0) (match-end 0))
	       (< (point) limit)
	       (forward-char 1))
	  (setq n 1))
      (and (re-search-forward regexp limit t n)
	   (progn
	     (setq substituted-something t)
	     (replace-match replac)))
      (or substituted-something
	  (error "Nothing substituted.")))
    (sam-set-dot (sam-addr-beg addr) limit)
    (set-marker limit nil)))

(defun sam-move (addr1 addr2)
  (let ((where (progn
		 (sam-command addr2)
		 (copy-marker (sam-addr-end addr2))))
	(text (progn
		(sam-command addr1)
		(buffer-substring (sam-addr-beg addr1)
				  (sam-addr-end addr1)))))
    (kill-region (sam-addr-beg addr1) (sam-addr-end addr1))
    (set-buffer (sam-addr-buffer addr2))
    (goto-char where)
    (let ((beg (point)))
      (insert-before-markers text)
      (sam-set-dot beg))
    (set-marker where nil)))

(defun sam-copy (addr1 addr2)
  (sam-command addr2)
  (let ((text (save-excursion
		(set-buffer (sam-addr-buffer addr1))
		(buffer-substring (sam-addr-beg addr1)
				  (sam-addr-end addr1)))))
    (goto-char (sam-addr-end addr2))
    (insert-before-markers text)
    (sam-set-dot (sam-addr-end addr2))))


;;; Display commands.

(defun sam-print (addr)
  (sam-command addr)
  (sam-set-dot (sam-addr-beg addr) (sam-addr-end addr))
  (sam-highlight-dot))

(defun sam-value (addr char-addr-only)
  (set-buffer (sam-addr-buffer addr))
  (let* ((mark (1- (sam-addr-beg addr)))
	 (point (1- (sam-addr-end addr)))
	 (text (if char-addr-only
		   (if (eq point mark)
		       (format "#%d\n" point)
		     (format "#%d,#%d\n" mark point))
		 (let* ((mark-line
			 (save-excursion
			   (1+ (count-lines (point-min)
					    (progn
					      (goto-char (1+ mark))
					      (beginning-of-line)
					      (point))))))
			(point-line
			 (if (= mark point)
			     mark-line
			   (save-excursion
			     (1+ (count-lines (point-min)
					      (progn
						(goto-char point)
						(beginning-of-line)
						(point))))))))
		   (cond
		    ((eq point mark)
		     (format "%d; #%d\n" point-line point))
		    ((eq point-line mark-line)
		     (format "%d; #%d,#%d\n"
			     point-line mark point))
		    (t
		     (format "%d,%d; #%d,#%d\n"
			     mark-line point-line mark point)))))))
    (message text)))


;;; File commands.

(defun sam-switch-buffer (file-list)
  (or file-list
      (error "File list is empty."))
  (while (and file-list
	      (not (get-buffer (car file-list))))
    (setq file-list (cdr file-list)))
  (or file-list
      (error "No matching buffers."))
  (save-excursion
    (set-buffer (car file-list))
    (sam-edit-mode))
  (display-buffer (car file-list)))

(defun sam-visit-files (file-list)
  (or file-list
      (error "File list is empty."))
  (let ((list file-list))
    (while list
      (or (get-buffer (car list))
	  (find-file (car list)))
      (setq list (cdr list))))
  (save-excursion
    (set-buffer (car file-list))
    (sam-edit-mode))
  (display-buffer (car file-list)))

(defun sam-buffer-menu ()
  (let ((buffer-list (sam-buffer-list))
	buffer)
    (while buffer-list
      (setq buffer (car buffer-list)
	    buffer-list (cdr buffer-list))
      (message (sam-buffer-menu-line buffer)))))

(defun sam-buffer-list ()
  (let ((buffer-list (buffer-list))
	(new-list nil)
	buffer)
    (while buffer-list
      (setq buffer (car buffer-list)
	    buffer-list (cdr buffer-list))
      (and (not (string-match "\\`[ *]" (buffer-name buffer)))
	   (save-excursion
	     (set-buffer buffer)
	     (not (eq major-mode 'dired-mode)))
	   (setq new-list (cons buffer new-list))))
    (nreverse new-list)))

(defun sam-buffer-menu-line (buffer)
  (format "%s%s%s %s\n"
	  (if (buffer-modified-p buffer) "'" " ")
	  "+"
	  (if (eq buffer sam-edit-buffer) "." " ")
	  (buffer-name buffer)))

(defun sam-kill-buffers (file-list)
  (or file-list
      (error "File list is empty."))
  (let (buffer)
    (while file-list
      (and (setq buffer (get-buffer (car file-list)))
	   (progn
	     (and (eq buffer sam-edit-buffer)
		  (sam-leave-edit-mode))
	     (kill-buffer buffer)))
      (setq file-list (cdr file-list)))))

(defun sam-file-list (str)
  (and (string-match "\\`<" str)
       (save-excursion
	 (set-buffer (get-buffer-create " *shell*"))
	 (erase-buffer)
	 (shell-command (substring str 1) t)
	 (setq str (buffer-substring (point-min) (point-max)))))
  (let ((list nil))
    (while (not (string= str ""))
      (setq str (sam-split-white str)
	    list (cons (car str) list)
	    str (cdr str)))
    (nreverse list)))


;;; I/O commands.

(defun sam-edit (filename)
  (and (string= filename "")
       (setq filename (buffer-file-name sam-edit-buffer)))
  (pop-to-buffer sam-edit-buffer)
  (sam-leave-edit-mode)
  (find-alternate-file filename)
  (sam-edit-mode))

(defun sam-read (addr filename)
  (and (string= filename "")
       (setq filename (buffer-file-name sam-edit-buffer)))
  (sam-command addr)
  (kill-region (sam-addr-beg addr) (sam-addr-end addr))
  (goto-char (sam-addr-beg addr))
  (let ((old-point-max (point-max))
	(beg (point)))
    (insert-file-contents filename)
    (forward-char (- (point-max) old-point-max))
    (sam-set-dot beg)))

(defun sam-write (addr filename)
  (set-buffer (sam-addr-buffer addr))
  (if (string= filename "")
      (save-buffer)
    (write-region (sam-addr-beg addr) (sam-addr-end addr) filename))
  (and (string= filename "")
       (setq filename (buffer-name sam-edit-buffer)))
  (message (format "%s: #%d\n"
		   filename (- (sam-addr-end addr)
			       (sam-addr-beg addr)))))

(defun sam-file (filename)
  (or (string= filename "")
      (save-excursion
	(set-buffer sam-edit-buffer)
	(set-visited-file-name filename)))
  (message (sam-buffer-menu-line sam-edit-buffer)))

(defun sam-pipe-in (addr command)
  (setq command (sam-last-shell-command command))
  (sam-command addr)
  (kill-region (sam-addr-beg addr) (sam-addr-end addr))
  (shell-command command t)
  (sam-set-dot (sam-addr-beg addr) (mark))
  (sam-highlight-dot))

(defun sam-pipe-out (addr command)
  (setq command (sam-last-shell-command command))
  (sam-command addr)
  (shell-command-on-region (sam-addr-beg addr) (sam-addr-end addr) command))

(defun sam-pipe-thru (addr command)
  (setq command (sam-last-shell-command command))
  (sam-command addr)
  (shell-command-on-region (sam-addr-beg addr) (sam-addr-end addr)
			   command t t)
  (sam-set-dot (sam-addr-beg addr) (mark))
  (sam-highlight-dot))

(defun sam-shell (command)
  (setq command (sam-last-shell-command command))
  (shell-command command))

(defun sam-cd (directory)
  (and (string= directory "")
       (setq directory "~/"))
  (cd directory)
  (force-mode-line-update))

(defun sam-last-shell-command (command)
  (or (string= command "")
      (setq sam-last-shell-command command))
  (or sam-last-shell-command
      (error "No remembered shell command."))
  sam-last-shell-command)


;;; Loops and conditionals.

(defun sam-for-each (addr regexp cmd)
  (set-buffer (sam-addr-buffer addr))
  (goto-char (sam-addr-beg addr))
  (let ((limit (copy-marker (sam-addr-end addr)))
	beg
	(end (make-marker))
	(continuing t)
	matches-something
	(case-fold-search sam-case-fold-search))
    (while (and continuing
		(re-search-forward regexp limit t)
		(or (setq matches-something
			  (/= (setq beg (match-beginning 0))
			      (set-marker end (point))))
		    (setq continuing (/= end limit))
		    (not (bolp))))
      (sam-set-dot beg)
      (eval cmd)
      (set-buffer (sam-addr-buffer addr))
      (goto-char end)
      (or matches-something
	  (and continuing
	       (forward-char 1))))
    (set-marker end nil)
    (set-marker limit nil)))

(defun sam-except-each (addr regexp cmd)
  (set-buffer (sam-addr-buffer addr))
  (goto-char (sam-addr-beg addr))
  (let ((last-end (copy-marker (sam-addr-beg addr)))
	(limit (copy-marker (sam-addr-end addr)))
	beg
	(end (make-marker))
	(continuing t)
	matches-something
	(case-fold-search sam-case-fold-search))
    (while (and continuing
		(re-search-forward regexp limit t)
		(or (setq matches-something
			  (/= (setq beg (match-beginning 0))
			      (set-marker end (point))))
		    (setq continuing (/= end limit))
		    (not (bolp))))
      (sam-set-dot last-end beg)
      (eval cmd)
      (set-buffer (sam-addr-buffer addr))
      (goto-char end)
      (or matches-something
	  (and continuing
	       (forward-char 1))))
    (sam-set-dot last-end limit)
    (eval cmd)
    (set-marker last-end nil)
    (set-marker limit nil)
    (set-marker end nil)))

(defun sam-for-each-buffer (regexp cmd)
  (let ((buffer-list (sam-buffer-list))
	buffer)
    (while buffer-list
      (setq buffer (car buffer-list)
	    buffer-list (cdr buffer-list))
      (and (string-match regexp (buffer-name buffer))
	   (let ((sam-edit-buffer buffer))
	     (eval cmd))))))

(defun sam-except-each-buffer (regexp cmd)
  (let ((buffer-list (sam-buffer-list))
	buffer)
    (while buffer-list
      (setq buffer (car buffer-list)
	    buffer-list (cdr buffer-list))
      (or (string-match regexp (buffer-name buffer))
	  (let ((sam-edit-buffer buffer))
	    (eval cmd))))))

(defun sam-when (addr regexp cmd)
  (set-buffer (sam-addr-buffer addr))
  (and (save-excursion
	 (goto-char (sam-addr-beg addr))
	 (let ((case-fold-search sam-case-fold-search))
	   (re-search-forward regexp (sam-addr-end addr) t)))
       (progn
	 (sam-set-dot (sam-addr-beg addr) (sam-addr-end addr))
	 (eval cmd))))

(defun sam-unless (addr regexp cmd)
  (set-buffer (sam-addr-buffer addr))
  (or (save-excursion
	(goto-char (sam-addr-beg addr))
	(let ((case-fold-search sam-case-fold-search))
	  (re-search-forward regexp (sam-addr-end addr) t)))
      (progn
	(sam-set-dot (sam-addr-beg addr) (sam-addr-end addr))
	(eval cmd))))


;;; Misc commands.

(defun sam-set-reference (addr)
  (set-buffer (sam-addr-buffer addr))
  (setq sam-reference addr))

(defun sam-undo (addr n)
  (sam-command addr)
  (and (eq sam-last-command 'sam-undo)
       (setq last-command 'undo))
  (undo n)
  (sam-set-dot))

(defun sam-goto (addr &optional was-defaulted)
  (sam-command addr)
  (and was-defaulted
       (let ((new-addr (sam-plus addr 0)))
	 (and (equal addr new-addr)
	      (setq new-addr (sam-plus addr 1)))
	 (setq addr new-addr)))
  (sam-set-dot (sam-addr-beg addr) (sam-addr-end addr)))

(defun sam-print-addr (addr)
  (let ((beg (1- (sam-addr-beg addr)))
	(end (1- (sam-addr-end addr))))
    (if (eq beg end)
	(message "%s: #%d" (sam-addr-buffer addr) end)
      (message "%s: #%d,#%d" (sam-addr-buffer addr) beg end))))


;;; Address run-time support functions.

(defun sam-pos (buffer n)
  (setq n (1+ n))
  (and (or (< n (point-min))
	   (> n (point-max)))
       (error "Address out of range."))
  (sam-make-addr buffer n n))

(defun sam-line (buffer n)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (forward-line (1- n))
    (sam-entire-line)))

(defun sam-point-min (buffer)
  (save-excursion
    (set-buffer buffer)
    (sam-make-addr buffer (point-min) (point-min))))

(defun sam-point-max (buffer)
  (save-excursion
    (set-buffer buffer)
    (sam-make-addr buffer (point-max) (point-max))))

(defun sam-all (buffer)
  (sam-comma (sam-point-min buffer) (sam-point-max buffer)))

(defun sam-dot (buffer)
  (set-buffer buffer)
  (let ((dot (sam-get-dot)))
    (sam-make-addr buffer (car dot) (cdr dot))))

(defun sam-reference (buffer)
  (set-buffer buffer)
  (or sam-reference
      (error "No mark set in this buffer."))
  sam-reference)

(defun sam-plus (addr offset)
  (save-excursion
    (set-buffer (sam-addr-buffer addr))
    (goto-char (sam-addr-end addr))
    (cond
     ((numberp offset)
      (if (bolp)
	  (forward-line (1- offset))
	(forward-line offset))
      (sam-entire-line))
     ((stringp offset)
      (let ((case-fold-search sam-case-fold-search)
	    (start (point)))
	(or (and (re-search-forward offset nil t)
		 (or (/= (match-beginning 0) (match-end 0))
		     (/= start (point))
		     (and (< (point) (point-max))
			  (progn
			    (forward-char 1)
			    (re-search-forward offset nil t)))))
	    (progn
	      (goto-char (point-min))
	      (re-search-forward offset))))
      (sam-entire-match))
     ((consp offset)
      (forward-char (car offset))
      (sam-make-addr (current-buffer) (point) (point))))))

(defun sam-minus (addr offset)
  (save-excursion
    (set-buffer (sam-addr-buffer addr))
    (goto-char (sam-addr-beg addr))
    (cond
     ((numberp offset)
      (forward-line (- offset))
      (sam-entire-line))
     ((stringp offset)
      (let ((case-fold-search sam-case-fold-search)
	    (start (point)))
	(or (and (re-search-backward offset nil t)
		 (or (/= (match-beginning 0) (match-end 0))
		     (/= start (point))
		     (and (> (point) (point-min))
			  (progn
			    (backward-char 1)
			    (re-search-backward offset nil t)))))
	    (progn
	      (goto-char (point-max))
	      (re-search-backward offset))))
      (sam-entire-match))
     ((consp offset)
      (backward-char (car offset))
      (sam-make-addr (current-buffer) (point) (point))))))

(defun sam-comma (addr1 addr2)
  (and (not (eq (sam-addr-buffer addr1) (sam-addr-buffer addr2)))
       (error "A comma cannot join addresses in different buffers."))
  (sam-make-addr (sam-addr-buffer addr1)
		 (sam-addr-beg addr1)
		 (sam-addr-end addr2)))

(defun sam-semi (addr)
  (sam-goto addr)
  addr)

(defun sam-last-regexp (regexp)
  (or (string= regexp "")
      (setq sam-last-regexp regexp))
  (or sam-last-regexp
      (error "No remembered search string."))
  sam-last-regexp)

(defun sam-regexp-to-buffer (regexp)
  (let ((buffer-list (buffer-list))
	name
	(buffer nil))
    (while buffer-list
      (and (not (string-match "\\` " (setq name
					   (buffer-name (car buffer-list)))))
	   (string-match regexp name)
	   (if buffer
	       (error "Regexp matches more than one buffer: %s and %s."
		      buffer (car buffer-list))
	     (setq buffer (car buffer-list))))
      (setq buffer-list (cdr buffer-list)))
    buffer))

(defun sam-entire-line ()
  (sam-make-addr (current-buffer)
		 (progn
		   (beginning-of-line)
		   (point))
		 (progn
		   (end-of-line)
		   (or (eobp)
		       (forward-char 1))
		   (point))))

(defun sam-entire-match ()
  (sam-make-addr (current-buffer) (match-beginning 0) (match-end 0)))


;;; Command compilation functions.

(defun sam-compile-command (str &optional default-command)
  (or default-command
      (setq default-command 'sam-goto))
  (let ((addr (sam-compile-address str))
	(real-addr nil)
	(cmd nil))
    (setq str (cdr addr)
	  real-addr (car addr)
	  addr (sam-fix-default real-addr 'sam-dot))
    ;; irregular syntax, arghh
    (string-match "\\`\\(cd\\|.?\\)" str)
    (setq cmd (substring str 0 (match-end 0))
	  str (sam-skip-white (substring str (match-end 0))))
    (let ((fun (cdr (assoc cmd sam-command-assoc))))
      (or fun
	  (error "Invalid sam command `%s'." cmd))
      (and (eq fun 'sam-default)
	   (setq fun default-command))
      (cond
       ((eq fun 'sam-compound)
	(error "Compound commands don't really word yet.")
	(let ((text (and (string-match "\\`\n\\(\\(.*\n\\)*\\)}\\'" str)
			 (substring str (match-beginning 1) (match-end 1)))))
	  (and text
	       (list fun addr text))))
       ((memq fun '(sam-append sam-change sam-insert))
	(let ((text (if (string-match "\\`$" str)
			(and (string-match
			      "\\`\n\\(\\(\\(\\|[^.\n]\\|..+\\)\n\\)*\\)\\.\\'"
			      str)
			     (substring str (match-beginning 1) (match-end 1)))
		      (sam-parse-text (car (sam-parse-string str))))))
	  (and text
	       (list fun addr text))))
       ((memq fun '(sam-delete sam-print sam-set-reference))
	(list fun addr))
       ((eq fun 'sam-goto)
	(list fun addr (not (sam-fix-default real-addr nil))))
       ((eq fun 'sam-value)
	(list fun addr (string-match "\\`#" str)))
       ((memq fun '(sam-move sam-copy))
	(list fun addr (or (car (sam-compile-address str))
			   '(sam-dot sam-edit-buffer))))
       ((eq fun 'sam-substitute)
	(let* ((n (if (string-match "\\`[1-9][0-9]* *" str)
		      (prog1
			  (string-to-number str)
			(setq str (substring str (match-end 0))))
		    1))
	       (pair1 (sam-parse-string str))
	       (regexp (sam-parse-regexp (car pair1)))
	       (pair2 (sam-parse-string (concat (substring (car pair1) 0 1)
						(cdr pair1))))
	       (replac (sam-parse-replac (car pair2)))
	       (global (and (string-match "g" (cdr pair2)) t)))
	  (list fun addr n regexp replac global)))
       ((memq fun '(sam-for-each sam-except-each))
	(let* ((pair (sam-parse-string str))
	       (regexp (sam-parse-regexp (car pair) "^.*\n?"))
	       (cmd (sam-compile-command (cdr pair) 'sam-print)))
	  (and cmd
	       (list fun addr regexp (list 'quote cmd)))))
       ((memq fun '(sam-for-each-buffer sam-except-each-buffer))
	(let* ((pair (sam-parse-string str))
	       (regexp (sam-parse-regexp (car pair) ".*"))
	       (cmd (sam-compile-command (cdr pair) 'sam-file)))
	  (and cmd
	       (list fun regexp (list 'quote cmd)))))
       ((memq fun '(sam-when sam-unless))
	(let* ((pair (sam-parse-string str))
	       (regexp (sam-parse-regexp (car pair)))
	       (cmd (sam-compile-command (cdr pair) nil)))
	  (and cmd
	       (list fun addr regexp (list 'quote cmd)))))
       ((eq fun 'sam-undo)
	(list fun addr (if (string= str "") 1 (string-to-number str))))
       ((memq fun '(sam-quit sam-buffer-menu))
	(list fun))
       ((memq fun '(sam-switch-buffer sam-visit-files sam-kill-buffers))
	(list fun (list 'sam-file-list str)))
       ((memq fun '(sam-edit sam-file sam-cd sam-shell))
	(list fun str))
       ((memq fun '(sam-read sam-write sam-pipe-in sam-pipe-out sam-pipe-thru))
	(and (eq fun 'sam-write)
	     (setq addr (sam-fix-default real-addr 'sam-all)))
	(list fun addr str))
       (t
	(error "Don't yet know how to compile that command."))))))


;;; Address compilation functions.

(defun sam-compile-address (str)
  (let ((addrs nil)
	(ops nil)
	(parsing t)
	addr
	op
	buffer)
    (while parsing
      (setq str (sam-skip-white str))
      (setq addr nil)
      ;; "regexp"
      (if (sam-match-delimited-string "\"" str)
	  (setq buffer
		(list 'sam-regexp-to-buffer
		      (sam-parse-regexp (substring str 0 (match-end 1))))
		str (substring str (match-end 0)))
	(setq buffer 'sam-edit-buffer))
      (cond
       ;; #n
       ((string-match "\\`# *\\([0-9]*\\)" str)
	(let ((n (if (eq (match-beginning 1) (match-end 1))
		     1
		   (string-to-number (substring str 1)))))
	  (setq addr (list 'sam-pos buffer n))))
       ;; n
       ((string-match "\\`[0-9]+" str)
	(let ((n (string-to-number str)))
	  (setq addr (if (zerop n)
			 (list 'sam-point-min buffer)
		       (list 'sam-line buffer n)))))
       ;; /regexp/
       ((sam-match-delimited-string "/" str)
	(setq addr (list 'sam-forward
			 buffer
			 (sam-parse-regexp (substring str 0 (match-end 1))))))
       ;; ?regexp?
       ((sam-match-delimited-string "?" str)
	(setq addr (list 'sam-backward
			 buffer
			 (sam-parse-regexp (substring str 0 (match-end 1))))))
       ;; $
       ((string-match "\\`\\$" str)
	(setq addr (list 'sam-point-max buffer)))
       ;; .
       ((string-match "\\`\\." str)
	(setq addr (list 'sam-dot buffer)))
       ;; '
       ((string-match "\\`'" str)
	(setq addr (list 'sam-reference buffer))))
      (and addr
	   (setq str (sam-skip-white (substring str (match-end 0)))))
      (or addr
	  (setq addr (list 'sam-default buffer)))
      (and nil (null addr)
	   (not (eq buffer 'sam-edit-buffer))
	   (setq addr (list 'sam-dot buffer)))
      (setq addrs (cons addr addrs))
      ;; implicit +
      (and addr
	   (string-match "\\`[#0-9/?$.'\"]" str)
	   (setq str (concat "+" str)))
      (if (string-match "\\`[-+,;]" str)
	  (progn
	    (setq op (cdr (assoc (substring str 0 1) sam-operator-assoc))
		  str (substring str 1))
	    (and ops
		 (>= (cdr (assq (car ops) sam-precedence-assoc))
		     (cdr (assq op sam-precedence-assoc)))
		 (setq addr (sam-addr-node (car ops)
					   (car (cdr addrs))
					   (car addrs))
		       addrs (cons addr (cdr (cdr addrs)))
		       ops (cdr ops)))
	    (setq ops (cons op ops)))
	(setq parsing nil)))
    (while ops
      (setq addr (sam-addr-node (car ops)
				(car (cdr addrs))
				(car addrs))
	    addrs (cons addr (cdr (cdr addrs)))
	    ops (cdr ops)))
    (setq addr (sam-fix-search (car addrs)))
    (cons addr str)))

(defun sam-addr-node (op addr1 addr2)
  (cond
   ((memq op '(sam-plus sam-minus))
    (setq addr1 (sam-fix-search (sam-fix-default addr1 'sam-dot))
	  addr2 (sam-fix-default addr2 1))
    (and (consp addr2)
	 (let ((op2 (car addr2)))
	   (cond
	    ((eq op2 'sam-pos)
	     (setq addr2 (list 'quote (list (car (cdr (cdr addr2)))))))
	    ((eq op2 'sam-line)
	     (setq addr2 (car (cdr (cdr addr2)))))
	    ((eq op2 'sam-point-min)
	     (setq addr2 0))
	    ((memq op2 '(sam-forward sam-backward))
	     (and (eq op2 'sam-backward)
		  (setq op (if (eq op 'sam-plus) 'sam-minus 'sam-plus)))
	     (setq addr2 (car (cdr (cdr addr2)))))))))
   ((memq op '(sam-comma sam-semi))
    (setq addr1 (sam-fix-search (sam-fix-default addr1 'sam-point-min))
	  addr2 (sam-fix-search (sam-fix-default addr2 'sam-point-max)))
    (and (eq op 'sam-semi)
	 (setq addr1 (list 'sam-semi addr1)
	       op 'sam-comma))))
  (list op addr1 addr2))

(defun sam-fix-default (addr default)
  (and (consp addr)
       (eq (car addr) 'sam-default)
       (setq addr
	     (if (and default (symbolp default))
		 (list default (car (cdr addr)))
	       default)))
  addr)

(defun sam-fix-search (addr) 
  (and (consp addr)
       (memq (car addr) '(sam-forward sam-backward))
       (setq addr (list (if (eq (car addr) 'sam-forward) 'sam-plus 'sam-minus)
			(list 'sam-dot (car (cdr addr)))
			(car (cdr (cdr addr))))))
  addr)


;;; Misc compilation functions.

(defun sam-skip-white (str)
  (if (string-match "\\`[ \t]*" str)
      (substring str (match-end 0))
    str))

(defun sam-split-white (str)
  (if (string-match "[ \t\n]+" str)
      (cons (substring str 0 (match-beginning 0))
	    (substring str (match-end 0)))
    (cons str "")))

(defun sam-match-delimited-string (str text)
  (let* ((c (substring str 0 1))
	 (re-c (regexp-quote c)))
    (string-match (concat "\\`" re-c "\\(\\([^" c "\\\\\n]\\|\\\\.\\)*\\)"
			  re-c "?")
		  text)))

(defun sam-parse-string (str)
  (setq str (sam-skip-white str))
  (if (string-match "\\`[^A-Za-z0-9\n]" str)
      (if (sam-match-delimited-string str str)
	  (cons (substring str 0 (match-end 1))
		(substring str (match-end 0)))
	(cons str ""))
    (cons nil str)))

(defun sam-parse-regexp (regexp &optional default)
  (if regexp
      (if sam-emacs-style-regexps
	  (sam-last-regexp (sam-parse-text regexp))
	(setq regexp (append regexp nil))
	(let ((new nil)
	      (delim (car regexp))
	      c)
	  (setq regexp (cdr regexp))
	  (while (and regexp
		      (progn
			(setq c (car regexp)
			      regexp (cdr regexp))
			(not (eq c delim))))
	    (cond
	     ((memq c '(?\( ?\) ?|))
	      (setq new (cons c (cons ?\\ new))))
	     ((eq c ?\[)
	      (let ((special nil)
		    (rest nil)
		    (complement nil))
		(and (eq (car regexp) ?^)
		     (setq complement t
			   regexp (cdr regexp)
			   rest (list ?\n)))
		(while (and regexp
			    (progn
			      (setq c (car regexp)
				    regexp (cdr regexp))
			      (not (eq c ?\]))))
		  (cond
		   ((eq c ?\\)
		    (setq c (car regexp)
			  regexp (cdr regexp))
		    (if (memq c '(?- ?\] ?^))
			(or (memq c special)
			    (setq special (cons c special)))
		      (setq rest (cons c rest))))
		   ((eq c ?^)
		    (or (memq c special)
			(setq special (cons c special))))
		   (t
		    (setq rest (cons c rest)))))
		(if (and (not complement)
			 (null rest)
			 (equal special '(?^)))
		    (setq new (cons ?^ new))
		  (setq new (cons ?\[ new))
		  (and complement
		       (setq new (cons ?^ new)))
		  (and (memq ?\] special)
		       (setq new (cons ?\] new)))
		  (setq new (nconc rest new))
		  (and (memq ?^ special)
		       (setq new (cons ?^ new)))
		  (and (memq ?- special)
		       (setq new (cons ?- new)))
		  (setq new (cons ?\] new)))))
	     ((eq c ?\\)
	      (setq c (car regexp)
		    regexp (cdr regexp))
	      (cond
	       ((eq c ?n)
		(setq new (cons ?\n new)))
	       ((eq c delim)
		(setq new (cons c new)))
	       ((memq c '(?. ?* ?+ ?\[ ?\] ?\\ ?^ ?$))
		(setq new (cons c (cons ?\\ new))))
	       (t
		(setq new (cons c new)))))
	     (t
	      (setq new (cons c new)))))
	  (setq new (mapconcat (function char-to-string) (nreverse new) ""))
	  (sam-last-regexp new)))
    default))

(defun sam-parse-replac (replac)
  (setq replac (append replac nil))
  (let ((new nil)
	(delim (car replac))
	c)
    (setq replac (cdr replac))
    (while (and replac
		(progn
		  (setq c (car replac)
			replac (cdr replac))
		  (not (eq c delim))))
      (cond
       ((eq c ?&)
	(setq new (cons c (cons ?\\ new))))
       ((eq c ?\\)
	(setq c (car replac)
	      replac (cdr replac))
	(cond
	 ((eq c ?n)
	  (setq new (cons ?\n new)))
	 ((eq c delim)
	  (setq new (cons c new)))
	 ((and (>= c ?0) (<= c ?9))
	  (setq new (cons c (cons ?\\ new))))
	 (t
	  (setq new (cons c (cons ?\\ (cons ?\\ new)))))))
       (t
	(setq new (cons c new)))))
    (mapconcat (function char-to-string) (nreverse new) "")))

(defun sam-parse-text (str)
  (setq str (append str nil))
  (let ((new nil)
	(delim (car str))
	c)
    (setq str (cdr str))
    (while (and str
		(progn
		  (setq c (car str)
			str (cdr str))
		  (not (eq c delim))))
      (cond
       ((eq c ?\\)
	(setq c (car str)
	      str (cdr str))
	(cond
	 ((eq c ?n)
	  (setq new (cons ?\n new)))
	 ((eq c delim)
	  (setq new (cons c new)))
	 (t
	  (setq new (cons c (cons ?\\ new))))))
       (t
	(setq new (cons c new)))))
    (mapconcat (function char-to-string) (nreverse new) "")))


;;; Command evaluation functions.

(defun sam-eval-command (cmd)
  (let ((buffer (current-buffer)))
    (unwind-protect
	(eval cmd)
      (set-buffer buffer))
    (setq sam-last-command (car cmd)))
  (save-excursion
    (while sam-affected-buffers
      (let* ((buffer (car sam-affected-buffers))
	     (window (get-buffer-window buffer)))
	(set-buffer buffer)
	(set-window-start window
			  (save-excursion
			    (goto-char (window-start window))
			    (beginning-of-line)
			    (point))
			  t)
	(set-window-point window (point))
	(undo-boundary)
	(sam-highlight-dot))
      (setq sam-affected-buffers (cdr sam-affected-buffers)))))

(defun sam-eval-last-command ()
  (interactive)
  (let* ((str (read-from-minibuffer "Edit: "))
	 (cmd (condition-case nil
		  (let ((case-fold-search nil))
		    (setq sam-command-in-progress
			  (concat sam-command-in-progress str))
		    (sam-compile-command sam-command-in-progress))
		(error
		 (setq sam-command-in-progress nil)
		 nil))))

    (if cmd
	(progn
	  (setq sam-command-in-progress nil)
	  (sam-eval-command cmd)
	  (and sam-please-go-away
	       (progn
		 (sam-leave-edit-mode))))
      (setq sam-command-in-progress (concat sam-command-in-progress "\n")))))



(provide 'sam)
