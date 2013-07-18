;;; perforger.el --- Alternative p4 integration, inspired by magit
;; 
;; Filename: perforger.el
;; Description: Alternative p4 integration, inspired by magit
;; Author: Jordon Biondo
;; Maintainer: Jordon Biondo <biondoj@mail.gvsu.edu>
;; Created: Thu Jul 18 12:19:30 2013 (-0400)
;; Version: .1
;; Last-Updated: Thu Jul 18 12:20:04 2013 (-0400)
;;           By: jorbi
;;     Update #: 1
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(defcustom p4/executable "p4"
  "P4 executable."
  :set (lambda(sym val) (if (not (stringp val))
			    (message "p4/executable must be a string")
			  (setq p4/executable val))))

(defvar p4/log-buffer nil "Buffer holding log information.")
(defvar p4/info-buffer nil "Buffer holding p4 info output.")

(defvar p4/client-name nil "Name of the current P4 client.
This value is set when `p4/info' is first run.
Use `p4/refresh-info' if you need to update the values")
(defvar p4/user-name nil "Name of the current P4 user.
This value is set when `p4/info' is first run.
Use `p4/refresh-info' if you need to update the values")
(defvar p4/client-root nil "Root directory of the current P4 client.
This value is set when `p4/info' is first run.
Use `p4/refresh-info' if you need to update the values")
(defvar p4/client-host nil "Name of the current host (computer name).
This value is set when `p4/info' is first run.
Use `p4/refresh-info' if you need to update the values")


(defvar p4/info-buffer-header
  "
### keymap:
### q: close window

#############################################
### P4 INFO
#############################################"
  "Text to put at the top of the `p4/info-buffer'.")

(defvar p4/gui-application "p4v" "The Application to run when calling `p4/gui'.")

(defun p4/info()
  "Run p4 info and dump the text into the the `p4/info-buffer'."
  (interactive)
  (with-current-buffer (p4/get-info-buffer)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert p4/info-buffer-header))
    (lexical-let ((p4Proc (start-process "p4-info" (p4/get-info-buffer) p4/executable "info")))
      (set-process-sentinel p4Proc
			    (lambda(proc event)
			      (if (string-match ".*finished.*" event)
				  (select-window (temp-buffer-window-show p4/info-buffer))
				(with-current-buffer (p4/get-info-buffer)
				  (let ((inhibit-read-only t))
				    (goto-char (point-max))
				    (if (search-backward-regexp "Proces.*finished" nil t)
					(progn (delete-region (point) (point-max))
					       (p4/refresh-info))))))))
      p4Proc)))

(defun p4/force-sync()
  "Run a FORCED p4 sync on the current buffer, revert the buffer if successful"
  (interactive)
  (if (< emacs-major-version 22) (print "Sorry, this function would crash Emacs 21.")
    (if (yes-or-no-p (concat "Really force a sync on " (buffer-name)"?"))
	(lexical-let ((synced-buffer (current-buffer)))
	  (p4/run-with 'p4/standard-filter
		       (lambda(process event)
			 (if (string-match ".*finished.*" event)
			     (revert-buffer t t t)
			   (p4\log t (concat "error syncing "
					     (buffer-name synced-buffer)))))
		       (list "sync" "-f" (concat (buffer-file-name) "#head")))))))

(defun p4/login(&optional prompt no-retry)
  "Login to p4. Simply wraps p4 login"
  (interactive)
  (lexical-let ((prompt prompt) (no-retry no-retry))
    (p4/run-with (lambda(proc str)
		   (p4/standard-filter proc str)
		   (if (string-match ".password:.*" str)
		       (let ((prompt-str (concat
					  (if prompt prompt
					    (if p4/user-name p4/user-name "p4user -")) " ")))
			 (process-send-string proc
					      (concat (password-read (concat prompt-str str)) "\n")))
		     (if (string-match ".*Password invalid.*" str)
			 (if no-retry (p4/log t "Password incorrect")
			   (p4/login "retry | " t))
		       (if (string-match ".*logged in.*" str)
			   (p4/log t str)))))
		 nil (list "login"))))

(defun p4/edit()
  "Run a FORCED p4 sync on the current buffer, revert the buffer if successful"
  (interactive)
  (if (yes-or-no-p (concat "Really edit " (buffer-name)"?"))
      (lexical-let ((buf-name (buffer-name)))
	(p4/run-with (lambda(proc str)
		       (p4/standard-filter proc str)
		       (if (string-match ".*currently opened.*" str)
			   (p4/log t (concat buf-name " already opened"))
			 (if (string-match ".*opened.*" str)
			     (p4/log t (concat "opened " buf-name))
			   (if (string-match ".*not on client.*" str)
			       (p4/log t (concat buf-name " not on client, can't open"))
			     (p4/log t str)))))
		     nil ;; no sentinal
		     (list "edit" (buffer-file-name))))))


(defun p4/refresh-info()
  "Scan the `p4/info-buffer' to update the values of the following variables, `p4/client-host' `p4/user-name' `p4/client-root' `p4/client-name' "
  (with-current-buffer (p4/get-info-buffer)
    (goto-char (point-min))
    (if (not (re-search-forward "^User name:[ \t]+\\(.*\\)$" nil t))
	(p4/info)
      (progn
	(goto-char (point-min))
	(if (re-search-forward "^User name:[ \t]+\\(.*\\)$" nil t)
	    (setq p4/user-name (match-string-no-properties 1)))
	(goto-char (point-min))
	(if (re-search-forward "^Client host:[ \t]+\\(.*\\)$" nil t)
	    (setq p4/client-host (match-string-no-properties 1)))
	(goto-char (point-min))
	(if (re-search-forward "^Client root:[ \t]+\\(.*\\)$" nil t)
	    (setq p4/client-root (match-string-no-properties 1)))
	(goto-char (point-min))
	(if (re-search-forward "^Client name:[ \t]+\\(.*\\)$" nil t)
	    (setq p4/client-name (match-string-no-properties 1)))))))


(defun p4/log(print-it format-str &rest args)
  "Write a message to the `p4/log-buffer' buffer with a time stamp.
If PRINT-IT is non-nil, also display the message to the user"
  (let ((text (format format-str args)))
    (if print-it (princ text))
    (with-current-buffer (p4/get-log-buffer)
      (let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert "\n"(format-time-string "--p4/log|%B:%e:%Y::%-I:%M:%S:%2N") "\n" text "\n" ))
      text)))


(defmacro p4/with-log(&rest body)
  "Evaluated the body inside the `p4/log-buffer'."
  `(with-current-buffer (p4/get-log-buffer) ,@body))


(defun p4/get-log-buffer()
  "Return the buffer in `p4/log-buffer', if it does not exist or has been killed, it will
be recreated"
  (if (or (not p4/log-buffer) (not (buffer-live-p p4/log-buffer)))
      (setq p4/log-buffer (generate-new-buffer (generate-new-buffer-name "*p4/log*")))
    (with-current-buffer p4/log-buffer  (p4/log-mode)))
  p4/log-buffer)


(defun p4/show-log-buffer()
  (interactive)
  (select-window (temp-buffer-window-show (p4/get-log-buffer)))
  (goto-char (point-max)))


(defun p4/kill-log-buffer()
  "Kill the log buffer."
  (interactive)
  (if (bufferp p4/log-buffer)
      (kill-buffer p4/log-buffer)))


(defun p4/get-info-buffer()
  "Return the buffer in `p4/log-buffer', if it does not exist or has been killed, it will
be recreated"
  (if (or (not p4/info-buffer) (not (buffer-live-p p4/info-buffer)))
      (setq p4/info-buffer (generate-new-buffer (generate-new-buffer-name "*p4/info*")))
    (with-current-buffer p4/info-buffer  (p4/log-mode)))
  p4/info-buffer)


(defun p4/run-with(filter sentinal &rest args)
  "Run p4 with a given filter sentinal and arguments put output in `p4/log-buffer'"
  (if (listp (first args)) (setq args (first args)))
  (lexical-let ((p4Proc (apply 'start-process "p4Proc" (p4/get-log-buffer)
			       p4/executable args)))
    (if sentinal (set-process-sentinel p4Proc sentinal))
    (if filter (set-process-filter p4Proc filter))
    p4Proc))


(defun p4/ediff-head()
  "Get the latest revision of the current buffer's file and compare it to current buffer"
  (interactive)
  (let ((head-buff (generate-new-buffer
		    (generate-new-buffer-name (concat "ediff#head#" (buffer-name)))))
	(ediff-split-window-function 'split-window-horizontally))
    (if (shell-command (concat "p4 print -q " (buffer-file-name) "#head") head-buff)
	(progn (with-current-buffer head-buff
		 (text-mode)
		 (if >=emacs24 (read-only-mode t))) ;; temp hack
	       (ediff-buffers head-buff (current-buffer))))))


(defun p4/reopen-at-default()
  "Run p4 reopen on the current buffer to move it to the default changelist"
  (interactive)
  (p4/run-with 'p4/standard-filter nil "reopen" "-c" "default" (buffer-file-name)))


(defun p4/standard-filter(proc str)
  "Outputs text to the `p4/log-buffer' in `p4/log-mode' formatting"
  (if (string-match ".*\\(Unknown command\\|p4 help simple\\).*" str)
      (p4/log t (concat "--ERROR: Unable to do anything with '"
			(p4/simple-proc-command proc) "'"))
    (if (string-match ".*nothing changed.*" str)
	(progn (p4/log nil (concat "--Exec: " (p4/simple-proc-command proc)))
	       (p4/log t "nothing changed"))
      (p4/log nil (concat "--Exec: " (p4/simple-proc-command proc) "\n"str)))))


(defun p4/gui()
  "Opens a p4 gui."
  (interactive)
  (start-process "p4gui" nil p4/gui-application))


(defun p4/run-like(arg)
  "Interactively run p4, output will go to the `p4/log-buffer'

Ex:
  (p4/run-like \"changes -u jorbi -m3\")

  M-x p4/run-like -> run p4 with args: changes -u jorbi -m3 "
  (interactive (list (read-string "run p4 with args: ")))
  (if (listp arg) (setq arg (first arg)))
  (p4/run-with 'p4/standard-filter nil (split-string arg)))


(defun p4/simple-proc-command(proc)
  "Returns the nice string representation of a process."
  (reduce (lambda(a b) (concat a " " b)) (process-command proc)))


(defvar p4/log-font-lock-keywords
  (list
   '("^--p4/log" . font-lock-type-face)
   '("^###.*$" . font-lock-comment-face)
   '("\\(^[a-zA-Z][a-zA-Z \-]+\\)\\(: \\)\\(.+$\\)" 1 font-lock-keyword-face)
   '("\\(^[a-zA-Z][a-zA-Z \-]+\\)\\(: \\)\\(.+$\\)" 3 font-lock-string-face)
   '("\\(^Change \\)\\(default\\|[0-9]+\\)" 1 font-lock-constant-face)
   '("\\(^Change \\)\\(default\\|[0-9]+\\)" 2 font-lock-preprocessor-face)
   '("\\( by \\)\\([^ ]+\\)\\(@\\)\\([^ ]+\\)" 2 font-lock-type-face)
   '("\\( by \\)\\([^ ]+\\)\\(@\\)\\([^ ]+\\)" 4 font-lock-string-face)
   '("\\(^--p4/log\\)\\(\|\\)\\(.*$\\)" 2 font-lock-function-name-face)
   '("\\(^--p4/log\\)\\(|\\)\\(.*$\\)" 3 font-lock-keyword-face)
   '("\\(^--Exec:\\)\\(.*$\\)" 1 font-lock-type-face)
   '("\\(^--ERROR:\\)\\(.*$\\)" . font-lock-warning-face)
   '("\\(^--Exec:\\)\\(.*$\\)" 2 font-lock-function-name-face))
  "Font lock keywoards for `p4/log-mode' which is active in the `p4/info-buffer' and `p4/log-buffer'.")


(define-derived-mode p4/log-mode text-mode "p4-log"
  "Major mode for highlighting the p4 log"
  (define-key p4/log-mode-map (kbd "q") 'delete-window)
  (set (make-local-variable 'font-lock-defaults) '(p4/log-font-lock-keywords)))


(provide 'perforger)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; perforger.el ends here
