;;; lunar-mode-line.el --- display lunar phase in mode line of Emacs -*-coding: utf-8 -*-

;; Copyright (C) 1997 Noah S. Friedman
;; Copyright (C) 2006 Ben Voui

;; Author: Ben Voui <intrigeri@boum.org>
;; Maintainer: intrigeri@boum.org
;; Keywords: extensions
;; Status: Works in Emacs 22
;; Created: 2006-05-22

;; $Id: lunar-mode-line.el 2 2006-05-23 17:22:58Z intrigeri $

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, you can either send email to this program's
;; maintainer or write to: The Free Software Foundation, Inc.; 59 Temple
;; Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by twiddle.el, by Noah S. Friedman, which was itself inspired by
;; a similar hack by Jim Blandy <jimb@cyclic.com>.

;;; Code:

(require 'calendar)
(require 'lunar)

(eval-and-compile
  (defconst lunar-mode-line-xemacs-p
    (save-match-data (string-match "XEmacs" (emacs-version))))
  (if lunar-mode-line-xemacs-p
      (require 'itimer)
    (require 'timer))
)

;; Customization

(defgroup lunar-mode-line nil
  "Display lunar phase in mode line of Emacs."
  :group 'modeline)

(defcustom lunar-mode-line-delay 3600
  "*Seconds between updates of lunar phase in the mode line."
  :type 'integer
  :group 'lunar-mode-line)

;; Internal lunar data

(defvar lunar-mode-line-string nil
  "String to display in the mode line.")

(defvar lunar-mode-line-timer nil
  "Interval timer object.")

(defvar lunar-mode-line-text-representation-alist
  '((0 . "(/)")
    (1 . "(|")
    (2 . "(·)")
    (3 . "|)"))
  "*Alist mapping phase numbers to the strings used to represent them in the mode-line.
  Note: 0: New Moon, 1: First Quarter Moon, 2: Full Moon, 3: Last Quarter Moon")

;; Aux. functions

(defun lunar-mode-line-current-phase ()
  (let* ((date (list (calendar-current-date)))
	 (day (extract-calendar-day (car date)))
	 (month (extract-calendar-month (car date)))
	 (year (extract-calendar-year (car date)))
	 (phase-list (lunar-phase-list month year))
	 (cur-phase (car phase-list))
	 (next-phase (car phase-list)))
    (while (calendar-date-compare next-phase date)
      (setq cur-phase (car phase-list))
      (setq phase-list (cdr phase-list))
      (setq next-phase (car phase-list)))
    (car (cdr (cdr cur-phase)))))

(defun lunar-mode-line-current-phase-name ()
  (lunar-phase-name (lunar-mode-line-current-phase)))

(defun lunar-mode-line-current-phase-text-representation ()
  (cdr (assoc (lunar-mode-line-current-phase) lunar-mode-line-text-representation-alist)))

(defun lunar-mode-line-timer-stop (timer)
  (cond (lunar-mode-line-xemacs-p
         (and (itimerp timer)
              (delete-itimer timer)))
        ((timerp timer)
         ;; If this function is called from the timer itself, the timer
         ;; object isn't present on timer-list so cancel-timer won't do
         ;; anything useful.  To work around this case, disable the timer
         ;; repeat so it will expire on its own.
         (timer-set-time timer '(0 0) 0)
         (cancel-timer timer))))

(defun lunar-mode-line-update ()
  "Update lunar phase information in the mode line."
  (setq lunar-mode-line-string
	(concat (lunar-mode-line-current-phase-text-representation) " "))
  (if lunar-mode-line-xemacs-p
      (redraw-modeline)
    (force-mode-line-update)))

;; Interactive functions


;;;###autoload
(defun lunar-mode-line-start ()
  "Start displaying lunar phase in mode line."
  (interactive)
  (lunar-mode-line-stop)
  (setq lunar-mode-line-string "")
  (add-to-list 'global-mode-string 'lunar-mode-line-string "append")
  (setq lunar-mode-line-timer
	(cond (lunar-mode-line-xemacs-p
	       (start-itimer "lunar-mode-line" 'lunar-mode-line-update 1 lunar-mode-line-delay))
	      (t
	       (apply 'run-with-timer 1 lunar-mode-line-delay (list 'lunar-mode-line-update))))))

(defun lunar-mode-line-stop ()
  "Stop displaying lunar phase in mode line."
  (interactive)
  (lunar-mode-line-timer-stop lunar-mode-line-timer)
  (setq lunar-mode-line-timer nil)
  (setq global-mode-string (delq 'lunar-mode-line-string global-mode-string))
  (setq lunar-mode-line-string nil)
  (lunar-mode-line-update))

(provide 'lunar-mode-line)

;;; lunar-mode-line.el ends here
