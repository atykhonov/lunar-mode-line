;;; lunar-mode-line.el --- display lunar phase in mode line of Emacs -*-coding: utf-8 -*-

;; Copyright (C) 2006 Ben Voui

;; Author: Ben Voui <intrigeri@boum.org>
;; Maintainer: intrigeri@boum.org
;; Keywords: extensions
;; Status: Works in Emacs 22
;; Created: 2006-05-22

;; URL: darcs get --partial https://gaffer.boum.org/darcs/pub/lunar-mode-line/

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

;; Inspired by battery.el, by Ralph Schleicher <rs@nunatak.allgaeu.org>.

;;; Code:

(require 'calendar)
(require 'lunar)
(require 'timer)

(defvar lunar-mode-line-string nil
  "String to display in the mode line.")
;;;###autoload (put 'lunar-mode-line-string 'risky-local-variable t)

(defgroup lunar-mode-line nil
  "Display lunar phase in mode line of Emacs."
  :group 'modeline)

(defcustom lunar-mode-line-update-interval 3600
  "*Seconds between updates of lunar phase in the mode line."
  :type 'integer
  :group 'lunar-mode-line)

(defcustom lunar-mode-line-prefix ""
  "Text to display before the lunar phase icon in the mode-line."
  :type 'string
  :group 'lunar-mode-line)

(defcustom lunar-mode-line-suffix ""
  "Text to display after the lunar phase icon in the mode-line."
  :type 'string
  :group 'lunar-mode-line)

(defvar lunar-mode-line-timer nil
  "Interval timer object.")

;;;###autoload
(defun display-lunar-phase ()
  "Display lunar phase information in the echo area."
  (interactive)
  (message "%s : %s"
	   (lunar-mode-line-current-phase-name)
	   (lunar-mode-line-current-phase-text-representation)))

;;;###autoload
(define-minor-mode display-lunar-phase-mode
  "Toggle display of lunar phase information in the mode line.
With a numeric arg, enable this display if arg is positive.

The mode line will be updated automatically every
`lunar-mode-line-update-interval' seconds."
  :global t :group 'lunar-mode-line
  (setq lunar-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and lunar-mode-line-timer (cancel-timer lunar-mode-line-timer))
  (if (not display-lunar-phase-mode)
      (setq global-mode-string
	    (delq 'lunar-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'lunar-mode-line-string t)
    (setq lunar-mode-line-timer (run-at-time nil lunar-mode-line-update-interval
					     'lunar-mode-line-update-handler))
    (lunar-mode-line-update)))

(defun lunar-mode-line-update-handler ()
  (lunar-mode-line-update)
  (sit-for 0))

(defun lunar-mode-line-update ()
  "Update lunar phase information in the mode line."
  (setq lunar-mode-line-string
	(propertize 
	 (concat
	  lunar-mode-line-prefix
	  (lunar-mode-line-current-phase-text-representation)
	  lunar-mode-line-suffix)
	 'help-echo "Lunar phase information"))
  (force-mode-line-update))

(defvar lunar-mode-line-text-representation-alist
  '((0 . "(/)")
    (1 . "|)")
    (2 . "(·)")
    (3 . "(|"))
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

(provide 'lunar-mode-line)

;;; lunar-mode-line.el ends here
