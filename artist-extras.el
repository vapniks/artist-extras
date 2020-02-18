;;; artist-extras.el --- Extra functions for artist-mode

;; Filename: artist-extras.el
;; Description: Extra functions for artist-mode
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2020, Joe Bloggs, all rites reversed.
;; Created: 2020-02-14 16:39:57
;; Version: 0.2
;; Last-Updated: Fri Feb 14 16:49:15 2020
;;           By: Joe Bloggs
;;     Update #: 2
;; URL: https://github.com/vapniks/artist-extras
;; Keywords: extensions
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((anaphora "1.0.4") (artist "1.2.6") (cl "1.0") (dash "2.17.0"))
;;
;; Features that might be required by this library:
;;
;; anaphora artist cl dash
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; Extra functions for artist-mode
;;
;;
;;; Installation
;;
;; To make sure you have the most up-to-date version of this library it is best to install
;; using the emacs package system, with the appropriate repository added (e.g https://melpa.org/)
;;
;; To install without using a package manager:
;;
;;  - Put the library in a directory in the emacs load path, like ~/.emacs.d/
;;  - Add (require 'artist-extras) in your ~/.emacs file
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `artist-flip-vertically'
;;    Flip/reflect the selected region vertically.
;;    Keybinding: C-c C-a -
;;  `artist-flip-horizontally'
;;    Flip/reflect the selected region horizontally.
;;    Keybinding: C-c C-a |
;;  `artist-rotate'
;;    Rotate the selected rectangular region 90 degrees.
;;    With no prefix rotate 90 degrees clockwise, with a single prefix rotate 90 degrees anti-clockwise,
;;    and with a double prefix rotate 180 degrees.
;;    Keybinding: C-c C-a @
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;

;;
;; All of the above can be customized by:
;;      M-x customize-group RET artist-extras RET
;;


;;; Require
(require 'artist)
(require 'dash)
(require 'anaphora)
(eval-when-compile (require 'cl))

;;; Code:

(define-key artist-menu-map [flip-vertically]
  '(menu-item "Flip vertically" artist-flip-vertically
	      :help "Flip/reflect the selected rectangular region vertically."))
(define-key artist-mode-map "\C-c\C-a-" 'artist-flip-vertically)

(define-key artist-menu-map [flip-horizontally]
  '(menu-item "Flip horizontally" artist-flip-horizontally
	      :help "Flip/reflect the selected rectangular region horizontally."))
(define-key artist-mode-map "\C-c\C-a|" 'artist-flip-horizontally)

(define-key artist-menu-map [rotate]
  '(menu-item "Rotate 90 degrees" artist-rotate
	      :help "Rotate the selected rectangular region 90 degrees."))
(define-key artist-mode-map "\C-c\C-a@" 'artist-rotate)


(defun artist-rotate-chars-in-rect (rect &rest chrs)
  "Within RECT replace the first char in CHRS with the last char, and each subsequent char its predecessor.
RECT is a list of strings, i.e. a rectangle. CHRS is a list of integers, strings, or nil's;
If a member of CHRS is < 8 it is assumed to refer to a member of variable `artist-arrows', other integers refer
to chars, and strings should be single char strings. If any member of CHRS is nil then no replacement will
be done for that char, and the next char will be replaced with \" \"."
  (let ((strs (mapcar (lambda (c) (if (integerp c)
				      (if (< c 8)
					  (awhen (aref artist-arrows c) (char-to-string it))
					(char-to-string c))
				    c))
		      chrs))
	(tchrs (mapcar 'char-to-string (number-sequence 160 200)))
	(case-fold-search nil))
    (if (< (length strs) 2)
	rect
      (cl-loop for line in rect
	       for tchr = (cl-loop for chr in tchrs
				   while (string-match (regexp-quote chr) line)
				   finally return chr)
	       for tchr2 = tchr
	       do (cl-loop for chr in strs
			   if chr do (setq line (replace-regexp-in-string (regexp-quote chr)
									  tchr2
									  line t t)
					   tchr2 chr)
			   else do (setq tchr2 " "))
	       collect (replace-regexp-in-string (regexp-quote tchr)
						 tchr2
						 line t t)))))

(defun artist-multirotate-chars-in-rect (rect chrslsts)
  "For each list of chars in chrslsts, rotate among those chars in RECT.
CHRSLSTS should be a list of lists of chars as numbers or strings.
RECT should be a list of strings, i.e. a rectangle.
See `artist-rotate-chars-in-rect'."
  (cl-loop for chrs in chrslsts
	   for rect2 = rect then rect3
	   for rect3 = (apply 'artist-rotate-chars-in-rect rect2 chrs)
	   finally return rect3))

;;;###autoload
(defun artist-flip-vertically nil
  "Flip/reflect the selected rectangular region vertically."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
	   (end (region-end))
	   (rect (extract-rectangle start end)))
      (delete-extract-rectangle start end)
      (goto-char start)
      (insert-rectangle (artist-multirotate-chars-in-rect
			 (reverse rect)
			 '(("/" "\\") (2 6) (1 7) (3 5))))
      (setq deactivate-mark nil))))

;;;###autoload
(defun artist-flip-horizontally nil
  "Flip/reflect the selected rectangular region horizontally."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
	   (end (region-end))
	   (rect (extract-rectangle start end)))
      (delete-extract-rectangle start end)
      (goto-char start)
      (insert-rectangle (artist-multirotate-chars-in-rect
			 (cl-loop for line in rect collect (reverse line))
			 (list '("/" "\\") '(0 4) '(1 3) '(5 7)
			       (list artist-ellipse-left-char artist-ellipse-right-char))))
      (setq deactivate-mark nil))))

;;;###autoload
(defun artist-rotate (arg)
  "Rotate the selected rectangular region 90 degrees.
With no prefix rotate 90 degrees clockwise, with a single prefix rotate 90 degrees anti-clockwise,
and with a double prefix rotate 180 degrees."
  (interactive "p")
  (when (use-region-p)
    (let* ((start (region-beginning))
	   (end (region-end))
	   (rect (extract-rectangle start end))
	   (newrect (if (> arg 4)
			;; 180 degree rotation
			(artist-multirotate-chars-in-rect
			 (reverse (mapcar 'reverse rect))
			 (list '(0 4) '(2 6) '(1 5) '(3 7)
			       (list artist-ellipse-left-char artist-ellipse-right-char)))
		      ;; reflection about diagonal (\)
		      (let ((rect2 (mapcar (lambda (_)
					     (let ((tp rect))
					       (mapconcat
						(lambda (_)
						  (prog1
						      (substring-no-properties (car tp) 0 1)
						    (setf (car tp) (substring-no-properties (car tp) 1))
						    (setq tp (cdr tp))))
						rect nil)))
					   (string-to-list (car rect)))))
			(if (= arg 1)
			    ;; 90 degree clockwise rotation TODO!!
			    (artist-multirotate-chars-in-rect
			     (mapcar 'reverse rect2)
			     '(("|" "-") ("/" "\\") (6 4 2 0) (7 5 3 1)))
			  ;; 90 degree anti-clockwise rotation TODO!!
			  (artist-multirotate-chars-in-rect
			   (reverse rect2)
			   '(("|" "-") ("/" "\\") (0 2 4 6) (1 3 5 7))))))))
      (delete-extract-rectangle start end)
      (goto-char start)
      (insert-rectangle newrect)
      (setq deactivate-mark nil))))

;;;###autoload
(defalias 'artist-reflect-vertically 'artist-flip-vertically)

;;;###autoload
(defalias 'artist-reflect-horizontally 'artist-flip-horizontally)

(provide 'artist-extras)

;; (org-readme-sync)
;; (magit-push)

;;; artist-extras.el ends here
