;;; artist-extras.el --- Extra functions for artist-mode

;; Filename: artist-extras.el
;; Description: Extra functions for artist-mode
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2020, Joe Bloggs, all rites reversed.
;; Created: 2020-02-14 16:39:57
;; Version: 0.1
;; Last-Updated: Fri Feb 14 16:49:15 2020
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/artist-extras
;; Keywords: extensions
;; Compatibility: GNU Emacs 25.2.2
;; Package-Requires: ((artist "1.2.6") (dash "2.17.0") (cl-lib "1.0"))
;;
;; Features that might be required by this library:
;;
;; artist dash
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
;;    Keybinding: M-x artist-flip-vertically
;;  `artist-flip-horizontally'
;;    Flip/reflect the selected region horizontally.
;;    Keybinding: M-x artist-flip-horizontally
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
(eval-when-compile (require 'cl-lib))

;;; Code:

(cl-defun artist-swap-chars-in-rect (chr1 chr2 &optional rect (chr3 "¬"))
  "Swap all occurrences of CHR1 with CHR2 and vice-versa in RECT.
RECT is a list of strings, i.e. a rectangle."
  (cl-loop for line in rect
	   do (setq line
		    (replace-regexp-in-string (regexp-quote chr1)
					      chr3
					      line t t))
	   do (setq line
		    (replace-regexp-in-string (regexp-quote chr2)
					      chr1
					      line t t))
	   collect (replace-regexp-in-string (regexp-quote chr3)
					     chr2
					     line t t)))

;;;###autoload
(defun artist-flip-vertically nil
  "Flip/reflect the selected region vertically."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
	   (end (region-end))
	   (pt (point))
	   (rect (extract-rectangle start end)))
      (clear-rectangle start end)
      (goto-char start)
      (insert-rectangle (->> rect reverse
			     (artist-swap-chars-in-rect "/" "\\")
			     (artist-swap-chars-in-rect "^" "V")))
      (setq deactivate-mark nil))))

;;;###autoload
(defun artist-flip-horizontally nil
  "Flip/reflect the selected region horizontally."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
	   (end (region-end))
	   (rect (extract-rectangle start end)))
      (clear-rectangle start end)
      (goto-char start)
      (goto-char start)
      (insert-rectangle (cl-loop for line in (->> rect
						  (artist-swap-chars-in-rect "<" ">")
						  (artist-swap-chars-in-rect "/" "\\"))
				 collect (reverse line)))
      (setq deactivate-mark nil))))

;;;###autoload
(defalias 'artist-reflect-vertically 'artist-flip-vertically)

;;;###autoload
(defalias 'artist-reflect-horizontally 'artist-flip-horizontally)

(provide 'artist-extras)

;; (org-readme-sync)
;; (magit-push)

;;; artist-extras.el ends here
