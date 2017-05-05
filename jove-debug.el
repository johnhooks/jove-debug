;;;; jove-debug.el --- The Jove Mode Debugging Tools -*- lexical-binding: t; -*-

;;; Copyright (C) 2017 John Hooks

;; Auther: John Hooks
;; URL: https://github.com/johnhooks/jove-debug
;; Keywords: debugging
;; Package-Requires: ((jove-mode "0.2.0"))

;; This file is part of Jove Debug.
;;
;; Jove Debug is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Jove Debug is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Jove Debug.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'jove-mode)

(defvar jove-debug-buffer-name "*JoveDebug*"
  "The name of the Jove Debug Log buffer.")

(defun jove-debug-scroll-to-bottom (window)
  (let ((buffer-lines (count-screen-lines (point-min) (point-max)))
        (window-lines (window-body-height window)))
    (save-excursion
      (when (< window-lines buffer-lines)
        ;; dont use goto-line because it messes with mark,
        (goto-char (point-min))
        (vertical-motion (- buffer-lines window-lines) window)
        (set-window-start window (point))))))

;; TODO: Make seperate logs for different buffers, and allow jumping
;; back and forth from the ast and the editing buffer.

(defun jove-debug-log (arg)
  "Print ARG to *jove-debug-log* buffer, inserts newline."
  (let ((buffer (get-buffer jove-debug-buffer-name))
        (inhibit-read-only t))          ; Allow writing to the buffer.
    (unless buffer
      (save-current-buffer
        (set-buffer (get-buffer-create jove-debug-buffer-name))
        (jove-debug-mode)))
    (save-current-buffer
      (set-buffer (get-buffer jove-debug-buffer-name))
      (goto-char (point-max))
      (if (jove-node-p arg)
          (jove-debug-stringify-inner arg)
        arg)
      (insert "\n" )
      (jove-debug-scroll-to-bottom (get-buffer-window jove-debug-buffer-name)))))

(defun jove-debug-log-clear ()
  "Clear the *jove-debug-log* buffer."
  (interactive)
  (save-current-buffer
    (set-buffer (get-buffer-create jove-debug-buffer-name))
    (erase-buffer)))

(defun jove-debug-stringify (node)
  "Use a temperary buffer as a string builder."
  ;; Unnecessary if writing to the *JoveDebug* buffer.
  (let ((string nil))
    (with-temp-buffer
      (jove-debug-stringify-inner node)
      (setq string (buffer-substring (point-min)
                                     (point-max))))
    string))

(defun jove-debug-stringify-inner (node &optional padding)
  (when (null padding)
    (setq padding 0))
  (let ((bol (point))
        (start (jove-start node))
        (end (jove-end node))
        (type (jove-type node)))
    (insert (format "%s%s,%s"
                    (make-string padding #x20)
                    start
                    end))
    (let ((start-pos (point)))
      (insert (format " %s" type))
      (put-text-property start-pos
                         (point)
                         'font-lock-face
                         'font-lock-type-face))
    (let ((start-pos (point))
          (value (jove-get-prop node :value)))
      (when value
        (progn
          (insert " ")
          (setq start-pos (point))
          (insert value)
          (put-text-property start-pos
                             (point)
                             'font-lock-face
                             'font-lock-variable-name-face)))
      (when (or (not start)
                (not end)
                (not type))
        (put-text-property bol
                           (point)
                           'font-lock-face
                           'font-lock-warning-face))))
  (insert "\n")
  (dolist (child (jove-children node))
    (jove-debug-stringify-inner child (+ 2 padding))))

(define-derived-mode jove-debug-mode special-mode "\u26A1JoveDebug"
  "A mode for the Jove Debug Log buffer."

  ;; Highlighting is handled by the printing function.
  (setq-local font-lock-defaults '(nil t))
  (setq-local syntax-propertize-function nil))

(provide 'jove-debug)

;;; jove-debug.el ends here
