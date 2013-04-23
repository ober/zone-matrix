;; -*- Emacs-Lisp -*-
;; zone-matrix.el --- The matrix screen saver on Emacs.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <dylan.wen.dw@gmail.com>
;; Created: Jan 25, 2011
;; Time-stamp: <2011-09-03 11:38>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `zone-matrix' is a Matrix screen saver for Emacs.
;;
;; A note on code:
;; EC represents "Encoded Code", which is a short list of chars highlighted
;; green in column on the screen, according to the film The Matrix.
;; An ec represent one list of that chars.

;;; Code:


(require 'zone)
(eval-when-compile (require 'cl))


(defgroup zone-matrix nil
  "Matrix screen saver for Emacs."
  :group 'faces
  :prefix "zone-matrix-")


(defface zone-matrix-ec-body-face
  '((((class grayscale) (background dark)) (:foreground "green"))
    (((class color) (min-colors 88)) (:foreground "#39C139"))
    (((class color) (min-colors 16)) (:foreground "#39C139"))
    (((class color) (min-colors 8)) (:foreground "green"))
    (t :inherit default :foreground "green"))
  "*Face used to highlight the ec body."
  :group 'zone-matrix)


(defface zone-matrix-ec-head-face-1
  '((((class grayscale) (background dark))
     (:bold t :foreground "green" :weight bold))
    (((class color) (min-colors 88))
     (:bold t :foreground "#B5E4B3" :weight bold))
    (((class color) (min-colors 16))
     (:bold t :foreground "#B5E4B3" :weight bold))
    (((class color) (min-colors 8))
     (:bold t :foreground "green" :weight bold))
    (t :bold t :inherit default :foreground "green" :weight bold))
  "*Face used to highlight the ec head."
  :group 'zone-matrix)


(defface zone-matrix-ec-head-face-2
  '((((class grayscale) (background dark))
     (:bold t :foreground "green" :weight bold))
    (((class color) (min-colors 88))
     (:bold t :foreground "lightgreen" :weight bold))
    (((class color) (min-colors 16))
     (:bold t :foreground "lightgreen" :weight bold))
    (((class color) (min-colors 8))
     (:bold t :foreground "green" :weight bold))
    (t :bold t :inherit default :foreground "green" :weight bold))
  "*Face used to highlight the ec head."
  :group 'zone-matrix)


(defcustom zone-matrix-ec-head-face-table
  '(zone-matrix-ec-head-face-1
    zone-matrix-ec-head-face-2)
  "*Face table of ec head face."
  :group 'zone-matrix)


(defface zone-matrix-ec-tail-face
  '((((class grayscale) (background dark))
     (:foreground "green" :weight light))
    (((class color) (min-colors 88))
     (:foreground "darkgreen" :weight light))
    (((class color) (min-colors 16))
     (:foreground "darkgreen" :weight light))
    (((class color) (min-colors 8))
     (:foreground "green" :weight light))
    (t :inherit default :foreground "green" :weight light))
  "*Face used to highlight the ec tail."
  :group 'zone-matrix)


(defcustom zone-matrix-reflesh-time 0.02
  "*The time to wait for before the next screen reflesh."
  :group 'zone-matrix)


(defcustom zone-matrix-reflesh-time-rectangle 0.02
  "*The time to wait for before the next screen reflesh (rectangle implementation)."
  :group 'zone-matrix)


(defcustom zone-matrix-reflesh-speed-factor 1.5
  "*The factor of ec movement speed on every screen reflesh.")


(defcustom zone-matrix-ec-max-length 30
  "*The maximum length of ec."
  :group 'zone-matrix)


(defcustom zone-matrix-ec-max-number 10
  "*The maximum number of generating ec.
A \"generating ec\" is an ec which is still partly hided in top of screen."
  :group 'zone-matrix)


(defvar zone-matrix-ec-state-alist nil
  "The internal state of all generating ec.
Used by `zone-matrix' internally. Never set it by hand.")


(defvar zone-matrix-ec-start-pross 3
  ;; TODO add doc string
  "")


(defvar zone-matrix-ec-hl-pross 2
  ;; TODO add doc string
  "")


(defvar zone-matrix-ec-end-pross 15
  ;; TODO add doc string
  "")


(defun zone-matrix-check-environ ()
  "Check the environment variable settings."
  (unless (and (> zone-matrix-reflesh-time 0)
               ;;(> zone-matrix-reflesh-speed-factor 1)
               (> zone-matrix-ec-max-length 1)
               (> zone-matrix-ec-max-number 1))
    (error "error in function `zone-matrix': wrong setting.")))



;; "バベビボブダデヂドヅファフェフィフォフガゲギゴグハヘヒホフジャジェジジョジュカケキコクラレリロルマメミモムンナネニノヌパペピポプラレリロルサセシソスタッテチトツヴヴァヴェヴィヴォヴワヱヰヲウァェィォゥヤイェヨyイABCDEFGHIGKLMNOPQRSTUVWXYZ\
(defcustom zone-matrix-char-table
 "イABCDEFGHIGKLMNOPQRSTUVWXYZ\
abcdefghigklmnopqrstuvwxyz\
0123456789"
  "*Char table to form ec."
  :group 'zone-matrix)


(defun zone-matrix-generate-random (table)
  "Return a random element in TABLE. TABLE is a sequence."
  :group 'zone-matrix
  (elt table
       (random (length table))))


(defun zone-matrix-generate-random-char ()
  "Return a random char to form ec."
  :group 'zone-matrix
  (zone-matrix-generate-random zone-matrix-char-table))


(defun zone-matrix-generate-random-ec-head-face ()
  "Return a random ec head face."
  :group 'zone-matrix
  (zone-matrix-generate-random zone-matrix-ec-head-face-table))


(defun zone-matrix ()
  "The Matrix screen saver of Eamcs based on `zone'."
  :group 'zone-matrix
  ;; Hide the mode line in order to avoid the fast reflesh of column value
  ;; at modeline when `column-mode' is on, which could be annoying.
  ;; In `zone-matrix' the point change a lot in a really fast speed.
  (zone-hiding-modeline
   ;;
   ;; Any proper message ouput to minibuffer sits here.
   ;; Personally I prefer to just clean it.
   (message "")
   ;; Reset the seed for built-in random generator
   (random t)
   ;; TODO add message before showing ec
   ;;
   (let* (;; To minus one from `(window-width)' to avoid
          ;; the continuation char '$' or '\' from displaying
          (win-width (1- (window-width)))
          ;; With modeline disabled, add one to `(window-height)'
          ;; when `tabbar-mode' is on, the tabbar takes up one line.
          ;; So the visual line is `(window-height)' minus one.
          (win-height (window-height))
          ;; Text content to filled out the buffer
          (text (make-string (* win-width win-height) ? ))
          ;; The number of column with char
          ;; (the last column would have the char newline)
          (char-column-number (1- win-width))
          (line-index 0)      ;; start from 0
          (column-index 0)    ;; start from 0
          quit)
     ;;
     ;; Simply check for environment variable settings
     (zone-matrix-check-environ)
     ;;
     ;; Initializations
     ;;
     ;; Add newlines into text
     (while (< line-index win-height)
       (aset text (1- (* (1+ line-index) win-width)) ?\n)
       (setq line-index (1+ line-index)))
     ;; Initialize the internal ec state alist
     (setq zone-matrix-ec-state-alist nil)
     (let ((ec-index 0))
       (while (< ec-index zone-matrix-ec-max-number)
         (setq zone-matrix-ec-state-alist
               (cons (copy-list `(-1 . 0)) zone-matrix-ec-state-alist))
         (setq ec-index (1+ ec-index))))
     ;;
     ;; Main process
     (condition-case nil
         (let ((reflesh-column-counter 0)
               (old-index 0)
               (new-index 0)
               (old-property nil)
               (ec-state nil)
               (ec-length 0)
               ;; Cursor position to move after screen reflesh
               (cursor-position 0))
           (while (not (input-pending-p))
             ;; Update every encoded code in column by random
             (setq reflesh-column-counter 0)
             (while (< reflesh-column-counter
                       (* char-column-number zone-matrix-reflesh-speed-factor))
               (setq column-index (random char-column-number))
               ;;
               ;; Move one position down for every char in current column
               (setq line-index (1- win-height))
               (while (> line-index 0)
                 (setq new-index (+ column-index (* line-index win-width))
                       old-index (+ column-index (* (1- line-index) win-width)))
                 ;; move face with its char
                 (aset text new-index (aref text old-index))
                 (put-text-property new-index (1+ new-index) 'face
                                    (get-text-property old-index 'face text)
                                    text)
                 (setq line-index (1- line-index)))
               ;;
               ;; A lightweight state machine of embedded switch implementation
               ;; to update the column at `column-index',
               ;; that is to add a new char and attach its face if necessary.
               (setq old-property (get-text-property old-index 'face text))
               (cond
                ;;
                ((equal old-property 'zone-matrix-ec-tail-face)
                 ;; A ec tail falls inside the screen
                 (aset text column-index ? )
                 ;; Update internal ec state alist
                 (setcdr (assoc column-index zone-matrix-ec-state-alist) 0)
                 (setcar (assoc column-index zone-matrix-ec-state-alist) -1)
                 (put-text-property column-index (1+ column-index)
                                    'face nil text))
                ;;
                ((member old-property zone-matrix-ec-head-face-table)
                 ;; A ec head falls inside the screen
                 (aset text column-index (zone-matrix-generate-random-char))
                 (setq cursor-position column-index)
                 (setq ec-state (assoc column-index zone-matrix-ec-state-alist))
                 (setq ec-length (1+ (cdr ec-state)))
                 (setcdr ec-state ec-length)
                 (put-text-property
                  column-index (1+ column-index) 'face
                  (cond
                   ((< ec-length (1- zone-matrix-ec-max-length))
                    (if (= (random zone-matrix-ec-hl-pross) 1)
                        (zone-matrix-generate-random-ec-head-face)
                      'zone-matrix-ec-body-face))
                   (t
                    'zone-matrix-ec-tail-face))
                  text))
                ;;
                ((equal old-property 'zone-matrix-ec-body-face)
                 ;; A ec body falls inside the screen
                 (aset text column-index (zone-matrix-generate-random-char))
                 (setq cursor-position column-index)
                 (setq ec-state (assoc column-index zone-matrix-ec-state-alist))
                 (setq ec-length (1+ (cdr ec-state)))
                 (setcdr ec-state ec-length)
                 (put-text-property
                  column-index (1+ column-index) 'face
                  (cond
                   ((< ec-length (1- zone-matrix-ec-max-length))
                    (if (= (random zone-matrix-ec-end-pross) 1)
                        'zone-matrix-ec-tail-face
                      'zone-matrix-ec-body-face))
                   (t
                    'zone-matrix-ec-tail-face))
                  text))
                ;;
                (t
                 ;; A space falls inside the screen
                 (cond
                  ((and (equal (random zone-matrix-ec-start-pross) 1)
                        (setq ec-state (assoc -1 zone-matrix-ec-state-alist)))
                   (aset text column-index (zone-matrix-generate-random-char))
                   (setq cursor-position column-index)
                   (setcar ec-state column-index)
                   (setcdr ec-state 1)
                   (put-text-property
                    column-index (1+ column-index) 'face
                    (zone-matrix-generate-random-ec-head-face)
                    text))
                  (t
                   (aset text column-index ? )))))
               (setq reflesh-column-counter (1+ reflesh-column-counter)))
             ;;
             ;; Clean the last screen
             (erase-buffer)
             ;; Insert the new content
             (insert text)
             ;; Move bace to the point of one column with new char in top line
             (goto-char (1+ cursor-position))
             ;;
             ;; Wait for some time to reflesh the screen
             (sit-for zone-matrix-reflesh-time)))
       (error (setq quit t)))))
  )


(defun zone-matrix-rectangle ()
  "The Matrix screen saver of Eamcs based on `zone' (rectangle implementation)."
  :group 'zone-matrix
  (zone-hiding-modeline
   (erase-buffer)
   (message "")
   (random t)
   (let* ((win-width (1- (window-width)))
          (win-height (window-height))
          (text (make-string (* win-width win-height) ? ))
          (char-column-number (1- win-width))
          (line-index 0)
          (column-index 0)
          quit)
     (zone-matrix-check-environ)
     (while (< line-index win-height)
       (aset text (1- (* (1+ line-index) win-width)) ?\n)
       (setq line-index (1+ line-index)))
     (insert text)
     (setq zone-matrix-ec-state-alist nil)
     (let ((ec-index 0))
       (while (< ec-index zone-matrix-ec-max-number)
         (setq zone-matrix-ec-state-alist
               (cons (copy-list `(-1 . 0)) zone-matrix-ec-state-alist))
         (setq ec-index (1+ ec-index))))
     (condition-case nil
         (let ((reflesh-column-counter 0)
               (old-property nil)
               (new-char nil)
               (ec-state nil)
               (ec-length nil))
           (while (not (input-pending-p))
             (setq reflesh-column-counter 0)
             (while (< reflesh-column-counter
                       (* char-column-number zone-matrix-reflesh-speed-factor))
               (setq column-index (random char-column-number))
               (goto-char (1+ column-index))
               (setq current-column
                     (delete-extract-rectangle
                      (point)
                      (+ (point) (* win-width (1-  win-height)) 1)))
               (setq old-property
                     (get-text-property 0 'face (nth 0 current-column)))
               (cond
                ((equal old-property 'zone-matrix-ec-tail-face)
                 (setq new-char (char-to-string
                                 (zone-matrix-generate-random-char)))
                 (setcdr (assoc column-index zone-matrix-ec-state-alist) 0)
                 (setcar (assoc column-index zone-matrix-ec-state-alist) -1)
                 (put-text-property 0 (length new-char) 'face nil new-char))
                ((member old-property zone-matrix-ec-head-face-table)
                 (setq new-char (char-to-string
                                 (zone-matrix-generate-random-char)))
                 (setq ec-state
                       (assoc column-index zone-matrix-ec-state-alist))
                 (setq ec-length (1+ (cdr ec-state)))
                 (setcdr ec-state ec-length)
                 (put-text-property
                  0 (length new-char) 'face
                  (cond
                   ((< ec-length (1- zone-matrix-ec-max-length))
                    (if (= (random zone-matrix-ec-hl-pross) 1)
                        (zone-matrix-generate-random-ec-head-face)
                      'zone-matrix-ec-body-face))
                   (t
                    'zone-matrix-ec-tail-face))
                  new-char))
                ((equal old-property 'zone-matrix-ec-body-face)
                 (setq new-char (char-to-string
                                 (zone-matrix-generate-random-char)))
                 (setq ec-state
                       (assoc column-index zone-matrix-ec-state-alist))
                 (setq ec-length (1+ (cdr ec-state)))
                 (setcdr ec-state ec-length)
                 (put-text-property
                  0 (length new-char) 'face
                  (cond
                   ((< ec-length (1- zone-matrix-ec-max-length))
                    (if (= (random zone-matrix-ec-end-pross) 1)
                        'zone-matrix-ec-tail-face
                      'zone-matrix-ec-body-face))
                   (t
                    'zone-matrix-ec-tail-face))
                  new-char))
                (t
                 (cond
                  ((and (equal (random zone-matrix-ec-start-pross) 1)
                        (setq ec-state (assoc -1 zone-matrix-ec-state-alist)))
                   (setq new-char (char-to-string
                                   (zone-matrix-generate-random-char)))
                   (setcar ec-state column-index)
                   (setcdr ec-state 1)
                   (put-text-property
                    0 (length new-char) 'face
                    (zone-matrix-generate-random-ec-head-face)
                    new-char))
                  (t
                   (setq new-char (char-to-string ? ))))))
               (setq current-column (cons new-char current-column))
               (setq current-column (nbutlast current-column))
               (insert-rectangle current-column)
               (goto-char (1+ column-index))
               (setq reflesh-column-counter (1+ reflesh-column-counter)))
             (sit-for zone-matrix-reflesh-time-rectangle)))
       (error setq quit t)))
   ))


(provide 'zone-matrix)
