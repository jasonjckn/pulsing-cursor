;;; pulsing-cursor.el --- Emacs cursor animation fading in & out while idling.

;; Copyright (C) 2022 Jason Jackson

;; Author: Jason Jackson
;; Created: 22 Apr 2022
;; Homepage: https://github.com/jasonjckn/pulsing-cursor
;; Keywords: extensions
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: UNLICENSE
;; Version: 1.0

;;; Commentary:


;; Please see https://github.com/jasonjckn/pulsing-cursor for more
;; information.



;;;; Libraries

(require 'pulse)


;;;; User Configuration

(defgroup pulsing-cursor nil
  "Displaying text cursors."
  :version "21.1"
  :group 'frames)

(defvar pulsing-cursor-idle-timer nil
  "Timer started after `pulsing-cursor-delay' seconds of Emacs idle time.
The function `pulsing-cursor-start' is called when the timer fires.")

(defvar pulsing-cursor-timer nil
  "Timer started from `pulsing-cursor-start'.
This timer calls `pulsing-cursor-timer-function' every
`pulsing-cursor-interval' seconds.")

(defcustom pulsing-cursor-delay 0.5
  "Seconds of idle time before the first blink of the cursor.
Values smaller than 0.2 sec are treated as 0.2 sec."
  :type 'number
  :group 'pulsing-cursor
  :set (lambda (symbol value)
         (set-default symbol value)
         (when pulsing-cursor-idle-timer (pulsing-cursor--start-idle-timer))))

(defcustom pulsing-cursor-interval 0.5
  "Length of cursor blink interval in seconds."
  :type 'number
  :group 'pulsing-cursor
  :set (lambda (symbol value)
         (set-default symbol value)
         (when pulsing-cursor-timer (pulsing-cursor--start-timer))))

(defcustom pulsing-cursor-blinks 0
  "How many times to blink before using a solid cursor on NS, X, and MS-Windows.
Use 0 or negative value to blink forever."
  :version "24.4"
  :type 'integer
  :group 'pulsing-cursor)

(defvar pulsing-cursor-blinks-done 1
  "Number of blinks done since we started blinking on NS, X, and MS-Windows.")


(defface pulsing-cursor-overlay-face1
  '((((class color) (background light))
     :background "#FF1D8F")
    (((class color) (background dark))
     :background "#FF1D8F"))
  "Overlay face.")


;;; Code:

;;;###autoload
(defun pulsing-cursor--start-idle-timer ()
  "Start the `pulsing-cursor-idle-timer'."
  (when pulsing-cursor-idle-timer (cancel-timer pulsing-cursor-idle-timer))
  (setq pulsing-cursor-idle-timer
        ;; The 0.2 sec limitation from below is to avoid erratic
        ;; behavior (or downright failure to display the cursor
        ;; during command execution) if they set pulsing-cursor-delay
        ;; to a very small or even zero value.
        (run-with-idle-timer (max 0.2 pulsing-cursor-delay)
                             :repeat #'pulsing-cursor-start)))

;;;###autoload
(defun pulsing-cursor--start-timer ()
  "Start the `pulsing-cursor-timer'."
  (when pulsing-cursor-timer (cancel-timer pulsing-cursor-timer))
  (setq pulsing-cursor-timer
        (run-with-timer pulsing-cursor-interval pulsing-cursor-interval
                        #'pulsing-cursor-timer-function)))

;;;###autoload
(defun pulsing-cursor-start ()
  "Timer function called from the timer `pulsing-cursor-idle-timer'.
This starts the timer `pulsing-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null pulsing-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; pulsing-cursor-end is not added to pre-command-hook.
    (setq pulsing-cursor-blinks-done 1)
    (pulsing-cursor--start-timer)
    (add-hook 'pre-command-hook 'pulsing-cursor-end)
    (internal-show-cursor nil nil)
    ))

;;;###autoload
(defun pulsing-cursor-timer-function ()
  "Timer function of timer `pulsing-cursor-timer'."

  (internal-show-cursor nil (not (internal-show-cursor-p)))

  (pulse-momentary-highlight-region (point) (+ 1 (point)) 'pulsing-cursor-overlay-face1)
  ;;(pulse-momentary-highlight-region (point-min) (point-max))

  ;; Suspend counting blinks when the w32 menu-bar menu is displayed,
  ;; since otherwise menu tooltips will behave erratically.
  (or (and (fboundp 'w32--menu-bar-in-use)
           (w32--menu-bar-in-use))
      (setq pulsing-cursor-blinks-done (1+ pulsing-cursor-blinks-done)))
  ;; Each blink is two calls to this function.
  (when (and (> pulsing-cursor-blinks 0)
             (<= (* 2 pulsing-cursor-blinks) pulsing-cursor-blinks-done))
    (pulsing-cursor-suspend)
    (add-hook 'post-command-hook 'pulsing-cursor-check)))

;;;###autoload
(defun pulsing-cursor-end ()
  "Stop cursor blinking.
This is installed as a pre-command hook by `pulsing-cursor-start'.
When run, it cancels the timer `pulsing-cursor-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook 'pulsing-cursor-end)

  (internal-show-cursor nil t)

  (when pulsing-cursor-timer
    (cancel-timer pulsing-cursor-timer)
    (setq pulsing-cursor-timer nil)))

;;;###autoload
(defun pulsing-cursor-suspend ()
  "Suspend cursor blinking.
This is called when no frame has focus and timers can be suspended.
Timers are restarted by `pulsing-cursor-check', which is called when a
frame receives focus."
  (pulsing-cursor-end)
  (when pulsing-cursor-idle-timer
    (cancel-timer pulsing-cursor-idle-timer)
    (setq pulsing-cursor-idle-timer nil)))

;;;###autoload
(defun pulsing-cursor--should-blink ()
  "Determine whether we should be blinking.
Returns whether we have any focused non-TTY frame."
  (and pulsing-cursor-mode
       (let ((frame-list (frame-list))
             (any-graphical-focused nil))
         (while frame-list
           (let ((frame (pop frame-list)))
             (when (and (display-graphic-p frame) (frame-focus-state frame))
               (setf any-graphical-focused t)
               (setf frame-list nil))))
         any-graphical-focused)))

;;;###autoload
(defun pulsing-cursor-check ()
  "Check if cursor blinking shall be restarted.
This is done when a frame gets focus.  Blink timers may be
stopped by `pulsing-cursor-suspend'.  Internally calls
`pulsing-cursor--should-blink' and returns its result."
  (let ((should-blink (pulsing-cursor--should-blink)))
    (when (and should-blink (not pulsing-cursor-idle-timer))
      (remove-hook 'post-command-hook 'pulsing-cursor-check)
      (pulsing-cursor--start-idle-timer))
    should-blink))

;;;###autoload
(defun pulsing-cursor--rescan-frames (&optional _ign)
  "Called when the set of focused frames changes or when we delete a frame."
  (unless (pulsing-cursor-check)
    (pulsing-cursor-suspend)))

;;; -------------------------------------------------------------------------------
(defun pulsing-cursor--start-idle-timer ()
  "Start the `pulsing-cursor-idle-timer'."
  (when pulsing-cursor-idle-timer (cancel-timer pulsing-cursor-idle-timer))
  (setq pulsing-cursor-idle-timer
        ;; The 0.2 sec limitation from below is to avoid erratic
        ;; behavior (or downright failure to display the cursor
        ;; during command execution) if they set pulsing-cursor-delay
        ;; to a very small or even zero value.
        (run-with-idle-timer (max 0.2 pulsing-cursor-delay)
                             :repeat #'pulsing-cursor-start)))

(defun pulsing-cursor--start-timer ()
  "Start the `pulsing-cursor-timer'."
  (when pulsing-cursor-timer (cancel-timer pulsing-cursor-timer))
  (setq pulsing-cursor-timer
        (run-with-timer pulsing-cursor-interval pulsing-cursor-interval
                        #'pulsing-cursor-timer-function)))

(defun pulsing-cursor-start ()
  "Timer function called from the timer `pulsing-cursor-idle-timer'.
This starts the timer `pulsing-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null pulsing-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; pulsing-cursor-end is not added to pre-command-hook.
    (setq pulsing-cursor-blinks-done 1)
    (pulsing-cursor--start-timer)
    (add-hook 'pre-command-hook 'pulsing-cursor-end)
    (internal-show-cursor nil nil)))

(defun pulsing-cursor-timer-function ()
  "Timer function of timer `pulsing-cursor-timer'."

  (internal-show-cursor nil (not (internal-show-cursor-p)))

  (pulse-momentary-highlight-region (point) (+ 1 (point)) 'pulsing-cursor-overlay-face1)
  ;;(pulse-momentary-highlight-region (point-min) (point-max))

  ;; Suspend counting blinks when the w32 menu-bar menu is displayed,
  ;; since otherwise menu tooltips will behave erratically.
  (or (and (fboundp 'w32--menu-bar-in-use)
           (w32--menu-bar-in-use))
      (setq pulsing-cursor-blinks-done (1+ pulsing-cursor-blinks-done)))
  ;; Each blink is two calls to this function.
  (when (and (> pulsing-cursor-blinks 0)
             (<= (* 2 pulsing-cursor-blinks) pulsing-cursor-blinks-done))
    (pulsing-cursor-suspend)
    (add-hook 'post-command-hook 'pulsing-cursor-check)))


(defun pulsing-cursor--start-idle-timer ()
  "Start the `pulsing-cursor-idle-timer'."
  (when pulsing-cursor-idle-timer (cancel-timer pulsing-cursor-idle-timer))
  (setq pulsing-cursor-idle-timer
        ;; The 0.2 sec limitation from below is to avoid erratic
        ;; behavior (or downright failure to display the cursor
        ;; during command execution) if they set pulsing-cursor-delay
        ;; to a very small or even zero value.
        (run-with-idle-timer (max 0.2 pulsing-cursor-delay)
                             :repeat #'pulsing-cursor-start)))

(defun pulsing-cursor--start-timer ()
  "Start the `pulsing-cursor-timer'."
  (when pulsing-cursor-timer (cancel-timer pulsing-cursor-timer))
  (setq pulsing-cursor-timer
        (run-with-timer pulsing-cursor-interval pulsing-cursor-interval
                        #'pulsing-cursor-timer-function)))

(defun pulsing-cursor-start ()
  "Timer function called from the timer `pulsing-cursor-idle-timer'.
This starts the timer `pulsing-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null pulsing-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; pulsing-cursor-end is not added to pre-command-hook.
    (setq pulsing-cursor-blinks-done 1)
    (pulsing-cursor--start-timer)
    (add-hook 'pre-command-hook 'pulsing-cursor-end)
    (internal-show-cursor nil nil)))

(defun pulsing-cursor-timer-function ()
  "Timer function of timer `pulsing-cursor-timer'."

  (internal-show-cursor nil (not (internal-show-cursor-p)))

  (pulse-momentary-highlight-region (point) (+ 1 (point)) 'pulsing-cursor-overlay-face1)
  ;;(pulse-momentary-highlight-region (point-min) (point-max))

  ;; Suspend counting blinks when the w32 menu-bar menu is displayed,
  ;; since otherwise menu tooltips will behave erratically.
  (or (and (fboundp 'w32--menu-bar-in-use)
           (w32--menu-bar-in-use))
      (setq pulsing-cursor-blinks-done (1+ pulsing-cursor-blinks-done)))
  ;; Each blink is two calls to this function.
  (when (and (> pulsing-cursor-blinks 0)
             (<= (* 2 pulsing-cursor-blinks) pulsing-cursor-blinks-done))
    (pulsing-cursor-suspend)
    (add-hook 'post-command-hook 'pulsing-cursor-check)))

(defun pulsing-cursor-end ()
  "Stop cursor blinking.
This is installed as a pre-command hook by `pulsing-cursor-start'.
When run, it cancels the timer `pulsing-cursor-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook 'pulsing-cursor-end)

  (internal-show-cursor nil t)

  (when pulsing-cursor-timer
    (cancel-timer pulsing-cursor-timer)
    (setq pulsing-cursor-timer nil)))

(defun pulsing-cursor-suspend ()
  "Suspend cursor blinking.
This is called when no frame has focus and timers can be suspended.
Timers are restarted by `pulsing-cursor-check', which is called when a
frame receives focus."
  (pulsing-cursor-end)
  (when pulsing-cursor-idle-timer
    (cancel-timer pulsing-cursor-idle-timer)
    (setq pulsing-cursor-idle-timer nil)))

(defun pulsing-cursor--should-blink ()
  "Determine whether we should be blinking.
Returns whether we have any focused non-TTY frame."
  (and pulsing-cursor-mode
       (let ((frame-list (frame-list))
             (any-graphical-focused nil))
         (while frame-list
           (let ((frame (pop frame-list)))
             (when (and (display-graphic-p frame) (frame-focus-state frame))
               (setf any-graphical-focused t)
               (setf frame-list nil))))
         any-graphical-focused)))

(defun pulsing-cursor-check ()
  "Check if cursor blinking shall be restarted.
This is done when a frame gets focus.  Blink timers may be
stopped by `pulsing-cursor-suspend'.  Internally calls
`pulsing-cursor--should-blink' and returns its result."
  (let ((should-blink (pulsing-cursor--should-blink)))
    (when (and should-blink (not pulsing-cursor-idle-timer))
      (remove-hook 'post-command-hook 'pulsing-cursor-check)
      (pulsing-cursor--start-idle-timer))
    should-blink))

(defun pulsing-cursor--rescan-frames (&optional _ign)
  "Called when the set of focused frames changes or when we delete a frame."
  (unless (pulsing-cursor-check)
    (pulsing-cursor-suspend)))

(define-minor-mode pulsing-cursor-mode
  "Toggle cursor blinking (Blink Cursor mode).

If the value of `pulsing-cursor-blinks' is positive (10 by default),
the cursor stops blinking after that number of blinks, if Emacs
gets no input during that time.

See also `pulsing-cursor-interval' and `pulsing-cursor-delay'.

This command is effective only on graphical frames.  On text-only
terminals, cursor blinking is controlled by the terminal."
  :init-value (not (or noninteractive
                       no-blinking-cursor
                       (eq system-type 'ms-dos)
                       (not (display-blink-cursor-p))))
  ;;:initialize 'custom-initialize-delay
  :group 'pulsing-cursor
  :global t
  (pulsing-cursor-suspend)
  (remove-hook 'after-delete-frame-functions #'pulsing-cursor--rescan-frames)
  (remove-function after-focus-change-function #'pulsing-cursor--rescan-frames)
  (when pulsing-cursor-mode
    (blink-cursor-mode -1)
    (add-function :after after-focus-change-function #'pulsing-cursor--rescan-frames)
    (add-hook 'after-delete-frame-functions #'pulsing-cursor--rescan-frames)
    (pulsing-cursor--start-idle-timer)))


(pulsing-cursor-mode +1)

(when nil
  (pulsing-cursor-mode +1)
  (setq blink-cursor-alist '((box . hollow)))
  ;; (setq blink-cursor-alist '((box . nil)))
  (setq pulsing-cursor-interval 0.25)
  (setq pulsing-cursor-blinks 20))

(provide 'pulsing-cursor)

;;; pulsing-cursor.el ends here
