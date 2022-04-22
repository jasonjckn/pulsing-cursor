;;; ui/pulse-cursor/config.el -*- lexical-binding: t; -*-

(require 'pulse)

(defface pulse-cursor-overlay-face1
  '((((class color) (background light))
     :background "#FF1D8F")
    (((class color) (background dark))
     :background "#FF1D8F"))
  "")

(defvar pulse-cursor-idle-timer nil
  "Timer started after `pulse-cursor-delay' seconds of Emacs idle time.
The function `pulse-cursor-start' is called when the timer fires.")

(defvar pulse-cursor-timer nil
  "Timer started from `pulse-cursor-start'.
This timer calls `pulse-cursor-timer-function' every
`pulse-cursor-interval' seconds.")

(defgroup pulse-cursor nil
  "Displaying text cursors."
  :version "21.1"
  :group 'frames)

(defcustom pulse-cursor-delay 0.5
  "Seconds of idle time before the first blink of the cursor.
Values smaller than 0.2 sec are treated as 0.2 sec."
  :type 'number
  :group 'pulse-cursor
  :set (lambda (symbol value)
         (set-default symbol value)
         (when pulse-cursor-idle-timer (pulse-cursor--start-idle-timer))))

(defcustom pulse-cursor-interval 0.5
  "Length of cursor blink interval in seconds."
  :type 'number
  :group 'pulse-cursor
  :set (lambda (symbol value)
         (set-default symbol value)
         (when pulse-cursor-timer (pulse-cursor--start-timer))))

(defcustom pulse-cursor-blinks 10
  "How many times to blink before using a solid cursor on NS, X, and MS-Windows.
Use 0 or negative value to blink forever."
  :version "24.4"
  :type 'integer
  :group 'pulse-cursor)

(defvar pulse-cursor-blinks-done 1
  "Number of blinks done since we started blinking on NS, X, and MS-Windows.")

(defun pulse-cursor--start-idle-timer ()
  "Start the `pulse-cursor-idle-timer'."
  (when pulse-cursor-idle-timer (cancel-timer pulse-cursor-idle-timer))
  (setq pulse-cursor-idle-timer
        ;; The 0.2 sec limitation from below is to avoid erratic
        ;; behavior (or downright failure to display the cursor
        ;; during command execution) if they set pulse-cursor-delay
        ;; to a very small or even zero value.
        (run-with-idle-timer (max 0.2 pulse-cursor-delay)
                             :repeat #'pulse-cursor-start)))

(defun pulse-cursor--start-timer ()
  "Start the `pulse-cursor-timer'."
  (when pulse-cursor-timer (cancel-timer pulse-cursor-timer))
  (setq pulse-cursor-timer
        (run-with-timer pulse-cursor-interval pulse-cursor-interval
                        #'pulse-cursor-timer-function)))

(defun pulse-cursor-start ()
  "Timer function called from the timer `pulse-cursor-idle-timer'.
This starts the timer `pulse-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null pulse-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; pulse-cursor-end is not added to pre-command-hook.
    (setq pulse-cursor-blinks-done 1)
    (pulse-cursor--start-timer)
    (add-hook 'pre-command-hook 'pulse-cursor-end)
    (internal-show-cursor nil nil)
    ))

(defun pulse-cursor-timer-function ()
  "Timer function of timer `pulse-cursor-timer'."

  (internal-show-cursor nil (not (internal-show-cursor-p)))

  (pulse-momentary-highlight-region (point) (+ 1 (point)) 'pulse-cursor-overlay-face1)
  ;;(pulse-momentary-highlight-region (point-min) (point-max))

  ;; Suspend counting blinks when the w32 menu-bar menu is displayed,
  ;; since otherwise menu tooltips will behave erratically.
  (or (and (fboundp 'w32--menu-bar-in-use)
	   (w32--menu-bar-in-use))
      (setq pulse-cursor-blinks-done (1+ pulse-cursor-blinks-done)))
  ;; Each blink is two calls to this function.
  (when (and (> pulse-cursor-blinks 0)
             (<= (* 2 pulse-cursor-blinks) pulse-cursor-blinks-done))
    (pulse-cursor-suspend)
    (add-hook 'post-command-hook 'pulse-cursor-check)))

(defun pulse-cursor-end ()
  "Stop cursor blinking.
This is installed as a pre-command hook by `pulse-cursor-start'.
When run, it cancels the timer `pulse-cursor-timer' and removes
itself as a pre-command hook."
  (remove-hook 'pre-command-hook 'pulse-cursor-end)

  (internal-show-cursor nil t)

  (when pulse-cursor-timer
    (cancel-timer pulse-cursor-timer)
    (setq pulse-cursor-timer nil)))

(defun pulse-cursor-suspend ()
  "Suspend cursor blinking.
This is called when no frame has focus and timers can be suspended.
Timers are restarted by `pulse-cursor-check', which is called when a
frame receives focus."
  (pulse-cursor-end)
  (when pulse-cursor-idle-timer
    (cancel-timer pulse-cursor-idle-timer)
    (setq pulse-cursor-idle-timer nil)))

(defun pulse-cursor--should-blink ()
  "Determine whether we should be blinking.
Returns whether we have any focused non-TTY frame."
  (and pulse-cursor-mode
       (let ((frame-list (frame-list))
             (any-graphical-focused nil))
         (while frame-list
           (let ((frame (pop frame-list)))
             (when (and (display-graphic-p frame) (frame-focus-state frame))
               (setf any-graphical-focused t)
               (setf frame-list nil))))
         any-graphical-focused)))

(defun pulse-cursor-check ()
  "Check if cursor blinking shall be restarted.
This is done when a frame gets focus.  Blink timers may be
stopped by `pulse-cursor-suspend'.  Internally calls
`pulse-cursor--should-blink' and returns its result."
  (let ((should-blink (pulse-cursor--should-blink)))
    (when (and should-blink (not pulse-cursor-idle-timer))
      (remove-hook 'post-command-hook 'pulse-cursor-check)
      (pulse-cursor--start-idle-timer))
    should-blink))

(defun pulse-cursor--rescan-frames (&optional _ign)
  "Called when the set of focused frames changes or when we delete a frame."
  (unless (pulse-cursor-check)
    (pulse-cursor-suspend)))

(define-minor-mode pulse-cursor-mode
  "Toggle cursor blinking (Blink Cursor mode).

If the value of `pulse-cursor-blinks' is positive (10 by default),
the cursor stops blinking after that number of blinks, if Emacs
gets no input during that time.

See also `pulse-cursor-interval' and `pulse-cursor-delay'.

This command is effective only on graphical frames.  On text-only
terminals, cursor blinking is controlled by the terminal."
  :init-value (not (or noninteractive
		       no-blinking-cursor
		       (eq system-type 'ms-dos)
		       (not (display-blink-cursor-p))))
  ;;:initialize 'custom-initialize-delay
  :group 'pulse-cursor
  :global t
  (pulse-cursor-suspend)
  (remove-hook 'after-delete-frame-functions #'pulse-cursor--rescan-frames)
  (remove-function after-focus-change-function #'pulse-cursor--rescan-frames)
  (when pulse-cursor-mode
    (add-function :after after-focus-change-function #'pulse-cursor--rescan-frames)
    (add-hook 'after-delete-frame-functions #'pulse-cursor--rescan-frames)
    (pulse-cursor--start-idle-timer)))

(setq blink-cursor-alist '((box . nil)))
(setq pulse-cursor-interval 0.5)
(setq pulse-cursor-blinks 0)
(setq pulse-cursor-delay 0.5)

(when (display-graphic-p)
  (pulse-cursor-mode +1))

(when nil
  (pulse-cursor-mode +1)
  (pulse-cursor-mode -1)
  (blink-cursor-mode +1)
  (blink-cursor-mode -1)

  (internal-show-cursor nil t)
  (internal-show-cursor nil nil)

  (blink-cursor-mode +1)
  (setq blink-cursor-alist '((box . hollow)))
  (setq blink-cursor-interval 0.25)
  (setq blink-cursor-blinks 20)
  )

(provide 'pulse-cursor)
