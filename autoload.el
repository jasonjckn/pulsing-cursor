;;; ui/pulse-cursor/autoload.el -*- lexical-binding: t; -*-



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
