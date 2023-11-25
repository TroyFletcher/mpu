;; MPU is your emacs voice assistant
;; designed to work with speech to text on cell phone
;; evaluates instructions on period such as:
;;   remind me to do to the store at 9am period
;;   set a timer for 30 minutes period
;; mpu-mode echos your instruction, then prints mpu's response
;; and optionally, reads the response back with espeak
;; REQUIRES: evil-mode (but easily avoided)

(define-minor-mode mpu-mode
"provide mpu line instruction processing for buffer on full stop

The following shortcuts are available mode:

.             'mpu-line-read
"
  :lighter " MPU-mode"
  :keymap (let ((map (make-sparse-keymap)))
	    ;; (keymap-set map "C-f" 'forward-char)
            (define-key map (kbd ".") 'mpu-line-read)
            map))

(provide 'mpu-mode)

;; (buffer-substring-no-properties (line-beginning-position) (line-end-position))

(defun mpu-verbalize (response)
  "take response string and reply using requested method(s)"
  (insert (response))
  (start-process "response" nil "espeak" (concat "\"" response "\""))
  )

(defun mpu-response-method (response)
  "take response string and reply using requested method(s)"
  (insert (concat "MPU> " response))
  (setq speed "175")
  (setq slow-speed "15")
  (start-process "mpu espeak response process" nil "espeak" "-s" speed (concat "\"" response "\""))
  )

(defun mpu-respond (instruction)
  "read current line and evaluate it as an instruction"
  (cond ((string-equal instruction "tell me a joke") "two bars walk into a rabbis")
	((string-equal instruction "how are you") "good")
	(t "unknown instruction")) ;; default response
  )

(defun mpu-line-read ()
  "read current line and evaluate it as an instruction"
  (interactive)
  (setq response (mpu-respond (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
  (self-insert-command 1)
  ;; (message (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (newline)
  (mpu-response-method response)
  (newline)
  (evil-normal-state)
  (evil-open-above 1)
  )

