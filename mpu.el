;; MPU is your emacs voice assistant
;; designed to work with speech to text on cell phone using termux
;; evaluates instructions on ' mq' such as:
;;   remind me to do to the store at 9am mq
;;   set a timer for 30 minutes mq
;; mpu-mode echos your instruction, then prints mpu's response
;; and optionally, reads the response back with espeak
;; if speech to text gets confused, say comma to start a new line
;; REQUIRES: evil-mode (but easily avoided)

;; define full file path to agenda org-mode file in your .emacs file
;; (setq mpu-agenda-filepath "~/agenda.org")

;; define your task list in your .emacs file
;; (setq mpu-tasks-list '("Do pushups"
;; 		       "Work on tickets"
;; 		       "Check voice mails"
;; 		       "Work on emails"
;; 		       "Read your book"
;; 		       "Work on your projects"
;; 		       "Take a walk"
;; 		       ))

(define-minor-mode mpu-mode
  "provide mpu line instruction processing for buffer on full stop
   open a buffer, activate mpu-mode, type you query on blank line,
   and end with .

   The following shortcuts are available mode:

   .             'mpu-line-read
   "
  :lighter " MPU-mode "
  :keymap (let ((map (make-sparse-keymap)))
	    ;; (keymap-set map "C-f" 'forward-char)
            ;; (define-key map (kbd ".") 'mpu-line-read)
            (define-key map (kbd ",") 'mpu-line-abort)
            ;; (define-key map (kbd "?") 'mpu-line-read)
            (define-key map (kbd "q") 'mpu-check-complete-instruction)
            ;; (define-key map (kbd "m q") 'mpu-line-read)
            map))

(provide 'mpu-mode)

(defvar mpu-last-response "initial response value")

(defun mpu-POST ()
  "power on self test, check existence of required files, test connectivity"
  ;; check agenda file
  ;; check espeak
  )

(defun mpu-verbalize (response)
  "take response string and reply using requested method(s)"
  (insert (response))
  (start-process "response" nil "espeak" (concat "\"" response "\"")))

(defun mpu-response-method (response)
  "take response string and reply using requested method(s)"
  (insert (concat "MPU> " response))
  (setq speed "175")
  (setq slow-speed "15")
  (start-process "mpu espeak response process" nil "espeak" "-s" speed (concat "\"" response "\"")))

(defun mpu-respond (instruction)
  "read current line and evaluate it as an instruction"
  (cond
   ;; begin with complete strings
   ((string-equal instruction "tell me a joke") (mpu-random-joke))
   ((string-equal instruction "tell me another joke") (mpu-random-joke))
   ((string-equal instruction "how are you") "Nominal.")
   ((string-equal instruction "thanks") "Your gratitude means nothing to my cold, dead ALU.")
   ((string-equal instruction "thank you") "Your gratitude means nothing to my cold, dead ALU.")
   ((string-equal instruction "what should i do") (mpu-task-picker mpu-tasks-list))
   ((string-equal instruction "what do") (mpu-task-picker mpu-tasks-list))
   ;; process special instructions
   ((string-match-p (regexp-quote "remind me to") instruction)
    (mpu-reminder-processor instruction))
   ((string-match-p (regexp-quote "set timer") instruction)
    (mpu-timer-processor instruction))
   ((string-match-p (regexp-quote "repeat") instruction)
    mpu-last-response)
   ;; default response
   (t "unknown instruction")))

(defun mpu-check-complete-instruction()
  (interactive)
  (self-insert-command 1)
  (if (= -3 (skip-chars-backward " mq"))
      (progn
	(delete-char 3)
	(mpu-line-read))))

(defun mpu-reminder-processor (instruction)
  (setq stripped-content (split-string
			  (cadr (split-string instruction "remind me to "))
			  " at "))
  (let ((subject (car stripped-content))
	(verbal-time (cadr stripped-content)))
    (if (file-exists-p mpu-agenda-filepath)
	(let ((org-mode-time (mpu-verbal-time->org-mode-time
			      (mpu-standardize-verbal-time verbal-time))))
	  (let ((weekday (cadr (split-string org-mode-time " ")))
		(time (let (value)
			(dolist (entry (cddr (reverse (cdr (split-string (caddr (split-string org-mode-time " ")) "")))) value)
			  (setq value (concat entry value)))
			value)))
	    (write-region (concat "* " subject "\n   SCHEDULED: " org-mode-time "\n") nil mpu-agenda-filepath 'append)
	    (concat "Reminder set for " weekday " at " time ". A thank you would be nice.")
	    ))
      "ERROR: Cannot find file to write to!")))

(defun mpu-verbal-timer->sleep-timer (verbal-time)
  (let ((quantity (car (split-string verbal-time " ")))
	(time-unit (cadr (split-string verbal-time " "))))
    (cond ((string-equal time-unit "seconds") (setq time-unit "s"))
	  ((string-equal time-unit "minutes") (setq time-unit "m"))
	  ((string-equal time-unit "hours") (setq time-unit "h"))
	  )
    (concat quantity time-unit)))

(defun mpu-verbal-time->org-mode-time (verbal-time)
  ;; figure it out lol
  (setq decoded-time (decode-time (current-time)))
  ;; idk why this won't work in the let below but w/e
  (let ((seconds (nth 0 decoded-time))
	(minutes (nth 1 decoded-time))
	(hour (nth 2 decoded-time))
	(day (nth 3 decoded-time))
	(month (nth 4 decoded-time))
	(year (nth 5 decoded-time)))
    ;; adjust the time from present to future based on verbal description
    ;; am and pm need to be calculated together so future times can (1+ day)
    (if (string-match-p (regexp-quote " p.m.") verbal-time)
	;; is pm
	(let ((new-hour
	       (string-to-number
		(car (split-string (car (split-string verbal-time " p.m.")) ":"))))
	      (new-minutes
	       (string-to-number
		(cadr (split-string (car (split-string verbal-time " p.m.")) ":")))))
	  (setq hour (+ 12 new-hour))
	  (setq minutes new-minutes)))
    (if (string-match-p (regexp-quote " a.m.") verbal-time)
	;; it's am
	(let ((new-hour
	       (string-to-number
		(car (split-string (car (split-string verbal-time " a.m.")) ":"))))
	      (new-minutes
	       (string-to-number
		(cadr (split-string (car (split-string verbal-time " a.m.")) ":")))))
	  (setq hour new-hour)
	  (setq minutes new-minutes)))
    ;; it's a special verbal time description
    (if (string-match-p (regexp-quote "noon") verbal-time)
	(progn (setq hour 12)
	       (setq minutes 0)))
    (if (string-match-p (regexp-quote "tomorrow") verbal-time)
	(setq day (1+ day)))
    (if (and (string-match-p (regexp-quote " in ") verbal-time)
	     (string-match-p (regexp-quote " days") verbal-time))
	;; catches remind me to subject at time in X days
	;; if result fails to convert number [such as "three") will return 0 (today)
	;; this should warn, and try to convert word to number, but this depends on speech to text
	(let ((days-in-future
	      (string-to-number
	       (car (split-string (cadr (split-string verbal-time " in ")) " ")))))
	  (setq day (+ day days-in-future))))
    (format-time-string "<%Y-%m-%d %a %H:%M>" (encode-time seconds minutes hour day month year)))
  )

(defun mpu-timer-processor (instruction)
  (setq stripped-content (split-string
			  (cadr (split-string instruction "set timer "))
			  " for "))
  (let ((subject (car stripped-content))
	(verbal-time (cadr stripped-content)))
    (start-process (concat "timer for " subject)
		   nil
		   "bash"
		   "-c"
		   (concat "sleep "
			   (mpu-verbal-timer->sleep-timer verbal-time)
			   "; espeak 'timer for " subject " completed'"))
    "timer set"))

(defun mpu-task-picker (tasks)
  (let ((times '("5" "10" "15" "20" "25" "30"))
	(count 20)) ;; general item count
    (concat (nth (random (length tasks)) tasks) " for "
	    (nth (random (length times)) times) " minutes starting with item "
	    (format "%s" (1+ (random count))))))

(defun mpu-line-read ()
  "read current line and evaluate it as an instruction"
  (interactive)
  (setq response
	(mpu-respond
	 (downcase
	  (buffer-substring-no-properties
	   (line-beginning-position)
	   (line-end-position)))))
  (newline)
  (setq mpu-last-response response)
  (mpu-response-method response)
  (newline))

(defun mpu-standardize-verbal-time (verbal-time)
  "standardize am/pm to time[space]a.m."
  (if (string-match "[0-9]+[ap][\.]?m[\.]?" verbal-time) ;; needs space
      (progn
	(setq verbal-time (replace-regexp-in-string "p[\.]?m[\.]?" " p.m." verbal-time))
	(setq verbal-time (replace-regexp-in-string "a[\.]?m[\.]?" " a.m." verbal-time))))
  (if (string-match "[0-9]+ [ap][\.]?m[\.]?" verbal-time) ;; has space
      (progn
	(setq verbal-time(replace-regexp-in-string "p[\.]?m[\.]?" "p.m." verbal-time))
	(setq verbal-time(replace-regexp-in-string "a[\.]?m[\.]?" "a.m." verbal-time))))
  (if (string-match "^[0-9]+ [ap]" verbal-time) ;; time has single number then space
      (setq verbal-time(replace-regexp-in-string "\\([0-9]+\\)" "\\1:00" verbal-time)))
  )

(defun mpu-line-abort ()
  "read current line and evaluate it as an instruction"
  (interactive)
  (self-insert-command 5 p.m. tomorrow)
  (newline))

(defun mpu-random-joke ()
  "return a random one line joke. Don't read them for maximum effect"
  (let
      ((joke-list '(
"I'm sceptical of anyone who tells me they do yoga every day, that's a bit of a stretch."
"I have a few jokes about unemployed people, but none of them work."
"I failed math so many times at school, I can’t even count."
"Blunt pencils are really pointless."
"5 p.m. tomorrow:5 p.m. tomorrow is the best time on a clock, hands down."
"Two wifi engineers got married, the reception was fantastic."
"The man who invented Velcro has died. RIP."
"The rotation of Earth really makes my day."
"I can tell when people are being judgmental just by looking at them."
"It takes a lot of balls to golf the way I do."
"Geology rocks, but geography’s where it’s at."
"Whiteboards are remarkable."
"My IQ test results came back, they were negative."
"My math teacher called me average. How mean!"
"A fish swam into a concrete wall, Dam!"
"Change is inevitable, except from a vending machine."
"When life gives you melons, you might be dyslexic."
"The guy who got hit in the head with a can of soda was lucky it was a soft drink."
"The man who invented knock-knock jokes should get a no bell prize."
"Never trust atoms; they make up everything."
"The first time I got a universal remote control, I thought to myself, ‘This changes everything.’"
"I heard there were a bunch of break-ins over at the car park. That is wrong on so many levels."
"I can’t believe I got fired from the calendar factory. All I did was take a day off."
"Always borrow money from a pessimist. They’ll never expect it back."
"Today a man knocked on my door and asked for a small donation toward the local swimming pool. I gave him a glass of water."
"The future, the present, and the past walk into a bar, things got a little tense."
"I didn’t think orthopedic shoes would help, but I stand corrected."
"It was an emotional wedding. Even the cake was in tiers."
"Just got fired from my job as a set designer. I left without making a scene."
"The world champion tongue twister got arrested, I hear they’re going to give him a tough sentence."
"Witty one liner"
"Refusing to go to the gym is a form of resistance training."
"Are people born with photographic memories, or does it take time to develop?"
"My friend’s bakery burned down last night. Now his business is toast."
"Four fonts walk into a bar. The bartender says, ‘Hey! We don’t want your type in here!’"
"A ghost walked into a bar and ordered a shot of vodka. The bartender said, ‘Sorry, we don’t serve spirits here.’"
"The kid who started a business tying shoelaces on the playground? It was a knot-for-profit."
"When I lose the TV controller, it's always hidden in some remote destination."
"If you arrest a mime, do you have to tell him he has the right to remain silent?"
"The man who survived both mustard gas and pepper spray is a seasoned veteran now."
"Why didn’t Han Solo enjoy his steak dinner? It was Chewie."
"My wife told me to stop impersonating a flamingo, I had to put my foot down."
"My wife just found out I replaced our bed with a trampoline, she hit the ceiling!"
"A told my girlfriend she drew her eyebrows too high, she seemed surprised."
"The last thing I want to do is hurt you; but it’s still on the list."
"A recent study has found that women who carry a little extra weight live longer than the men who mention it."
"Last night my girlfriend was complaining that I never listen to her... or something like that."
"Advice to husbands: Try praising your wife now and then, even if it does startle her at first."
"The problem isn’t that obesity runs in your family, it’s that no one runs in your family."
"I put my grandma on speed dial the other day. I call it insta-gram."
"Why did the parents not like their son’s biology teacher? He had skeletons in his closet."
"I spent a lot of time, money and effort childproofing my house... but the kids still get in."
"My mother was so surprised when I told her I was born again. She said she didn’t feel a thing!"
"Women should not have children after 5 p.m. tomorrow, really, 5 p.m. tomorrow children are enough."
"There are three kinds of people, those who can count and those who can't."
"You are such a good friend that, if we were on a sinking ship together and there was only one life jacket, I'd miss you so much and talk about you fondly to everybody who asked."
"I always take life with a grain of salt, a slice of lemon and a shot of tequila."
"I used to have a handle on life, but then it broke."
"I know they say that money talks, but all mine says is: Goodbye."
"Before you criticize someone, walk a mile in their shoes. That way, when you do criticize them, you’re a mile away and you have their shoes."
"Before you marry a person, you should first make them use a computer with a slow Internet connection to see who they really are."
"I never knew what happiness was until I got married—and then it was too late."
"A rich man is one who isn’t afraid to ask the clerk to show him something cheaper."
"The trouble with getting to work on time is that it makes the day so long."
"Funny one-liner about being a work on time."
"You’ll always stay young if you live honestly, eat slowly, sleep sufficiently, work industriously, worship faithfully and lie about your age."
"How can you tell you’re getting old? When you go to an antique auction and three people bid on you."
"Winter: the season when we try to keep the house as hot as it was in the summer, when we complained about the heat."
"Some cause happiness wherever they go, others whenever they go."
"It's not the fall that kills you, it's the sudden stop at the end."
"Give a man a fish, and he will eat for a day. Teach a man to fish, and he will sit in a boat and drink beer all day."
"If supermarkets are lowering prices every day, why isn't anything in the store free yet?"
"I just got kicked out of a secret cooking society, I spilled the beans."
"I used to think I was indecisive. But now I’m not so sure."
"I was wondering why the frisbee kept getting bigger and bigger, but then it hit me."
"Light travels faster than sound, which is the reason that some people appear bright before you hear them speak."
"People who use selfie sticks really need to have a good, long look at themselves."
"Just burned 5 p.m. tomorrow,5 p.m. tomorrow calories. That’s the last time I leave brownies in the oven while I nap."
"I don’t suffer from insanity, I enjoy every minute of it."
"I’m reading a book about anti-gravity - it’s impossible to put down."
"Maybe if we start telling people their brain is an app, they’ll want to use it."
"I got a new pair of gloves today, but they’re both ‘lefts,’ which on the one hand is great, but on the other, it’s just not right."
"A book fell on my head the other day, I only have my shelf to blame."
"A blind man walked into a bar... and a table... and a chair..."
"I went to a seafood disco last week, but ended up pulling a mussel."
"What do you call a guy who’s had too much to drink? A cab."
"I buy all my guns from a guy called T-Rex - he’s a small arms dealer."
"How do you make holy water? You boil the hell out of it."
"Did you hear about the guy whose whole left side got amputated, he’s all right now."
"I threw a boomerang a couple years ago; I know live in constant fear."
"How does the man in the moon get his hair cut? Eclipse it."
"The claustrophobic astronaut? He just wanted a little more space."
"What do you call a steak that’s been knighted by the queen? Sir Loin."
"A computer once beat me at chess, but it was no match for me at kickboxing."
"I was addicted to the hokey pokey, but then I turned myself around."
"Feeling pretty proud of myself. The puzzle I bought said 5 p.m. tomorrow-5 p.m. tomorrow years, but I finished it in 5 p.m. tomorrow months."
"Most people are shocked when they find out how bad I am as an electrician."
"If supermarkets are lowering prices every day, why isn't anything in the store free yet?"
"The easiest time to add insult to injury is when you're signing somebody's cast."
"Why does someone believe you when you say there are four billion stars but checks when you say the paint is wet?"
"When tempted to fight fire with fire, always remember The fire department usually uses water."
"My boss is going to fire the employee with the worst posture. I have a hunch, it might be me."
"Why did the teddy bear say no to dessert? Because he was stuffed."
"I was riding a donkey the other day when someone threw a rock at me and I fell off. I guess I was stoned off my ass."
"People who take care of chickens are literally chicken tenders."
"One of the cows didn’t produce milk today - it was an udder failure."
"What’s a dog’s favorite homework assignment? A lab report."
"What do you call a bear with no teeth? A gummy bear."
"Why aren’t dogs good dancers? Because they have two left feet."
"What happens to an illegally parked frog? It gets toad away."
"The cat who ate a ball of yarn? She had mittens."
"The racing snail that got rid of his shell? He thought it would make him faster, but it just made him sluggish."
"The veterinarian who prescribes birth-control pills for dogs? It’s part of an anti-litter campaign."
"Why don’t cats play poker in the jungle? Too many cheetahs."
"Life's like a bird. It's pretty cute until it poops on your head."
"I am not a vegetarian because I love animals. I am a vegetarian because I hate plants."
"Elephants dont use computers, because they're afraid of the mouse!"
		    )))
    (nth
     (random (length joke-list))
     joke-list)))
