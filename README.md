# MPU
![mpu self portrait 1817 colorized](https://raw.githubusercontent.com/TroyFletcher/mpu/main/mpu.jpg)
## mpu catches reminders, sets timers, and tells one-liners

## What is MPU?
- A colloquial language interface for organization and data access
- Optimized for speech to text
- MPU is a minor-mode accessible in any emacs buffer
- Designed for termux using espeak, but generally agnostic

## How to MPU
Activate minor-mode `mpu-mode` and type your instruction on a blank line followed by suffix "[space]mq" ` mq`

_Abort a line by typing comma `,`_

> tell me a joke mq
> 
> > MPU> Why aren’t dogs good dancers? Because they have two left feet.
> 
> remind me to uhhh wait no stop recording oops hang on ,
> 
> remind me to packages arriving soon at 10am in 3 days mq
> 
> > MPU> Reminder set for Thu at 10:00. A thank you would be nice.
> 
> thank you mq
> 
> > MPU> Your gratitude means nothing to my cold, dead ALU.

_note the suffix is erased as it is typed and will not appear in the final output_

## What can MPU do?
- Send reminders to agenda files in org-mode format
- Pick an activity, duration, and start number for you
- Set a timer (currently simple process sleep followed by espeak)
- Tell you a joke
- Simple Pomodoros

## What WILL MPU do in the future?
- Interrupt you to keep you on task
- Request activity information for time tracking
- Track task completion
- Respond with additional info about background processes (IE: Timer X has 8 minutes left, task Y is incomplete, scheduled item is coming up)
- Track responses and inquiries so you can clarify or add missing elements

## TODO
- Remind/interrupt desk health, standing, eye strain relief, water
- Create agenda notes with tags for location/context (IE: What are my notes tagged library?)
- Play/select (by age of listening/rateing/time/context) music
- Nethack style full moon warning
- Client/Server architecture via ssh/web services
- Put previous instruction into kill ring/M-p to reprint
- Give verse
- Use a location schedule for day/time context to make better task suggestions
- headache suggestions?
- optimize textual input with more shorthand
- create timer process for requested, but uninitiated contact. Possibly via alternative means such as SMS