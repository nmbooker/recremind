recremind
=========

Small web app to set reminders to record TV programmes, to get around time limitations of timers on recording devices.

The web form at /setrec accepts the name of the programme, the channel name,
the date and time of first showing, and the number of days in advance your
video recorder (e.g. Sky+, Freeview+, VCR) can schedule recordings.  The latter defaults to 7 but you must enter the rest yourself.

Date must be in format dd/mm/YYYY.

Time must be in format HH:MM

Number of days must be a positive integer.

Runs on port 8000.

You must set the environment variable `RECREMIND_TO_EMAIL` to the email
address you want to send reminders to.

I'm sure there are loads of improvements to the usability, security and
code layout of the program.  Suggestions and patches welcome.

I'm a beginner with Haskell so constructive criticism of my code is also
welcome.
