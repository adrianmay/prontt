PRONTT
======

What is it?
-----------

It's a web-based Gantt chart generator written in prolog.

There's an instance running at http://quorve.com:8888 but not necessarily the latest.

Why would you care what language it's written in? Because you might know that resource planning with well developed rules can only be done correctly in a constraint-based language.

Right now the rules include sequential dependencies and avoiding double bookings with optimisation based on minimising the latest end date in the system, but much more is planned.

The invitation is open to participate - I didn't do any CSS at all yet, a prolog guru could probably improve my code, and all those other rules need writing.


How do I deploy it?
-------------------

You choose a server that's only accessible to your trusted friends, install SWI-prolog on it (see INSTALL), make the executable with make, and arrange for it to be running however you see fit. It doesn't lose data when it's terminated, in fact, I'm not sure how to make it lose data when I want it to.

If you don't have any trusted friends, you can uncomment the authorisation code in the sources. This is plain http authorisation, which means that passwords are send in plaintext over the wire. That is completely useless unless you know exactly what's along the wire or the wire is encrypted with e.g. https.

