PRONTT
======

What is it?
-----------

It's a web-based Gantt chart generator written in prolog.

Why would you care what language it's written in? Because you might know that resource planning with rules like __"a Java guru like Mary could do this in half the time"__ can only be done correctly in a constraint-based language.

Right now the resource planning has no such rules, just sequential dependencies, but the web-related stuff was a bigger job than the underlying logic ever will be. In that light, it could be interesting as a starting point for academic exercises.


How do I deploy it?
-------------------

You choose a server that's only accessible to your trusted friends, install SWI-prolog on it (see INSTALL), make the executable with make, and arrange for it to be running however you see fit. It doesn't lose data when it's terminated, in fact, I'm not sure how to make it lose data when I want it to.

If you don't have any trusted friends, you can uncomment the authorisation code in the sources. This is plain http authorisation, which means that passwords are send in plaintext over the wire. That is completely useless unless you know exactly what's along the wire or the wire is encrypted with e.g. https.
