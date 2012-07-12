---
title: Crowdsourcing Organization
---

There’s recently been a lot of work in [MusicBrainz](http://musicbrainz.org)
trying to clarify the direction of the project, and roughing out a schedule of
how urgent tasks are. I initially selected a few people who are extremely active
in the community to help me with the task of sorting through around 200 issues,
with the hope that we could decide how important they were. After getting
through 30 issues, and scheduling ~25, it was clear that this was a fairly
hopeless endeavor. Not only were we not going to get through everything, part of
me was left uneasy by the alienation of the rest of the community.

Along with the scheduling discussions, there has been light talk about
decentralizing our organisation (at least away from the IRC centralization we
currently have) and introducing more democracy into the process. Sorting out
this scheduling seems like a perfect candidate to try this out, but it needed
organisation to get the results we wanted.

Last Friday, I began work hacking away on mashing up a subset of issues in our
bug tracker in a tiny little web app which I fairly unimaginatively called “the
Voting Game.” The idea is simple: present users an issue at random and ask they
if they think it has to be fixed within 3 months, 12 months, or can remain
unscheduled.

The results have consistently amazed me.

I threw the first version of the application online a few hours after I start
working it, and results came in thick and fast. At first I put this down
slightly to people just enjoying using something new/wanting to break it, but
good data came in after the initial announcement. I deemed the project a good
idea and continued work on it, finally getting it officially online yesterday.

In less than 24 hours since I
[announced the launch](http://blog.musicbrainz.org/?p=1152), I’ve had users
encounter problems of *emptying the queue* - they’ve managed to vote on every
issue in the system!  Not only that, some issues have feedback from 8 people,
considerably outweighing the input of a little group of IRC regulars.

I’m sure to a lot of people this sounds obvious, but with a community as vocal
as MusicBrainz, it can often become a burden to manage so much input. This tool
has shown me that if you give people the right tools, it can do just the work
for you, and the answers just fall into place.

[The Scheduling Game is live on ocharles.org.uk](http://scheduling.ocharles.org.uk/),
and source code is available at
[GitHub](https://github.com/ocharles/VotingGame).
