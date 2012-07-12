---
title: NES Does It Better: Introducing the New Edit System
---

This article is the first in a series of articles discussing NES, the New Edit
System for [MusicBrainz](http://musicbrainz.org). In this post, I'm going to
explain briefly why we need NES and motivations to move away from the current
edit system, and then look at some of the features that NES will bring.

## You're Gonna Need a Bigger Edit System

### Scaling Issues

MusicBrainz has been running for over a decade, and in that time has stuck with
the same model for an editing system. It's done well, given what it does, but
it's no surprise that in that time things have changed. If you're unaware of how
editing in MusicBrainz works, users make *edits* which then have to be
peer-reviewed and *voted on* by other editors. Edits have 2 weeks in this voting
period phase, when they are said to be *open*. If an edit has not received
sufficient votes to be applied in this period, it will be applied by default
after 2 weeks.

![A graph of edits over time](/img/2012-07-10-edits.png)

This graph shows the total amount of edits in the MusicBrainz database - roughly
matching exponential growth, with a worrying knee around the start of 2012. This
graph would be ok, if it wasn't accompanied by this one:

![A graph of open edits over time](/img/2012-07-10-open-edits.png)

This graph shows the amount of *open* edits at any point in time. It's noisy,
but we can roughly say there are at least 50,000 edits that need to be peer
reviewed at any point in time. Finally, one last graph to really clarify the
problem:

![A graph of votes per day, over time](/img/2012-07-10-votes.png)

This shows the amount of votes a day - generally around 6000. An impressive
number, but it's not inline with the size of the open edit queue. The mere fact
that open edits is growing is cause for alarm enough. The edits will eventually
get applied - as mentioned above after the 2 week voting phase they get applied
anyway. However, and this is the crucial problem, they are being applied without
sufficient peer review. We pride ourselves on data quality, but without ample
peer review, we risk losing this magnificant quality.

It's clear that there is a problem with an excessive amount of open edits, but
it's also important to understand why that is. The amount of active editors in
MusicBrainz has sadly gone down, year-on-year, so it's not due to more
editors. The real problem is that the edit system does not match how people
actually edit.

Edits are currently very granular, almost corresponding directly to single
database operations. There are edits to create a release, edits to add mediums,
edits to add relationships, and so on. From a technical perspective, this makes
sense - we have lots of small operations that can be sequenced to create bigger
changes, but it doesn't make sense socially. A single edit doesn't have the
context of what an editor was actually doing, and for voting people want to see
the bigger picture. I think we can agree that the goal of peer review is to
ensure that the end result is to assure that an editor's work is correct as a
whole, the individual steps are not as important.

### Lack of Features

The MusicBrainz edit system offers little for editors. There are plenty of
things that people have been asking for, yet the design of the edit system makes
it very difficult for us to implement them. Amongst all the various requests for
new features, some of those that stand out are:

- **The ability to group edits**. As alluded to earlier, editor's work on
    projects bigger than a single edit. Grouping them will allow people to see
    all changes together, and really understand how the changes interact.
- **Improve history**. Edits don't really capture a huge amount of history, and
    this makes it difficult to understand how things looked in the past.
- **Improve searchability**. Due to the way the edit system is architectured, it's
    very expensive to search 'inside' edits, to find edits that make specific
    changes.
- **Allow for amendments**. We're all human, and we all make mistakes. The edit
    system is not at all forgiving about that, and currently if you get
    something wrong the edit has to be rejected entirely, or we have to accept
    known bad data. Neither of these are optimal.
- **Allow undoing changes**. If something doesn't get peer reviewed, it will get
    accepted, but it's not always a good change. If this change is found later,
    people want to be able to revert that change and return to a known correct
    state. The only way to do this in the current edit system is to do it by
    hand.

### Technical Debt

*[Wikipedia's article on technical debt says:](http://en.wikipedia.org/wiki/Technical_debt)*

> Technical debt (also known as design debt or code debt) is a neologistic
> metaphor referring to the eventual consequences of poor or evolving software
> architecture and software development within a codebase.

The edit system was designed a long time ago, and has grown in a very ad-hoc
fashion since then. This has led to bad design decisions, poor reliability, a
large maintainence burden and a lack of trust. As a developer, it's difficult
for me to trust changes I make to the edit system, and for users it's worse -
they risk losing changes that they've spent time working on.

NES has been designed without the constraints of the current edit system, and
has been thought out to make it either extremely difficult or impossible to
introduce the critical bugs we have now. We'll look at how NES is really
designed in a future article, but lets move on to see the high level picture of
the new edit system.

## A NES For Everyone

As NES is fresh start, it's a different model from the current edit system, so
it will take a little bit of time to understand how everything fits
together. I've discussed with people the technical implementation of NES (which
is important), but before I go into that lets take a look at the birds eye view
of NES.

### An Intricate System of Levers And Pulleys

There are a few moving parts in NES, so lets have a look at each of these in
turn.

#### Entities

*Entities* are roughly the same as what we have at the moment in
MusicBrainz. They are the core data of the project, and MusicBrainz has 7 of
them: artists, labels, recordings, releases, release groups, urls and works. An
entity is something that has <abbr title="MusicBrainz ID">MBIDs</abbr>,
maintains a history of edits and can be edited via the new edit system. Entities
have both versioned data, data that will show up in the history, but can also
have non-versioned data such as tags and ratings.

#### Revisions

When a user makes changes to an entity, they don't change the data of the entity
directly, instead they create *revisions*. This is handled transparently by the
edit system, but revisions work a bit like the undo log in other
applications. As you make changes to a release, for example, you will build up
various revisions.

When an editor is happy with their changes, they need to publish these revisions
for peer review, which brings us onto the next concept.

#### Merge Requests

An editor publishes their changes under a *merge request*, which is the closest
NES gets to the old edit system. A merge request groups all changes together and
presents these changes for other editors to review and vote on. A merge request
can be small, such as just correcting the case of a track on a release, or it
can be large such as entering a classical box set with works, artist
relationships, and more. While there is no limit to the size of merge requests
in the system itself, I expect that the community will quickly find out what a
manageable merge request looks like, and more editing editique will emerge.

### What Does NES Offer?

It's hard to enumerate everything that NES can do, but here are some of the
features that I think are significant:

- **Extensive history**. Edits store an entire snapshot of history, at that point
  in time. This lets us perform rollbacks, browse the site as it looked a year
  ago. History goes as far as tracking when MBIDs were added, so you can even
  revert merges.
- **Versioned in the database**. NES stores its data in exactly the same
  database schema as the latest version of data. This means that we can do more
  flexible queries on this data, and we also gain guarantees that the data is
  correct.
- **Variable size edits**. Merge requests let you determine how big a change you
  want to publish.
- **Saving edits**. If you don't have time to make a large edit, that's ok - you
  can take a break! As NES works in revisions, you can always stop when you want
  and pick up your work later.
- **Improved conflict handling**. If your changes fail due to a conflict that's
  ok, your changes haven't been lost. All you need to do is make some
  amendments and submit a new merge request.

## Coming Up On Next Week's Show

In the next post I'll explain how NES is structured internally, and look at a
rough plan of action and where we are now. If you want a sneak peak, you can
have a look at the presentation I gave about NES, [Towards a New Edit
System](http://ocharles.org.uk/EditSystemPresentation/edit-system.html) (note
that sadly this presentation only seems to work if you have a Webkit based
browser. Sorry!).
