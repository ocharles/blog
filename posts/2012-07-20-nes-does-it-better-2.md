---
title: "NES Does it Better: Building NES"
---

In the
[previous article](http://ocharles.org.uk/blog/posts/2012-07-10-nes-does-it-better-1.html)
we looked at why MusicBrainz needs NES, and summarised the major components. In
this article, we'll have a look at the design decisions behind NES, and how I
reached I came to this final design.

## Requirements

We looked at the problems in more detail in the previous post, but lets
reiterate them so we all understand where we're trying to head:

  - Simpler code base. Ideally we don't want to have different edit types, and
    would prefer to have a simpler interface for making changes.

  - Database integration. Rather than having a separate database with a
    separate schema, integrate with the database so FKs and other data
    integrity checks are valid.

  - Full versioning. It should be possible to rollback changes to entities. This
    is more than just undoing basic changes, we should be able to undo entire
    deletes and merges.

  - Deferred application. It's still important that some changes have to be
    voted on before they are applied.

  - Aggregated changes. It should be possible to make changes between entities
    in one go - for example adding a release and adding new artists at the
    same time.

Now that we know the problems we are trying to solve, lets derive a better edit
system!

## Deriving NES

I will now walk you through a derivation of the new edit system. We will
consider the `artist` table first, but the steps apply to any other
entity. We will begin with a schema that is almost identical to the current
MusicBrainz schema, and then slowly turn it into the schema that NES uses.

### Versioning 101

The `artist` table contains all basic data for an artist - the artist's name,
when they were born/founded, the 'latest' MBID for the artist, and so on. There
are also various tables joined to `artist`, such as `artist_alias`,
`artist_ipi`.

These tables currently only have enough data to version a single version of an
artist. If we introduce another increasing integer column, `revision_id`,
we now support multiple versions of artists. As the `artist` table now really
contains revisions (versions) of artists, lets rename it `artist_revision`. This
table now has a primary key on `(mbid, revision_id)`.

As the primary key has changed, this means we have to change the foreign key for
all the other tables that refer to artists. In the case of `artist_alias` it
makes sense to use `(mbid, revision_id)` as an alias only exists for a specific
version of an artist - aliases can change or be deleted. However, for some other
data such as ratings and tags, the data is not dependent on a version so it
seems incorrect to have to specify the version of an artist a tag applies
to. Lets reintroduce an `artist` table, but in this case it will only contain
the unique MBIDs of artists. Now `artist_tag` can point to the `artist`
table. Perfect!

### Trees

Stepping back from artists, and looking at versioning more generally, we can see
there are 3 main components to versioned data. The data itself; revisions, which
associate meta-data with a link to the actual data; and entities which pull it
all together and provide us with a way to point a public facing ID (in our case,
MBIDs) with a canonical master revision.

Yet our current schema seems to blur the first 2 aspects together - the data
itself has a very close tie to the actual revision. If we split this
abstraction, we gain more power. This split comes from 'tree' objects.

If you imagine your computer, your data is laid out in a tree. You have lots of
files and folders. The same can apply to artists. Running with the file system
analogy, an artist has basic data (a file), and a list of aliases (a folder with
alias files). So we have an `artist_tree`, and this artist tree has
`artist_data` and `artist_alias` links. `artist_revision` now has a pointer to
the `artist` it belongs to, the `artist_tree` containing all its data, and a
`revision_id` column.

Interestingly, it's now really simple to do rollbacks. Say we have version 1 of
an artist, and make some changes to produce version 2. However, these changes
were completely incorrect and we'd like to go back to version 1, while also
capturing the fact that we have explicitly reverted changes. All we have to do
is create a new revision, and point back to the 'version 1' artist tree. Simple!

### Credit Where Credit Due

We're now at a stage where we have a schema that can have data changed, and we
keep a full history. The data is all inside the database, and we benefit from
PostgreSQL doing all sorts of data integrity checks. However, it's not very
social - there's no way of associating this work with the user who actually did
that work. Furthermore, we would benefit from a bit of extra meta-data -
importantly when the change was created.

This meta-data is quite general, so lets begin modelling it independent of other
data for now. We create a `revision` table with a unique `revision_id`, and a
reference to an `editor_id`. We also store the time the revision was created
with a `created_at` timestamp.

Now all that's left is to unite our grand revision table with the artist stuff
we already worked on. There's a reason we took a little detour with the tree
objects as well, now that all the artist data is dependent on the `artist_tree`,
we can re-purpose the `artist_revision.revision_id` column and link this to
`revision` objects.

### Deferring Edits

Reflecting back on our requirements list we have achieved most of the versioning
requirement, certainly achieved full database integration, but we're lacking
deferred edits. This is where things might get a little bit complicated, so hold
on to your seats!

To defer edits we need to record a change the moment an editor makes it, but
*not* change the representation of what is considered canonical (that is, what
people will see on the web site). Our system lets us make whatever changes we
like, but if we simply take the latest changes, we lose the ability to defer
changes. What we need is a way to explicitly point an artist MBID to a known
revision and change this when deferred changes passed peer review.

We augment the `artist` table to gain a new column, `publisher_revision_id`
which points to an `artist_revision` that is considered peer reviewed. Now we
can freely make other revisions without impacting the published data.

To apply these changes later we will need a merge, specifically a three-way
merge between the new changes, the currently published version, and the common
base between the new changes and the current version. To do this, revisions are
linked together via the `revision_parent` table. This lets us say that if we have
revision 1 and we make changes to get to revision 2, revision 1 is the *parent*
of revision 2. A revision can have multiple children, but interestingly a child
can have multiple parents - if we merge revision 1 into revision 2, we create
revision 3 which has parents revision 2 *and* revision 1. This is what is known
as a directed acyclic graph, DAG.

### Might I Make a Request?

We're almost there! We now have multiple, in database versions of entities, with
the ability to defer these changes until later. What we don't have though, is a
way to discuss these changes, and a way to request peer-review. This is where
merge requests come in.

If we think of edits as very small changes, akin to the undo log in a text
editor, the merge request is much larger operation. A merge request is made when
an editor has finished all of their work, and they wish to request peer
review. Merge requests have a special place on the website where people can view
all of the changes that are being made, and have the ability to discuss these
changes. People can vote on merge requests to vote whether they agree with the
changes, or disagree with them. Using the current MusicBrainz model, these are
about as close as you can get to the old 'edits'.

In terms of data, a merge request contains one or more revisions to merge. I say
one or more, as this allows us to merge changes to an artist, a release, and a
label, all in one go. These revisions must be unique over MBIDs, it wouldn't
make much sense to merge multiple versions of the same artist!

## Are We There Yet?

I wish. NES is a very large project, and is going to take a lot of coordination
to get to. I currently have implemented the NES schema for MusicBrainz entirely,
but it's a bit useless if there's nothing using it! The next major chunk of work
is to integrate NES with the MusicBrainz web site and web service. We're
currently trying to figure out how to get there, and doing some rethinking of
our MusicBrainz architecture at the same time.

I did plan to have some concrete plans to share with you, but I sadly do not
have these yet. We are currently focusing on how to re-architect MusicBrainz
in site of NES. I will share these details as I get them!

In spite of all the technology planning we have to do, there's still NES work
that you can help with - question it! I encourage lots of discussion about NES,
and the best place to do this is within the various MusicBrainz discussion
channels - musicbrainz-devel and #musicbrainz-devel are both areas I am very
active in, if you need to get directly to me.

A wiki page will be coming shortly to try and consolidate NES work, and will
feature a schema diagram and explanation of tables, but without all the
hand-holding and motivation that is in these blog posts.

