---
title: A Journey of Optimizing Perl
---

Perl has a lot of ill-informed preconceptions in the programming community, and
speed does seem to be one of them. There is an assumption that one must either
make a mutually exclusive decision – clean, readable code that runs slowly, and
noisey code that performs all sorts of tricks to become efficient. I’ve recently
been in a position of having to do some fast work with Perl, so I am now sharing
some of my experiences.

## Context: Data transformations

In order to set the context for this article, allow me to touch on what I was
working on. At [MusicBrainz](http://musicbrainz.org/), we are moving from an old
database schema to a much newer one. In the process of this, we need to migrate
data from the old schema, to a new one. For the most part, this can be done in
SQL, but some of the data requires some more intricate processing.

We store records called “edits” in our database, which represent some sort of
change to data in the system – for example adding data, editing data and
removing data are all represented as an edit. Previously, edits consisted of an
old value and a new value, in various formats. Some edits used a custom
serialization format of a hashmap, others used single strings, others used a
scheme that made sense at the time, but does not fit in consistently with any
common format.

The new schema changes some of this. Firstly, both new value and previous value
have been combined into one (as it doesn’t make sense to have either of these
for some edits), and the serialization format is always JSON. This alone is a
transformation that would be difficult to achieve in SQL.

On top of this, the row IDs that this serialized data points to may no longer be
valid. The new schema merges many rows together (segmenting some data out to
other tables), so we need to alter these FKs appropriately.

The data set being migrated here is roughly 12 million rows, and there are 70
different types of rows – not a trivial data migration problem. To add further
constraints, the migration process can only be run at the point of schema change
– when we launch the new version. As we don’t want downtime for the site during
upgrade we put our database into a readonly state while we migrate, but we need
to still minimize the amount of time here – 4 hours in total is really at the
edge of this limit.

## So Make it FAST

### Step 1: Prototype

I spent a fair amount of time considering the correct approach to migrate this
data. Do I try to be clever and handle as much as possible in a single complex
transformation? Do I operate on a distinct type at a time, and transform a
stream? Do I operate on a row by row basis?

In order to make this decision, I decided to TIAS. I grabbed my toolkit of
[CPAN](http://search.cpan.org/) modules that make my life easier and threw some
solutions together. In the end, the solution that felt most productive to work
with and understand was operating on a row by row basis.

In this approach, I created [Moose](http://moose.iinteractive.com/) classes for
each transformation. I would inflate a row hash (from DBD) into one of these
classes, and call the upgrade method, which gave me a new object back (correctly
migrated and ready for re-insertion).

Finally I ended up with a migration script that ran, but sadly, not within the
time frame that we were constrained by. However, program correctness was my main
concern here, we are now ready to continue to the next step.

### Step 2: Understand your constraints

Before really diving in and understand the flaws of the program, it’s important
to understand your constraints. Where will your program be running? What
hardware do you have available? What is the timeframe/rate your program needs to
run within? Is your program running in isolation, or do you need to consider
load issues?

These are all important questions that will have an impact on the decisions you
make later. The more you can ask, the better an idea of the problem domain you
will have. In our case, we would be running the script on a machine with at
least 4gb of RAM, and as the main process. These constraints are generous –
giving us a lot of freedom to optimize.

### Step 3: Identify the bottlenecks

We have all done it, but diving into a program and blindly rewriting code under
the assumption it will run faster is an extremely unproductive task, that rarely
yields the goal of program optimization. Rather, the more productive task is to
instrument code to identify the critical points in a program – where the real
time is spent.

There are many approaches to instrumentation, and it may be enough to instrument
code yourself. We began by lightly instrumenting our master script with the
difference in time using
[Time::HiRes](http://search.cpan.org/~jhi/Time-HiRes-1.9721/HiRes.pm), a high
resolution implementation of gettimeofday for Perl. Later, we needed further
granularity, so we used a combination of
[Devel::NYTProf](http://search.cpan.org/dist/Devel-NYTProf/) and the included
nytprofhtml to generate reports. I may well have more to say on using
Devel::NYTProf in the future…

### Step 4: Reducing the impact of bottlenecks

With a clear idea of the problem, and the most significant bottlenecks, you are
now reading to begin optimization. There is no hard and fast rule on the best
way to remove bottlenecks, but here are some of the tricks we employed:

#### Trade space for time

This is the catchy motto of
[Memoize](http://search.cpan.org/~mjd/Memoize-1.01/Memoize.pm), a Perl module
implementing automatic memoization of subroutines, based on the input
given. [Memoization](http://en.wikipedia.org/wiki/Memoization) is the process of
caching the result of function, based on its input. Memoization thus allows
constant time calls to functions when called with repeated input. There are a
few considerations for memoization:

##### Referential transparency

It does not make sense to memoize functions that are not
[referentially transparent](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)). A
function with the property of referential transparent is a function in the
purely mathematically sense – for the same input, you will always see the same
output. This means functions that depend on outside state will not be suitably
for memoization, for example subroutines that depend on network access, random
values, and so on may not be suitable.

However, if you can guarantee a consistent world state /during the execution of
your program/, then memoization may turn out to be a useful optimization –
avoiding heavy disk I/O when you know the result will be the same.

##### Hit rate

Another consideration of memoization is one that will be familiar to those who
have had to implement caching – the hit rate. Memoizing a function called 20000
times sounds sensible at first, but when you discover it’s called with 20000
inputs, it becomes less sensible. All you achieve here is more memory usage, but
the cache is never hit. However, if it’s called with only 200 inputs, then
memoization is a strong contender for reducing the impact of this bottleneck.

#### Approach the problem from a different angle

One of the first bottlenecks we encountered was how to get data into the script
for the migration process. `SELECT`ing the entire table was not a solution, as
this would use too much memory. The prototype `SELECT`ed in small chunks, but to
do this we had to order the entire table for each select, and pass an offset –
an expensive operation, and one that becomes more expensive as the offset
increased.

The next approach we took was to dump the table into CSV files of chunks, and
then read from these. [Text::CSV_XS](http://search.cpan.org/dist/Text-CSV_XS/)
allowed us to read rows in with confidence (trusting the correctness of someone
else over ourselves) and drastically reduced the times. Reading a chunk from
disk into memory almost felt like a no-op compared to the speed of our SQL
query.

Finally, after more optimizations this too became a bottleneck again, and with a
little more thought we used a COPY statement to stream the table into the
script. The important thing here though is that we were able to gradually reduce
this bottleneck – we did only what was needed to make something else the most
important optimization.

#### Reduce logic where constraints are known

After reducing the larger bottlenecks, a simple function had become the
bottleneck. This function decodes a string from an old encoding format, into a
Perl string. The initial implementation of the function, copied straight out of
the legacy code base was:

    sub _decode_value {
        my ($scheme, $data) = $_[1] =~ /\A\x1B(\w+);(.*)\z/s
        	or return $_[1];
        return uri_unescape($data) if $scheme eq "URI";
        die "Unknown encoding scheme '$scheme'";
    }

Our initial approach was to memoize this. However, the hit rate is fairly low,
so this still didn’t truly help the problem. But looking at this code it felt as
simple as it could be, what were we missing?

The import realisation was that this code contains logic that can be hard
coded. Notice how there is a regular expression to check for if any encoding is
present, and a further condition for the scheme itself? But there is only a
single possible scheme! With this understand, we can use this much more
efficient code:

    sub decode_value {
        my $value = shift;
        return uri_unescape(substr($value, 5));
    }

This code doesn’t scale well, if the scheme changes, but we know this cannot
happen – as the data we are operating on has already been created (and as the
data is immutable, we know our assumptions will hold).

#### Substitute for more efficient technologies

Once we had reached program correctness, profiling showed that a substantial
amount of time was actually spent within Moose::new_object – constructing
objects. Our objects themselves did not really use much of the power given by
Moose however – no need for type checks, meta programming, the expressiveness of
attributes. In the end, we replaced all classes with much simpler objects that
simply used
[Class::Accessor::Fast::XS](http://search.cpan.org/dist/Class-Accessor-Fast-XS/). This
achieved a speed up in this bottleneck by an order of magnitude.

## Mistakes

This is my first foray into real optimizations, and I definitely made a lot of
mistakes along the way. Here are some that I think stand out the most:

### A lack of understanding of bottlenecks

I spent a lot of time making assumptions about my code and where it was slow –
after all, I wrote it. Sadly, this is the classic problem of programmer ego
taking control. Only after watching my futile attempts actually result in a
program that ran continually slower, did I step back and take a more scientific
approach to the problem. I could have saved a lot of time if I was willing to
slow down and actually gain a true understanding of the problem first.

### Overly ambitious optimizations

As we moved through the process of optimizing, our ideas for optimizing became
drastically over thought. The best example of this follows once we had a working
chunking system. Our system was able to migrate small chunks of data, and
appeared CPU bound, so we concluded the best approach would be multi-threading
it and using some shared memory. This is a classic example of over-engineering a
problem. It’s also a solution I don’t have enough experience in to implement
reliably (not to mention I don’t believe Perl is the right job for concurrent
programming).

In the end a few hours of my time were spent into failing to get this system
working, when really it was a solution that was not directly addressing the
bottleneck. Rather than working around a bottleneck, the better approach would
have been to attack the bottleneck head on.

## Conclusion

Hopefully here I’ve shown you that it is possible to write fast Perl code, we
just have to be a little bit more careful in our planning before we begin to
approach the problem. Optimizing any code is a delicate operation – you’re
really striving for a balance between all parts of the system, and upsetting
this balance can easily be done. This is not specific to Perl, and I imagine
most parts of this article people will already be aware of from other
languages. That said, it’s certainly helped me to step back and work out exactly
what I’m trying to achieve, even if this did take a few days more than I would
have liked.
