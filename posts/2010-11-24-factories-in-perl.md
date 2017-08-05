---
title: "Implementing Factories in Perl"
---

Factories are a useful construct, and even though their usage is common in Java
and other heavily biased object-orientation languages, they don’t see as much in
more dynamic languages. This doesn’t mean you can’t use them or they don’t have
their uses though. In this article, I’m going to try and explain why and when we
need to use factories and how we can go about implementing them in Perl.

This article was inspired be a recent discussion I had with someone in
[`#moose`](irc://irc.perl.org/moose/), but hopefully this larger write up will
be useful to some people, though understand the concepts here are simple and I’m
writing for a different audience than usual. With that introduction over, lets
get started!

## What Are Factories

A factory is really just the name for something that creates objects. We could
say new is a very specific factory (that only creates objects of the same type
as the class), but normally a factory performs a little more logic. We usually
use factories when we need to create objects, but we don’t know what type of
object until we get to run time. We will work with the following example.

Our system has `Flight` objects, and there are different types of `Flight`s. For
now, lets say we have `Flight::Holiday`s and `Flight::Cargo`s. Holiday flights
take a set of passengers, and cargo flights take a set of cargo items. Our job
is to take flight bookings, and store them in a database somehow. As part of our
solution to this problem we decide that we will need to create the appropriate
`Flight` object, and then it can be stored.

```perl
package Flight;
use Moose::Role;

package Flight::Holiday;
use Moose;
with 'Flight';

has 'passengers' => ( is => 'ro' );

package Flight::Cargo;
use Moose;
with 'Flight';

has 'cargo' => ( is => 'ro' );
```

Simple so far, right? Where do factories come into play? The problem is that the
external data we get doesn’t specify which type of flight we need to create!
Lets pretend we’re given a hash reference of parameters to new. We would like to
inspect this to decide how to create objects. Rather than doing this every time
we create a `Flight`, we should put this in a separate function:

```perl
sub new_flight {
    my ($class, $data) = @_;
    if (exists $data->{cargo}) {
        return Flight::Cargo->new($data);
    }
    elsif (exists $data->{passengers}) {
        return Flight::Holiday->new($data);
    }
    else {
        die "I don't know how to create this type of Flight";
    }
}
```

Nothing complicated here? Well guess what… we just wrote a factory! Move this to
a separate `FlightFactory` class, and we’re done. We can now create flights by
calling `FlightFactory->new_flight({ cargo => [] })` and we will get a
`Flight::Cargo` back.

Neat.

## Going Further

This is great, we’ve already abstracted the object construction away, but we can
do better. There is a problem with our current factory, it introduces multiple
points of change. Our factory is also doing too much – why should
`FlightFactory` care about what makes a `Flight::Cargo`? Surely that’s
`Flight::Cargo`‘s job. Let’s address this issue first:

```perl
sub new_flight {
    my ($class, $data) = @_;
    if (Flight::Cargo->understands($data)) {
        return Flight::Cargo->new($data);
    }
    elsif (Flight::Holiday->understands($data)) {
        return Flight::Holiday->new($data);
    }
    else {
        die "I don't know how to create this type of Flight";
    }
}
```

And we add code like the following to `Flight::Holiday` and `Flight::Cargo`:

```perl
sub understands {
    my ($class, $data) = @_;
    return exists $data->{cargo};
}
```

Great! Now the logic for deciding which class to instantiate has been moved to
the appropriate area of responsibility. But we still have the problem about
multiple points of change. Let’s have a look at that deeper to see what the
problem is.

Imagine our requirements change, and we’re now asked to handle Flight::Personals
– people flying their own planes. What changes does this require? Well, we need
a Flight::Personal class, that’s for sure:

```perl
package Flight::Personal;
use Moose;
with 'Flight';

sub understands {
    my ($class, $data) = @_;
    return exists $data->{owner};
}
```

This should be enough, but it’s not. If we pass `{ owner => 'Ollie' }` to
`new_flight` we won’t get a `Flight::Personal` back because the factory does not
yet know about `Flight::Personal`, so let’s add it in:

```perl
sub new_flight {
    ...
    elsif (Flight::Personal->understands($data)) {
        return Flight::Personal->new($data);
    }
    ...
}
```

Wait a minute! I see an abstraction emerging here! We seem to be doing the same
sort of code for each branch in our if statement, lets see if we can do better
here… maybe it will reveal a solution to the problem we’re investigating

```perl
sub new_flight {
    my ($class, $data) = @_;

    my @classes = (
        Flight::Personal,
        Flight::Holiday,
        Flight::Cargo;
    );
    for my $subclass (@classes) {
        return $subclass->new($data)
            if $subclass->understands($data);
    }

    die "I don't know how to create this type of flight";
}
```

Aha! Not only have we abstracted out some repetition and made it easier to
change, we’ve reduced the effort to add a new type of Flight. We’re not happy
that the factory has to change at all though – can you see how to achieve this
yet? We need a way to dynamically set `@classes`. There are a few ways to do
this, but I’ll show you a solution using
[`Module::Pluggable`](http://search.cpan.org/dist/Module-Pluggable/):

```perl
package FlightFactory;
use Module::Pluggable search_path => 'Flight', sub_name => 'classes';

sub new_flight {
    my ($class, $data) = @_;

    for my $subclass ($class->classes) {
        # As before
    }
}
```

`Module::Pluggable` gives us the classes class method, which returns all classes
under the `Flight::` namespace. We probably want to be a bit more specific here
and make sure we only get things that are concrete classes – checking that they
do the `Flight` role would be a start. I’ll leave this to readers as an
exercise.

## Don’t reinvent the wheel

LeoNerd in the comments below has pointed out that the idiom of looping over
clases to filter a specific one, is what `Module::PluginFinder` was designed
for. So, in the spirit of writing even better code, lets try using that!
`Module::PluginFinder` works like `Module::Pluggable`, but we can specify a
filter for matching classes. It can also handle the instantiation for us:

```perl
package FlightFactory;
use Module::PluginFinder;

my $finder = Module::PluginFinder->new(
    search_path => 'Flight',
    filter => sub {
        my ($class, $data) = @_;
        $class->understands($data)
    }
);

sub new_flight {
    my ($self, $data) = @_;

    return $finder->construct($data, $data)
        or die "I don't know how to create this type of Flight";
}
```

## Conclusion

Hopefully in this post I’ve given you a clear illustration of the need for
factories, when we might want to use them, and how we can implement them. We
went past basic factories to make them dynamic, and extendible (even extendible
outside the distribution). Along the way, I tried to illustrate this in the
approach I would take while I do this at work, which also has hopefully given
you a good idea of how you can apply basic refactoring to your code as you write
it, and end up with clean, separated code.

If you want to follow along with this tutorial, I have pushed out a Git
repository to
[my Github account](https://github.com/ocharles/OCharles-Blog-Factories). You
can follow along with this by checking out the code, and then reading the log
with patches, in reverse:

    git clone git@github.com:ocharles/OCharles-Blog-Factories.git
    git log --reverse -p

It’s a little different, as I wrote it after the article, but hopefully it’s
useful. This is the first time I’ve tried posting accompanying code, so I’m
curious to see how useful people find it. If you want to run Example.pm you will
need Moose, Module::Pluggable, and a reasonable version of Perl (5.8 upwards
should do the job).
