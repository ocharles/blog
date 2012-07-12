---
title: SQL: It's Data, Not a String
---

Recently I’ve been becoming more and more frustrated with how we handle database
interactions at work. We’ve just written a new version of our data object API
and while they are considerably better than what we had before, they are still
both limited and have too much boiler plate for my liking. As such, I spent some
of my free time researching a better solution. So far, I’ve settled on
[Fey](http://search.cpan.org/~drolsky/Fey/) and have been extremely happy with
the results.

One of the key differences between our old system and Fey is how SQL is
represented. Previously, SQL was just written as a string, using a few helpers
functions and the odd measure of string interpolation. To be able to compose
queries, there were methods that returned just the set of columns, just the
tables and joins – it worked, to a degree. Fey takes a different approach –
instead of working with SQL strings, you work with SQL as data. And this
abstraction turns out to be extremely powerful.

Here’s a real world example of why this matters so much. Our schema has “entity
tables” and “name tables.” Many entities can have the same name, and good
database design tells us we shouldn’t repeat ourselves, so we have “name tables”
too. That is, each entity has one (or more) names. But not every entity has
this, some do have the name just as a string, only the so called “core entities”
have name tables.

But these name tables are are a technical detail, not something to be exposed by
the API. That, is if I do `Artist->get_by_id`, I shouldn’t see this relationship
at all. It turns out with roles and a bit of method modification, we can
introduce this transparently. Here’s how I’ve started using Fey query objects:

    package Entity;
    has 'table' => ( is => 'ro', required => 1, init_arg => undef );
    has 'select' => ( lazy_build => 1, is => 'ro' );
    method _build_select {
        return Fey::SQL->new_select->select($self->table)->from($self->table);
    }

Entity is an abstract base class – it can’t be instantiated because it requires
the table accessor. So, our core entity just extends it:

    package CoreEntity;
    extends 'Entity';
    has '+table' => ( default => sub { $schema->table('core_entity') } );

Just like this, we will bring in the name foreign key, but we actually want to
bring in the actual name – the relationship should be transparent. We could
modify core entity to do this, but that’s not particularly elegant. A better
approach is to have a Name role:

    package Name;
    use Moose::Role;
    around _build_select => sub {
        my $orig = shift;
        my ($self) = @_;
        $self->$orig->from($self->table, $schema->table('name'));
    };

Now, our core entity role only needs to consume the name role, and any selects
done (using select) will automatically bring in that relationship. Even better,
you can now throw this select object around to other classes and let them do
even more with it. This is proving to be an extremely powerful way to write code
that just dwim, with a minimum amount of boilerplate.

The example above is simplified, the actual implementation of these classes does
a bit more by doing a little bit of database metadata poking – determining
foreign keys and working out the join tables for you. But it’s built on the same
principles of above. I think this reinforces one of my guiding points of
programming – never use strings unless you are passing them over a boundary
between your program. No XML strings, no HTML strings, no SQL strings, no
strings to represent data (MooseX::Types instead of the standard Moose type
constraints)… the more places you can apply this, the more you can transform
your code with code.

Maybe I’ve been reading too much Lisp.
