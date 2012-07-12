---
title: Leaving Gentoo
---

## Picture the Following

You're happily at work coding away, completely in the zone and you realise you
need some free disk space to do a bunch of database prototyping. No problem,
you've got a spare hard drive in your machine thats unused, so you decide to
wipe that drive and reclaim that space. You fire up `fdisk` and clear the disk
ready to create just a single partition. You try and run `mkfs.ext3` but are
warned that `/dev/sda` is still in use. That's odd, it's meant to be inactive -
but never mind, a reboot will clear all of that up.

Wait, why is the system not coming up? Invalid system disk? Your heart sinks as
you suddenly realise you've just deleted the partition table of the wrong disk -
you've actually just removed your system partition table and are now left
without an unbootable machine.

You spend an hour or so trying to fix things, prodding around with `dumpe2fs`
and the like, but eventually decide to just `dd` the drive somewhere else and
reinstall and just get back to work.

The next part of this story will be continued 8 hours later.

## A Shift From Worker to Worked

It's an idiotic, stupid mistake to make, and could have been quite easily solved
by just taking the time to really read the output of `fdisk`. However, that
doesn't change the fact that the above scenario did happen. I could have lived
with that, but my choice of operating system - [Gentoo](http://gentoo.org) -
added to the torture.

I've been a huge fan of Gentoo, and a happy user for the last 3 years, but
Thursday's events have led me to critically re-evaluate my choice of
distribution. Gentoo is simply no longer catering to a productive work
environment.

Gentoo is almost entirely a source based distribution, which means a lot of time
will be spent compiling packages, rather than installing a prebuilt binary. In
this case, over 8 hours. My workspace consists of XMonad, Chromium, Emacs and
rxvt-unicode, and little more - this is not a complicated set up. However, there
is a base cost of any system - just getting a working kernel and X11 is
significantly time consuming.

You might argue that the above example is extreme, it's not every day you need
to do massive compilations. This is true, but even small adjustments to a
machine become more laborious than they need to be. Gentoo ebuilds have a system
of `USE` flags, which allow you to precisely define what you want built from a
package. It can be inviting to constrain your `USE` flags to the bare minimum,
but in the last year I must have had to recompile PostgreSQL at least 3 times
for UUID, Python and Perl support. This is not a background task, this is
something blocking my work.

I chose Gentoo not for it's source based nature, but because it encourages a
deep understanding that not many other distributions require. The installation
process requires understanding partition tables, `chroot`, various `etc`
configuration files, and so on. This is fantastic hands on knowledge that would
be hard to pick up from simply reading things.

There is also a definite feeling of confidence in the system when things are
working correctly. Sadly, getting to a correct working state is impaired by
being source based. Recompling a library can lead to broken packages down the
line, if you're not careful to rebuild reverse dependencies. I've impaired my
work by accidently recompiling a dependency of Chromium, and having to wait
hours for Chromium to rebuild just to carry on with work.

This, along with increased build times, leads to a significant role reversal. I
am no longer working my system, it is now working me.

## Onwards to Pastures New

I've decided to return to <a href="http://archlinux.org">Arch Linux</a>, a
distribution I last used at university around 4 years ago. Arch has officially
supported binary repositories, but also plays well when compiling from source,
with it's AUR/pkgconfig system. Pacman is an acceptable package manager, and I
love its speed, but I do long for the integrity that <a
href="http://paludis.exherbo.org">Paludis</a> gave me. Arch is also driven by
simple configuration files in `etc`, leading to a very transparent system
configuration.

One of my worries is that it feels like Arch will make it easy to build up a
system with a lot of installed packages, but not much understanding as to why I
need certain packages. Optional dependencies seem to be handled quite loosely,
without any link between the dependency and dependent package.

I've also changed away from the normal init system to systemd. This is pretty
orthogonal to the issues I was experiencing, but I felt like it was appropriate
time to make some subtle changes. I am so far loving systemd, it feels very
polished compared to a bunch of odd scripts all over the place, and it's
certainly fast.

In conclusion, I had a great time with Gentoo and have learnt a lot about
configuring Linux systems. I'm glad I'm leaving with generally positive
experiences, and certainly don't have any remorse about investing a few years
into it. Though I must say, it's certainly refreshing to have a working desktop
environment in less than an hour.
