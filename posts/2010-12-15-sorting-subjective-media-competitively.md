---
title: Sorting Subjective Media Competitively
---

I own a lot of music, but I find it difficult to find what I want to play. Being
a DJ, I’ve developed a pretty good memory of what I like to listen and what goes
together to create a consistent mood, but I’m also a computer programmer so I
like to make the computer work for me as much as possible.

Back when I was a gamer, I spent some time looking at applying sport statistics
to the games I played – in particular I spent some time working with the Elo
rating system. It worked great, and the results I got mostly matched the
communities opinion of who the best teams were. However, what really struck me
was the simplicity of the Elo rating functions – you just give it 2 scores, tell
who “won” and you get 2 new scores back. There is little more to this, and it
leaves a lot of scope back to the user of what the score means, and what a
victory or defeat is. This has left a bit of a nagging curiosity in my mind – if
we can do this for sorting games, can we apply it to subjective media?

I’m currently in the process of planning how I want to approach this problem,
but allow me to quickly summarize my thought so far.

At the beginning of the process, all the music belongs to a single set. We then
sample this set (I haven’t decided if this should be done randomly or with
heuristics yet), playing each piece of music one at a time. Once the user has
heard 2 pieces of music, they need to decide which they liked more. Here are my
current options:

- The previous track was better
- The recent track was better
- I enjoyed them equally
- I couldn’t compare them

The first 3 options map to a victory, a loss and a draw, respectively. The last
one is a little different – but if I declare that 2 pieces of music could not
really be compared, then a new set is created (X’). We also make a note that the
set’s X and X’ cannot be compared.

Now, as this system goes on, we will hopefully build up a set of sets, and we’ll
also define a set of relationships between these sets.

I’m not really sure what the results of this will be, or how useful they will
be. My motivation is to create some sort of equivalent of the music genome
project, but I’m not sure it can really get that far. However, this should at
least end up giving me a set of genre collections, sorted by rating as well.

Now, just to implement this and get some data in!
