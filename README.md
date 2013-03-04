StringEdit
==========

_The following is transcribed from the `Haddock` documentation. For a more
complete (and prettier) overview, please see `doc/StringEdit.html`._

Usage
-----

You can either build with `ghc Main.hs -O2` and then run

	./Main word1 word2 [word3 ...]

Or you can issue

	./Main word1 word2 [word3 ...]

Each command will calculate the minimum `String` edit between all adjacent pairs
of `String`s given.

About
-----

This package provides functions for calculating a `String` edit of minimum
distance between two `String`s (the notion of distance or `cost` here is
synonymous with _Levenshtein distance_). That is, given two `String`s, for each
character, we can either associate the two according to some `cost` function or
insert/delete a character from one of the `String`s with some penalty (in this
case, 1). Then we want to find a `String` edit of minimum distance.

Included is a naive implementation that takes exponential time, as well as a
memoizing implementation that takes _O(nm)_ time (where _n_ and _m_ are the
lengths of each word). Additionally, the memoizing version allows us to recover
a `String` edit of minimum distance.

### Costs

The costs of aligning two characters:

* Vowel-to-vowel costs 0.5,
* Vowel-to-consonant costs 1.2,
* Consonant-to-consonant costs 0.6, and
* Aligning the same characters costs 0.

Example Program
---------------

An example program run:

	$ runhaskell StringEdit.hs thenameofthegame theresmyname
	Minimum String Edit Cost: 7.0
	Minimum String Edit: 
	thenameofthegame
	ther  e smy name

Also note that the minimum String edit between A and B is the same as that
between B and A, as expected.

	$ runhaskell StringEdit.hs ninakushukuru unamshukuru ninakushukuru
	Minimum String Edit Cost: 3.1
	Minimum String Edit: 
	ninakushukuru
	 unam shukuru
	
	Minimum String Edit Cost: 3.1
	Minimum String Edit: 
	 unam shukuru
	ninakushukuru
