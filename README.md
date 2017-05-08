# Aquifer

## Summary

A water consumption tracker running in the browser, written
with [GHCJS]() and [Reflex FRP]().

## Usage

Browse to https://matthias.benkard.de/aquifer/ and make sure to have
JavaScript enabled.

Add drinks by clicking the _add drink_ button on the bottom of the
page.

Track your water consumption by checking the running total on the top
of the page.  For a quick glance at how you're doing, look at the
color of the top bar.  If the background is **red**, you haven't
approached your daily goal yet.  If it is **yellow**, you are well on
your way to reaching your daily goal.  If it is **green**, you're fine
for today.

## Implementation notes

The application is written in Haskell, compiled with [GHCJS](), and
based on the [Reflex FRP]() library.

[Reflex FRP]: https://github.com/reflex-frp/reflex
[GHCJS]:      https://github.com/ghcjs/ghcjs
