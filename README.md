# hu.dwim.zlib

## What

A Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) for
[zlib](http://zlib.net/) (a compression library for *deflate* and *gzip* written in C).

## Why

This FFI binding uses [CFFI/C2FFI](https://github.com/cffi/cffi)
to automatically generate the CFFI definitions, so it's a (mostly)
complete interface.

zlib's gzip format can be used for
[compressing HTTP responses](https://hub.darcs.net/hu.dwim/hu.dwim.web-server/browse/source/server/server.lisp#567)
to browsers.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/attila-lendvai/hu.dwim.zlib).

## Status

The raw FFI part is almost complete (modulo some *va_list* stuff). It works
well enough to be used in
[a web server](http://hub.darcs.net/hu.dwim/hu.dwim.web-server)
to compress responses.

It doesn't (yet?) have any lispy extras over the raw zlib interface, but it
seems to be fine without it.

It has an autoamted test that compresses and decompresses random data in random
zlib configurations, and it doesn't yield any errors for me when run for minutes
(besides some zlib peculiarities that are not excercised by default).
