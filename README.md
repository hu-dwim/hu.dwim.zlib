# hu.dwim.zlib

## What

A Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) for
[zlib](http://zlib.net/) (which is a compression library for *deflate* and *gzip* written in C).

## Why

It uses [CFFI/C2FFI](https://github.com/cffi/cffi)
to automatically generate the CFFI definitions, so it's a
complete interface based on the C header files.

zlib's gzip format can be used for
[compressing HTTP responses](https://hub.darcs.net/hu.dwim/hu.dwim.web-server/browse/source/server/server.lisp#571)
to browsers.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/hu-dwim/hu.dwim.zlib).

## Status

The raw FFI part is almost complete (except some *va_list* stuff).

No fancy lispy overlay API has been added; it seems to be convenient enough without it.

It's used in
[hu.dwim.web-server](https://hub.darcs.net/hu.dwim/hu.dwim.web-server/browse/source/server/server.lisp#571)
to compress HTTP responses.

It has a test that compresses and decompresses random data in random
zlib configurations, and it doesn't yield any errors for me when run for minutes
(besides some zlib peculiarities/bugs that are avoided by the test code).
