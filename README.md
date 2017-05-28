# hu.dwim.zlib

## What

A Common Lisp [FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) for
[zlib](http://zlib.net/), a compression library for deflate and gzip.

## Why

This FFI binding uses CFFI/C2FFI to automatically generate the CFFI definitions,
so it's a (mostly) complete interface.

zlib's gzip format can be used for compressing HTTP responses to browsers.

## Who

Written by [attila@lendvai.name](mailto:attila@lendvai.name).

## Where

The primary communication channel is the facilities on
[the project's GitHub page](https://github.com/attila-lendvai/hu.dwim.zlib).

## Status

The FFI part is almost complete (modulo some va_list stuff). It works
well enough to be used in a web server to compress responses.

It has a test that compresses and decompresses random data in random
zlib configurations, and it doesn't yield any errors for me.
