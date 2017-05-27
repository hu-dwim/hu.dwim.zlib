(in-package :hu.dwim.zlib)

(defpackage :hu.dwim.zlib/test
  (:use :common-lisp
        :hu.dwim.stefil
        :hu.dwim.zlib
        :hu.dwim.zlib.ffi))

(import-all-owned-symbols :hu.dwim.zlib.ffi :hu.dwim.zlib/test)
