(in-package :hu.dwim.zlib)

(hu.dwim.def:def hu.dwim.def:package :hu.dwim.zlib/fancy
  (:use :hu.dwim.common
        :hu.dwim.def
        :metabang-bind)
  (:import-from :hu.dwim.zlib
                ub8-vector
                +default-memory-level+
                +default-buffer-size+
                c-fun
                c-ref
                c-fun/zlib)
  (:local-nicknames
   (#:zlib :hu.dwim.zlib)
   (#:zlib.ffi :hu.dwim.zlib.ffi))
  ;; To be able to use C-c C-c in Slime: (asdf:load-systems :hu.dwim.def+swank :hu.dwim.zlib/fancy)
  (:readtable-setup
   (hu.dwim.syntax-sugar:enable-sharp-boolean-syntax)
   (hu.dwim.syntax-sugar:enable-feature-cond-syntax)
   (hu.dwim.syntax-sugar:enable-case-preserving-syntax :packages '(:hu.dwim.zlib.ffi))))
