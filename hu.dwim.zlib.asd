(defsystem :hu.dwim.zlib
  :description "Common Lisp FFI wrapper for zlib, aka http://zlib.net/"
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:alexandria
               :cffi
               :cffi/c2ffi
               :cffi-libffi)
  :in-order-to ((test-op (test-op :hu.dwim.zlib/test)))
  :components ((:file "package-stage-1"
                :pathname "source/package-stage-1")
               (:module "source"
                :depends-on ("c2ffi-spec" "package-stage-1")
                :serial t
                :components ((:file "package-stage-2")
                             (:file "package-stage-3")
                             (:file "zlib")))
               (:module "c2ffi-spec"
                :depends-on ("package-stage-1")
                :components ((:cffi/c2ffi-file "zlib.h"
                              :package #:hu.dwim.zlib.ffi
                              :foreign-library-name "hu.dwim.zlib.ffi::zlib"
                              :foreign-library-spec ((t (:default "libz")))
                              :include-sources ("zlib\\.h$"
                                                "zconf\\.h$")
                              :exclude-sources :all
                              :include-definitions ()
                              :exclude-definitions ())))))

(defsystem :hu.dwim.zlib/fancy
  :description "Fancier API extensions for hu.dwim.zlib for the price of more dependencies."
  :author "Attila Lendvai"
  :license "BSD or Bugroff"
  :version "0.1"

  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:hu.dwim.zlib
               :hu.dwim.def+hu.dwim.common
               ;;:hu.dwim.defclass-star+hu.dwim.def
               :hu.dwim.syntax-sugar)
  :components ((:module "source"
                :serial t
                :components ((:file "package-fancy")
                             (:file "fancy")))))

(defsystem :hu.dwim.zlib/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.stefil
               :hu.dwim.zlib
               ;; you probably also want to load :hu.dwim.stefil+swank one way or another
               )
  ;; Unfortunately ASDF swallows the return value (i.e. it cannot be
  ;; inspected in Slime), so we at least print it.
  :perform (test-op (o c) (print (funcall (intern (string '#:test)
                                                  (find-package :hu.dwim.zlib/test)))))
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "zlib" :depends-on ("suite"))
                             (:file "random" :depends-on ("suite"))))))
