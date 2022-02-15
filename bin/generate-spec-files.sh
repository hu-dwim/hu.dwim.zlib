#!/usr/bin/env bash
#| -*- mode: lisp; coding: utf-8-unix -*-

#set -o xtrace

PROJECT_NAME=hu.dwim.zlib

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`
PROJECT_HOME=`readlink -f ${SCRIPT_DIR}/..`

# For LIBRARY_PATH see:
# https://gcc.gnu.org/onlinedocs/gcc/Environment-Variables.html#Environment-Variables
# `guix shell --development foo` sets this variable to the profile's lib/ directory
# that contains the .so files.
# Note that timing matters: "If, at *the time that the program was started*, the
# environment variable LD_LIBRARY_PATH was defined to contain..."
# More details in: https://github.com/cffi/cffi/pull/194
if command -v guix &> /dev/null; then
  echo "Guix detected, entering the environment."
  PACKAGES_FOR_SDL=""
  eval $(guix shell --pure --search-paths c2ffi zlib libffi jq pkg-config sbcl --development zlib)
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}${LIBRARY_PATH}"
  echo "Setting LD_LIBRARY_PATH based on LIBRARY_PATH to ${LD_LIBRARY_PATH}"
fi

LISP=sbcl

cd "${PROJECT_HOME}"

echo "*** "`date`" Generating c2ffi spec files for ${PROJECT_NAME} in ${PROJECT_HOME}"

BUILD_LOG_FILE="/tmp/${PROJECT_NAME}.build-log"

# TODO reenable source registry override once cffi is fresh enough in ql
#export CL_SOURCE_REGISTRY="(:source-registry (:tree \"${PROJECT_HOME}\") :ignore-inherited-configuration)"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${PROJECT_HOME}\" (\"${PROJECT_HOME}/build/fasls/\" :implementation)) :inherit-configuration)"

mkdir --parents build/

# install quicklisp if needed
if [ ! -e "build/quicklisp/setup.lisp" ] ; then
    curl --output build/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
    ${LISP} --noinform --end-runtime-options --no-sysinit --no-userinit \
            --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
            --load "build/quicklisp.lisp" \
            --eval '(quicklisp-quickstart:install :path "build/quicklisp/" :dist-url "http://beta.quicklisp.org/dist/quicklisp/2021-12-30/distinfo.txt")' \
            --eval '(quit)'
fi

# "call" the lisp part below.
${LISP} --noinform --end-runtime-options --no-sysinit --no-userinit \
        --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
        --load "build/quicklisp/setup.lisp" \
        --eval "(with-open-file (s \"${0}\" :element-type 'character) (read-line s) (load s))" \
        --end-toplevel-options 2>&1 ${PROJECT_NAME} | tee ${BUILD_LOG_FILE}

echo "*** "`date`" About to filter the generated c2ffi spec files for ${PROJECT_NAME}"

${SCRIPT_DIR}/filter-spec-files.sh

echo "*** "`date`" About to run the tests again for ${PROJECT_NAME}"

${LISP} --noinform --end-runtime-options --no-sysinit --no-userinit \
        --eval "(require :asdf)" --eval "(asdf:load-system :asdf)" \
        --load "build/quicklisp/setup.lisp" \
        --eval "(asdf:test-system :${PROJECT_NAME})" \
        --eval "(quit)" \
        --end-toplevel-options 2>&1 | tee ${BUILD_LOG_FILE}

# let's quit the shell part before the shell interpreter runs on the lisp stuff below
exit 0

# and from here follows the lisp part that gets invoked by the above shell part |#

(in-package :cl-user)

(format t "~2&Running on ~A ~A, using Quicklisp dist version ~A~%"
        (lisp-implementation-type)
        (lisp-implementation-version)
        (or #+quicklisp (ql:dist-version "quicklisp")
            "n/a"))

(ql:quickload '(:cffi/c2ffi :cffi/c2ffi-generator :split-sequence
                ;; need to quickload :hu.dwim.asdf explicitly, otherwise i get:
                ;; Component "hu.dwim.asdf" not found, required by NIL
                :hu.dwim.asdf))

(defpackage :build-tmp
  (:use :common-lisp
        :alexandria))

(in-package :build-tmp)

(defparameter *project-name* (intern (string-upcase (first uiop:*command-line-arguments*))
                                     (find-package :keyword)))

(defun nix-includes->c2ffi-args ()
  (loop :with str = (or (uiop:getenv "NIX_CFLAGS_COMPILE") "")
        :with pieces = (remove "-isystem"
                               (split-sequence:split-sequence #\Space str :remove-empty-subseqs t)
                               :test 'equal)
        :for el :in pieces
        :collect "--sys-include"
        :collect el))

(cond
  ;; NixOS
  ((uiop:getenv "NIX_CFLAGS_COMPILE")
   (appendf cffi/c2ffi::*c2ffi-extra-arguments*
            (nix-includes->c2ffi-args))

   ;; KLUDGE this is an enormous kludge that is only needed on nixos
   ;; because there clang is a wrapper script that defines some extra
   ;; include paths. see https://github.com/rpav/c2ffi/pull/89
   (appendf cffi/c2ffi::*c2ffi-extra-arguments*
            (list "--sys-include"
                  "/nix/store/kaicsq9mskqvs7ww03rpz7cbjiwamh8i-glibc-2.31-dev/include")))
  ;; Guix
  ((uiop:getenv "GUIX_ENVIRONMENT")
   (appendf cffi/c2ffi::*c2ffi-extra-arguments*
            (list "--sys-include"
                  (concatenate 'string (uiop:getenv "GUIX_ENVIRONMENT")
                               "/include/")))))

;; (setf cffi/c2ffi::*trace-c2ffi* t)
;; (trace cffi/c2ffi::ensure-spec-file-is-up-to-date
;;        cffi/c2ffi::generate-spec-with-c2ffi)

(cffi/c2ffi:generate-spec *project-name*)

(asdf:load-system *project-name*)

(uiop:quit)
