(in-package :hu.dwim.zlib)

;;;;;;
;;; types

(deftype ub8-vector ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype operation-kind ()
  '(member :deflate :inflate))

(deftype container-kind ()
  '(member :raw :zlib :gzip))

(deftype window-bits ()
  '(integer 8 15))

(defconstant +default-memory-level+ 8)
(defconstant +default-buffer-size+ 4096)

;;;;;;
;;; utils

(defun %c-fun/zlib/check-error (rc fn-name whole-form)
  (when (minusp rc)
    (error "zlib call failed. Name: ~S, return-code: ~S, expression: ~S."
           fn-name rc whole-form)))

(defmacro c-fun/zlib (&whole whole fn-name &rest args)
  (with-unique-names (rc)
    `(let ((,rc (,fn-name ,@args)))
       (%c-fun/zlib/check-error ,rc ',fn-name ',whole)
       ,rc)))

;; TODO handle this: (c-ref object type slot-name count) where count should count in the referenced type size
(defmacro c-ref (object type slot)
  `(foreign-slot-value ,object ',type ',slot))

(defmacro stream-ref (stream slot)
  `(foreign-slot-value ,stream '|z_stream| ',slot))

;; don't blame me, it's mimiced from: https://github.com/madler/zlib/blob/master/examples/zpipe.c
(defun %inflate-or-deflate (operation make-stream-fn free-stream-fn
                            input-fn output-fn &key
                                                 (buffer-size +default-buffer-size+))
  (check-type operation operation-kind)
  (assert (>= buffer-size 8))
  (let* ((input-buffer-size (floor buffer-size 2))
         (output-buffer-size (floor buffer-size 2))
         (stream (funcall make-stream-fn))
         (lisp-input-buffer (cffi:make-shareable-byte-vector input-buffer-size))
         (lisp-output-buffer (cffi:make-shareable-byte-vector output-buffer-size))
         (total-output-bytes 0))
    (macrolet
        ((debug (message &rest args)
           (declare (ignorable message args))
           `(progn
              (format *debug-io* ,message ,@args)
              (terpri *debug-io*)
              (finish-output *debug-io*))
           nil)
         (stream-slot (slot-name)
           `(c-ref stream |z_stream| ,slot-name)))
      (unwind-protect
           (cffi:with-pointer-to-vector-data (input-buffer lisp-input-buffer)
             (cffi:with-pointer-to-vector-data (output-buffer lisp-output-buffer)
               (labels
                   ((maybe-refill-input-buffer ()
                      (let ((avail-in (stream-slot |avail_in|)))
                        (when (zerop avail-in)
                          (setf (stream-slot |next_in|) input-buffer)
                          (let ((bytes-read (funcall input-fn lisp-input-buffer 0 input-buffer-size)))
                            (setf (stream-slot |avail_in|) bytes-read)
                            (debug " in: ~A" bytes-read)))
                        (values)))
                    (reset-output-buffer ()
                      (debug "resetting output buffer")
                      (setf (stream-slot |next_out|) output-buffer)
                      (setf (stream-slot |avail_out|) output-buffer-size)
                      (values))
                    (write-output-buffer ()
                      (let* ((avail-out (stream-slot |avail_out|))
                             (bytes-to-write (- output-buffer-size avail-out)))
                        (unless (zerop bytes-to-write)
                          (debug " out: ~A" bytes-to-write)
                          (funcall output-fn lisp-output-buffer 0 bytes-to-write)
                          (incf total-output-bytes bytes-to-write)))
                      (values)))
                 (debug "*** starting outer loop in mode ~S, buffer-size ~A" operation buffer-size)
                 (loop :named outer
                   :with rc = nil
                   :with flush = |Z_NO_FLUSH|
                   :do (progn
                         (debug ">outer")
                         (maybe-refill-input-buffer)
                         (let ((has-more-input? (not (zerop (stream-slot |avail_in|)))))
                           (ecase operation
                             (:deflate (setf flush (if has-more-input?
                                                       |Z_NO_FLUSH|
                                                       |Z_FINISH|)))
                             (:inflate (unless has-more-input?
                                         (return-from outer)))))
                         (loop :named inner
                           :do (progn
                                 (debug ">inner, flush: ~A, avail-in: ~A" flush (stream-slot |avail_in|))
                                 (reset-output-buffer)
                                 (maybe-refill-input-buffer)
                                 (assert (not (zerop (stream-slot |avail_out|))))
                                 (setf rc (ecase operation
                                            (:deflate (|deflate| stream flush))
                                            (:inflate (|inflate| stream |Z_NO_FLUSH|))))
                                 (debug "inflate/deflate returned with code: ~A" rc)
                                 (assert (not (eql rc |Z_STREAM_ERROR|)))
                                 (unless (member rc '#.(list |Z_OK| |Z_STREAM_END|))
                                   (error "zlib deflate/inflate call failed with rc ~A" rc))
                                 (debug "avail-in is ~A, avail-out is ~A" (stream-slot |avail_in|) (stream-slot |avail_out|))
                                 (write-output-buffer))
                           ;; loop while we receive a full output buffer
                           :while (zerop (stream-slot |avail_out|)))
                         (when (eq operation :deflate)
                           (assert (zerop (stream-slot |avail_in|)))))
                   :until (ecase operation
                            (:deflate (eql flush |Z_FINISH|))
                            (:inflate (eql rc |Z_STREAM_END|)))
                   ;;:finally (assert (eql rc |Z_STREAM_END|))
                   ))))
        (funcall free-stream-fn stream))
      (debug "returning with length ~A" total-output-bytes)
      total-output-bytes)))

;;;;;;
;;; API

(defun allocate-compress-buffer (source &key (source-start 0) (source-end (length source)))
  (cffi:make-shareable-byte-vector (ceiling (* (+ (- source-end source-start) 12) 1.01))))

(defun make-deflate-z-stream (&key (level |Z_DEFAULT_COMPRESSION|) (method |Z_DEFLATED|)
                                (window-bits |MAX_WBITS|) (container :zlib) (memory-level +default-memory-level+)
                                (strategy |Z_DEFAULT_STRATEGY|))
  (check-type window-bits window-bits "WINDOW-BITS must be between 8 and 15. See the CONTAINER argument for requesting raw, zlib, or gzip header/footers.")
  (check-type container container-kind)
  (setf window-bits (ecase container
                      (:zlib (- window-bits))
                      (:gzip (+ window-bits 16))
                      (:raw window-bits)))
  (let ((stream (cffi:foreign-alloc '|z_stream|))
        (ok nil))
    (unwind-protect
         (progn
           (setf (stream-ref stream |zalloc|) (cffi:null-pointer))
           (setf (stream-ref stream |zfree|)  (cffi:null-pointer))
           (setf (stream-ref stream |opaque|) (cffi:null-pointer))
           (setf (stream-ref stream |avail_in|) 0)
           (setf (stream-ref stream |avail_out|) 0)
           (c-fun/zlib |deflateInit2_|
                       stream level method window-bits memory-level strategy
                       |ZLIB_VERSION| (cffi:foreign-type-size '|z_stream|))
           (setf ok t)
           stream)
      (unless ok
        (cffi:foreign-free stream)))))

(defun free-deflate-z-stream (stream)
  (c-fun/zlib |deflateEnd| stream)
  (cffi:foreign-free stream)
  (values))

(defun make-inflate-z-stream (&key (window-bits |MAX_WBITS|) (container :zlib))
  (check-type window-bits window-bits "WINDOW-BITS must be between 8 and 15. See the CONTAINER argument for requesting raw, zlib, or gzip header/footers.")
  (check-type container container-kind)
  (setf window-bits (ecase container
                      (:zlib (- window-bits))
                      (:gzip (+ window-bits 16))
                      (:raw window-bits)))
  (let ((stream (cffi:foreign-alloc '|z_stream|))
        (ok nil))
    (unwind-protect
         (progn
           (setf (stream-ref stream |zalloc|) (cffi:null-pointer))
           (setf (stream-ref stream |zfree|)  (cffi:null-pointer))
           (setf (stream-ref stream |opaque|) (cffi:null-pointer))
           (setf (stream-ref stream |avail_in|) 0)
           (setf (stream-ref stream |avail_out|) 0)
           (c-fun/zlib |inflateInit2_|
                       stream window-bits |ZLIB_VERSION| (cffi:foreign-type-size '|z_stream|))
           (setf ok t)
           stream)
      (unless ok
        (cffi:foreign-free stream)))))

(defun free-inflate-z-stream (stream)
  (c-fun/zlib |inflateEnd| stream)
  (cffi:foreign-free stream)
  (values))

(defun compress (source destination &key
                                      (source-start 0)
                                      (source-end (length source))
                                      (destination-start 0)
                                      (level |Z_DEFAULT_COMPRESSION|))
  "A simpler API for the stream based DEFLATE.

Compress the first SOURCE-START bytes of SOURCE into DESTINATION. DESTINATION should be an array of (unsigned-byte 8), and should be large enough to hold the compressed contents. ALLOCATE-COMPRESS-BUFFER can be used to allocate a proper buffer.

Note that the size of the DESTINATION array should be at least 0.1% more than the souce plus 12 bytes, but the actual number of array elements filled in by the compression algorithm will usually be smaller (depending on how 'predictable' the input data is).

Returns DESTINATION-END i.e. compressed length if DESTINATION-START was zero."
  (check-type source ub8-vector)
  (check-type destination (or null ub8-vector))
  (cffi:with-foreign-object (compressed-length '|uLongf|)
    (setf (cffi:mem-ref compressed-length '|uLongf|) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (c-fun/zlib |compress2|
                    (cffi:inc-pointer destination-bytes destination-start)
                    compressed-length
                    (cffi:inc-pointer source-bytes source-start)
                    (- source-end source-start)
                    level)
        (let ((destination-end (+ destination-start (cffi:mem-ref compressed-length '|uLongf|))))
          destination-end)))))

(defun uncompress (source destination &key
                                        (source-start 0)
                                        (source-end (length source))
                                        (destination-start 0))
  "A simpler API for the stream based INFLATE.

Pretty useless because DESTINATION must be long enough to hold the uncompressed contents, otherwise it errors out. Returns (values destination-end)."
  (check-type source ub8-vector)
  (check-type destination ub8-vector)
  (cffi:with-foreign-object (uncompressed-length '|uLongf|)
    (setf (cffi:mem-ref uncompressed-length '|uLongf|) (- (length destination) destination-start))
    (cffi:with-pointer-to-vector-data (source-bytes source)
      (cffi:with-pointer-to-vector-data (destination-bytes destination)
        (c-fun/zlib |uncompress|
                    (cffi:inc-pointer destination-bytes destination-start)
                    uncompressed-length
                    (cffi:inc-pointer source-bytes source-start)
                    (- source-end source-start))
        (let ((destination-end (+ destination-start (cffi:mem-ref uncompressed-length '|uLongf|))))
          destination-end)))))

(defun deflate (input-fn output-fn &key (buffer-size +default-buffer-size+) (level |Z_DEFAULT_COMPRESSION|)
                                     (method |Z_DEFLATED|) (window-bits |MAX_WBITS|) (container :zlib)
                                     (memory-level +default-memory-level+) (strategy |Z_DEFAULT_STRATEGY|))
  "See DEFLATE-SEQUENCE for usage."
  (%inflate-or-deflate
   :deflate
   (lambda ()
     (make-deflate-z-stream :level level :method method :window-bits window-bits
                            :container container :memory-level memory-level
                            :strategy strategy))
   'free-deflate-z-stream
   input-fn
   output-fn
   :buffer-size buffer-size))

(defun deflate-sequence (source &rest args &key (start 0) (end (length source)) &allow-other-keys)
  (check-type source sequence)
  ;; this allocates a large enough output buffer where output is guaranteed to fit as per zlib docs
  (let ((compressed-bytes (allocate-compress-buffer source)))
    (values compressed-bytes
            (apply 'deflate
                   (let ((position start))
                     (named-lambda deflate-sequence/input-fn
                         (buffer buffer-start size)
                       (let ((size (min size (- end position))))
                         (assert (not (minusp size)))
                         (replace buffer source :start1 buffer-start :end1 size :start2 position)
                         (incf position size)
                         size)))
                   (let ((position 0))
                     (named-lambda deflate-sequence/output-fn
                         (buffer buffer-start size)
                       (replace compressed-bytes buffer :start1 position :start2 buffer-start :end2 size)
                       (incf position size)
                       (values)))
                   (alexandria:remove-from-plistf args :start :end)))))

(defun inflate (input-fn output-fn &key (buffer-size +default-buffer-size+) (window-bits |MAX_WBITS|) (container :zlib))
  (%inflate-or-deflate
   :inflate
   (lambda ()
     (make-inflate-z-stream :window-bits window-bits :container container))
   'free-inflate-z-stream
   input-fn
   output-fn
   :buffer-size buffer-size))

(defun inflate-sequence (compressed-bytes &key (start 0) (end (length compressed-bytes))
                                            (buffer-size +default-buffer-size+) (window-bits |MAX_WBITS|)
                                            (container :zlib))
  (check-type compressed-bytes sequence)
  (flet ((allocate-buffer (length)
           (make-array (* 2 length) :element-type '(unsigned-byte 8))))
    (let ((decompressed-bytes (allocate-buffer (round (* 1.8 (length compressed-bytes))))))
      (values decompressed-bytes
              (inflate (let ((position start))
                         (named-lambda inflate-sequence/input-fn
                             (buffer buffer-start size)
                           (let ((size (min size (- end position))))
                             (assert (not (minusp size)))
                             (replace buffer compressed-bytes :start1 buffer-start :end1 size :start2 position)
                             (incf position size)
                             size)))
                       (let ((position 0))
                         ;; TODO reallocate output if not large enough
                         (named-lambda inflate-buffer/output-fn
                             (buffer start size)
                           (replace decompressed-bytes buffer :start1 position :start2 start :end2 size)
                           (incf position size)
                           (values)))
                       :buffer-size buffer-size
                       :window-bits window-bits
                       :container container)))))
