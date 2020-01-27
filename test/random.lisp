(in-package :hu.dwim.zlib/test)

(in-suite test/random)

(deftest test/random/deflate-random-bytes (&key (repeat 300000))
  (declare (optimize (debug 3)))
  (flet ((random-buffer-size ()
           (+ 8 (random 128))))
    (loop
      :with min-window-bits = 9 ; this could be 8 theoretically (or 9? see zlib docs), but 8 triggers a -3 rc from zlib
      :for count :upfrom 1
      :repeat repeat
      ;; the first 256 random test vectors will have the exact size of 0..256, the rest will have their size also randomized
      :for source = (if (< count 256)
                        (make-ub8-vector/random-content count :random)
                        (make-ub8-vector/random-content-and-length 256))
      :for source-length = (length source)
      :for level = (random |Z_BEST_COMPRESSION|)
      :for window-bits = (+ min-window-bits (random (- |MAX_WBITS| min-window-bits)))
      :for container = (alexandria:random-elt '(:raw :zlib :gzip))
      :for compress-buffer-size = (random-buffer-size)
      :for decompress-buffer-size = (random-buffer-size)
      :for start = (min source-length (random (max 1 (floor source-length 2))))
      :for end = (max start (- source-length (random (max 1 (floor source-length 8)))))
      :do (when (zerop (mod count 1000))
            (print count))
      :do (when (and (eq container :gzip)
                     (zerop level))
            ;; TODO this is probably a zlib bug
            (setf level 1))
      :do (when (and (not (eq container :zlib))
                     (eql window-bits 8))
            ;; see zlib docs
            (setf window-bits 9))
      :do (multiple-value-bind (compressed compressed-length)
              (deflate-sequence source :start start :end end :buffer-size compress-buffer-size :level level :window-bits window-bits :container container)
            (multiple-value-bind (decompressed decompressed-length)
                (inflate-sequence compressed :end compressed-length :buffer-size decompress-buffer-size :window-bits window-bits :container container)
              (is (eql (- end start) decompressed-length))
              (is (vector-equal decompressed source :start1 0 :end1 decompressed-length :start2 start :end2 end)))))))
