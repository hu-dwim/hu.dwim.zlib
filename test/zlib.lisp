(in-package :hu.dwim.zlib/test)

(in-suite test)

(deftest test/small-inputs ()
  (loop
    :for container :in '(:raw :zlib :gzip)
    :do (loop
          :for size :from 0 :below 512 :do
          (let* ((data (make-ub8-vector/random-content size :random))
                 (compressed-data (deflate-sequence data :container container)))
            (multiple-value-bind
                  (uncompressed-data uncompressed-length)
                (inflate-sequence compressed-data :container container)
              (is (vector-equal data uncompressed-data :end2 uncompressed-length)))))))
