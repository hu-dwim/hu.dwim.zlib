(in-package :hu.dwim.zlib)

(export
 '(
   zlib
   c-fun/zlib
   +default-buffer-size+
   allocate-compress-buffer
   make-deflate-z-stream
   free-deflate-z-stream
   make-inflate-z-stream
   free-inflate-z-stream
   compress
   uncompress
   deflate
   deflate-sequence
   inflate
   inflate-sequence
   )
 :hu.dwim.zlib)
