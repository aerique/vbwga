@ECHO OFF

REM Replace '\' in the path with '/' since SBCL barfs on the '\\' that gets
REM passed on.
SET UNIX_CD=%CD:\=/%

sbcl --load %UNIX_CD%/victory-boogie-woogie-genetic-algorithm.lisp --eval "(let ((*default-pathname-defaults* #P\"%UNIX_CD%/\")) (vbw::write-pdf (vbw::resolution-independent-drawing (vbw::main \"reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png\")) \"vbw.pdf\") (quit))"
