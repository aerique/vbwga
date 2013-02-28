#!/bin/sh

sbcl --load victory-boogie-woogie-genetic-algorithm.lisp --eval "(progn (vbw::write-pdf (vbw::resolution-independent-drawing (vbw::main \"reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png\")) \"vbw.pdf\") (quit))"
