#!/bin/sh

umask 0027;
rm -rf vbwga;
mkdir vbwga;
mkdir vbwga/reference-pictures;
cp victory-boogie-woogie-genetic-algorithm.lisp \
   victory-boogie-woogie-genetic-algorithm.html \
   start-drawing.bat start-drawing.sh           \
   quicklisp.lisp vbw-example.pdf               \
   vbwga/;
cp reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png vbwga/reference-pictures/;
tar cvfz vbwga-1.1.tar.gz vbwga/;
