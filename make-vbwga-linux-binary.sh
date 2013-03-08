#!/bin/sh

sbcl --load victory-boogie-woogie-genetic-algorithm.lisp --eval "(save-lisp-and-die \"vbwga-linux\" :toplevel #'vbw::toplevel-for-exe :executable t)"
