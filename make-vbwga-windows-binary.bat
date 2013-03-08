@ECHO OFF

sbcl --load victory-boogie-woogie-genetic-algorithm.lisp --eval "(save-lisp-and-die \"vbwga-windows.exe\" :toplevel #'vbw::toplevel-for-exe :executable t)"
