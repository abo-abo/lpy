#* Welcome to the `lpy-mode' Tutor
#
# `lpy-mode' is a powerful way to interact with a Python REPL.
#
# ATTENTION:
# The commands in the lessons will modify the code. You can always
# revert the file using git.
#
# It is important to remember that this tutor is set up to teach by
# use.  That means that you need to execute the commands to learn
# them properly.  If you only read the text, you will forget the
# commands!
#
# Remember that this is Emacs, you can rebind any binding that you
# don't like. However, a lot of thought has been put into the current
# bindings so don't be surprised if you find stuff inconvenient after
# rebinding without thinking it through.
#
# Your point should now be at beginning of the first line.
# Press =M-<= if it isn't.
#
# Now press =j= to go to Lesson 1.
#
# Press =i= to show/hide the lesson.

#* Lesson 1
# You are now on an outline, since your point is before "#*".
# Since there is one "*", this is a first level outline.
#
# You can press =k= to go back to the introduction.
# Or press =l= to go to the first child outline.

#** Python version:
# You are now on an outline, since your point is before "#**".
# Since there are two "*", this is a second level outline, a child of Lesson 1.
#
# You can press =h= to go back to Lesson 1.
#
# Or press =e= to eval the current outline and move to the next one.
#
# Since this outline ends in ":", the result will be inserted into the
# buffer as a comment.
import sys
print(sys.version)

#** Your OS version:
# You can press =e= to eval this outline.
#
# Or press =h= to go to the parent and =e= to eval the parent.
# Evaluating the parent means evaluating all children outlines.
import os
os.uname()

#** Clean up results
# You can press =C= to clean up the evaluated results
# Try pressing =CkkeeC=.
#
# You can use =j= and =k= to traverse outlines in a structured way,
# i.e. go from outline level 2 to the next outline level 2, even
# though the next closest outlint is an outline level 3.
#
# To traverse outlines in an unstructured way, use =J= and =K=.  This
# is useful now to move from a level 2 outline to Lesson 2, which is a
# level 1 outline.

#* Lesson 2
# In the same way as you use =l= to descend into a child outline, you
# can descent into code.
# You can step through the code with =j= and eval it with =e=.
x = 10
x + x
if x > 1:
    print("x:", x)
