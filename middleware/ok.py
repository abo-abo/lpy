#* Imports
import sys

#* Classes
class Stack:
    def __init__ (self, tb):
        self.stack = []
        while tb:
            self.stack.append ((tb.tb_frame.f_code.co_filename,
                                tb.tb_frame.f_code.co_firstlineno,
                                tb.tb_frame))
            tb = tb.tb_next
        self.stack_top = len (self.stack) - 1
        self.set_frame (self.stack_top)

    def frame_string (self, i):
        f = self.stack[i][2]
        res = "  File \"%s\", line %d, Frame [%d/%d]:" % \
            (f.f_code.co_filename, f.f_lineno, i, self.stack_top)
        return res

    def __repr__ (self):
        frames = []
        for i in range (self.stack_top + 1):
            s = self.frame_string (i)
            if i == self.stack_idx:
                s += "*"
            frames.append (s)
        return "\n".join (frames)

    def set_frame (self, i):
        f = self.stack[i][2]
        self.stack_idx = i
        tf = top_level ()
        tf.f_globals["lnames"] = f.f_locals.keys ()
        for (k, v) in f.f_locals.items ():
            tf.f_globals[k] = v
        print (self.frame_string (self.stack_idx))

    def up (self, delta = 1):
        if self.stack_idx <= 0:
            print ("top frame already")
            print (self.frame_string (self.stack_idx))
        else:
            self.stack_idx = max (self.stack_idx - delta, 0)
            self.set_frame (self.stack_idx)

    def down (self, delta = 1):
        if self.stack_idx >= self.stack_top:
            print ("bottom frame already")
            print (self.frame_string (self.stack_idx))
        else:
            self.stack_idx = min (self.stack_idx + delta, self.stack_top)
            self.set_frame (self.stack_idx)

class Autocall:
    def __init__ (self, f):
        self.f = f

    def __repr__ (self):
        self.f ()
        return ""

#* Functions
def top_level ():
    """Return the topmost frame."""
    f = sys._getframe ()
    while f.f_back:
        f = f.f_back
    return f

def crash (n = 1):
    """Go up N frames and store that frame's locals to top level globals."""
    tf = top_level ()
    f = sys._getframe ()
    for i in range (n):
        f = f.f_back
    tf.f_globals["lnames"] = f.f_locals.keys ()
    for (k, v) in f.f_locals.items ():
        tf.f_globals[k] = v
    raise RuntimeError ("\n  File \"%s\", line %d:" % (f.f_code.co_filename, f.f_lineno))

def uncrash ():
    """Recover locals from the last traceback."""
    global stack
    stack = Stack (sys.last_traceback)
    tl = top_level ()
    tl.f_globals["up"] = Autocall (stack.up)
    tl.f_globals["dn"] = Autocall (stack.down)

def check (condition):
    if not condition:
        crash (2)
