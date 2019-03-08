from z3 import *
s = Solver()
s.from_file(sys.argv[1])
neurons=int(sys.argv[2])
s.check()
a = len(sys.argv)
m = s.model()
for d in m.decls():
	print "%s = %s" % (d.name(), m[d])
