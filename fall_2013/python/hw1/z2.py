import pyprimes

def test(num):
	fact = pyprimes.factors(num)
	return len(fact)==2 and len(set(fact))==2

n=int(raw_input())
for num in xrange(2, n):
	if test(num):
		print num
