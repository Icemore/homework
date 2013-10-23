def gcd(a, b):
	if b==0:
		return a
	else:
		return gcd(b, a%b)

n=int(raw_input())
ans=1
for t in xrange(1, n+1):
	ans*=t/gcd(ans, t)
print ans