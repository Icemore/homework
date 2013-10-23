import math

def isPrime(num):
	for t in xrange(2, int(math.sqrt(num)+1)):
		if num%t==0:
			return False
	return True

cnt=1
num=2
while cnt!=10001:
	num+=1
	if isPrime(num):
		cnt+=1
print num
	