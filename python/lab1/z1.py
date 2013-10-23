def isOk(n):
	num=n
	for t in (3, 5):
		while num!=1 and num%t==0:
			num/=t
	return num==1

n=int(raw_input())
sum=0
for num in xrange(1, n):
	if isOk(num):
		sum+=num
print sum