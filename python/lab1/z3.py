
ans=0
a, b = 0, 1
while a<4*10**6:
	if a%2==0:
		ans+=a
	a, b = b, a+b
print ans