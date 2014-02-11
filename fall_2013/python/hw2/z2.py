# число хранится в массиве, с цифрами в обратном порядке. первый элемент - знак (1, -1)

def __sum(a, b, sign):
	
	ans = [0] * max(len(a), len(b))
	rem = 0
	for i in xrange(1, len(ans)):
		ans[i] = rem
		ans[i]+= a[i] if i < len(a) else 0
		ans[i]+= sign*b[i] if i<len(b) else 0
		
		rem = ans[i]/10
		ans[i] = ans[i]%10
		
	if rem!=0:
		ans.append(rem)
	
	i=len(ans)-1
	while i>0 and ans[i]==0:
		i=i-1
	
	return ans[:i+1]

def mul(a, b):
	ans=[0]*(len(a)+len(b))
	ans[0]=a[0]*b[0]
	
	for i in xrange(1, len(a)):
		rem=0
		for j in xrange(1, len(b)):
			ans[i+j-1]+=a[i]*b[j]+rem
			rem=ans[i+j-1]/10
			ans[i+j-1]%=10
		ans[i+len(b)-1]+=rem
		
	i=len(ans)-1
	while i>1 and ans[i]==0:
		i=i-1
	
	return ans[:i+1]

def less(a, b, abs=False):
	if not abs and a[0] != b[0]:
		return a[0] < b[0]
	
	if len(a)!= len(b):
		return len(a) < len(b)
	
	res=True
	for i in xrange(len(a)-1, 0, -1):
		if a[i]!= b[i]:
			res=(a[i]<b[i])
			break
	
	if abs:
		return res
	else:
		return (res if a[0]==1 else not res)
	
def add(a, b):
	if a[0]==b[0]:
		ans=__sum(a, b, 1)
		ans[0]=a[0]
	else:
		if less(a, b, abs=True):
			a, b= b, a
		ans=__sum(a, b, -1)
		ans[0]=a[0]
	
	if len(ans)==1:
		ans.append(0)
		ans[0]=1
	
	return ans

def sub(a, b):
	b[0]=-1*b[0]
	ans=add(a, b)
	b[0]=-1*b[0]
	
	return ans

def toString(a):
	res= '-' if a[0]<0 else ''
	res+=''.join([str(x) for x in a[1:][::-1]])
	return res

def init(num):
	ans=[1]
	num=str(num)
	if num[0]=='-':
		ans[0]=-1
		num=num[1:]
	
	ans.extend([int(x) for x in str(num)[::-1]])
	return ans
	
str_a, str_b = raw_input().split()
#str_a, str_b = 234, -2
a=init(str_a)
b=init(str_b)

added = toString(add(a, b))
subed = toString(sub(a, b))
muled = toString(mul(a, b))

print "%s %s + %s = %s" % ("OK" if str(int(str_a)+int(str_b))==added else "WRONG!", str_a, str_b, added)
print "%s %s - %s = %s" % ("OK" if str(int(str_a)-int(str_b))==subed else "WRONG!", str_a, str_b, subed)
print "%s %s * %s = %s" % ("OK" if str(int(str_a)*int(str_b))==muled else "WRONG!", str_a, str_b, muled)