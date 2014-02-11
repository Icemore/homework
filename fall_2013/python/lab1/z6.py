l=[1, 3, 5, 2, 2, 3, 5, 7, 2]
ans=[]
i=0
while i<len(l):
	ans.append(tuple(l[i:i+l[i]]))
	i+=l[i]
print ans