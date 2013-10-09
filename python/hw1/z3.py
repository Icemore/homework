from __future__ import print_function

def printMatrix(mat):
	for line in mat:
		for elem in line:
			print("%4d" % elem, end=' ')
		print("")

def mulMatrix(a, b):
	c=[]
	for t in xrange(len(a)):
		c.append([0]*len(b[0]))
	
	for k in xrange(len(a[0])):
		for i in xrange(len(a)):
			for j in xrange(len(b[0])):
				c[i][j]+=a[i][k]*b[k][j]
	return c

a=[[1, 2, 3], [2, 4, 5], [4, 2, 5]]
b=[[3, 5, 2], [4, 5, 7], [14, 1, 2]]

c=mulMatrix(a, b)
printMatrix(c)