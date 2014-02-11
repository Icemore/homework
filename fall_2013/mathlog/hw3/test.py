d = \
{
	'asdf' : ('orEl', 
	{ 
		'asdasdf' : ('R', {}), 
		'qweqwe' : ('andE', 
		{ 
			'one' : ('R', {}), 
			'two' : ('R', {}) 
		}) 
	})
} 

# d = { 
	# 'asdf' : ('orEl', { 
		# 'asdasdf' : ('R', {}),
		# 'qweqwe' : ('andE', {
			# 'one' : ('R', {}),
			# 'two' : ('R', {})})})	
# }	

def treetoLatex(dict):
	for (k, v) in dict.items():
		treetoLatex(v[1])
		print "\\%s{$%s$}" % (v[0], k)

treetoLatex(d)