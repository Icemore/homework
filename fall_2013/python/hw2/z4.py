import requests
import json
from urlparse import urlparse

search="python"
js=json.loads(requests.get("http://ajax.googleapis.com/ajax/services/search/web?v=1.0&rsz=8&q=%s"%search).text)

res={}
for site in js[u'responseData'][u'results']:
	url = site[u'url']
	domain = urlparse(url).netloc.split('.')[-1]
	if domain not in res:
		res[domain]=0
	res[domain]+=1
	
for k, v in res.items():
	print "%s: %s%%" %(k, v/8.0*100)
