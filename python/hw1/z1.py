# -*- coding: utf-8 -*-

import re
import codecs
from pymorphy import get_morph
morph = get_morph('dicts/ru')

f = codecs.open('book1.txt', encoding='utf-8')

ans=0
for word in re.findall(u'[а-яА-Яё]+', f.read()):
	info = morph.get_graminfo(word.upper())
	if info and info[0]['class']==u'С':
		ans+=1
print ans