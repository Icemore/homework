import itertools

data=[(3, 'a'), (1, 'b'), (2, 'a')]
keyfunc=lambda x: x[1]
res = (sorted(g) for k, g in itertools.groupby(sorted(data, key=keyfunc), key=keyfunc))

for i in res:
    for j in i:
        print j