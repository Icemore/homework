import itertools
import random

class random_iterator:
    def __init__(self, n):
        self.cnt=0
        self.maxn=n

    def __iter__(self):
        return self

    def next(self):
        self.cnt+=1
        if self.cnt <= self.maxn:
            return random.randint(-100, 100)
        raise StopIteration()

def merge(it1, it2):
    j = next(it2, None)
    for i in it1:
        while (j is not None) and j<i:
            yield j
            j = next(it2, None)
        yield i
    while j is not None:
        yield j
        j=next(it2, None)

def merge_sort(it, n):
    if n < 2:
        return it

    left=merge_sort(itertools.islice(it, n/2), n/2)
    right = merge_sort(it, n-n/2)
    return merge(left, right)

n=10
for i in merge_sort(random_iterator(n), n):
    print i