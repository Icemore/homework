class Chess:
    def __init__(self, start):
        self.__beg = start

    def coordToPair(self, point):
        return (ord(point[0])-ord('a'), ord(point[1]) - ord('1'))

    def pairToCoord(self, t):
        return chr(t[0]+ord('a')) + chr(t[1] + ord('1'))

    def __call__(self):
        dx = [-2, -2, -1, -1, 1, 1, 2, 2]
        dy = [-1, 1, -2, 2, -2, 2, -1, 1]

        start = self.coordToPair(self.__beg)
        used = [[False] * 8 for i in range(8)]

        q = [start]
        used[start[0]][start[1]] = True

        while len(q) > 0:
            cur = q.pop(0)
            yield self.pairToCoord(cur)

            for t in range(8):
                to = (cur[0]+dx[t], cur[1]+dy[t])

                if to[0]>=0 and to[0]<8 and to[1]>=0 and to[1]<8:
                    if not used[to[0]][to[1]]:
                        q.append(to)
                        used[to[0]][to[1]]=True

a = Chess("e4")
for i in a():
    print(i)