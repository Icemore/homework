import operator
import math

class Vector:
    def __init__(self, x, y, z):
        self.a = [x, y, z]
        self.a = map(float, self.a)

    def __getitem__(self, item):
        return self.a[item]

    def __setitem__(self, key, value):
        self.a[key] = value
        return self.a[key]

    def __repr__(self):
        return self.a.__repr__()

    def __str__(self):
        return self.a.__str__()

class Matrix:
    def __init__(self, matrix=None):
        if matrix is None:
            self.a = [[0]*3, [0]*3, [0]*3]
        else:
            self.a = matrix

        self.a = [map(float, line) for line in self.a]

    def __add__(self, other):
        res = Matrix()

        for i in xrange(3):
            for j in xrange(3):
                res[i][j] = self.a[i][j] + other[i][j]

        return res
    __radd__ = __add__

    def __mul__(self, other):
        if isinstance(other, int):
            return self._MulScalar(other)
        elif isinstance(other, Vector):
            return self._MulVector(other)
        elif isinstance(other, Matrix):
            return self._MulMatrix(other)
        else:
            raise TypeError("unsupported operand type(s) for +: '{}' and '{}'").format(self.__class__, type(other))

    def _MulScalar(self, num):
        res = Matrix()
        for i in xrange(3):
            for j in xrange(3):
                res[i][j] *= num
        return res

    def _MulVector(self, vec):
        res = Vector(0, 0, 0)
        for i in xrange(3):
            res[i] = sum([x*y for x, y in zip(self.a[i], vec)])
        return res

    def _MulMatrix(self, mat):
        res = Matrix()
        for k in xrange(3):
            for i in xrange(3):
                for j in xrange(3):
                    res[i][j] += self.a[i][k] * mat[k][j]
        return res

    def __invert__(self):
        # find determinant
        det = 0
        for j in xrange(3):
            det += reduce(operator.mul, [self[i][(i+j)%3] for i in xrange(3)], 1)
            det -= reduce(operator.mul, [self[i][(j-i+3)%3] for i in xrange(3)], 1)

        if abs(det) < 1e-10:
            raise ValueError("determinant is zero")

        # calculate inverse matrix using transpose of the matrix of cofactors
        res = Matrix()
        for i in xrange(3):
            for j in xrange(3):
                submat=[]
                for ti in xrange(3):
                    for tj in xrange(3):
                        if ti!=i and tj!=j:
                            submat.append(self[ti][tj])
                res[j][i] = submat[0]*submat[3] - submat[1]*submat[2]
                res[j][i] /= det * (1 if (i+j) % 2 == 0 else -1)

        return res

    def __getitem__(self, item):
        return self.a[item]

    def __repr__(self):
        return self.a.__repr__()

    def __str__(self):
        return '\n'.join([' '.join(map(str, line)) for line in self.a])

class RotationMatrix(Matrix):
    def __init__(self, angle):
        mat=[[math.cos(angle), -math.sin(angle), 0],
             [math.sin(angle), math.cos(angle), 0],
             [0, 0, 1]]
        Matrix.__init__(self, mat)


m = Matrix([[3, 2, 5], [2, 4, 3], [1, 2, 4]])
print ~m * m

rot = RotationMatrix(math.pi/2)
vec = Vector(1, 1, 0)
print rot*vec