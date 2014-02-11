class ClassBase(type):
    def __init__(cls, name, bases, dct):
        super().__init__(name, bases, dct)
        cls.addFieldsFromFile(name)

    def addFieldsFromFile(cls, name):
        with open(name + '.txt') as file:
            for line in file:
                key, val = line.strip().split(':')
                setattr(cls, key, val[1:])

class A(metaclass=ClassBase):
    pass

a = A()
print(a.x)
print(a.y)