class Abstract(type):
    def __call__(cls, *args, **kwargs):
        for base in [cls]+list(cls.__bases__):
            for item in base.__dict__:
                if cls.isAbstract(base.__dict__[item]):
                    if item not in cls.__dict__ or cls.isAbstract(cls.__dict__[item]):
                        raise TypeError("Class %s has unimplemented abstract method %s" % (base.__name__, item))

        return super().__call__(*args, **kwargs)

    def isAbstract(cls, fn):
        return hasattr(fn, "__abstract")

def abstract(fn):
    setattr(fn, "__abstract", None)
    return fn

class Interface(object, metaclass=Abstract):
    @abstract
    def foo(self):
        pass

    def bar(self):
        print("bar")

class Implementation(Interface):
    def foo(self):
        print("implemented foo")

f = Implementation()
b = Interface()