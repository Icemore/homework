class DeniedKeyException(Exception):
    pass


class MyDict(dict):
    _initialized = False
    def __init__(self, denied_keys=None):
        self.denied_keys = denied_keys
        self.ordered_keys = []
        self._initialized = True

    def __getattr__(self, item):
        return self.__getitem__(item.replace('_', ' '))

    def __setattr__(self, key, value):
        if not self._initialized:
            dict.__setattr__(self, key, value)
        else:
            return self.__setitem__(key.replace('_', ' '), value)

    def __setitem__(self, key, value):
        if key not in self.denied_keys:

            if isinstance(key, int):
                key = self.ordered_keys[key]

            if key not in self.ordered_keys:
                self.ordered_keys.append(key)

            return dict.__setitem__(self, key, value)
        else:
            raise DeniedKeyException("Key '{}' is denied.".format(key))

    def __getitem__(self, key):
        if isinstance(key, int):
            key = self.ordered_keys[key]

        return dict.__getitem__(self, key)


if __name__ == "__main__":
    d = MyDict(("one", "two"))

    try:
        d.a_one="qwe"
        d["asd"] = 4342
        d[0]="vfr"
        d.one = 32
    except DeniedKeyException, e:
        print e

    print d
    print d[1]
