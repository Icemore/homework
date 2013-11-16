class Singleton:
    def __init__(self, decorated):
        self._decorated = decorated
        self._instance = None

    def __call__(self, *args, **kwargs):
        if not self._instance:
            self._instance = self._decorated(*args, **kwargs)
        return self._instance


@Singleton
class F:
    pass

t = F()
q = F()

print(t is q)