import time

spy_data = {}

def spy(fn):
    fn_dict = {'time': [], 'args': []}
    spy_data[fn.__name__] = fn_dict

    def wrapped(*args):
        fn_dict['time'].append(time.clock())
        fn_dict['args'].append(args)

        return fn(*args)
    return wrapped

def bond(fn_name):
    return spy_data[fn_name]

@spy
def fn(one, two):
    return one**2 + two

fn(5, 3)
fn(2, 5)

print(bond("fn"))