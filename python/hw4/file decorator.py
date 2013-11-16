import sys
import re

fileName = sys.argv[1]

header = """\
import time

_scriptStartTime = time.clock()

def timeDecorator(fn):
    def wrapped(*args, **kwargs):
        print("%.3fms %s" % ((time.clock() - _scriptStartTime)*1000, fn.__name__))
        start = time.clock()
        res = fn(*args, **kwargs)
        end = time.clock()
        print("   +%.3fms" % ((end-start)*1000))
        return res
    return wrapped

"""

with open(fileName) as inputFile:
    fileContent = inputFile.read()

executable = header
executable += re.sub("(.*?)(def .*)", r"\1@timeDecorator\n\1\2", fileContent)

exec(executable)

