
import sys
import re
import os


def main(args):
    data = []
    chunk = sys.stdin.read(4096)
    while chunk:
        data.append(chunk)
        chunk = sys.stdin.read(4096)

    data = "".join(data)

    reg1 = re.compile(r"\s*([\d\.]*)s elapsed cpu time")
    reg2 = re.compile(r"\s*([\d\.]*)s elapsed real time")
    t1 = reg1.findall(data)[0]
    t2 = reg2.findall(data)[0]
    print "RESULT-cpu:%s\nRESULT-gc: 0\nRESUL-total:%s\n" % (t1, t2),

if __name__ == '__main__':
    main(sys.argv[1:])

