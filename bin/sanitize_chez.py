
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
    runtime  = float(t1) * 1000.0
    realtime = float(t2) * 1000.0
    print "RESULT-cpu: %0.1f\nRESULT-gc: 0.0\nRESULT-total: %0.1f\n" % (runtime, realtime),

if __name__ == '__main__':
    main(sys.argv[1:])
    sys.exit(0)

