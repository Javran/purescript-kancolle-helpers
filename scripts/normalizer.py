#!/usr/bin/python3

import sys
import json

def normalizeJSONFile(filename):
    foname = filename + "_norm"
    with open(filename,"r") as f:
        d = json.load(f)
        with open(foname,"w") as fo:
            json.dump(d,fo,separators=(',', ':'))

    print("{} -> {}".format(filename,foname))

# python's JSON seralization seems to put
# object property in a deterministic manner
# we can take advantage of this to wipe out
# hidden information in a JSON file, if any
if __name__ == '__main__':
    for fn in sys.argv[1:]:
        normalizeJSONFile(fn)
