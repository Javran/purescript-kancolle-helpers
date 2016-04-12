#!/usr/bin/env python3

import json
import sys

if __name__ == '__main__':
    if len( sys.argv ) == 2:
        originFile = sys.argv[1]
        with open(originFile) as f:
            obj = json.load( f )
        normStr = json.dumps( obj, separators=(',', ':') )
        print( normStr )
        pass
    else:
        exit(-1)
    pass
