#!/usr/bin/env python3

import json
import os
import sys

def read_records(path):
    with open(path) as f:
        return [json.loads(l) for l in f.readlines()]

def extract(out, hws):
    dat = [e for r in map(read_records, hws) for e in r]
    min = [(o, e['body'])
           for e in dat
           if 'ocaml' in e
           for o in e['ocaml']
           if e['event']['type'] == 'eval'
           and o['min'] != '']
    #print(len(min))
    good = [e for e in min if e[0]['type'] == 'type' and 'MINIMAL' not in e[0]['out']]
    #print(len(good))
    seen = set()
    uniq = [(o['min'],o['out'],b) for o,b in good
            if not (o['min'] in seen or seen.add(o['min']))]
    #print(len(uniq))
    for i, (m,o,b) in enumerate(uniq, start=1):
        dest = 'cnstr' if 'variant' in o or 'constructor' in o else 'unify'
        path = os.path.join(out, dest, 'prog{:04d}'.format(i))
        with open(path+'.ml', 'w') as f:
            f.write(m)
        with open(path+'.orig.ml', 'w') as f:
            f.write(b)
        with open(path+'.ml.out', 'w') as f:
            f.write(o)

def main():
    out = sys.argv[1]
    hws = sys.argv[2:]
    extract(out, hws)

if __name__ == '__main__':
    main()
