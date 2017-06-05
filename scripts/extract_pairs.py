#!/usr/bin/env python3

import argparse
from collections import OrderedDict
import json
import re
import os
import subprocess,shlex,shutil
from subprocess import check_output
import sys
import tempfile
from threading import Timer

class OrderedDefaultListDict(OrderedDict): #name according to default
  def __missing__(self, key):
    self[key] = value = [] #change to whatever default you want
    return value

# homework problem list
hw1 = ['palindrome', 'listReverse', 'digitalRoot', 'additivePersistence', 'digitsOfInt', 'sumList','???']
hw2 = ['build', 'eval', 'exprToString', 'fixpoint', 'wwhile', 'removeDuplicates', 'assoc','???']
hw3 = ['bigMul', 'mulByDigit', 'bigAdd', 'removeZero', 'padZero', 'clone', 'stringOfList', 'sepConcat', 'pipe', 'sqsum','???']

# homework problem signature list
signature1 = ['let palindrome w',
              'let rec listReverse l',
              'let rec digitalRoot n',
              'let rec additivePersistence n',
              'let rec digitsOfInt n',
              'let rec sumList xs',
              '']
signature2 = ['let rec build (rand, depth)',
              'let rec eval (e,x,y)',
              'let rec exprToString e',
              'let fixpoint (f,b)',
              'let rec wwhile (f,b)',
              'let removeDuplicates l',
              'let rec assoc (d,k,l)',
              '']
signature3 = ['let bigMul l1 l2',
              'let rec mulByDigit i l',
              'let bigAdd l1 l2',
              'let rec removezero l',
              'let padZero l1 l2',
              'let rec clone x n',
              'let stringOfList f l',
              'let rec sepConcat sep sl',
              'let pipe fs',
              'let sqsum xs',
              '']


"""
Match home work with the corresponding set of problems
Input: 'hw1'
Output: ['palindrome', 'listReverse', ...]
Returns empty list if none can be matched
"""
def find_problem_set(hw_num):
  if hw_num == 'hw1':
    return hw1
  if hw_num == 'hw2':
    return hw2
  if hw_num == 'hw3':
    return hw3
  return list()


"""
Match home work with the corresponding set of problems
Input: 'hw1'
Output: ['palindrome', 'listReverse', ...]
Returns empty list if none can be matched
"""
def find_signature_set(hw_num):
  if hw_num == 'hw1':
    return signature1
  if hw_num == 'hw2':
    return signature2
  if hw_num == 'hw3':
    return signature3
  return list()


"""
Find which homework function they are working on
Input
arg 1: 'hw1'
arg 2: 'string_of_code'
Output: 'function_name'
"""
def find_function_name(hw_num, code):
  signature_set = find_signature_set(hw_num)
  problem_set = find_problem_set(hw_num)
  label = ''
  for i in range(len(signature_set)):
    if signature_set[i] in code:
      label = problem_set[i]
      break
  if not label:
    label = '???'
  return label


"""
Find out the hw_num and author of the file
Input: 'student_name.hw1.ml.json'
Output: [student_name, hw1]
"""
def file_info(inputFile):
  return str.split(os.path.basename(inputFile), '.')


problems1 = hw1[::-1]
problems2 = hw2[::-1]
problems3 = hw3[::-1]

annotation1 = ["",\
  " : int list -> int", \
  " : int -> int list", \
  " : int -> int", \
  " : int -> int", \
  " : 'a list -> 'a list", \
  " : string -> bool"]

annotation2 = ["",\
  " : 'a * 'b * ('b * 'a) list -> 'a ", \
  " : 'a list -> 'a list", \
  " : ('a -> 'a * bool) * 'a -> 'a", \
  " : ('a -> 'a) * 'a -> 'a", \
  " : expr -> string", \
  " : expr * float * float -> float", \
  " : ((int * int -> int) * int) -> expr"]

annotation3 = ["",\
  " : int list -> int ", \
  " : ('a -> 'a) list -> ('a -> 'a)", \
  " : string -> string list -> string", \
  " : ('a -> string) -> 'a list -> string", \
  " : 'a -> int -> 'a list", \
  " : int list -> int list -> int list  * int list", \
  " : int list -> int list", \
  " : int list -> int list -> int list", \
  " : int -> int list -> int list ", \
  " : int list -> int list -> int list"]

# add annotation for given program
def add_annotation(annotation, problem_name, code):
  var_regex = '(?<=' + problem_name + ' )(.*?)(?=\=)'   # match the variables
  var = re.search(var_regex, code)
  my_regex = '(?<=' + problem_name + ' )(.*?)\='        # match the string to replace
  result = re.sub(my_regex, lambda match: replace(match, var.group(), annotation), code,1)
  return result

# helper for regex sub to add annotation
def replace(match, variables, annotation):
  replacement = annotation
  replacement += ' = fun '
  replacement += variables
  replacement += ' -> '
  return replacement

# given a function name, find out which hw it is
def find_annotation_with_label(hw_num, label):
  annotation = ''
  if (hw_num == 'hw1'):
    annotation = annotation1[problems1.index(label)]
  elif (hw_num =='hw2'):
    annotation = annotation2[problems2.index(label)]
  elif (hw_num == 'hw3'):
    annotation = annotation3[problems3.index(label)]
  return(annotation)

def annotate_and_compile(indice, label, hw_num):
  annotation = find_annotation_with_label(hw_num, label)
  annotated_prog = add_annotation(annotation, label, indice['min'])

  try:
    error_output = check_output(['ocaml'], input = annotated_prog, stderr = subprocess.PIPE,
      universal_newlines = True, timeout = 5)
  except subprocess.TimeoutExpired:
    print('timeout')
    error_output = 'Expired'

  if(error_output == 'Expired'):
    return 'Runtime Error: timeout'

  if 'rror' in error_output:
    return error_output
  else:
    return ''


"""
Goal: Pre-process the dataset to feed into list_of_errors

Input: (sp14)
{
  'file': 'hw1.ml',
  'ocaml': [{
              'in': 'string_of_code',
              'min': 'string_of_code',
              'type': '',
              'out': ''
            }],
  'event': {
              'region': { 'start': 12, 'stop': 14 },
              'type': 'eval'
            },
  'cursor': 324,
  'time': 34534536,
  'body': 'bunch_of_code'
}

Output:
{
  'palindrome': [{
              'type': '',
              'in': 'string_of_code',
              'min': 'string_of_code',
              'out': ''
              }]
}
"""

# helper that appends all qualifying events to the big dictionary
def runForEachJsonObject(item, hw_num, dic):
  if item['event']['type'] != 'eval': return
  for v in item['ocaml']:
    if v['in'].startswith('let _ ='): continue
    if v['type'] not in ['', 'type']: continue
    if 'MINIMAL' in v['out']: continue

    label = find_function_name(hw_num, v['in'])
    if label == '???': continue

    dic[label].append(v)


# build a dict for each student file that holds all events for each problem
def do_preprocess(inputFile, outputFile):
  info = file_info(inputFile)
  student = info[0]
  hw_num = info[1]

  with open(inputFile, 'r') as inf, open(outputFile, 'w') as of:
    events_by_func = OrderedDefaultListDict()
    for line in inf:
      runForEachJsonObject(json.loads(line), hw_num, events_by_func)
    json.dump(events_by_func, of)
    of.write('\n')

  inf.close()
  of.close()


"""
Goal: Obtain a list of errors that pair a list of bad events to one fix

Input: (sp14_preprocessed)
{
  'palindrome': [{
              'type': '',
              'in': 'string_of_code',
              'min': 'string_of_code',
              'out': ''
              }]
}


Output:
{
  'hw': 'hw1',
  'problem': 'palindrome',
  'bad': ['string_of_code_1', 'string_of_code_2']
  'fix': 'string_of_code'
}
"""

# initialize a dictionary for pairing up the bad programs with a fix
def build_dict(hw_num, problem):
  new_dict = OrderedDict()
  new_dict['hw'] = hw_num
  new_dict['problem'] = problem
  new_dict['bad'] = []
  new_dict['fix'] = ''
  return new_dict

# whether the program in an event has a type error
def is_bad(event):
  return event['type'] == 'type'

# whether the program in an event is an actual fix
def is_true_fix(event, problem, hw_num):
  if 'failwith' in event['min']: return False         # if the student uses a 'failwith' to avoid type errors, skip it
  # TODO: maybe we DO want to use the ERSP folks type_annotate_and_compile check?
  # would restrict to programs that not only type-check, but have the CORRECT type.
  # better training source?
  #return True
  ret = annotate_and_compile(event, problem, hw_num)
  return False if 'rror' in ret else True

# given a list of problems, find all bad-fix pairs and write to the output file
def pair_up(hw_num, problem, list_of_events, outputFile):
  # print('NEW PROBLEM', problem)
  index = 0
  dic = build_dict(hw_num, problem)

  while index < len(list_of_events):
    event = list_of_events[index]
    index = index + 1
    if event['min'] == '': continue
    if is_bad(event) and event['min'] not in dic['bad']:
      # type error
      # print('bad')
      dic['bad'].append(event['min'])
    elif len(dic['bad']) == 0:                      # cannot find bad programs to pair (not a fix)
      # print('not a fix')
      continue
    elif not is_true_fix(event, problem, hw_num):   # false fix
      # print('false fix')
      continue
    else:                                           # true fix
      # print('true fix')
      dic['fix'] = event['min']
      #dic['bad'] = dic['bad']
      json.dump(dic, outputFile)
      outputFile.write('\n')
      dic = build_dict(hw_num, problem)

  #if len(dic['bad']) > 0:                          # reached end of list, no fix can be found
  #  dic['bad'] = list(dic['bad'])
  #  json.dump(dic, outputFile)
  #  outputFile.write('\n')


def do_pair(inputFile, outputFile):
  info = file_info(inputFile)
  student = info[0]
  hw_num = info[1]
  # print('pairing up bad/fix programs for:', student, hw_num)

  inf = open(inputFile, 'r')
  of = open(outputFile, 'a')

  for key, value in sorted(json.load(inf).items(), key=lambda p: p[0]):
    pair_up(hw_num, key, value, of)

  inf.close()
  of.close()

"""
Goal: create pairs of bad/fix programs with a single bad program and a single fix program

Input:
{
  'hw': 'hw1',
  'problem': 'palindrome',
  'bad': ['string_of_code_1', 'string_of_code_2']
  'fix': 'string_of_code'
}

Output:
{
  'index': 1,
  'hw': 'hw1',
  'problem': 'palindrom',
  'bad': 'string_of_code_1',
  'fix': 'string_of_code'
}
{
  'index': 2,
  'hw': 'hw1',
  'problem': 'palindrom',
  'bad': 'string_of_code_2',
  'fix': 'string_of_code'
}

How to Run:
$ cd codes
$ python main.py -m bf_split.run -s sp14_paired -r result

"""

index = 0

def split(inputFile, outputFile):
  global index
  for line in inputFile:
    item = json.loads(line)
    hw = item['hw']
    problem = item['problem']
    fix = item['fix']
    for i in item['bad']:
      # ensure consistent traversal order,
      # so git doesn't think files have changed between runs
      myDic = OrderedDict()
      myDic['hw'] = hw
      myDic['index'] = index
      myDic['problem'] = problem
      myDic['bad'] = i
      myDic['fix'] = fix

      index = index + 1
      json.dump(myDic, outputFile)
      outputFile.write('\n')


def do_split(inputFile, outputFile):
  with open(inputFile, 'r') as inf, open(outputFile, 'a') as of:
    split(inf, of)


if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('-s', type=str, required=True, help='the source directory')
  parser.add_argument('-o', type=str, required=True, help='the output directory')

  args = parser.parse_args(sys.argv[1:])

  src = args.s
  out = args.o

  # preprocess
  tmp1 = tempfile.mkdtemp()
  for i in os.listdir(src):
    inputFile = os.path.join(src, i)
    outputFile = os.path.join(tmp1, i)
    do_preprocess(inputFile, outputFile)

  # pair
  tmp2 = tempfile.mkdtemp()
  for i in os.listdir(tmp1):
    inputFile = os.path.join(tmp1, i)
    outputFile = os.path.join(tmp2, i)
    do_pair(inputFile, outputFile)

  # split
  if not os.path.exists(out):
    os.makedirs(out)
  outputFile = os.path.join(out, 'pairs.json')
  if os.path.exists(outputFile):
    os.remove(outputFile)
  for i in os.listdir(tmp2):
    inputFile = os.path.join(tmp2, i)
    do_split(inputFile, outputFile)

  shutil.rmtree(tmp1)
  shutil.rmtree(tmp2)

