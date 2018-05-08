#!/usr/bin/env python
from __future__ import (absolute_import, division, print_function, unicode_literals)

import sys
import shutil
import os
from os import path

try:
  input = raw_input
except:
  pass

orig_dir = path.abspath('./home')
dest_dir = path.expanduser('~')
recurse_ext = ".recurse"

def answer(question):
  while True:
    print(question + ' [y/N] ', end='')
    try:
      choice = input().lower()
    except KeyboardInterrupt as e:
      print('Aborting.')
      sys.exit(1)
    if choice == '' or choice[0] == 'n':
      return False
    if choice[0] == 'y':
      return True
    else:
      print('Please respond with "yes" or "no" (or "y" or "n").')

orig_entries=[]
for entry in os.listdir(orig_dir):
  entry = path.realpath(path.join(orig_dir, entry))
  if entry.endswith(recurse_ext):
    continue
  if not path.isdir(entry):
    orig_entries.append(entry)
  else:
    flag = entry + recurse_ext
    if not path.exists(flag):
      orig_entries.append(entry)
    else:
      for dirpath, _, filenames in os.walk(entry):
        for f in filenames:
          sub_entry = path.join(dirpath, f)
          if path.isfile(sub_entry):
            orig_entries.append(sub_entry)

for orig_entry in orig_entries:
  entry=path.relpath(orig_entry, orig_dir)
  dest_entry = path.join(dest_dir, entry)
  print('Entry "%s" -> "%s"' % (entry, dest_entry))

  skip = False
  if path.exists(dest_entry):
    if path.islink(dest_entry) and path.realpath(dest_entry) == orig_entry:
        print('Nothing to do. Destination already points to source')
        skip = True
    else:
      print('Destination already exists and does not point to source.')
      if not answer('Do you want to replace "%s"?' % dest_entry):
        skip = True
      else:
        print('Removing "%s"' % dest_entry)
        try:
          if path.isdir(dest_entry):
            shutil.rmtree(dest_entry)
          else:
            os.remove(dest_entry)
        except Exception as e:
          print('Failed to delete "%s": %s"' % (dest_entry, str(e)))
          skip = True

  if skip:
    print('Skipping "%s"' % dest_entry)
  else:
    print('Creating symbolic link "%s" pointing to "%s".' % (dest_entry, orig_entry))
    try:
      dirname = path.dirname(dest_entry)
      if not path.exists(dirname):
        os.makedirs(dirname)
      os.symlink(orig_entry, dest_entry)
    except Exception as e:
      print('Failed to create symbolic link "%s": %s"' % (dest_entry, str(e)))
  print()

