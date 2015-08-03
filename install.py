#!/usr/bin/env python
from __future__ import (absolute_import, division, print_function, unicode_literals)

import sys
import shutil
import os
from os import path

orig_dir = path.abspath('./home')
dest_dir = path.expanduser('~')

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

for entry in os.listdir(orig_dir):
  orig_entry = path.realpath(path.join(orig_dir, entry))
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
      os.symlink(orig_entry, dest_entry)
    except Exception as e:
      print('Failed to create symbolic link "%s": %s"' % (dest_entry, str(e)))
  print()

