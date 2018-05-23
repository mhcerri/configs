#! /usr/bin/env python2
import re
from subprocess import check_output

def gpg_read(file_path):
    "Get contents from a GPG encrypted file."
    return check_output("gpg2 -dq %s" % (file_path), shell=True).strip("\n")

def folder_filter(folder):
    "Return true for folders that should be processed."
    exclude = "(^\[Gmail\]$|All Mail$|Spam$|^LKML)"
    return not re.search(exclude, folder)

def folder_transform(folder):
    "Normalize folder names."
    # Remove weird gmail prefix
    folder = re.sub("^\[Gmail\]/?", "", folder)
    # Normalize folder names
    folder = re.sub ("Sent mail", "Sent", folder)
    folder = re.sub ("Starred", "Flagged", folder)
    return folder

def folder_cmp(x, y):
    "Prioritize special folders."
    for prefix in ["INBOX"]:
        x_has_priority = x.startswith(prefix)
        y_has_priority = y.startswith(prefix)
        if x_has_priority and y_has_priority:
            return cmp(x, y)
        elif x_has_priority:
            return -1
        elif y_has_priority:
            return +1
    return cmp(x, y)
