# -*- coding: utf-8 -*-
"""
Created on Wed Oct 14 01:38:04 2015

@author: matthias
"""

from os import getcwd
from os.path import sep, join, exists

from json import load as loadjson, dump as dumpjson

from socket import gethostname

from pprint import pprint

import logging
logging.basicConfig(level=logging.INFO)

import datetime

import shutil

import re

from os import makedirs

global settings
global project_base_dir

def find_next_settings_files(filename='settings.json', b_return_path=False):
    # traverse the directories upwards to find and load the project's settings file
    cur_dirs = getcwd().split(sep)
    settings = None

    b_settings_found = False
    while len(cur_dirs) > 1:
        filepath = join(sep.join(cur_dirs), filename)

        if exists(filepath):
            with open(filepath) as file:
                settings = load_settings(file)
                logging.info("settings file found: {}".format(filepath))
                b_settings_found = True

                settings['project_base_dir'] = join(sep.join(cur_dirs))
                break
        else:
            # one directory up in the tree
            cur_dirs = cur_dirs[:-1]
    if not b_settings_found:
        logging.info("no settings file found.")

    if b_return_path:
        return settings, filepath
    else:
        return settings
        
def load_settings(filepath):
    settings = loadjson(filepath)

    markers = {'hauptmaschine': '_home',
               'hero\d*': '_hero',
               'mpc.*': '_hero'}

    # process "special" entries (ending on "_hero"/"_work")
    remaining_items = settings.items()
    keys_to_pop = []
    new_entries = {}
    for index, (key, value) in enumerate(remaining_items):
        if key in keys_to_pop:
            continue

        b_markers_found = [False] * len(markers)
        
        if key.endswith(tuple(markers.values())):
            # the current key ends with one of the markers

            # try to find all markers
            for idx_marker, (marker_name, marker) in enumerate(markers.items()):
                for key2, value2 in remaining_items:
                    pos_marker_start = key2.find(marker)

                    if pos_marker_start != -1:
                        b_markers_found[idx_marker] = True

                        break

                if all(b_markers_found):
                    break

            if all(b_markers_found):
                # find which marker we have
                for idx_marker, (marker_name, marker) in enumerate(markers.items()):
                    pos_marker_start = key.find(marker)

                    if pos_marker_start != -1:
                        # found_marker = marker

                        basename = key[:pos_marker_start]

                # pick the value from the settings list
                # 1. find the marker that applies here
                b_match = False
                for marker_key, marker_value in markers.items():
                    b_match = re.match(marker_key + '$', gethostname())
                    if b_match:
                        break
                    
                if b_match:
                    new_key = basename
                    new_value = settings[basename + marker_value]
    
                    new_entries[new_key] = new_value
    
                    for key_to_pop in [basename + marker for marker in set(markers.values())]:
                        keys_to_pop.append(key_to_pop)
                else:
                    logging.info("marker \"" + basename + "\" seems to be valid, but none of the hostnames (" + str(markers.keys()) + ") can be found.")
                    logging.info("(the current hostname is \"" + gethostname() + "\".")
                    
    for key_to_pop in keys_to_pop:
        settings.pop(key_to_pop)

    # add new entries
    for new_key, new_value in new_entries.items():
        settings[new_key] = new_value

    return settings
    
def new_setting(key, value):
    # try to load the settings
    settings, settings_path = find_next_settings_files(b_return_path=True)
    
    # check whether the key is already in there
    if key in settings.keys():
        logging.info("the key is already stored.")
    
    # pre-process the value
    if isinstance(value, str):
        value.replace('\n', '\\n')
    settings[key] = value
    
    # store the settings
    with open(settings_path, 'w') as file:
        dumpjson(settings, file)
        logging.info("new settings written.")

#settings = find_next_settings_files()

def store_file(filename, comment="quicksave"):
    # create timestamp
    # timestamp = datetime.datetime.strftime(datetime.datetime.now(), "%Y-%M-%d_%H-%m-%S")

    dir_output = datetime.datetime.strftime(datetime.datetime.now(), "%Y-%m-%d")
    filename_output = datetime.datetime.strftime(datetime.datetime.now(), "%H-%M-%S")

    dir_output = join(settings['project_base_dir'], 'inbox', dir_output)

    if not exists(dir_output):
        makedirs(dir_output)

    # copy the file to the target directory
    shutil.copyfile(src=filename, dst=join(dir_output, filename_output + '_' + filename))

    # create a txt file in the target directory to store the comment
    with open(join(dir_output, filename_output + '_' + filename + '_comment.txt'), 'w') as file:
        file.write(comment)

    # return timestamp

if __name__ == "__main__":
    settings = find_next_settings_files()

    pprint(settings)

    print(store_file("test.csv"))
