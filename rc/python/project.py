# -*- coding: utf-8 -*-
"""
Created on Wed Oct 14 01:38:04 2015

@author: matthias
"""

from os import getcwd
from os.path import sep, join, exists

from json import load as loadjson

from socket import gethostname

from pprint import pprint

import logging
logging.basicConfig(level=logging.INFO)

def find_next_settings_files(filename='settings.json'):
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
                break
        else:
            # one directory up in the tree
            cur_dirs = cur_dirs[:-1]
    if not b_settings_found:
        logging.info("no settings file found.")

    return settings

def load_settings(filepath):
    settings = loadjson(filepath)

    markers = {'hauptmaschine': '_home',
               'blabla, hero': '_hero'}

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
                local_marker = markers[gethostname()]

                new_key = basename
                new_value = settings[basename + local_marker]

                new_entries[new_key] = new_value

                for key_to_pop in [basename + marker for marker in markers.values()]:
                    keys_to_pop.append(key_to_pop)

    for key_to_pop in keys_to_pop:
        settings.pop(key_to_pop)

    # add new entries
    for new_key, new_value in new_entries.items():
        settings[new_key] = new_value

    return settings

if __name__ == "__main__":
    settings = find_next_settings_files()

    pprint(settings)
