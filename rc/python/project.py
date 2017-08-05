# -*- coding: utf-8 -*-
"""
Created on Wed Oct 14 01:38:04 2015

@author: matthias
"""

from os import getcwd, rename
from os.path import sep, join, exists, split, dirname, basename, splitext, abspath


from json import load as loadjson, dump as dumpjson

from socket import gethostname

from pprint import pprint

from inspect import getsource, getmodule, stack
from importlib import import_module

import logging
logging.basicConfig(level=logging.INFO)

import datetime

import shutil

import re

import pandas as pd

from os import makedirs, listdir

global settings
global project_base_dir

# load local settings
if exists(join(dirname(__file__), "settings.py")):
    from settings import *

def athome():
    return gethostname() == 'hauptmaschine'

if athome():
    # stuff concerning ggplot2
    from rpy2.robjects.packages import importr
    import rpy2.robjects as robj
    import rpy2.robjects.lib.ggplot2 as ggplot
    import rpy2.robjects.pandas2ri
    from rpy2.rinterface import RRuntimeWarning
    robj.pandas2ri.activate()

    plot_fixed_width = robj.r("""
            function(p) {
            grob = ggplotGrob(p)

            # TODO: the magic 3 here could lead to problems. find the right "spacer" element...
            grob$widths[3] = unit(1, "cm")

            grid.draw(grob)
            }
        """)

def point2inch(point):
    return point / 72

textwidth_pt = 386.95749

golden_ratio = (1 + 5**(1/2)) / 2





# globals
GRAPHIC_FORMATS = ['tikz', 'pdf']

def cleancode(code):
    # find the number of tabs in the first line
    N_spaces = 0
    while code[N_spaces] == " ":
        N_spaces += 1

    lines = code.split("\n")

    for idx, line in enumerate(lines):
        lines[idx] = line[N_spaces:]

    return "\n".join(lines)


def find_settings_file(filename='settings.json'):
    # traverse the directories upwards to find and load the project's settings file
    cur_dirs = getcwd().split(sep)
#    settings = None

    b_settings_found = False
    while len(cur_dirs) > 1:
        filepath = join(sep.join(cur_dirs), filename)

        if exists(filepath):
            with open(filepath) as file:
#                settings = load_settings(file)
                logging.info("settings file found: {}".format(filepath))
                b_settings_found = True

#                settings['project_base_dir'] = join(sep.join(cur_dirs))
                break
        else:
            # one directory up in the tree
            cur_dirs = cur_dirs[:-1]
#    if not b_settings_found:
#        logging.info("no settings file found.")
    if b_settings_found:
        return sep.join(cur_dirs), filename
    else:
        raise Exception("no settings file found.")
        
def load_settings(process_markers=True):
    # first get the path of the settings file
    settings_filepath, settings_filename = find_settings_file()
    
    if settings_filepath:
        with open(join(settings_filepath, settings_filename)) as file:
            settings = loadjson(file)
            logging.info("settings file loaded.")
            settings['project_base_dir'] = settings_filepath
    else:
        logging.info("no settings file found.")
    
#    settings = loadjson(filepath)

    markers = {'hauptmaschine': '_home',
               'hero\d*': '_hero',
               'mpc.*': '_hero',
               'vxs\d*': '_hero'}
               
    if process_markers:
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
    settings_path, settings_filename = find_settings_file()
    
    settings = load_settings(process_markers=False)
    
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

    settings = load_settings()

    dir_output = datetime.datetime.strftime(datetime.datetime.now(), "%Y-%m-%d")
    filename_output = datetime.datetime.strftime(datetime.datetime.now(), "%H-%M-%S")

    dir_output = join(settings['project_base_dir'], 'inbox', dir_output)

    if not exists(dir_output):
        makedirs(dir_output)
        
    # obtain the full path to the source file
    filepath = join(getcwd(), filename)

    # copy the file to the target directory
    shutil.copyfile(src=filepath, dst=join(dir_output, filename_output + '_' + filename))

    # create a txt file in the target directory to store the comment
    with open(join(dir_output, filename_output + '_' + filename + '_comment.txt'), 'w') as file:
        file.write(comment)

    # return timestamp


def save_figure(f_plot, tab_data, filename_output, b_serialize=True):
    if b_serialize:
        # write the code of the plot function to a file in the target directory
        dir_target, file_target = split(filename_output)

        with open(filename_output + '.py', 'w') as f:
            f.write("import sys\n")
            f.write("import numpy as np\n")
            f.write("sys.path.append(\"{}\")\n".format(dirname(__file__)))
            # f.write("from project import ggplot, theme_minimal, theme_my, plot_fixed_width\n")
            f.write("from project import plot_fixed_width\n")
            f.write("import rpy2.robjects as robj\n\n")
            f.write(cleancode(getsource(f_plot)))
            # f.write("\n    width, height = set_panel_size(pp)\n")
            # f.write("\n    pp.plot()\n")
            # f.write("    plot_fixed_width(pp)")
            # f.write("    return width, height")

        def savedata(data, filename_output):
            # write file type information to the file
            with open(filename_output, 'w') as file:
                file.write(str(type(data)) + "\n")
            if isinstance(data, pd.DataFrame):
                data.to_csv(filename_output, mode='a')
            elif isinstance(data, float):
                with open(filename_output, 'a') as f:
                    f.write(str(data))

        # write the data
        if isinstance(tab_data, list):
            # this is newly added support for multiple parameters
            for idx, param in enumerate(tab_data):
                filename_output_iter = filename_output + '_' + str(idx) + '.data'
                savedata(param, filename_output_iter)
        else:
            # the usual case: one table
            #tab_data.to_csv(filename_output + '.data')
            savedata(tab_data, filename_output + '.data')

        generate_figure(filename_output)
        return
    else:
        generate_figure(filename_output)

def generate_figure(filepath):
    # this is all for plotting functions ---


    from warnings import filterwarnings

    # switch off R warnings
    filterwarnings("ignore", category=RRuntimeWarning)

    grdevices = importr("grDevices")
    tikzDevice = importr("tikzDevice")
    scales = importr("scales")
    grid = importr("grid")


    # ---

    # load the data

    # here starts the newly added support for multiple parameters

    # first: find the associated files
    # N_files = 1
    # find the file names of data files
    data_file_names = []
    for file in listdir(dirname(filepath)):
        if splitext(file)[1] != '.data':
            continue
        file = splitext(file)[0]
        data_file_names.append(file)

    print(data_file_names)

    cur_data_file_names = []
    indices = []
    for file in data_file_names:
        re_result = re.match("^{}((?=_)_(?P<idx>[0-9])*)?$".format(basename(filepath)), file)
        if re_result:
            cur_data_file_names.append(file)
            idx = re_result.group('idx')
            if idx:
                indices.append(int(idx))
            else:
                indices.append(len(cur_data_file_names)-1)

    cur_data_file_names = [cur_data_file_names[_] for _ in indices]

    # this is a function that reads data from ascii files
    def loaddata(filename):
        with open(filename + '.data', 'r') as file:
            format = file.readline()[:-1] # remove newline character

        if format == "<class 'pandas.core.frame.DataFrame'>":
            return pd.read_csv(filename + '.data', skiprows=1)
        elif format == '<class \'float\'>':
            with open(filename + '.data', 'r') as file:
                for _ in range(2):
                    line = file.readline()

                return float(line)
        else:
            print(format)
            raise RuntimeError('unknown format')

    if len(cur_data_file_names) == 1:
        tab_data = loaddata(join(dirname(filepath), cur_data_file_names[0])) #pd.read_csv(file[0] + '.data')
    else:
        tab_data = []
        for file in cur_data_file_names:
            tab_data.append(loaddata(join(dirname(filepath), file)))

    path_package = split(filepath)[0].replace(".", "").replace("/", ".")
    filename = split(filepath)[1]

    frm = stack()[2]
    mod = getmodule(frm[0])

    # plot = import_module(path_package + '.' + filename, package=mod.__name__)
    with open(filepath + ".py", "r") as f:
        code = f.read()

    # add additional ggplot themeing code from the settings.py file
    code = code[:-1] + " \\\n"
    code += " " * 8 + "+ ggplot.theme(**theme_minimal) \\\n" + " " * 8 + "+ ggplot.theme(**theme_my)\n"

    # add command that actually plots the plot
    code += " " * 4 + "plot_fixed_width(pp)"

    # print(code)

    exec(code, globals())

    # plot.tab_data = tab_data

    robj.r("Sys.setlocale('LC_NUMERIC', 'en_GB.UTF-8')")
    robj.r("options(tikzLatexPackages = c(getOption('tikzLatexPackages'), '\\\\usepackage{amsmath}'))")

    robj.r("options(tikzMetricPackages = c(getOption('tikzMetricPackages'), '\\\\usepackage{amsmath}'))")
    #
    # switch off warnings
    # robj.r('options(\'warn\'=-1)')

    # filename_output = "figure_7_totalErrorProbability_upper"

    width_in = point2inch(textwidth_pt)
    height_in = point2inch(textwidth_pt) / golden_ratio

    for format in GRAPHIC_FORMATS:
        if format == 'tikz':
            tikzDevice.tikz(file=filepath + ".tikz", standAlone=False, \
                    documentDeclaration="\documentclass[12pt]{scrreport}", \
                    width=width_in, height=height_in)#, packages="\\usepackage{amsmath}")
            #tikzDevice.tikzAnnotate("\let\pgfimageWithoutPath\pgfimage")
            #tikzDevice.tikzAnnotate("\renewcommand{\pgfimage}[2][]{\pgfimageWithoutPath[#1]{figures/#2}}")
        elif format == 'pdf':
            # for pdf output
            grdevices.pdf(file=filepath + ".pdf")


    # tikzDevice.tikzAnnotate(
    #     "\\renewcommand{\pgfimage}[2][]{\pgfimageWithoutPath[#1]{./figures/articles/hum_detector/#2}}")

    #    exec(f_plot)
    #     exec(f_plot, locals())
    #     pp =
    # plot.plot()
        plot(tab_data)
        # plot_fixed_width
        # pp.plot()
        # ggplot.ggplot.save(filename_output + '.pdf')

        grdevices.dev_off()

    adjust_relative_raster_path(filepath)
    run_tikzexternalize(filepath)

def adjust_relative_raster_path(filepath):
    plot_name = split(filepath)[1]
    subdir = split(filepath)[0].replace("../", "")

    # rename the original file
    rename(filepath + '.tikz', filepath + '_orig.tikz')

    # read the file line by line and replace every occurrence of [plot_name] with the relative-path-adjusted version
    with open(filepath + '_orig.tikz', 'r') as file_in:
        with open(filepath + '.tikz', 'w') as file_out:
            for line in file_in:

                line = line.replace(plot_name, join(subdir, plot_name))

                file_out.write(line)



def run_tikzexternalize(filepath):
    # remove relative path indicators
    jobname =  filepath.replace("../", "")

    global filename_tex

    # determine the location of the main tex file
    path_tex = abspath(split(filename_tex)[0])
    filename_tex_base = basename(filename_tex)

    # run pdflatex with the current plot's jobname
    command = "pdflatex"
    arguments = "--jobname={}".format(jobname, filename_tex_base)

    from subprocess import Popen
    p = Popen([command, arguments, filename_tex_base], cwd=path_tex)
    p.wait()

    # call([command, arguments])

if __name__ == "__main__":
    settings = find_next_settings_files()

    pprint(settings)

    print(store_file("test.csv"))
