import sys

from os.path import exists, join, dirname
from os import mkdir

sys.path.append(join(dirname(__file__), ".."))
from project import save_figure

import pandas as pd

# for plotting in r
import rpy2.robjects.pandas2ri
import rpy2.robjects as robj
import rpy2.robjects.lib.ggplot2 as ggplot

# the output directory
dir_output = 'figures'
if not exists(dir_output):
    mkdir(dir_output)

# generate some datatab_
N = 100
tab_data = pd.DataFrame({'x': range(N),
                         'y': pd.np.random.randn(100)})

# generate a ggplot2-figure
def plot(tab_data):
    gp = ggplot.ggplot(tab_data)
    pp = gp + ggplot.aes_string(x="x", y="y") \
        + ggplot.theme(axis_title_x=ggplot.element_blank(),
                       axis_text_x=ggplot.element_blank()) \
        + ggplot.geom_line()

save_figure(plot, tab_data, join(dir_output, "test1"))

# generate some more data
N = 100
tab_data = pd.DataFrame({'x': range(N),
                         'y': 10000 * pd.np.random.randn(100)})

# generate a second ggplot2-figure
def plot(tab_data):
    gp = ggplot.ggplot(tab_data)
    pp = gp + ggplot.aes_string(x="x", y="y") \
        + ggplot.theme(axis_title_y=ggplot.element_blank()) \
        + ggplot.geom_line()

save_figure(plot, tab_data, join(dir_output, "test2"))

