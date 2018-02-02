# stuff concerning ggplot2
from rpy2.robjects.packages import importr
import rpy2.robjects as robj
import rpy2.robjects.lib.ggplot2 as ggplot

# for tikxexternalize support
from os.path import join, dirname, abspath
scales = importr("scales")
grid = importr("grid")



# ggplot themes
# theme_minimal = {'legend.background': ggplot.element_blank(),
#                  'legend.key': ggplot.element_blank(),
#                 'panel.background': ggplot.element_blank(),
#                 "panel.border": ggplot.element_blank()}
# theme_my = {'legend.background': ggplot.element_rect(color="gray", size = 0.1, fill=scales.alpha("white", 0.8)),
#             'legend.justification': robj.r.c(1, 1),
#             'legend.position': robj.r.c(0.9, 0.9),
#             'panel.border': ggplot.element_rect(fill=robj.r("NA"), color="black", size=0.5, linetype="solid"),
#             'panel.margin': grid.unit(robj.FloatVector((1, 2, 3, 4)), "cm"),
#             'panel.background': ggplot.element_blank(),
#             'panel.border': ggplot.element_blank()}

theme_base = {'legend.background': ggplot.element_rect(color="black", size = 0.1, fill=scales.alpha("white", 0.8)),
              'legend.key': ggplot.element_blank(),
              'legend.justification': robj.r.c(1, 1),
              'legend.position': robj.r.c(0.9, 0.9),
              'panel.border': ggplot.element_rect(fill=robj.r("NA"), color="black", size=0.5, linetype="solid"),
              'panel.background': ggplot.element_blank(),
              'panel.grid.major': ggplot.element_line(colour="grey65"),
              'panel.grid.minor': ggplot.element_line(colour="grey65"),
              'plot.margin': grid.unit(robj.r.c(0, 0, 0, 0), "cm")}
            #'panel.border': ggplot.element_blank()}
# 'panel.spacing': grid.unit(robj.FloatVector((1, 2, 3, 4)), "cm"),

# theme_base = {''}

# theme_base = theme_minimal
