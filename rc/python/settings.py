# stuff concerning ggplot2
from rpy2.robjects.packages import importr
import rpy2.robjects as robj
import rpy2.robjects.lib.ggplot2 as ggplot

# for tikxexternalize support
from os.path import join, dirname, abspath
filename_tex = abspath(join(dirname(__file__), *[".."]*6, "article.tex"))

scales = importr("scales")
grid = importr("grid")



# ggplot themes
theme_minimal = {'legend.background': ggplot.element_blank(),
                 'legend.key': ggplot.element_blank(),
                'panel.background': ggplot.element_blank(),
                "panel.border": ggplot.element_blank()}
theme_my = {'legend.background': ggplot.element_rect(color="gray", size = 0.1, fill=scales.alpha("white", 0.8)),
            'legend.justification': robj.r.c(1, 1),
            'legend.position': robj.r.c(0.9, 0.9),
            'panel.border': ggplot.element_rect(fill=robj.r("NA"), color="black", size=0.5, linetype="solid"),
            'panel.margin': grid.unit(robj.FloatVector((1, 2, 3, 4)), "cm"),
            'panel.background': ggplot.element_blank(),
            'panel.border': ggplot.element_blank()}

theme_base = theme_my
