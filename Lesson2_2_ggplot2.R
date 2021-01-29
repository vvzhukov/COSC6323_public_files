# 01/29/2021
# Vitalii Zhukov
# COSC 6323

# Web: https://ggplot2.tidyverse.org
# Book: https://amzn.to/2TU78ip
# State of art :) Cheat Sheets: 
# https://github.com/rstudio/cheatsheets/blob/master/data-visualization-2.1.pdf

# GGPLOT2

library(ggplot2)

# ggplot2 is based on the grammar of graphics
# same components for every graph: dataset, coordinate system, 'geoms' - marks to represent data points

# ggplot() Begins a plot that you finish by adding layers to.
# ggsave("name.png", width = x, height = y) Saves last plot as x' x y' file "name.png" in current wd.


ggplot(diamonds)  # only the dataset 
ggplot(diamonds, aes(x=carat))  # X-axis
ggplot(diamonds, aes(x=carat, y=price))
ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
ggplot(diamonds, aes(x=carat, y=price)) + geom_point() + geom_smooth() 

# Adding scatterplot geom (layer1) and smoothing geom (layer2).
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + geom_smooth() 
ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_smooth() 

# Replace aesthetics to the geom_smooth layer
ggplot(diamonds) + geom_smooth(aes(x=carat, y=price, color=cut)) 


# Here are some useful geoms:
# GRAPHICAL PRIMITIVES
?economics
?seals
a <- ggplot(economics, aes(date, unemploy)) 
b <- ggplot(seals, aes(x = long, y = lat))

?geom_blank()
a + geom_blank()

?geom_curve()
b + geom_curve(aes(yend = lat + 1, xend=long+1),curvature=1) 

?geom_path()
a + geom_path(lineend="butt", linejoin="round", linemitre=1)
# ... (check cheat sheet)

# ONE VARIABLE continuous
?mpg
c <- ggplot(mpg, aes(hwy))
c2 <- ggplot(mpg)

?geom_density()
# Ex. Smoothed density estimates for highway miles per gallon
c + geom_density(kernel = "gaussian")
?density()

?geom_histogram()
# Ex. Histogram for highway miles per gallon
c + geom_histogram(binwidth = 5)



# ONE VARIABLE discrete
d <- ggplot(mpg, aes(fl))

?geom_bar()
# Ex. Bar chart for highway miles per gallon
d + geom_bar()


# TWO VARIABLES continuous x, continuous y
e <- ggplot(mpg, aes(cty, hwy))

?geom_label()
# Ex. Labeling plot city miles per gallon VS highway miles per gallon
e + geom_label(aes(label = cty))

?geom_point()
# Ex. Point plot city miles per gallon VS highway miles per gallon
e + geom_point()

?geom_smooth()
# Ex. Smoothed conditional means (patterns) city miles per gallon VS highway miles per gallon
e + geom_smooth(method = lm)

?geom_text()
# Ex. Text city miles per gallon VS highway miles per gallon
e + geom_text(aes(label = cty))


# TWO VARIABLES discrete x , continuous y
f <- ggplot(mpg, aes(class, hwy))

?geom_col()
# Ex. Barchart "type" of car vs highway miles per gallon 
# (the heights of the bars to represent values in the data)
f + geom_col()

?geom_boxplot()
# Ex. Boxplot "type" of car vs highway miles per gallon 
f + geom_boxplot()

?geom_violin()
# Ex. violin plot "type" of car vs highway miles per gallon 
f + geom_violin(scale = "area")

# TWO VARIABLES discrete x, discrete y
g <- ggplot(diamonds, aes(cut, color))

?geom_count()
# Ex. Count overlapping points sample sizes of diamonds certain color and cut 
g + geom_count()

# CONTINUOUS BIVARIATE DISTRIBUTION
h <- ggplot(diamonds, aes(carat, price))

?geom_bin2d()
h + geom_bin2d(binwidth = c(0.25, 500))

?geom_density2d()
h + geom_density2d()

# CONTINUOUS FUNCTION
i <- ggplot(economics, aes(date, unemploy))

?geom_area()
i + geom_area()
# ...

# THREE VARIABLES
seals$z <- with(seals, sqrt(delta_long^2 + delta_lat^2)) 
l <- ggplot(seals, aes(long, lat))

?geom_contour()
# Ex. geom contour of coordinates / surface (seals trajectories)
l + geom_contour(aes(z = z))

?geom_raster()
# Ex.
l + geom_raster(aes(fill = z), hjust=0.5, vjust=0.5, interpolate=FALSE)

?geom_tile()
# Ex.
l + geom_tile(aes(fill = z))

# ERROR
df <- data.frame(grp = c("A", "B"), fit = 4:5, se = 1:2)
j <- ggplot(df, aes(grp, fit, ymin = fit-se, ymax = fit+se))

j + geom_crossbar(fatten = 2)
j + geom_errorbar()
j + geom_pointrange()

# ... and much much more: Scales, Coordinate systems, Themes, Labels, Position adjustment
# check 2nd cheat sheet.