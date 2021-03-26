# 03/25/2021
# Vitalii Zhukov
# COSC 6323
# Ref.: 
# https://www.tutorialspoint.com/r/r_multiple_regression.htm
# https://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr_interact.html
# https://www.rdocumentation.org/packages/GGally/versions/1.5.0/topics/ggpairs
# https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
# https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html

# PLAN
# 1. Sankey diagram (basic diagram, color for nodes, color for connections)
# 2. Multiple regression (mtcars)
# 3. Generalized pairs plot (prestige) // ggPredict()
# 4. Excercise review
# 5. Extra


# 1. Sankey diagram

# Library
library(networkD3)
library(dplyr)

# Make a connection data frame
links <- data.frame(
    source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
    target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
    value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame
# it lists every entities involved in the flow
nodes <- data.frame(
    name=c(as.character(links$source), as.character(links$target)) %>% 
        unique()
)

# With networkD3, connection must be provided using id, not using 
# real name like in the links dataframe. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: One specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

# Make the Network. Color scale with the colorScale argument
sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color, fontSize= 14)

# Set color for groups of nodes

# Add a 'group' column to the nodes data frame:
nodes$group <- as.factor(c("a","a","a","a","a","b","b","b"))

# Give a color for each group:
grp_color <- 'd3.scaleOrdinal() .domain(["a", "b"]) .range(["#69b3a2", "steelblue"])'

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   colourScale=grp_color, NodeGroup="group")

# Set color of connections
# Add a 'group' column to each connection:
links$group <- as.factor(c("type_a","type_a","type_a","type_b","type_b","type_b"))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group <- as.factor(c("my_unique_group"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "my_unique_group"]) .range(["#69b3a2", "steelblue", "grey"])'

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, LinkGroup="group", NodeGroup="group")




# 2. Mtcars multiple regression

# Establish the relationship between "mpg" as a response variable with 
# "disp","hp" and "wt" as predictor variables

input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))

# Create Relationship Model & get the Coefficients

model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(model)


# Get the Intercept and coefficients as vector elements.
coef(model)

Interc <- coef(model)[1]
Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

# For a car with disp = 221, hp = 102 and wt = 2.91 the predicted mileage is
x1 <- 221
x2 <- 102
x3 <- 2.91
Y = Interc + Xdisp*x1 + Xhp*x2 + Xwt*x3

# Matrix scatterplot:
plot(input, pch=16, col="blue",
     main="Matrix Scatterplot of mpg, disp, hp, and wt")

# Diagnostic plots for the model
par(mfrow=c(2, 2))
plot(model)




# 3. Generalized pairs plot (prestige)

library(car)
head(Prestige)

# Plot first four numeric variables
plot(Prestige[,c(1:4)], pch=16, col="blue", main="Matrix Scatterplot
of Income, Education, Women and Prestige")

# GGPAIRS
#install.packages("GGally")
library(GGally)
ggpairs(Prestige, aes(col = type, alpha=0.4))


# GGPREDICT
require(ggiraph)
require(ggiraphExtra)
require(plyr)

input <- mtcars[,c("mpg","disp","hp","wt")]
model2 <- lm(mpg~hp+wt, data = input)
summary(model2)
ggPredict(model2,se=TRUE,interactive=TRUE)


require(moonBook)
?radial
model3=lm(NTAV~age*weight*HBP,data=radial)
summary(model3)
ggPredict(model3,interactive = TRUE)


# 4. Excersize review

# 5. EXTRA

# Useful functions:
coefficients(model) # model coefficients
confint(model, level=0.95) # CIs for model parameters 
anova(model) # anova table 
vcov(model) # covariance matrix for model parameters 
influence(model) # regression diagnostics

# Comparing Models
# Cross Validation
# Variable Selection
# Relative Importance
