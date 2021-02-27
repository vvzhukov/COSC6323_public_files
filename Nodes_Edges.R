setwd('C:/Users/Rubinzone/Desktop/STAT/Project/Net_reproduction/data')

library(dplyr)
library(ggplot2)

df_author = read.csv('brain_author.csv', stringsAsFactors = FALSE)
df_cip_cat = read.csv('unique_CIP_list_3category.csv', stringsAsFactors = FALSE)
full_data <- read.csv('ArticleLevel.csv', stringsAsFactors = FALSE)
full_author <- read.delim('AuthorArticle.txt', header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

df_author = df_author %>% filter(min_pub_year > 1960) %>% filter(!is.na(region)) %>% filter(!is.na(cip_title))

nodes <- read.csv('Nodes.csv', stringsAsFactors = FALSE)
edges_2009 <- read.csv('Edges_2009.csv', stringsAsFactors = FALSE)
edges_2014 <- read.csv('Edges_2014.csv', stringsAsFactors = FALSE)


# Steps:
# --------------------------------------------------------------
# 1. Load Node and edge files of 2014
# 2. Set color based on cip_type
# 3. Run all the Statistics algorithms
# 4. Set node size based on degree
# 5. Apply all layouts
# 8. Save images for 2014
# 9. Use Filter Attributes Equal y_2009 = 1. Create another  workspace. Named FULL_2009
# 10. Delete edges from new workspaces. Then add new edges for that year.  
# 11. Set node sizes based on below logic.
# 12. Use Filter Attributes Equal y_2009 = 1 in  GIANT_2014. Create another  workspace. Named 2009
# 13. Delete edges from new workspaces. Then add new edges for that year.
# 14. Use Filter Giant component and create new workspace. Named it GIANT_2009
# 15. Set node sizes based on below logic.
# 16. Save Images.
# 17. Repeat steps 12 to 16 for year 2004 and 1999





# Coloring Logic:
# --------------------------------------------------------------------
# 1. Health             : Orange
# 2. Neuro/Biology      : GREEN
# 3. Sci/Eng            : MAGENTA





# Layouts Design:
# --------------------------------------------------------------------
# 1. Circle pack layout:
# Hierarchy1:	cip_type (Attribute)

# 2. Force atlas 2:
# Approximate Repulsion	false
# Stronger Gravity	true
# Gravity	0.5
# Dissuade Hubs	true
# LinLog mode	true
# Prevent Overlap	true

# 3. Rotate : for better looks
# 4. Expansion:
# Scale factor	1.02

