# Models for the Brain project

library(dplyr)
library(ggplot2)
library(rcompanion)
library(rms)
library(questionr)

#### Read data
df_article <- read.csv('ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv')


# ARTICLE LEVEL
#### Filters
###### Filter Year [1970-2018]
###### Filter Kp >= 2 and Wp >= 2
df_article = df_article %>% filter(Yp >= 1970)
df_article = df_article %>% filter(Yp <= 2018)
df_article = df_article %>% filter(Kp >= 2)
df_article = df_article %>% filter(nMeSHMain >= 2)
df_article = df_article %>% filter(IRegionRefinedp > 0 & IRegionRefinedp < 7)

df_article$IRegionRefinedp = as.double(df_article$IRegionRefinedp)

df_article <- df_article %>%
    mutate(Regionp = if_else(IRegionRefinedp %in% c(1,2,3),IRegionRefinedp,0))

df_article <- df_article %>%
  mutate(XSACIPp = if_else(((XSAp == XCIPp) & (XCIPp == 1)),1,0))

df_article <- df_article %>%
    mutate(NOTXSACIPp = if_else((XSAp == XCIPp),0,1))

df_article <- df_article %>%
  mutate(NEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp) 
                                     & (NEUROLONGXCIPp == 1)),1,0))

df_article <- df_article %>%
    mutate(NOTNEUROLONGXSACIPp = if_else(((NEUROLONGXSAp == NEUROLONGXCIPp)),0,1))

df_article <- df_article %>%
  mutate(NEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp) 
                                        & (NEUROSHORTXCIPp == 1)),1,0))

df_article <- df_article %>%
  mutate(NOTNEUROSHORTXSACIPp = if_else(((NEUROSHORTXSAp == NEUROSHORTXCIPp)),0,1))

#### Convert Data types
df_article$eidsp = as.factor(df_article$eidsp)
df_article$Yp = as.integer(df_article$Yp)
df_article$Kp = as.integer(df_article$Kp)
df_article$MeanZJp = as.double(df_article$MeanZJp)
df_article$XSAp = as.factor(df_article$XSAp)
df_article$XCIPp = as.factor(df_article$XCIPp)
df_article$NRegp = as.integer(df_article$NRegp)
df_article$NSAp = as.integer(df_article$NSAp)
df_article$NCIPp = as.integer(df_article$NCIPp)
df_article$nMeSHMain = as.integer(df_article$nMeSHMain)
df_article$IRegionRefinedp = as.factor(df_article$IRegionRefinedp)
df_article$logKp = as.double(log(df_article$Kp))
df_article$logMajorMeSHp = as.double(log(df_article$nMeSHMain))
df_article$Regionp = as.factor(df_article$Regionp)
df_article$XSACIPp = as.factor(df_article$XSACIPp)
df_article$NOTXSACIPp = as.factor(df_article$NOTXSACIPp)
df_article$NOTNEUROLONGXSACIPp = as.factor(df_article$NOTNEUROLONGXSACIPp)
df_article$NEUROSHORTXSACIPp = as.factor(df_article$NEUROSHORTXSACIPp)
df_article$NEUROLONGXSACIPp = as.factor(df_article$NEUROLONGXSACIPp)

# FILTER DATA
df_article_bm3 = df_article %>% filter(NOTXSACIPp!=1) 
df_article_bm6 = df_article %>% filter(NOTXSACIPp!=1) 
df_article_dm3 = df_article %>% filter(NOTNEUROLONGXSACIPp!=1)
df_article_dm6 = df_article %>% filter(NOTNEUROLONGXSACIPp!=1) 
df_article_nm3 = df_article %>% filter(NOTNEUROSHORTXSACIPp!=1) 
df_article_nm6 = df_article %>% filter(NOTNEUROSHORTXSACIPp!=1) 

## BROAD Model 1 - for X_SA
model1 <- glm(XSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp, 
              data = df_article, family=binomial(link='logit'))


## BROAD Model 2 - for X_CIP
model2 <- glm(XCIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NSAp , 
              data = df_article, family=binomial(link='logit'))

## BROAD Model 3 - for X_SA&CIP

model3 <- glm(XSACIPp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp , 
              data = df_article_bm3, family=binomial(link='logit'))

## BROAD Model 4 - for X_SA, Shift after funding
model4 <- glm(XSAp ~ Yp + MeanZJp + logKp + logMajorMeSHp + NRegp + NCIPp + 
                  IYBRAINPROJECTp + Regionp + (Regionp * factor(IYBRAINPROJECTp))
              , data = df_article, family=binomial(link='logit'))


apply(odds.ratio(model1), 2, formatC, format="f", digits=4)
nagelkerke(model3)
summary(model3)
exp(coef(model3))

