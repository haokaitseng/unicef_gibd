library(tidyverse)
library(tidyr)
library(dplyr)
library(tidyverse)

rm(list = ls())

setwd("C:/Users/three/Neu Kasten_5440/027 Gavi/unicef_gibd/")
getwd()

#source("utils/data_cleaning.R")


load("input_data/cleaned_data.RData")


###############
# ANLAYSIS ####
###############
library(ggplot2)


# all covariates corelation ####
library(tidyverse)
library(ggplot2)
library(corrplot)
library(randomForest)
# install.packages("randomForest")
library(ggpubr)

num_cols <- df %>%
  select(budget_L1, budget_L1_programme, budget_L1_vaccine,budget_L1_immunisation, GNIpc, GDPpc, gghed_usd,
         coverage_MCV1, coverage_MCV2, coverage_DTPCV1, coverage_DTPCV3,growth_rate_budget_L1,growth_rate_GNI,
           growth_rate_GDP,growth_rate_GNIpc,growth_rate_GDPpc,growth_rate_gghed_usd)

corr_matrix <- cor(num_cols, use="pairwise.complete.obs", method="spearman")

corrplot(corr_matrix, method="color", type="upper", tl.cex=0.8, number.cex=0.7,
         title="Spearman Correlation Matrix", mar=c(0,0,1,0))



# 7. Mixed effect - random on countries ####
install.packages(c("lme4", "lmerTest", "sjPlot", "performance"))
library(lme4)        # for mixed models
library(lmerTest)    # for p-values in mixed models
library(sjPlot)      # for model visualization
library(performance) 

df$iso3 <- as.factor(df$iso3)

model_lagged_growth2 <- lmer(
  growth_rate_budget_L1 ~ #GNIpc + GDPpc + gghed_usd + gghed_GDP + gghed_GNI +
              lag_growth_rate_GNI + #lag_growth_rate_GDP +
              #lag_growth_rate_GNIpc + #lag_growth_rate_GDPpc +
              lag_growth_rate_gghed_usd + 
              lag_growth_rate_gghed_GDP +
              lag_growth_rate_gghed_GNI +
    (1 | iso3),
  data = df
)
summary(model_lagged_growth2)
  # country random effect is now not useful

# 2-3 years avg gorwth rates ####
model_lagged_growth3 <- lm(
  growth_rate_budget_L1 ~ 
    avg2_growth_rate_GNI+
    avg2_growth_rate_gghed_GNI +
    avg2_growth_rate_GNI * avg2_growth_rate_gghed_GNI#+
    #(1 | iso3)
  ,data = df)

summary(model_lagged_growth3)
# avg2_growth_rate_gghed_GNI    p 0.1701
# avg3_growth_rate_GNI  p 0.104

# GDP 2-3 years avg gorwth rates ####
## simple linear ####
model_3 <- lmer(
  growth_rate_budget_L1 ~ 
    avg3_growth_rate_GDP+
    avg3_growth_rate_gghed_GDP +
    avg3_growth_rate_GDP * avg3_growth_rate_gghed_GDP +
    `Income group`+
    (1 | iso3)
  ,data = df)

summary(model_3)
isSingular(model_3, tol = 1e-4)

## simple linear: iso3 as fixed effect ####
model_4 <- lm(
  growth_rate_budget_L1 ~ 
    avg3_growth_rate_GDP+
    avg3_growth_rate_gghed_GDP +
    avg3_growth_rate_GDP * avg3_growth_rate_gghed_GDP #+
    #as.factor(iso3)
  ,data = df)

summary(model_4)
#######################
# INLA Bayesian methods ####
df <- df %>% rename(income_group = `Income group`)
df <- df %>%   mutate(year = as.numeric(as.character(year)))
df <- df[order(df$iso3, df$year), ]  
df$year_index <- df$year  

library(INLA)

formula <- 
  growth_rate_budget_L1 ~
  avg3_growth_rate_GDP + 
  avg3_growth_rate_gghed_GDP +
  income_group +
  f(iso3, 
    model = "iid", 
    hyper = list(prec= list(prior = "pc.prec",
                            param = c(1, 0.01))))+
  f(year, model = "rw1", hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01))))


model_inla_1 <- inla(
  formula,
  data = df,
  family = "gaussian",
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)

summary(model_inla_1)





