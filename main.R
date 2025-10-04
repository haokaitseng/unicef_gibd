rm(list = ls())
library(INLA)
library(tidyverse)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(dplyr)
library(purrr)
library(ggpubr)
library(stringr)


setwd("C:/Users/three/Neu Kasten_5440/027 Gavi/unicef_gibd/")
getwd()

#source("utils/data_cleaning.R")
source("utils/visualisation.R")


load("input_data/cleaned_data.RData") # output is called df_all
#################################
# 1. prepare anlaysis dataset####
#################################
df <- df_all %>% 
  filter(income_group %in% c("Low income","Lower middle income"))%>%
  filter(year %in% c(2021:2024))%>% #if year 2020 included, n obs = 86, will still make dtp1 significant though
  filter(!is.na(budget_L1),  
         !is.na(growth_rate_budget_L1))
    # why Vanuatu L1 $0 in 2021?

#important 
table(df$year,df$country_name)
df <- df %>% # n obs drop from 84 to 70
  group_by(iso3) %>%
  filter(n() >= 3) %>%
  ungroup()

# year indexing
df <- df %>% mutate(year_adj = as.integer(as.character(year)))
df <- df[order(df$iso3, df$year_adj), ]
year_levels <- sort(unique(df$year_adj))
df$year_index <- match(df$year_adj, year_levels)

############################
# 2. Summary statistics #### 
############################
table(df$iso3,df$year)# all observations are consecutive

# number of countries studied
df%>%
  pull(country_name)%>%
  unique() 

# median L1 budget and IQR
df %>%
  #group_by(country_name)%>% # no need for groupping since no iso3 random effect
  summarise(
    budget_L1_median = median(budget_L1),
    budget_L1_p25 = quantile(budget_L1, 0.25),
    budget_L1_p75 = quantile(budget_L1, 0.75))

# median growth rates for L1 budget and IQR
df %>%
  summarise(
    growth_rate_budget_L1_median = median(growth_rate_budget_L1),
    growth_rate_budget_L1_p25 = quantile(growth_rate_budget_L1, 0.25),
    growth_rate_budget_L1_p75 = quantile(growth_rate_budget_L1, 0.75))

########################
# 3. INLA modelling ####
######################## 
response_vars <- c("growth_rate_coverage_DTPCV1", "growth_rate_coverage_DTPCV3", 
"growth_rate_coverage_MCV1","growth_rate_coverage_MCV2","growth_rate_coverage_POL3",
"growth_rate_coverage_IPV1","growth_rate_coverage_IPV2","growth_rate_coverage_BCG",
"growth_rate_coverage_PCV3","growth_rate_coverage_pooled")

model_results <- list()

for (i in response_vars) {
  formula_str <- as.formula(paste0(
    i, " ~ 1+ growth_rate_budget_L1 + ",
    "f(year_index, model = 'rw1', ",# rw1 has been approved better than ar1, according to WAIC and DIC. no diff on the number of significant antigens 
    "hyper = list(prec = list(prior = 'pc.prec', param = c(1, 0.05))))"
  ))
  
  model <- inla(
    formula_str,
    data = df,
    family = "gaussian",
    control.compute = list(dic = TRUE, waic = TRUE)
  )
  
  fixed_summary <- model$summary.fixed
  
  model_info <- list(
    fixed = fixed_summary,
    waic = model$waic$waic,
    dic = model$dic$dic,
    loglik = model$mlik[1],  # marginal log-likelihood
    random_year_index = model$summary.random$year_index)
  model_results[[i]] <- model_info
}

print(model_results)

# 3-2. fixed effect visualisation ####
table_fixed_effect <- map_df(names(model_results), function(mn) {
  fx <- as.data.frame(model_results[[mn]]$fixed)
  fx$term  <- rownames(model_results[[mn]]$fixed)
  fx$model <- mn
  fx
}) %>%
  filter(term == "growth_rate_budget_L1") %>%
  rename(
    q025 = `0.025quant`,
    q975 = `0.975quant`,
    q50  = `0.5quant`
  ) %>%
  mutate(model_short = str_remove(model, "^growth_rate_coverage_")) %>%
  filter(model_short != "pooled") %>%
  arrange(model_short)

plot_fixed_effect_combined <- table_fixed_effect %>%
  split(.$model_short) %>%
  map(make_fixed_effect_panel)

plot_fixed_effect_combined <-ggarrange(plotlist = plot_fixed_effect_combined,
          ncol = 3, nrow = 3, align = "hv")
print(plot_fixed_effect_combined)

ggsave(  filename = "output/graph/grid_fixed_effect_95CrL.png",  plot = plot_fixed_effect_combined,
  width = 12, height = 10, units = "in", dpi = 300 )

#########################
# 4. predicting 2025 ####
#########################
df_2025 <- df_all%>%
  filter(
    iso3 %in% unique(df$iso3),# need to predict for countries fitted
    year ==2025,
    !is.na(growth_rate_budget_L1))%>%
  mutate(year_index = 5 )#year 2021:2024 have used 1:4

list_predicted_iso3 <- df_2025$iso3

df_2025[,c("growth_rate_budget_L1","growth_rate_coverage_DTPCV1")]

df_2024 <- df %>% # why cannot derive from df?
  filter(year == 2024,
     iso3 %in% list_predicted_iso3)%>%
  mutate(iso3 = factor(iso3, levels = list_predicted_iso3)) %>%
  arrange(iso3)


summary(df_2025$growth_rate_budget_L1)
summary(df_2024$coverage_DTPCV1)

# fitting
df_predict <- rbind(df, df_2025)#training + NA

prediction <- inla(
  formula = growth_rate_coverage_DTPCV1 ~ 1 + 
            growth_rate_budget_L1 + 
            f(year_index, model = "rw1",
              hyper = list(prec = list(prior = 'pc.prec', param = c(1, 0.05)))),
  data = df_predict,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE, config = TRUE))

summary(prediction)

df_2025_indices <- which(df_predict$year == 2025)
df_predict[df_2025_indices,"iso3"]
prediction$summary.fitted.values[df_2025_indices, c("mean", "sd", "0.025quant", "0.975quant")]

# average of posterior mean for dtp1 growth rate 2025
mean(prediction$summary.fitted.values[df_2025_indices,"mean"])
mean(prediction$summary.fitted.values[df_2025_indices,"0.025quant"])
mean(prediction$summary.fitted.values[df_2025_indices,"0.975quant"])



# 2025 coverage = 2024 coverage * 2025 
coverage_DTPCV1_2025_predicted_mean  <- df_2024$coverage_DTPCV1 * 
  (1 + prediction$summary.fitted.values[df_2025_indices,"mean"])
coverage_DTPCV1_2025_predicted_025quant  <- df_2024$coverage_DTPCV1 * 
  (1 + prediction$summary.fitted.values[df_2025_indices,"0.025quant"])
coverage_DTPCV1_2025_predicted_975quant  <- df_2024$coverage_DTPCV1 * 
  (1 + prediction$summary.fitted.values[df_2025_indices,"0.975quant"])
coverage_DTPCV1_2025_predicted_975quant <- pmin(1, coverage_DTPCV1_2025_predicted_975quant)# clip the upper bound to 100% maximum

table_predict = data.frame(
  df_2024$iso3,
  df_2024$country_name,
  coverage_DTPCV1_2024 = df_2024$coverage_DTPCV1,
  budget_2024 = df_2024$budget_L1,
  growth_rate_budget_2024 = df_2024$growth_rate_budget_L1,
  budget_2025 = df_2025$budget_L1,
  growth_rate_budget_2025 = df_2025$growth_rate_budget_L1,
  growth_rate_coverage_DTPCV1_2025 = prediction$summary.fitted.values[df_2025_indices,"mean"],
  coverage_DTPCV1_2025_predicted_mean,
  coverage_DTPCV1_2025_predicted_025quant,
  coverage_DTPCV1_2025_predicted_975quant,
  coverage_diff_20242025 = coverage_DTPCV1_2025_predicted_mean - df_2024$coverage_DTPCV1
)
print(table_predict)


mean(table_predict$coverage_DTPCV1_2024)
mean(table_predict$coverage_DTPCV1_2025_predicted_mean)
mean(table_predict$coverage_DTPCV1_2025_predicted_025quant)
mean(table_predict$coverage_DTPCV1_2025_predicted_975quant)



# posterior_samples <- inla.posterior.sample(10, prediction)
