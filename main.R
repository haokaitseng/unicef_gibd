library(tidyverse)
library(tidyr)
library(dplyr)

setwd("C:/Users/three/Neu Kasten_5440/027 Gavi/1 UNICEF budget database/")
getwd()

# READ
gibd_raw <- read.csv("20250705_UNICEF_GIBD_data.csv")|>arrange(iso3)
gni_raw <- read.csv("input_data/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_38331.csv", skip =4)
# gdp_raw <- read.csv("input_data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_38344.csv", skip =4) # all
gdp_raw <- read.csv("input_data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_38293.csv", skip =4) #per capita
income_group <- readxl::read_excel("input_data/CLASS_2025_07_02.xlsx")|>select(-Economy, -'Lending category')
vac_measles_raw <- readxl::read_excel("input_data/Measles vaccination coverage 2025-03-07 08-50 UTC.xlsx")
case_measles_raw <- readxl::read_excel("input_data/Measles reported cases and incidence 2025-03-07 09-07 UTC.xlsx")
vac_dtp_raw <- readxl::read_excel("input_data/Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage 2025-03-07 09-10 UTC.xlsx")
ghed_raw <- readxl::read_excel("input_data/GHED_data.xlsx")


# CLEAN DATA ####
# only us dollar and L1 level
gibd <- gibd_raw %>%
  mutate(
    budget_us = str_remove_all(budget_us, "[$,]") %>% as.numeric(),
    budget_local = str_remove_all(budget_local, "[$,]") %>% as.numeric()) %>%
  pivot_wider(
    names_from = func_L1,
    values_from = budget_us,
    names_prefix = "L1_" )%>%
  group_by(iso3, country_name, year) %>% 
  summarise(budget_L1_immunisation = sum(`L1_Immunization (Not Disaggregated)`, na.rm=T),
            budget_L1_programme = sum(`L1_Programme Management and Implementation`, na.rm=T),
            budget_L1_vaccine = sum(`L1_Vaccines and Injection Supplies`, na.rm=T), .groups="drop") %>% 
  mutate( budget_L1 = budget_L1_immunisation + budget_L1_programme + budget_L1_vaccine )
  
gni_long <- gni_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GNIpc" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2018)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

gdp_long <- gdp_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GDPpc" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2018)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

vac_measles_flat <- vac_measles_raw %>%
  filter(COVERAGE_CATEGORY == "WUENIC") %>%   
  mutate(YEAR = as.integer(YEAR)) %>%
  select(CODE, YEAR, ANTIGEN, COVERAGE) %>%   
  pivot_wider(
    names_from = ANTIGEN,
    values_from = COVERAGE,
    names_prefix = "coverage_"
  )

vac_dtp_flat <- vac_dtp_raw %>%
  filter(COVERAGE_CATEGORY == "WUENIC") %>%   
  mutate(YEAR = as.integer(YEAR)) %>%
  select(CODE, YEAR, ANTIGEN, COVERAGE) %>%   
  pivot_wider(
    names_from = ANTIGEN,
    values_from = COVERAGE,
    names_prefix = "coverage_"
  )


ghed_raw <- ghed_raw %>% 
  select(code, year, gghed_usd)%>%
  mutate(year = as.integer(year))





# MERGE MASTER DATA ####
# country-year as one raw
#only filter low and lower middle income groups
df_all <- gibd %>%
    left_join(income_group, by = c("iso3" = "Code"))|>
    left_join(gni_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(gdp_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(vac_measles_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_dtp_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(ghed_raw, by=c("iso3"="code",
                              "year"="year"))|> 
    filter(`Income group` %in% c("Low income","Lower middle income"))

table(df_all$country_name,df_all$year)

# drop missing countries, obs >=3
(country_obs <- df_all %>%
  group_by(country_name) %>%
  summarise(n_years = n()))

selected_countries <- country_obs %>%
  filter(n_years >= 0) %>%
  pull(country_name)

df <- df_all %>%
  filter(country_name %in% selected_countries)



###############
# ANLAYSIS ####
###############
library(ggplot2)


# 1. trend of MCV coverage and budget ####
ggplot(df, aes(x = year)) +
  geom_line(aes(y = budget_L1 / 1e6, color = "Budget (Million USD)"), size=1) +
  geom_line(aes(y = coverage_DTPCV1, color = "DTP1 Coverage (%)"), size=1) +
  geom_line(aes(y = coverage_MCV2, color = "MCV2 Coverage (%)"), size=1) +
  geom_line(aes(y = coverage_MCV1, color = "MCV1 Coverage (%)"), size=1) +
  scale_y_continuous(
    name = "Budget (Million USD)",
    sec.axis = sec_axis(~ ., name = "MCV1/MCV2 Coverage (%)")
  ) +
  scale_color_manual(values = c("Budget (Million USD)" = "steelblue", 
                                "MCV2 Coverage (%)" = "darkgreen",
                                "MCV1 Coverage (%)" = "red",
                                "DTP1 Coverage (%)" = "purple")) +
  labs(
    title = "Budget and MCV Coverage Trends by Country",
    x = "Year",
    color = "Variable"
  ) +
  facet_wrap(~ country_name, scales = "free_y") +
  theme_minimal()


# by country linear regression ####
library(tidyverse)

# Wrap model summary extraction in possibly to avoid errors

safe_lm_summary <- possibly(function(df) {
  model <- lm(coverage_DTPCV1 ~ budget_L1, data = df)
  coef <- summary(model)$coefficients
  tibble(beta = coef[2, 1], p_value = coef[2, 4])
}, otherwise = tibble(beta = NA_real_, p_value = NA_real_))

# Run per-country regression safely
lm_results <- df %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model_stats = map(data, safe_lm_summary)) %>%
  unnest(model_stats)

print(lm_results)

# 2. gdp/gni vs budget ####
safe_lm_summary_gdp <- possibly(function(df) {
  model <- lm(budget_L1 ~ GDPpc, data = df)
  coef <- summary(model)$coefficients
  tibble(beta_gdp = coef[2,1], p_value_gdp = coef[2,4])
}, otherwise = tibble(beta_gdp = NA_real_, p_value_gdp = NA_real_))

# Define safe linear regression summary function for GNIpc
safe_lm_summary_gni <- possibly(function(df) {
  model <- lm(budget_L1 ~ GNIpc, data = df)
  coef <- summary(model)$coefficients
  tibble(beta_gni = coef[2,1], p_value_gni = coef[2,4])
}, otherwise = tibble(beta_gni = NA_real_, p_value_gni = NA_real_))

# Apply by country for GDPpc
lm_results_gdp <- df %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model_stats_gdp = map(data, safe_lm_summary_gdp)) %>%
  unnest(model_stats_gdp)

# Apply by country for GNIpc
lm_results_gni <- df %>%
  group_by(country_name) %>%
  nest() %>%
  mutate(model_stats_gni = map(data, safe_lm_summary_gni)) %>%
  unnest(model_stats_gni)

# Combine both results for comparison
lm_results_combined <- lm_results_gdp %>%
  select(country_name, beta_gdp, p_value_gdp) %>%
  left_join(
    lm_results_gni %>% select(country_name, beta_gni, p_value_gni),
    by = "country_name"
  )

# View results
print(lm_results_combined, n = Inf)

df_long <- df %>%
  select(country_name, year, budget_L1, GDPpc, GNIpc) %>%
  pivot_longer(cols = c(budget_L1, GDPpc, GNIpc),
               names_to = "variable",
               values_to = "value")

ggplot(df_long, aes(x = year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 0.8) +
  facet_wrap(~ country_name, scales = "free_y") +
  labs(
    title = "Budget L1, GDP per capita, and GNI per capita over Years by Country",
    x = "Year",
    y = "Value (USD)",
    color = "Variable"
  ) +
  scale_color_manual(values = c(
    "budget_L1" = "steelblue",
    "GDPpc" = "forestgreen",
    "GNIpc" = "orange"
  )) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8)
  )

# 3. GHED and budget ###

df  %>%
    mutate( perc_budget_L1_gghed_usd = budget_L1 / (gghed_usd*1000000 )) %>% # in million
    group_by(`Income group`)%>%
    summarise( mean(perc_budget_L1_gghed_usd,na.rm=T) )

median(table_temp$perc_budget_L1_gghed_usd, na.rm=T)
# low income countries have lower share

# 4. all covariates corelation ###
library(tidyverse)
library(ggplot2)
library(corrplot)
library(randomForest)
# install.packages("randomForest")
library(ggpubr)

num_cols <- df %>%
  select(budget_L1, budget_L1_programme, budget_L1_vaccine,budget_L1_immunisation, GNIpc, GDPpc, gghed_usd,
         coverage_MCV1, coverage_MCV2, coverage_DTPCV1, coverage_DTPCV3)

corr_matrix <- cor(num_cols, use="pairwise.complete.obs", method="spearman")

corrplot(corr_matrix, method="color", type="upper", tl.cex=0.8, number.cex=0.7,
         title="Spearman Correlation Matrix", mar=c(0,0,1,0))



# 5. Group comparison ####

kruskal.test(budget_L1_immunisation ~ `Income group`, data=df)


# 6. random forest for variable importance ####
df$Income_group_factor <- as.factor(df$`Income group`)
model_df <- df %>%
  select(budget_L1, Income_group_factor, GNIpc, GDPpc, gghed_usd,
         coverage_MCV1, coverage_MCV2, coverage_DTPCV1, coverage_DTPCV3) %>%
  drop_na()

set.seed(42)
rf <- randomForest(budget_L1 ~ ., data=model_df, importance=TRUE, ntree=500)

# View model summary
print(rf)
