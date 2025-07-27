
# READ DATA
gibd_raw <- read.csv("input_data/20250705_UNICEF_GIBD_data.csv")|>arrange(iso3)
gni_raw <- read.csv("input_data/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_38331.csv", skip =4)
gni_total_raw <- read.csv("input_data/API_NY.GNP.ATLS.CD_DS2_en_csv_v2_29111.csv", skip =4)
gdp_total_raw <- read.csv("input_data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_38344.csv", skip =4) # all
gdp_raw <- read.csv("input_data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_38293.csv", skip =4) #per capita
income_group <- readxl::read_excel("input_data/CLASS_2025_07_02.xlsx")|>select(-Economy, -'Lending category')
# case_measles_raw <- readxl::read_excel("input_data/Measles reported cases and incidence 2025-03-07 09-07 UTC.xlsx")
vac_measles1_raw <- readxl::read_excel("input_data/Measles vaccination coverage 2025-15-07 09-29 UTC mcv1.xlsx")
vac_measles2_raw <- readxl::read_excel("input_data/Measles vaccination coverage 2025-15-07 09-29 UTC mcv2.xlsx")
vac_pcv_raw <- readxl::read_excel("input_data/Pneumococcal vaccination coverage 2025-24-07 08-06 UTC.xlsx")
vac_dtp_raw <- readxl::read_excel("input_data/Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage 2025-15-07 09-31 UTC dtp1.xlsx")
vac_dtp3_raw <- readxl::read_excel("input_data/Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage 2025-15-07 09-31 UTC dtp3.xlsx")
vac_bcg_raw <- readxl::read_excel("input_data/Bacillus Calmette–Guérin (BCG) vaccination coverage 2025-15-07 09-54 UTC.xlsx")
vac_ipv1_raw <- readxl::read_excel("input_data/Poliomyelitis vaccination coverage 2025-15-07 09-31 UTC ipv1.xlsx")
vac_ipv2_raw <- readxl::read_excel("input_data/Poliomyelitis vaccination coverage 2025-24-07 07-52 UTC ipv2.xlsx")
vac_polio3_raw <- readxl::read_excel("input_data/Poliomyelitis vaccination coverage 2025-15-07 09-31 UTC polio3.xlsx")
ghed_raw <- readxl::read_excel("input_data/GHED_data.xlsx")
imf_gdp_raw <- readxl::read_excel("input_data/imf-dm-export-20250714.xls", skip = 0)

# CLEAN DATA ####
# only us dollar and L1 level
# expand so that every country has rows for all years from 2018 to 2025, and fill in NA for missing year_range <- 2018:2025

year_range <- 2015:2026

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
  mutate( budget_L1 = budget_L1_immunisation + budget_L1_programme + budget_L1_vaccine ) %>%
  complete(
    iso3, year = year_range, 
    fill = list(
      budget_L1_immunisation = NA_real_,
      budget_L1_programme = NA_real_,
      budget_L1_vaccine = NA_real_)) %>%
  group_by(iso3) %>%
  fill(country_name, .direction = "downup") %>%  # fill country name
  ungroup()

gni_long <- gni_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GNIpc" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2014)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

gni_total_long <- gni_total_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GNI" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2014)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

gdp_long <- gdp_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GDPpc" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2014)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

gdp_total_long <- gdp_total_raw %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "GDP" ) %>%
  mutate( Year = as.integer(sub("X", "", Year))) %>%
  filter(Year > 2014)%>%
  select(-Country.Name, -Indicator.Name, -Indicator.Code)

library(countrycode)
imf_gdp_raw <- imf_gdp_raw [-1,]
imf_gdp <- imf_gdp_raw %>%
  pivot_longer(
    cols = 2:52,
    names_to = "year",
    values_to = "GDP_imf" )  %>%
  filter(year > 2014)%>%
  mutate( GDP_imf = 1000000000*as.numeric(if_else(GDP_imf =="no data", NA, GDP_imf)),
          year = as.integer(year)  )%>%
  rename(country_name=`GDP, current prices (Billions of U.S. dollars)`)%>%
  mutate(iso3 = countrycode(country_name,
                            origin = "country.name",
                            destination = "iso3c"))%>%
  filter(!is.na(iso3))

gdp_combined <- full_join(
  gdp_total_long,
  imf_gdp,
  by = c("Country.Code" = "iso3", "Year" = "year")
) %>%
  mutate(
    GDP_combined = coalesce(GDP, GDP_imf)
  ) %>%
  select(
    iso3 = Country.Code,
    year = Year,
    GDP_combined
  ) %>%
  arrange(iso3, year)%>%rename(GDP=GDP_combined)

func_clean_vaccine <- function(df) {
  df %>%
    filter(COVERAGE_CATEGORY == "WUENIC") %>%
    mutate(YEAR = as.integer(YEAR)) %>%
    select(CODE, YEAR, ANTIGEN, COVERAGE) %>%
    pivot_wider(
      names_from = ANTIGEN,
      values_from = COVERAGE,
      names_prefix = "coverage_")%>%
    mutate(across(starts_with("coverage_"), ~ .x / 100))
}

vac_measles1_flat <- func_clean_vaccine(vac_measles1_raw)
vac_measles2_flat <- func_clean_vaccine(vac_measles2_raw)
vac_dtp_flat     <- func_clean_vaccine(vac_dtp_raw)
vac_dtp3_flat    <- func_clean_vaccine(vac_dtp3_raw)
vac_bcg_flat     <- func_clean_vaccine(vac_bcg_raw)
vac_ipv1_flat    <- func_clean_vaccine(vac_ipv1_raw)
vac_ipv2_flat    <- func_clean_vaccine(vac_ipv2_raw)
vac_polio3_flat  <- func_clean_vaccine(vac_polio3_raw)
vac_pcv_flat  <- func_clean_vaccine(vac_pcv_raw)

ghed_raw <- ghed_raw %>% 
  select(code, year, gghed_usd)%>%
  mutate(year = as.integer(year))

########################
# MERGE MASTER DATA ####
# country-year as one raw
#only filter low and lower middle income groups
df_all <- gibd %>%
    left_join(income_group, by = c("iso3" = "Code"))|>
    rename(income_group = `Income group`)|>
    left_join(gni_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(gdp_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(gni_total_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(gdp_combined, by=c("iso3" = "iso3", 
                             "year" = "year"))|>
    left_join(vac_measles1_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_measles2_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_pcv_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_dtp_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_dtp3_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_bcg_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_ipv1_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_ipv2_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_polio3_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(ghed_raw, by=c("iso3"="code",
                              "year"="year"))|> 
#    filter(`Income group` %in% c("Low income","Lower middle income"))|>
    mutate(gghed_GDP = gghed_usd *1000000 /GDP)|>
    mutate(gghed_GNI = gghed_usd *1000000 /GNI)

# compute growth rate
growth_rate <- function(x) {(x - lag(x)) / lag(x)}

growth_vars <- c(
  "budget_L1", "budget_L1_vaccine", "budget_L1_immunisation", "budget_L1_programme",
  "GNI", "GDP", "GNIpc", "GDPpc", "gghed_usd", "gghed_GDP", "gghed_GNI",
  "coverage_MCV1","coverage_MCV2", "coverage_DTPCV1", "coverage_DTPCV3",
  "coverage_BCG", "coverage_IPV1","coverage_IPV2", "coverage_POL3","coverage_PCV3"
)

df_all <- df_all %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(across(
    all_of(growth_vars),
    ~ growth_rate(.),
    .names = "growth_rate_{.col}"
  ))%>%
  ungroup()%>%
   mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA, .)))# L1 NaN as NA


 
# pooled average for vaccine coverages ####
df_all <- df_all %>%
  mutate(coverage_pooled = rowMeans(select(., starts_with("coverage_")), na.rm = TRUE)) %>%
  mutate(growth_rate_coverage_pooled = rowMeans(select(., starts_with("growth_rate_coverage_")), na.rm = TRUE)) 

df_all[,c("coverage_pooled","coverage_MCV1","coverage_MCV2","coverage_DTPCV1","coverage_DTPCV3","coverage_PCV3","coverage_BCG" ,"coverage_IPV1","coverage_IPV2","coverage_POL3")]
df_all[,c("growth_rate_coverage_pooled","growth_rate_coverage_MCV1","growth_rate_coverage_DTPCV1","growth_rate_coverage_BCG" ,"growth_rate_coverage_IPV1")]


#df_all[,c("iso3","year","gghed_usd","GNI","gghed_GNI","growth_rate_gghed_GNI","lag_growth_rate_gghed_GNI","avg2_growth_rate_gghed_GNI","avg3_growth_rate_gghed_GNI")]

# clean
rm(list = ls(pattern = "raw"))
rm(gdp_long,gdp_combined,gdp_total_long,gni_long,year_range,imf_gdp,gni_total_long)

save.image(file = "input_data/cleaned_data.RData") 

