
# READ
gibd_raw <- read.csv("20250705_UNICEF_GIBD_data.csv")|>arrange(iso3)
gni_raw <- read.csv("input_data/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_38331.csv", skip =4)
gni_total_raw <- read.csv("input_data/API_NY.GNP.ATLS.CD_DS2_en_csv_v2_29111.csv", skip =4)
gdp_total_raw <- read.csv("input_data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_38344.csv", skip =4) # all
gdp_raw <- read.csv("input_data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_38293.csv", skip =4) #per capita
income_group <- readxl::read_excel("input_data/CLASS_2025_07_02.xlsx")|>select(-Economy, -'Lending category')
vac_measles_raw <- readxl::read_excel("input_data/Measles vaccination coverage 2025-03-07 08-50 UTC.xlsx")
case_measles_raw <- readxl::read_excel("input_data/Measles reported cases and incidence 2025-03-07 09-07 UTC.xlsx")
vac_dtp_raw <- readxl::read_excel("input_data/Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage 2025-03-07 09-10 UTC.xlsx")
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
    left_join(gni_total_long, by=c("iso3" = "Country.Code", 
                             "year" = "Year"))|>
    left_join(gdp_combined, by=c("iso3" = "iso3", 
                             "year" = "year"))|>
    left_join(vac_measles_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(vac_dtp_flat, by=c("iso3"="CODE",
                              "year"="YEAR"))|>
    left_join(ghed_raw, by=c("iso3"="code",
                              "year"="year"))|> 
    filter(`Income group` %in% c("Low income","Lower middle income"))|>
    mutate(gghed_GDP = gghed_usd *1000000 /GDP)|>
    mutate(gghed_GNI = gghed_usd *1000000 /GNI)

# compute growth rate
df_all <- df_all %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    growth_rate_budget_L1  = (budget_L1  - lag(budget_L1))  / lag(budget_L1),
    growth_rate_GNI        = (GNI        - lag(GNI))        / lag(GNI),
    growth_rate_GDP        = (GDP        - lag(GDP))        / lag(GDP),
    growth_rate_GNIpc      = (GNIpc      - lag(GNIpc))      / lag(GNIpc),
    growth_rate_GDPpc      = (GDPpc      - lag(GDPpc))      / lag(GDPpc),
    growth_rate_gghed_usd  = (gghed_usd  - lag(gghed_usd))  / lag(gghed_usd),
    growth_rate_gghed_GDP  = (gghed_GDP  - lag(gghed_GDP))  / lag(gghed_GDP),
    growth_rate_gghed_GNI  = (gghed_GNI  - lag(gghed_GNI))  / lag(gghed_GNI)
  ) %>%
  ungroup()


df_all <- df_all %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    lag_growth_rate_GNI = lag(growth_rate_GNI),
    lag_growth_rate_GDP = lag(growth_rate_GDP),
    lag_growth_rate_GNIpc = lag(growth_rate_GNIpc),
    lag_growth_rate_GDPpc = lag(growth_rate_GDPpc),
    lag_growth_rate_gghed_usd = lag(growth_rate_gghed_usd),
    lag_growth_rate_gghed_GDP = lag(growth_rate_gghed_GDP),
    lag_growth_rate_gghed_GNI = lag(growth_rate_gghed_GNI),
  ) %>%
  ungroup()

# 2-3 years average gorwth rates ####
df_all <- df_all %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    # 2-year average growth rates
    avg2_growth_rate_GNI        = rowMeans(cbind(lag(growth_rate_GNI, 1), lag(growth_rate_GNI, 2)), na.rm = TRUE),
    avg2_growth_rate_GDP        = rowMeans(cbind(lag(growth_rate_GDP, 1), lag(growth_rate_GDP, 2)), na.rm = TRUE),
    avg2_growth_rate_GNIpc      = rowMeans(cbind(lag(growth_rate_GNIpc, 1), lag(growth_rate_GNIpc, 2)), na.rm = TRUE),
    avg2_growth_rate_GDPpc      = rowMeans(cbind(lag(growth_rate_GDPpc, 1), lag(growth_rate_GDPpc, 2)), na.rm = TRUE),
    avg2_growth_rate_gghed_usd  = rowMeans(cbind(lag(growth_rate_gghed_usd, 1), lag(growth_rate_gghed_usd, 2)), na.rm = TRUE),
    avg2_growth_rate_gghed_GDP  = rowMeans(cbind(lag(growth_rate_gghed_GDP, 1), lag(growth_rate_gghed_GDP, 2)), na.rm = TRUE),
    avg2_growth_rate_gghed_GNI  = rowMeans(cbind(lag(growth_rate_gghed_GNI, 1), lag(growth_rate_gghed_GNI, 2)), na.rm = TRUE),

    # 3-year average growth rates
    avg3_growth_rate_GNI        = rowMeans(cbind(lag(growth_rate_GNI, 1), lag(growth_rate_GNI, 2), lag(growth_rate_GNI, 3)), na.rm = TRUE),
    avg3_growth_rate_GDP        = rowMeans(cbind(lag(growth_rate_GDP, 1), lag(growth_rate_GDP, 2), lag(growth_rate_GDP, 3)), na.rm = TRUE),
    avg3_growth_rate_GNIpc      = rowMeans(cbind(lag(growth_rate_GNIpc, 1), lag(growth_rate_GNIpc, 2), lag(growth_rate_GNIpc, 3)), na.rm = TRUE),
    avg3_growth_rate_GDPpc      = rowMeans(cbind(lag(growth_rate_GDPpc, 1), lag(growth_rate_GDPpc, 2), lag(growth_rate_GDPpc, 3)), na.rm = TRUE),
    avg3_growth_rate_gghed_usd  = rowMeans(cbind(lag(growth_rate_gghed_usd, 1), lag(growth_rate_gghed_usd, 2), lag(growth_rate_gghed_usd, 3)), na.rm = TRUE),
    avg3_growth_rate_gghed_GDP  = rowMeans(cbind(lag(growth_rate_gghed_GDP, 1), lag(growth_rate_gghed_GDP, 2), lag(growth_rate_gghed_GDP, 3)), na.rm = TRUE),
    avg3_growth_rate_gghed_GNI  = rowMeans(cbind(lag(growth_rate_gghed_GNI, 1), lag(growth_rate_gghed_GNI, 2), lag(growth_rate_gghed_GNI, 3)), na.rm = TRUE)
  ) %>%
  ungroup()


glimpse(df_all)

table(df_all$country_name,df_all$year)

# drop missing countries, obs >=0
# (country_obs <- df_all %>%
#   group_by(country_name) %>%
#   summarise(n_years = n()))
# selected_countries <- country_obs %>%
#   filter(n_years >= 0) %>%
#   pull(country_name)
# df <- df_all %>%
#   filter(country_name %in% selected_countries)

df <- df_all

df[,c("iso3","year","gghed_usd","GNI","gghed_GNI","growth_rate_gghed_GNI","lag_growth_rate_gghed_GNI","avg2_growth_rate_gghed_GNI","avg3_growth_rate_gghed_GNI")]


# clean
rm(gdp_long,gdp_combined,gdp_raw,gdp_total_long,gdp_total_raw,ghed_raw,gibd_raw,gni_long,gni_raw,vac_measles_raw,year_range,imf_gdp,imf_gdp_raw,gni_total_long,gni_total_raw)

save.image(file = "input_data/cleaned_data.RData") 
