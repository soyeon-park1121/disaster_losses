EDA
================
Soyeon Park
05/20/2021

This explains steps to copy the way the paper “A Louisiana Perspective
on a National Issue” did. Since the pape’s standard year is 2016, every
data cleaning steps includes converting money values into 2016 values
using Consumer Price Index(CPI).

``` r
library(readr)
library(tidyverse)
library(readxl)
library(readxl)
library(janitor)
options(scipen = 999)

cpi <- cpi <- read_excel("~/Vanderbilt University/research/disaster_losses/data/cpi.xlsx")
pdd_pa_year <- read_csv("~/Vanderbilt University/research/disaster_losses/data/DisasterDeclarationsSummaries.csv")
la_population <- read_excel("~/Vanderbilt University/research/disaster_losses/data/la_population.xlsx")
hma <- read_csv("~/Vanderbilt University/research/disaster_losses/data/hma/HazardMitigationAssistanceProjects.csv")
pa_pdd_county_amount <- read_csv("~/Vanderbilt University/research/disaster_losses/data/pa/PA_PDD_County_Funding_Based_On_PDD_Name_Project_Amount_XXXXXX.csv")
hud_yearly_county_pdd <- read_csv("~/Vanderbilt University/HMG/data/cdbg/HUD_Yearly_County_Funding_by_PDD_Years_XXXXXX.csv")
Owners <- read_csv("~/Vanderbilt University/research/disaster_losses/data/ia/HousingAssistanceOwners.csv")
```

    ## Warning: 2 parsing failures.
    ##   row     col               expected actual                                                                                   file
    ## 10767 zipCode a double                ELSIE '~/Vanderbilt University/research/disaster_losses/data/ia/HousingAssistanceOwners.csv'
    ## 18865 zipCode no trailing characters   0    '~/Vanderbilt University/research/disaster_losses/data/ia/HousingAssistanceOwners.csv'

``` r
Renters <- read_csv("~/Vanderbilt University/research/disaster_losses/data/ia/HousingAssistanceRenters.csv")
lousiana_county_fips <- read_excel("~/Vanderbilt University/research/disaster_losses/data/lousiana_county_fips.xlsx")
```

# Cleaning HMG data (- 2016)

## Inserting FIPS coloumn

``` r
hma_la <- hma %>% filter(state == "Louisiana") %>% select(projectAmount, programArea, programFy, disasterNumber, stateNumberCode, state, countyCode, county) # filter only Louisiana's data
hma_la_1 <- hma_la %>% filter(countyCode < 10) # the number of countyCode's digit == 1
hma_la_2 <- hma_la %>% filter(nchar(as.character(countyCode)) == 2) # the number of countyCode's digit == 2
hma_la_3 <- hma_la %>% filter(countyCode >= 100) # the number of countyCode's digit == 3

# Making FIPS format
hma_la_1 <- hma_la_1 %>% mutate(fips = paste0(as.character(stateNumberCode),"00",as.character(countyCode)))
hma_la_2 <- hma_la_2 %>% mutate(fips = paste0(as.character(stateNumberCode),"0",as.character(countyCode)))
hma_la_3 <- hma_la_3 %>% mutate(fips = paste0(as.character(stateNumberCode),as.character(countyCode)))

hma_la_fips <- rbind(hma_la_1, hma_la_2, hma_la_3) # Combining the three data
```

## Dividing HMG data by categories

``` r
hmgp_la <- hma_la_fips %>% filter(programArea == "HMGP") %>% filter(programFy <= 2016) # HMGP data until 2016
pdm_la <- hma_la_fips %>% filter(programArea == "PDM")%>%  filter(programFy <= 2016) %>% filter(!is.na(county)) # PDM data until 2016
fma_la <- hma_la_fips %>% filter(programArea == "FMA") %>% filter(programFy <= 2016) %>% filter(!is.na(county)) # FMA data until 2016
```

## Converting projectAmount data into 2016 value

Using the Consumer Price Index(CPI), I converted all projectAmount into
2016 January value. (2016 value = cpi \* projectAmount) \* data source:
<https://www.bls.gov/data/inflation_calculator.htm>

``` r
hmgp_la <- left_join(hmgp_la, cpi)
```

    ## Joining, by = "programFy"

``` r
hmgp_la <- hmgp_la %>% mutate(after_cpi = cpi * projectAmount) 
pdm_la <- left_join(pdm_la, cpi)
```

    ## Joining, by = "programFy"

``` r
pdm_la <- pdm_la %>% mutate(after_cpi = cpi * projectAmount)
fma_la <- left_join(fma_la, cpi)
```

    ## Joining, by = "programFy"

``` r
fma_la <- fma_la %>% mutate(after_cpi = cpi * projectAmount)
```

The data above shows the amount of project budgets by year and FIPS. I
summed the total money spent by each county across all years outlined in
the original chart. FMA and HMGP data has “22000(statewide)”. I divided
this data by the number of all counties in Louisiana and added it in
each county’s projetAmount.

``` r
pdm_la_after_cpi <- pdm_la %>% group_by(fips, county) %>% summarise(total = sum(after_cpi))
```

    ## `summarise()` regrouping output by 'fips' (override with `.groups` argument)

``` r
pdm_la_df <- pdm_la_after_cpi %>% mutate(pdm = total) %>% select(-total)

fma_la_after_cpi <- fma_la %>% group_by(fips, county) %>% summarise(total = sum(after_cpi))
```

    ## `summarise()` regrouping output by 'fips' (override with `.groups` argument)

``` r
fma_la_df <- fma_la_after_cpi %>% mutate(fma = total + (18261851.3/64)) %>% filter(fips != "22000") %>% select(-total) # 18261851.3 is 22000(statewide) value. 64 is the number of states in Louisiana.

hmgp_la_after_cpi <- hmgp_la %>% group_by(fips, county) %>% summarise(total = sum(after_cpi))
```

    ## `summarise()` regrouping output by 'fips' (override with `.groups` argument)

``` r
hmgp_la_df <- hmgp_la_after_cpi %>% mutate(hmgp = total + (1364168183.4/64)) %>% filter(fips != "22000") %>% select(-total) # 1364168183.4 is 22000(statewide) value. 64 is the number of states in Louisiana.
```

# Cleaning PA data (1999 - 2016)

PA data are divided by disasterNumber. In order to convert all money
spent into 2016 value, year information of each disaster is needed. I
matched disasterNumber with year data and changed the money spent into
2016 value.

``` r
pa_la <- pa_pdd_county_amount %>% filter(State == "Louisiana") # filtering Louisiana's data
pa_la_temp <- pa_la %>% select(-fip, -State, -County)
pa_la_temp <- as.data.frame(colSums(pa_la_temp)) # Sum all the amount of money spent on each county
pa_la_temp <- pa_la_temp %>% filter(`colSums(pa_la_temp)` > 0) # remove all 0 values.
pa_la_temp <- cbind(rownames(pa_la_temp), pa_la_temp)
rownames(pa_la_temp) <- NULL
colnames(pa_la_temp) <- c("disasterNumber","total")
pa_la_temp <- pa_la_temp %>% mutate(disasterNumber = as.numeric(disasterNumber))
```

``` r
pa_temp <- left_join(pa_la_temp, pdd_pa_year)
```

    ## Joining, by = "disasterNumber"

``` r
pa_temp <- pa_temp[!duplicated(pa_temp$disasterNumber), ]
row.names(pa_temp) <- NULL
colnames(cpi) <- c("fyDeclared", "cpi")

pa_list <-left_join(pa_temp, cpi) %>% select(disasterNumber, fyDeclared, cpi)
```

    ## Joining, by = "fyDeclared"

``` r
pa <- clean_names(pa_pdd_county_amount)

# converting data into 2016 value
pa_cpi <- pa %>% filter(state == "Louisiana") %>%
  mutate(cpi_1264 = x1264 * 1.44) %>%
  mutate(cpi_1269 = x1269 * 1.44) %>%
  mutate(cpi_1314 = x1314 * 1.40) %>%
  mutate(cpi_1357 = x1357 * 1.35) %>%
  mutate(cpi_1380 = x1380 * 1.35) %>%
  mutate(cpi_1435 = x1435 * 1.34) %>%
  mutate(cpi_1437 = x1437 * 1.30) %>%
  mutate(cpi_1548 = x1548 * 1.28) %>%
  mutate(cpi_1601 = x1601 * 1.24) %>%
  mutate(cpi_1603 = x1603 * 1.24) %>%
  mutate(cpi_1607 = x1607 * 1.24) %>%
  mutate(cpi_1668 = x1668 * 1.17) %>%
  mutate(cpi_1786 = x1786 * 1.12) %>%
  mutate(cpi_1792 = x1792 * 1.12) %>%
  mutate(cpi_1863 = x1863 * 1.09) %>%
  mutate(cpi_3172 = x3172 * 1.30) %>%
  mutate(cpi_3322 = x3322 * 1.08) %>%
  mutate(cpi_3376 = x3376 * 1) %>%
  mutate(cpi_4015 = x4015 * 1.08) %>%
  mutate(cpi_4041 = x4041 * 1.05) %>%
  mutate(cpi_4080 = x4080 * 1.05) %>%
  mutate(cpi_4102 = x4102 * 1.03) %>%
  mutate(cpi_4228 = x4228 * 1.01) %>%
  mutate(cpi_4263 = x4263 * 1) %>%
  mutate(cpi_4277 = x4277 * 1) %>%
  select(1:3,1426:1450)

pa_cpi_number <- pa_cpi %>% select(4:28)
pa_cpi_name <- pa_cpi %>% select(1:3)
pa_cpi_number <- pa_cpi_number %>% mutate(pa = rowSums(pa_cpi_number))
pa_cpi_final <- cbind(pa_cpi_name, pa_cpi_number)
```

As I did to HMG data, I divided value for 22000(statewide) by the number
of all counties in Louisiana and added it in each county’s projetAmount.

``` r
pa_cpi_final <- pa_cpi_final %>% select(fip, state, county, pa)
pa_df <- pa_cpi_final %>% mutate(total = pa + (6795623024.2/64)) %>% select(fip, state, county, total) %>% mutate(pa = total) %>% select(-total) # 6795623024.2 is the statewide(22000) value and 64 is the number of states in Louisiana.
pa_df <- pa_df %>% filter(fip != "22000")
pa_df <- pa_df %>% select(fip, pa) %>% mutate(fips = fip) %>% select(-fip)
```

# Merge cleaned HMG(HMGP, PDM, and FMA) data and PA data

``` r
df1 <- left_join(hmgp_la_df, pdm_la_df)
```

    ## Joining, by = c("fips", "county")

``` r
df2 <- left_join(df1, fma_la_df)
```

    ## Joining, by = c("fips", "county")

``` r
df3 <- left_join(df2, pa_df)
```

    ## Joining, by = "fips"

# Cleaning CDBG data (2006 - 2016)

I also converted CDBG data into 2016 value using CPI. The paper “A
louisiana Perspective on a National Issue” has data from 2006. However,
in our dataset, 2005 data exist. Just in case, I prepared two categories
data: data from 2005, and another data from 2006

``` r
cdbg <- hud_yearly_county_pdd %>% filter(State == "LA") %>%
  select(1:21)

cdbg_temp <- cdbg %>% select(4:21)
cdbg_temp <- as.data.frame(colSums(cdbg_temp))
cdbg_temp %>% filter(`colSums(cdbg_temp)` > 0)
```

    ##      colSums(cdbg_temp)
    ## 2005         7617794652
    ## 2006         9929570253
    ## 2008         1192647769
    ## 2012          161911484
    ## 2013          306200489
    ## 2016           97204689

``` r
cdbg <- clean_names(cdbg)
cdbg_la <- cdbg %>% mutate(cpi_2005 = x2005 * 1.24) %>%
  mutate(cpi_2006 = x2006 * 1.19) %>% # 2006 cpi
  mutate(cpi_2008 = x2008 * 1.12) %>% # 2008 cpi
  mutate(cpi_2012 = x2012 * 1.05) %>% # 2012 cpi
  mutate(cpi_2013 = x2013 * 1.03) %>% # 2013 cpi
  mutate(cpi_2016 = x2016 * 1) %>% select(fips, county, cpi_2005, cpi_2006, cpi_2008, cpi_2012, cpi_2013, cpi_2016)

cdbg_number_2005 <- cdbg_la %>% select(3:8)
cdbg_number_2006 <- cdbg_la %>% select(4:8)
cdbg_name <- cdbg_la %>% select(1:2)

cdbg_number_2005 <- cdbg_number_2005 %>% mutate(cdbg_2005 = rowSums(cdbg_number_2005)) %>% select(cdbg_2005) # from 2005
cdbg_number_2006 <- cdbg_number_2006 %>% mutate(cdbg_2006 = rowSums(cdbg_number_2006)) %>% select(cdbg_2006) # from 2006

cdbg_df <- cbind(cdbg_name, cdbg_number_2005, cdbg_number_2006)
cdbg_df <- cdbg_df %>% select(-county)
cdbg_df <- cdbg_df %>% mutate(fips = as.character(fips))
```

# Merge \[HMG+PA\] data and CDBG data

``` r
df4 <- left_join(df3, cdbg_df)
```

    ## Joining, by = "fips"

# Cleaning IA data(2004 - 2016)

For IA data, I brought \[Housing Assistancwe Program data - Owners\] and
\[Housing Assistance Program Data - Renters\] from openFEMA. - data
source:
<https://www.fema.gov/openfema-data-page/housing-assistance-program-data-owners-v2>,
<https://www.fema.gov/openfema-data-page/housing-assistance-program-data-renters-v2>

``` r
owners_ia <- Owners %>% filter(state == "LA") %>% select(disasterNumber, county, totalApprovedIhpAmount)
renters <- Renters %>% filter(state == "LA") %>% select(disasterNumber, county, totalApprovedIhpAmount)

ia_temp <- rbind(owners_ia, renters)
ia_temp <- left_join(ia_temp, pdd_pa_year)
```

    ## Joining, by = "disasterNumber"

I also converted all value in IA data into 2016 value.

``` r
ia_temp_2016 <- ia_temp %>% arrange(fyDeclared) %>% filter(fyDeclared <= 2016) 

# Converting values
ia_cpi <- left_join(ia_temp_2016, cpi)
```

    ## Joining, by = "fyDeclared"

``` r
ia_cpi <- ia_cpi %>% mutate(cpi_ihp = totalApprovedIhpAmount * cpi) %>% select(disasterNumber, county, cpi_ihp, fyDeclared)
ia_cpi_2004 <- ia_cpi %>% group_by(county) %>% summarise(ia_2004 = sum(cpi_ihp))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ia_cpi_2004 <- ia_cpi_2004 %>% filter(county != "Statewide")
ia <- ia_cpi_2004
```

``` r
# Adding FIPS code
lousiana_county_fips <- left_join(ia, lousiana_county_fips) 
```

    ## Joining, by = "county"

``` r
lousiana_county_fips <- lousiana_county_fips %>% mutate(fips = as.character(fips))
lousiana_county_fips <- lousiana_county_fips %>% select(fips, ia_2004)
```

# Merge the previous combined data with IA data

``` r
df5 <- left_join(df4, lousiana_county_fips)
```

    ## Joining, by = "fips"

# Organize the data set.

``` r
lousiana <- df5 %>% mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(mitigation = hmgp + pdm +fma) %>%
  mutate(recovery = pa + cdbg_2006 + ia_2004) %>%
  select(fips, county, mitigation, recovery, everything()) %>%
  mutate(ia = ia_2004) %>%
  select(-ia_2004)
```

    ## `mutate_if()` ignored the following grouping variables:
    ## Column `fips`

For population data, I used U.S. census 2010 data.

\[mitigation\_per\_capita\] = \[mitigation\] / \[population\]

\[recovery\_per\_capita\] = \[recovery\] / \[population\]

``` r
la_population <- la_population %>% select(-county)
la_population <- la_population %>% mutate(fips = as.character(fips))

left_join(lousiana, la_population) %>%
  mutate(mitigation_per_capita = mitigation / population) %>%
  mutate(recovery_per_capita = recovery / population)
```

    ## Joining, by = "fips"

    ## # A tibble: 64 x 14
    ## # Groups:   fips [64]
    ##    fips  county   mitigation  recovery     hmgp   pdm     fma       pa cdbg_2005
    ##    <chr> <chr>         <dbl>     <dbl>    <dbl> <dbl>   <dbl>    <dbl>     <dbl>
    ##  1 22001 Acadia    25120424.    1.38e8   2.51e7     0  0        1.31e8    7.79e6
    ##  2 22003 Allen     23250651.    1.15e8   2.33e7     0  0        1.11e8    3.74e6
    ##  3 22005 Ascensi…  39074430.    2.70e8   3.87e7     0  3.50e5   2.42e8    2.80e7
    ##  4 22007 Assumpt…  28095843.    1.49e8   2.81e7     0  0        1.15e8    3.45e7
    ##  5 22009 Avoyell…  22862432.    1.19e8   2.29e7     0  0        1.13e8    5.97e6
    ##  6 22011 Beaureg…  23571057.    1.16e8   2.36e7     0  0        1.13e8    3.06e6
    ##  7 22013 Bienvil…  21840748.    1.09e8   2.18e7 57620  0        1.07e8    2.40e6
    ##  8 22015 Bossier   32150474.    1.61e8   3.11e7     0  1.07e6   1.61e8    0     
    ##  9 22017 Caddo     29422280.    1.19e8   2.94e7     0  0        1.19e8    0     
    ## 10 22019 Calcasi…  55444242.    3.59e8   5.30e7     0  2.48e6   2.73e8    3.04e8
    ## # … with 54 more rows, and 5 more variables: cdbg_2006 <dbl>, ia <dbl>,
    ## #   population <dbl>, mitigation_per_capita <dbl>, recovery_per_capita <dbl>
