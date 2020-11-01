library(tidyverse)
library(sf)


##Philadelphia COVID
covid_raw <- read_csv("/home/dsobolew/Documents/COVID/covid_cases_by_zip.csv")
pop <- read_csv("/home/dsobolew/Documents/COVID/phila_pop.csv")

phila_covid <- covid_raw %>%
  inner_join(pop, by = c("zip_code"="Zip")) %>%
  select(covid_status,count,zip_code,Population) %>%
  mutate(zip_code = factor(zip_code)) %>%
  filter(covid_status == "POS") %>%
  rename(zip = zip_code,pop = Population) %>%
  mutate(cases_per_cap = (count/pop) * 1000) %>%
  select(-covid_status) %>%
  rename(cases = count)
  
ph_zips <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  st_transform(26918)


phila_covid_sf <- phila_covid %>%
  inner_join(ph_zips, by = c("zip"="CODE")) %>%
  select(-OBJECTID,-COD,-Shape__Area,-Shape__Length) %>%
  st_sf()


phila_covid_sf %>%
  ggplot() +
  geom_sf(aes(fill = cases_per_cap)) +
  scale_fill_gradient(low = "#FFF5F0" , high = "#A50F15", name = "Cases Per 1000")

##Chicago COVID

chi_covid <- read_csv("/home/dsobolew/Documents/COVID/chi_covid.csv") %>%
  mutate(zip = factor(Zip)) %>%
  rename(cases = Cases, pop = Pop) %>%
  select(-Zip,-Tested) %>%
  mutate(cases_per_cap = (cases/pop) * 1000)

chi_zip <- st_read("/home/dsobolew/Documents/COVID/chi.geojson") %>%
  st_transform(32616) %>%
  filter(!objectid %in% c(51,15))

chi_covid_sf <- chi_covid %>%
  inner_join(chi_zip, by = c("zip"="zip")) %>%
  select(-objectid,-shape_area,-shape_len) %>%
  st_sf()


chi_covid_sf %>%
  ggplot() +
  geom_sf(aes(fill = cases_per_cap)) +
  scale_fill_gradient(low = "#FFF5F0" , high = "#A50F15", name = "Cases Per 1000")

## SF COVID

sf_covid <- read_csv("/home/dsobolew/Documents/COVID/sf_covid.csv") %>%
  mutate(cases_per_cap = (cases/pop) * 1000,
         zip = as.character(zip)) %>%
  filter(cases > 0)

sf_zips <- st_read("/home/dsobolew/Documents/COVID/sf.geojson") %>%
  st_transform(26710) 



sf_covid_sf <- sf_covid %>%
  inner_join(sf_zips, by = c("zip"="zip")) %>%
  select(-area,-state,-po_name,-length) %>%
  st_sf()


sf_covid_sf %>%
  ggplot() +
  geom_sf(aes(fill = cases_per_cap)) +
  scale_fill_gradient(low = "#FFF5F0" , high = "#A50F15", name = "Cases Per 1000")

covid_sf <- list(phila = phila_covid_sf, 
                 sf = sf_covid_sf, 
                 goinchi = chi_covid_sf)
 

library(spdep)

## Autocorrelation tests for phia

phila_sp <- as(phila_covid_sf, "Spatial")

nb_phila <- poly2nb(phila_sp, queen = T, row.names = phila_sp$zip)
coords <- coordinates(phila_sp)

plot(phila_sp)
plot(nb_phila, coords = coords, add = T, col = "#F78764")




## EBI Morans I
set.seed(1988)
phila_moran_mc <- EBImoran.mc(n = phila_sp$cases, x = phila_sp$pop, listw = nb2listw(nb_phila, style = "W"), nsim = 9999)

phila_moran_mc

plot(phila_moran_mc)


## local morans I

phila_lc_moran <- localmoran(phila_sp$cases_per_cap, listw = nb2listw(nb_phila, style = "W"), p.adjust.method = "bonferroni", alternative = "two.sided")

phila_lc_moran_tidy <- broom::tidy(phila_lc_moran) %>%
  rename(p_value = 6 ,zip = .rownames, morans_i = 2, z_score = 5) %>%
  #select(zip, p_value, test_stat) %>%
  mutate(clustered = ifelse(p_value <= .8,"Y","N"),
         lag_cases_per_cap = lag.listw(var = phila_sp$cases_per_cap, x =  nb2listw(nb_phila, style = "W")),
         lag_mean = mean(lag.listw(var = phila_sp$cases_per_cap, x =  nb2listw(nb_phila, style = "W"))))


moran.plot(phila_sp$cases_per_cap, listw = nb2listw(nb_phila, style = "W"))


phila_morans_stage <- phila_covid_sf %>%
  inner_join(phila_lc_moran_tidy, by = c("zip"="zip")) %>%
  mutate(cases_mean = mean(lag_cases_per_cap),
         quad = case_when(
           cases_per_cap < cases_mean & lag_cases_per_cap < lag_mean ~ "Low-Low",
           cases_per_cap < cases_mean & lag_cases_per_cap >= lag_mean ~ "Low-High",
           cases_per_cap >= cases_mean & lag_cases_per_cap < lag_mean ~ "High-Low",
           cases_per_cap >= cases_mean & lag_cases_per_cap >= lag_mean ~ "High-High"
         ))


ggplot() +
  geom_sf(data = phila_morans_stage) +
  geom_sf(data = phila_morans_stage %>% filter(p_value <= .1), aes(fill = quad)) +
  labs(title = "Philadelphia Significant COVID-19 Clustering", x = "", y = "", fill = "")

## Autocorrelation tests for Chi

chi_sp <- as(chi_covid_sf, "Spatial")


nb_chi <- poly2nb(chi_sp, queen = T, row.names = chi_sp$zip)
coords <- coordinates(chi_sp)

plot(chi_sp)
plot(nb_chi, coords = coords, add = T, col = "#F78764")



## EBI Morans I
set.seed(1988)
chi_moran_mc <- EBImoran.mc(n = chi_sp$cases, x = chi_sp$pop, listw = nb2listw(nb_chi, style = "W"), nsim = 9999)

chi_moran_mc

plot(chi_moran_mc)


## local morans I

chi_lc_moran <- localmoran(chi_sp$cases_per_cap, listw = nb2listw(nb_chi, style = "W"), p.adjust.method = "bonferroni", alternative = "two.sided")

chi_lc_moran_tidy <- broom::tidy(chi_lc_moran) %>%
  rename(p_value = 6 ,zip = .rownames, morans_i = 2, z_score = 5) %>%
  #select(zip, p_value, test_stat) %>%
  mutate(clustered = ifelse(p_value <= .1,"Y","N"),
         lag_cases_per_cap = lag.listw(var = chi_sp$cases_per_cap, x =  nb2listw(nb_chi, style = "W")),
         lag_mean = mean(lag.listw(var = chi_sp$cases_per_cap, x =  nb2listw(nb_chi, style = "W"))))


moran.plot(chi_sp$cases_per_cap, listw = nb2listw(nb_chi, style = "W"))


chi_morans_stage <- chi_covid_sf %>%
  inner_join(chi_lc_moran_tidy, by = c("zip"="zip")) %>%
  mutate(cases_mean = mean(lag_cases_per_cap),
         quad = case_when(
           cases_per_cap < cases_mean & lag_cases_per_cap < lag_mean ~ "Low-Low",
           cases_per_cap < cases_mean & lag_cases_per_cap >= lag_mean ~ "Low-High",
           cases_per_cap >= cases_mean & lag_cases_per_cap < lag_mean ~ "High-Low",
           cases_per_cap >= cases_mean & lag_cases_per_cap >= lag_mean ~ "High-High"
         ))


chi_morans_stage %>%
  ggplot() +
  geom_sf(aes(fill = quad))



ggplot() +
  geom_sf(data = chi_morans_stage) +
  geom_sf(data = chi_morans_stage %>% filter(p_value <= .1), aes(fill = quad)) +
  labs(title = "Chicago Significant COVID-19 Clustering", x = "", y = "", fill = "")


## Autocorrelation tests for SF

sf_sp <- as(sf_covid_sf, "Spatial")


nb_sf <- poly2nb(sf_sp, queen = T, row.names = sf_sp$zip)
coords <- coordinates(sf_sp)

plot(sf_sp)
plot(nb_sf, coords = coords, add = T, col = "#F78764")



## EBI Morans I
set.seed(1988)
sf_moran_mc <- EBImoran.mc(n = sf_sp$cases, x = sf_sp$pop, listw = nb2listw(nb_sf, style = "W"), nsim = 9999)

sf_moran_mc

plot(sf_moran_mc)


## local morans I

sf_lc_moran <- localmoran(sf_sp$cases_per_cap, listw = nb2listw(nb_sf, style = "W"), p.adjust.method = "bonferroni", alternative = "two.sided")

sf_lc_moran_tidy <- broom::tidy(sf_lc_moran) %>%
  rename(p_value = 6 ,zip = .rownames, morans_i = 2, z_score = 5) %>%
  #select(zip, p_value, test_stat) %>%
  mutate(clustered = ifelse(p_value <= .1,"Y","N"),
         lag_cases_per_cap = lag.listw(var = sf_sp$cases_per_cap, x =  nb2listw(nb_sf, style = "W")),
         lag_mean = mean(lag.listw(var = sf_sp$cases_per_cap, x =  nb2listw(nb_sf, style = "W"))))


moran.plot(sf_sp$cases_per_cap, listw = nb2listw(nb_sf, style = "W"))



sf_morans_stage <- sf_covid_sf %>%
  inner_join(sf_lc_moran_tidy, by = c("zip"="zip")) %>%
  mutate(cases_mean = mean(lag_cases_per_cap),
         quad = case_when(
           cases_per_cap < cases_mean & lag_cases_per_cap < lag_mean ~ "Low-Low",
           cases_per_cap < cases_mean & lag_cases_per_cap >= lag_mean ~ "Low-High",
           cases_per_cap >= cases_mean & lag_cases_per_cap < lag_mean ~ "High-Low",
           cases_per_cap >= cases_mean & lag_cases_per_cap >= lag_mean ~ "High-High"
         ))



ggplot() +
  geom_sf(data = sf_morans_stage) +
  geom_sf(data = sf_morans_stage %>% filter(p_value <= .1), aes(fill = quad)) +
  labs(title = "SF Significant COVID-19 Clustering", x = "", y = "", fill = "")



sf_morans_stage %>%
  ggplot() +
  geom_sf(aes(fill = clustered))




















