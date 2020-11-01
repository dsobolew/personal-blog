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
  select(-covid_status)
  
ph_zips <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson") %>%
  st_transform(26918)


phila_covid_sf <- phila_covid %>%
  inner_join(ph_zips, by = c("zip"="CODE")) %>%
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
  st_transform(32616)

chi_covid_sf <- chi_covid %>%
  inner_join(chi_zip, by = c("zip"="zip")) %>%
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
  st_sf()


sf_covid_sf %>%
  ggplot() +
  geom_sf(aes(fill = cases_per_cap)) +
  scale_fill_gradient(low = "#FFF5F0" , high = "#A50F15", name = "Cases Per 1000")

library(spdep)
library(rgdal)
rgdal:

phila_sp <- as(phila_covid, "Spatial")

nb_queen <- poly2nb(phila_sp, queen = T, row.names = phila_sp$CODE)
coords <- coordinates(phila_sp)

plot(phila_sp)
plot(nb_queen, coords = coords, add = T, col = "#F78764")


##Global Morans I
gl_moran <- moran.test(phila_sp$cases_per_pop, listw = nb2listw(nb_queen, style = "W"))

gl_moran


##Global MC Morans I
set.seed(1988)
gl_moran_mc <- moran.mc(phila_sp$cases_per_pop, listw = nb2listw(nb_queen, style = "W"), nsim = 9999)

gl_moran_mc

plot(gl_moran_mc)


## EBI Morans I
gl_moran_ebi <- EBImoran.mc(n = phila_sp$count, x = phila_sp$Population, listw = nb2listw(nb_queen, style = "W"), nsim = 9999)

gl_moran_ebi

plot(gl_moran_mc)

?EBImoran.mc



## local morans I

lc_moran <- localmoran(phila_sp$cases_per_pop, listw = nb2listw(nb_queen, style = "W"), p.adjust.method = "bonferroni", alternative = "two.sided")

lc_moran_tidy <- broom::tidy(lc_moran) %>%
  rename(p_value = Pr.z....0.,zip = .rownames) %>%
  #select(zip, p_value, test_stat) %>%
  mutate(clustered = ifelse(p_value <= .1,"Y","N"),
         lag_cases_per_pop = lag.listw(var = phila_sp$cases_per_pop, x =  nb2listw(nb_queen, style = "W")),
         lag_mean = mean(lag.listw(var = phila_sp$cases_per_pop, x =  nb2listw(nb_queen, style = "W"))))


moran.plot(phila_sp$cases_per_pop, listw = nb2listw(nb_queen, style = "W"))



phila_morans <- phila_covid %>%
  inner_join(lc_moran_tidy, by = c("CODE"="zip")) %>%
  mutate(cases_mean = mean(cases_per_pop),
         quad = case_when(
           cases_per_pop < cases_mean & lag_cases_per_pop < lag_mean ~ "Low-Low",
           cases_per_pop < cases_mean & lag_cases_per_pop >= lag_mean ~ "Low-High",
           cases_per_pop >= cases_mean & lag_cases_per_pop < lag_mean ~ "High-Low",
           cases_per_pop >= cases_mean & lag_cases_per_pop >= lag_mean ~ "High-High"
           
         ))


phila_morans %>%
  ggplot() +
  geom_sf(aes(fill = quad))


phila_covid %>%
  ggplot(aes(fill = cases_per_pop)) +
  geom_sf() +
  scale_fill_gradient(low = "#FFF5F0" , high = "#A50F15", name = "Cases Per 1000")



























