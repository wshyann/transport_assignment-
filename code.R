library(tidyverse)
library(readxl)
library(plm)
library(sf)
library(tmap)
library(cartography)

#---- Data prep ----

# Preparing the data from 2013-2017 

data_10_17 = read_csv("co2-emissions-borough-leggi.csv") |> 
  filter(Sector == "Transport", Fuel == "Road Transport", Data_Year != 2010) |> 
  select("Borough", "Data_Year",  "KtCO2e") |> 
  rename("Emissions" = "KtCO2e") 

# Preparing the data for 2018 

data_18 = excel_sheets("LEGGI_2018_FINAL.xlsx")

data_18 = read_excel("LEGGI_2018_FINAL.xlsx", sheet = "02 CO2") 

data_18 = data_18[6:38,] |> select("Table 2.0", "...18") |> 
  rename("Borough"="Table 2.0", "Emissions"="...18") |> 
  mutate(Data_Year = 2018) 

data_18$Emissions = as.numeric(data_18$Emissions)

# Preparing the data for 2019

data_19 = excel_sheets("LEGGI 2019 Final.xlsx")

data_19 = read_excel("LEGGI 2019 Final.xlsx", sheet = "II. Transport ") 

data_19 = data_19[8:40,] |> select("...2", "...4") |> 
  rename("Borough"="...2", "Emissions"="...4") |> 
  mutate(Data_Year = 2019)

data_19$Emissions = as.numeric(data_19$Emissions)

data_19 = data_19 |> mutate(Emissions = Emissions/1000,
                            Borough = if_else(Borough == "Kingston upon Thames", "Kingston", Borough))


# Data Merger

final_data = rbind(data_10_17, data_18, data_19) |> 
  filter(Borough != "Unapportioned", Borough != "London")



# ---- Interrupted time series -----

final_data_its = final_data |> mutate(YearsAfter = if_else(Data_Year >= 2017, Data_Year - 2016, 0),
                                      Year_new = Data_Year - 2012)

its_model = plm ( Emissions ~ Year_new + Intervention + YearsAfter, final_data_its, model="within")
summary(its_model)

# Conterfactual 

before_interv = final_data_its |> filter(Data_Year %in% c(2013, 2014, 2015, 2016)) 

counterfactual = plm(Emissions ~ Year_new, before_interv, model="within")
summary(counterfactual)

fixed_effect = data.frame(Borough = names(fixef(counterfactual)), 
                          Fixed = as.vector(fixef(counterfactual))) |> 
  tibble()

cont_data = final_data_its |> filter(Data_Year %in% c(2017, 2018, 2019)) |> 
  select(Borough, Year_new) 

cont_data = left_join(cont_data, fixed_effect, by = "Borough") |> 
  mutate(Emissions = Fixed + 0.97*Year_new)




# ---- Spatial analytics -----

london_borough = read_sf('London_Boroughs.gpkg') |> arrange(name)

largefinal = final_data |> select(-Intervention) |> pivot_wider(names_from = Data_Year, values_from = Emissions) |> 
  rename("data_2014" = "2014", "data_2015" = "2015", "data_2016" = "2016", "data_2017" = "2017", "data_2018" = "2018",
         "data_2019" = "2019") |>
  mutate(Before = data_2014 + data_2015 + data_2016,
         After = data_2017 + data_2018 + data_2019,
         Before_After = Before - After,
         Immediate_Diff = data_2016 - data_2017,
         LT_Diff = data_2017 - data_2019,
         Borough = if_else(Borough == "Kingston", "Kingston upon Thames", Borough),
         Borough = if_else(Borough == "Richmond", "Richmond upon Thames", Borough)) |> 
  select(Borough, Before_After, Immediate_Diff, LT_Diff)

final_geospatial = left_join(london_borough, largefinal, by = c("name"="Borough"))

counterfactual_geo = before_interv |> select(Borough, Year_new, Emissions) |> 
  rbind(select(cont_data, -Fixed)) |> 
  pivot_wider(names_from = Year_new, values_from = Emissions) |> 
  rename("data_2014" = "2", "data_2015" = "3", "data_2016" = "4", "data_2017" = "5", "data_2018" = "6",
         "data_2019" = "7") |>
  mutate(Before = data_2014 + data_2015 + data_2016,
         After = data_2017 + data_2018 + data_2019,
         Before_After = Before - After, 
         Borough = if_else(Borough == "Kingston", "Kingston upon Thames", Borough),
         Borough = if_else(Borough == "Richmond", "Richmond upon Thames", Borough)) |> 
  select(Borough, Before_After)

counterfactual_geo = left_join(london_borough, counterfactual_geo, by = c("name"="Borough")) 

blue_to_red_palette <- colorRampPalette(c("deepskyblue3", "white", "red4"))

# Before VS After

tmap_mode("view")
tm_shape(final_geospatial)+
  tm_fill(col='Before_After', palette = blue_to_red_palette(6) )+
  tm_text ("name", size=.9, col='black', shadow=TRUE) +
  tm_borders()+
  tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.3)) +
  tm_compass(size = 3, type = 'rose') +
  tm_scale_bar(size = 0.5)

# Immediate Difference

tmap_mode("view")
tm_shape(final_geospatial)+
  tm_fill(col='Immediate_Diff', palette = blue_to_red_palette(6) )+
  tm_text ("name", size=.9, col='black', shadow=TRUE) +
  tm_borders()+
  tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.3)) +
  tm_compass(size = 3, type = 'rose') +
  tm_scale_bar(size = 0.5)

# Long term Difference

tmap_mode("view")
tm_shape(final_geospatial)+
  tm_fill(col='LT_Diff', palette = blue_to_red_palette(6) )+
  tm_text ("name", size=.9, col='black', shadow=TRUE) +
  tm_borders()+
  tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.3)) +
  tm_compass(size = 3, type = 'rose') +
  tm_scale_bar(size = 0.5)

# Counterfactual

tmap_mode("view")
tm_shape(counterfactual_geo)+
  tm_fill(col='Before_After', palette = blue_to_red_palette(6) )+
  tm_text ("name", size=.9, col='black', shadow=TRUE) +
  tm_borders()+
  tm_layout(inner.margins = c(0.02, 0.02, 0.02, 0.3)) +
  tm_compass(size = 3, type = 'rose') +
  tm_scale_bar(size = 0.5)
