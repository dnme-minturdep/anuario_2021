library(tidyverse) 
library(lubridate) # Make Dealing with Dates a Little Easier, CRAN v1.7.9
library(openxlsx)
library(janitor)
library(googlesheets4)
library(cowplot)
library(readr)
library(haven)
library(dplyr)
library(dtplyr)
library(readr)
library(herramientas)


base_eoh <- read_file_srv("/eoh/bases/eoh_serie_historica.rds") 

glosario_eoh <- read_file_srv("/eoh/bases/glosario_eoh.xlsx") 

base_eoh_total <- base_eoh %>%  
  janitor::clean_names() %>% 
  select(anio,mes,id_establecimiento, w2, p5_t2, p2_00_ptr) %>% 
  rename(total_pernoctes="p5_t2", puestos_promedio="p2_00_ptr") %>% 
  group_by(anio,mes) %>% 
  summarise(total_establecimientos = sum(w2, na.rm = T),
            total_puestos = sum((puestos_promedio*w2), na.rm = T),
            total_pernoctes= sum((total_pernoctes*w2), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio,mes) %>% 
  summarise( prom_puestos = mean(total_puestos),
             total_pernoctes=sum(total_pernoctes),
             prom_establecimientos= mean(total_establecimientos)) %>% 
  ungroup() %>% 
  filter(anio>=2012)


write_file_srv(base_eoh_total,"/economia2/anuario/base_eoh_total.csv")


base_eoh_region <- base_eoh %>%  
  janitor::clean_names() %>% 
  select(anio,mes, region, id_establecimiento, w2, p5_t2, p2_00_ptr) %>% 
  rename(total_pernoctes="p5_t2", puestos_promedio="p2_00_ptr") %>% 
  group_by(anio, mes, region) %>% 
  summarise(total_establecimientos = sum(w2, na.rm = T),
            total_puestos = sum((puestos_promedio*w2), na.rm = T),
            total_pernoctes= sum((total_pernoctes*w2), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio,region) %>% 
  summarise( prom_puestos = mean(total_puestos),
             total_pernoctes=sum(total_pernoctes),
             prom_establecimientos= mean(total_establecimientos)) %>% 
  ungroup()%>% 
  filter(anio>=2012)

write_file_srv(base_eoh_region,"/srv/DataDNMYE/economia2/anuario/base_eoh_region.csv")

base_eoh_region_2021 <- base_eoh %>%  
  janitor::clean_names() %>% 
  select(anio,mes, region, id_establecimiento, w2, p5_t2, p2_00_ptr) %>% 
  rename(total_pernoctes="p5_t2", puestos_promedio="p2_00_ptr") %>% 
  group_by(anio, mes, region) %>% 
  summarise(total_establecimientos = sum(w2, na.rm = T),
            total_puestos = sum((puestos_promedio*w2), na.rm = T),
            total_pernoctes= sum((total_pernoctes*w2), na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio,mes,region) %>% 
  summarise( prom_puestos = mean(total_puestos),
             total_pernoctes=sum(total_pernoctes),
             prom_establecimientos= mean(total_establecimientos)) %>% 
  ungroup()%>% 
  filter(anio==2021)

write_file_srv(base_eoh_region_2021,"/srv/DataDNMYE/economia2/anuario/base_eoh_region_2021.csv")


