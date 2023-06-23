#-----------------------ETI RECURSOS DATA ABIERTA---------------------#

#-------------------activamos paquetes y librerias------------#


library(tidyverse)
library(magrittr) #pipe
library(janitor)
library(lubridate)
library(haven)
library(dplyr) #Group_by,summarise, lag, filter
library(glue)
library (gt)
library(comunicacion)
library(ggplot2)
library(ggtext)

### Se defininen algunos parámetros

#anio de referencia de publicación

anio_ref <- 2021

#-----------------------Se levanta la base---------------------#

eti_e <- readRDS("/srv/DataDNMYE/eti/bases/eti_nr_2009_2023.rds") %>% 
  filter(p3_3<=anio_ref & vis==2 & wpf>0) 

eti_e_perfiles <- eti_e %>%
  filter((paso_final=="Ezeiza y Aeroparque" & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-01-01" | anio_trim=="2021-10-01"))) |
           (paso_final %in% c("Aep. Córdoba","Puerto de Buenos Aires","Aep. Mendoza","Cristo Redentor") & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-10-01"))))

options(scipen = 9999)

#--------------------------------------------------------#

#Cuadro 1

#```{r eti1, fig.cap=glue("Llegadas de turistas no residentes por año según paso. Año {anio_ref}.")}

#Insumo:

etie1 <- eti_e %>% 
  group_by(p3_3,paso_final) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  ungroup() %>% 
  complete(p3_3 = seq(from = min(p3_3), 
                      to = max(p3_3), 
                      by = 1),
           paso_final,
           fill = list(viajeros = NA_real_)) %>%
  pivot_wider(names_from = paso_final,
              values_from = viajeros)%>%
  mutate(var_1=round((`Ezeiza y Aeroparque`/lag(`Ezeiza y Aeroparque`,1)-1),3),
         var_2=round((`Aep. Córdoba`/lag(`Aep. Córdoba`,1)-1),3),
         var_3=round((`Puerto de Buenos Aires`/lag(`Puerto de Buenos Aires`,1)-1),3),
         var_4=round((`Aep. Mendoza`/lag(`Aep. Mendoza`,1)-1),3),
         var_5=round((`Cristo Redentor`/lag(`Cristo Redentor`,1)-1),3)) %>% 
  select(p3_3,`Ezeiza y Aeroparque`,var_1,
         `Aep. Córdoba`,var_2,
         `Aep. Mendoza`,var_4,
         `Puerto de Buenos Aires`,var_3,
         `Cristo Redentor`,var_5) %>% 
  filter(p3_3>=2010) %>% 
  mutate(`Aep. Mendoza`=case_when(p3_3==2017~NA_real_,
                                  TRUE~`Aep. Mendoza`),
         var_4=case_when(p3_3==2017~NA_real_,
                         TRUE~var_4))
#Tabla:

etie1 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(p3_3 = "Año",
             `Ezeiza y Aeroparque`="Turistas",
             `Aep. Córdoba`="Turistas",
             `Aep. Mendoza`="Turistas",
             `Puerto de Buenos Aires`="Turistas",
             `Cristo Redentor`="Turistas",
             var_1 ="var % i.a",
             var_2 = "var % i.a",
             var_3 = "var % i.a",
             var_4 = "var % i.a*",
             var_5 = "var % i.a")  %>%
  fmt_number(columns = c(2,4,6,8,10), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,5,7,9,11), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(#table.font.size = 12,
    data_row.padding = px(3),
    column_labels.font.size = 14,
    column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,4,6,8,10) ~ px(100),
             columns = c(3,5,7,9,11) ~ px(70),
             columns = c(1) ~ px(60)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>%
  tab_source_note(
    source_note = md("**(*)** El Aep. Int. de Mendoza comenzó a ser relevado en el 3er trim de 2017, no se incluyen los datos del 2017 ya que no hay datos del año completo.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  tab_spanner(label = "Eze / Aep.", columns = c(2:3)) %>% 
  tab_spanner(label = "Aep. Córdoba", columns = c(4:5)) %>% 
  tab_spanner(label = "Aep. Mendoza*", columns = c(6:7)) %>%
  tab_spanner(label = "Puerto CABA", columns = c(8:9)) %>%
  tab_spanner(label = "Cto. Redentor", columns = c(10:11))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(var_1),
      rows = var_1 <= 0
    )) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = nrow(etie1))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(etie1)
    )) %>%  
  gtsave("imagenes/eti/etie1.png")


rm(etie1)


###Cuadro 2.2.2

#```{r eti2, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Ezeiza y Aeroparque. Año {anio_ref}.")}

#Ezeiza y aeroparque (dato chequeado)

#Insumo:

etie2 <- eti_e_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Ezeiza y Aeroparque") %>% 
  group_by(p3_3,orig_eya) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (orig_eya=factor(orig_eya,levels =c("Total","Bolivia",
                                             "Brasil","Chile",
                                             "Paraguay","Uruguay",
                                             "EE.UU y Canadá","Resto América",
                                             "Europa","Resto del Mundo"))) %>% 
  arrange(orig_eya)

if (anio_ref==2021) {
  etie2 <- etie2 %>% select(orig_eya,t0,prop_)
} else {
  etie2 <- etie2 %>% select(orig_eya,t0,var_1,var_2,prop_)
}  


#Tabla:

etie2 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(orig_eya = "Lugar de residencia",
             t0="Turistas",
             prop_ = "Distribución %")  %>%
  fmt_number(columns = c(2), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(data_row.padding = px(3),
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(1,2) ~ px(140),
             columns = c(3) ~ px(140)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-") %>% 
  cols_align(
    align = "left",
    columns = 1) %>%
  cols_align(
    align = "center",
    columns = 2:3) %>%
  # tab_header(
  #   subtitle = 
  #     glue("Ezeiza y Aeroparque, {anio_ref}."),
  #   title =""
  # ) %>%
  tab_source_note(
    source_note = md("**Nota:** Datos que corresponden al 1er y 4to trim. de 2021, en el resto de los trims. no se realizó el relevamiento.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>%  
  gtsave("imagenes/eti/etie2.png")


rm(etie2)



#```{r eti3, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Aeropuerto Internacional de Córdoba. Año {anio_ref}.")}

#AEP Córdoba (dato chequeado)

#Insumo:

etie3 <- eti_e_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Aep. Córdoba") %>% 
  group_by(p3_3,orig_cor) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros))%>%
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>% 
  mutate(prop_=prop.table(t0)) %>% 
  ungroup() %>% 
  adorn_totals()%>%
  mutate (orig_cor=factor(orig_cor,levels =c("Total","Brasil","Chile",
                                             "EE.UU, Canadá y México","Resto de América",
                                             "Europa y resto del mundo"))) %>% 
  arrange(orig_cor)



if (anio_ref==2021) {
  etie3 <- etie3 %>% select(orig_cor,t0,prop_)
} else {
  etie3 <- etie3 %>% select(orig_cor,t0,var_1,var_2,prop_)
}  


#Tabla GT ()

etie3 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(orig_cor = "Lugar de residencia",
             t0="Turistas",
             prop_ = "Distribución %")  %>%
  fmt_number(columns = c(2), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(data_row.padding = px(3)) %>% 
  cols_width(columns = c(1) ~ px(160),
             columns = c(2:3) ~ px(140)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-") %>%
  tab_header(
    subtitle = 
      glue("Aeropuerto Internacional de Córdoba, {anio_ref}."),
    title =""
  ) %>%
  tab_source_note(
    source_note = md("**Nota:** Datos que corresponden al 4to trim. de 2021, en el resto de los trims. no se realizó el relevamiento.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  opt_all_caps() %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>%  
  gtsave("imagenes/eti/etie3.png")


rm(etie3)


#```{r eti4, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Puerto de Buenos Aires. Año {anio_ref}.")}

# Puerto de buenos aires (dato chequeado)

#Insumo:

etie4 <- eti_e_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Puerto de Buenos Aires") %>% 
  group_by(p3_3,orig_puerto) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (orig_puerto=factor(orig_puerto,levels =c("Total","Uruguay",
                                                   "Resto del Mundo"))) %>% 
  arrange(orig_puerto)

if (anio_ref==2021) {
  etie4 <- etie4 %>% select(orig_puerto,t0,prop_)
} else {
  etie4 <- etie4 %>% select(orig_puerto,t0,var_1,var_2,prop_)
}  


#Tabla:


etie4 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(orig_puerto = "Lugar de residencia",
             t0="Turistas",
             prop_ = "Distribución %")  %>%
  fmt_number(columns = c(2), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(#table.font.size = 12,
    data_row.padding = px(3),
    #column_labels.font.size = 14,
    column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(1,2) ~ px(120),
             columns = c(3) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "-") %>%
  tab_source_note(
    source_note = md("**Nota:** Datos que corresponden al 4to trim. de 2021, en el resto de los trims. no se realizó el relevamiento.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>%  
  gtsave("imagenes/eti/etie4.png")

rm(etie4)


#```{r eti5, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Aeropuerto Internacional de Mendoza. Año {anio_ref}.")}

### Aep Mendoza (dato chequeado)

#Insumo:

etie5 <- eti_e_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Aep. Mendoza") %>% 
  group_by(p3_3,orig_mdz) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (orig_mdz=factor(orig_mdz,levels =c("Total","Brasil",
                                             "Chile","EE.UU, Canadá",
                                             "Resto de América",
                                             "Europa y resto del mundo"))) %>% 
  arrange(orig_mdz)

if (anio_ref==2021) {
  etie5 <- etie5 %>% select(orig_mdz,t0,prop_)
} else {
  etie5 <- etie5 %>% select(orig_mdz,t0,var_1,var_2,prop_)
}  

#Tabla:

etie5 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(orig_mdz = "Lugar de residencia",
             t0="Turistas",
             prop_ = "Distribución %")  %>%
  fmt_number(columns = c(2), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(#table.font.size = 12,
    data_row.padding = px(3),
    #column_labels.font.size = 14,
    column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(1,2) ~ px(160),
             columns = c(3) ~ px(140)) %>%
  tab_source_note(
    source_note = md("**Nota:** Datos que corresponden al 4to trim. de 2021, en el resto de los trim no se realizó el relevamiento.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>%  
  gtsave("imagenes/eti/etie5.png")

rm(etie5)


#```{r eti6, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Paso Internacional Cristo Redentor. Año {anio_ref}.")}

###Cristo Redentor (dato chequeado)

#Insumo:

etie6 <- eti_e_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Cristo Redentor") %>% 
  group_by(p3_3,orig_cristo) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (orig_cristo=factor(orig_cristo,levels =c("Total","Chile",
                                                   "Resto del mundo"))) %>% 
  arrange(orig_cristo)


if (anio_ref==2021) {
  etie6 <- etie6 %>% select(orig_cristo,t0,prop_)
} else {
  etie6 <- etie6 %>% select(orig_cristo,t0,var_1,var_2,prop_)
}  

#Tabla:


etie6 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(orig_cristo = "Lugar de residencia",
             t0="Turistas",
             prop_ = "Distribución %")  %>%
  fmt_number(columns = c(2), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(data_row.padding = px(3)) %>% 
  cols_width(columns = c(1,2) ~ px(160),
             columns = c(3) ~ px(140)) %>%
  tab_source_note(
    source_note = md("**Nota:** Datos que corresponden al 4to trim. de 2021, en el resto de los trims. no se realizó el relevamiento.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  opt_all_caps() %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>%  
  gtsave("imagenes/eti/etie6.png")


rm(etie6)


#```{r eti7, fig.cap=glue("Distribución de los turistas no residentes según motivo de viaje, por paso. Año {anio_ref}.")}


# Llegadas de turistas no residentes por motivo de viaje (dato chequeado)

#Insumo:

etie7_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo_viaje) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etie7 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo_viaje) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etie7_total) %>% 
  mutate(motivo_viaje=factor(motivo_viaje,levels = c("Visita de familiares o amigos","Vacaciones, ocio o recreación",
                                                     "Negocios, congreso o conferencia","Otros")),
         prop_=round(prop_,3)*100)


rm(etie7_total)

#Gráfico:

ggplot(data=etie7 %>% mutate(motivo_viaje=factor(motivo_viaje,
                                                 levels = c("Visita de familiares o amigos",
                                                            "Vacaciones, ocio o recreación",
                                                            "Negocios, congreso o conferencia",
                                                            "Otros")),
                             paso_final=factor(paso_final,
                                               levels = c("TOTAL",
                                                          "Ezeiza y Aeroparque",
                                                          "Aep. Córdoba",
                                                          "Aep. Mendoza",
                                                          "Puerto de Buenos Aires",
                                                          "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo_viaje,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC)")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal"),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face = "bold"),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etie7)


#```{r eti8, fig.cap=glue("Distribución de los turistas no residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}


# Llegadas de turistas no residentes por tipo alojamiento (dato chequeado)

#Insumo:

etie8_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etie8 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etie8_total) %>% 
  mutate(prop_=round(prop_,3)*100) %>% 
  mutate(alojamiento=case_when(alojamiento=="Casa de familiares o amigos"~"Casa flia/amigos",
                               alojamiento=="Hotel de 1, 2 o 3 estrellas"~"Hotel 1/2/3 est.",
                               alojamiento=="Hotel de 4 y 5 estrellas"~"Hotel 4/5 est.",
                               alojamiento=="Otros"~"Otros"))


rm(etie8_total)

#Gráfico:


ggplot(data=etie8 %>% mutate(alojamiento=factor(alojamiento,
                                                levels = c("Casa flia/amigos",
                                                           "Hotel 1/2/3 est.",
                                                           "Hotel 4/5 est.",
                                                           "Otros")),
                             paso_final=factor(paso_final,
                                               levels = c("TOTAL",
                                                          "Ezeiza y Aeroparque",
                                                          "Aep. Córdoba",
                                                          "Aep. Mendoza",
                                                          "Puerto de Buenos Aires",
                                                          "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=alojamiento,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)
            #,size=6
  ) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face = "bold"),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etie8)


#```{r eti9, fig.cap=glue("Distribución de los turistas no residentes según organización del viaje, por paso. Año {anio_ref}.")}


# Llegadas de turistas no residentes según USO DE PAQUETE

#Insumo:

etie9_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etie9 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etie9_total) %>% 
  pivot_wider(names_from = paso_final,
              values_from = prop_) %>%
  mutate(paq=case_when(paq==0~"No utilizó paquete",
                       paq==1~"Utilizó paquete")) %>% 
  select(paq,`TOTAL`,`Ezeiza y Aeroparque`,`Aep. Córdoba`,`Puerto de Buenos Aires`,
         `Aep. Mendoza`,`Cristo Redentor`) %>% 
  #mutate_all(~replace(., is.na(.), 0)) %>% 
  pivot_longer(cols=c(2:7),
               names_to = "paso_final",
               values_to="prop_") %>% 
  arrange(paso_final) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etie9_total)


#Gráfico:

ggplot(data=etie9 %>% mutate(paq=factor(paq,
                                        levels = c("No utilizó paquete",
                                                   "Utilizó paquete")),
                             paso_final=factor(paso_final,
                                               levels = c("TOTAL",
                                                          "Ezeiza y Aeroparque",
                                                          "Aep. Córdoba",
                                                          "Aep. Mendoza",
                                                          "Puerto de Buenos Aires",
                                                          "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=paq,
           label=glue("{prop_} %"),group=paq))+
  geom_col()+ 
  geom_label(fill="white",size=2.5,position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face = "bold"),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etie9)


#```{r eti10, fig.cap=glue("Llegadas, gasto total, gasto por viajero, pernoctaciones y estadÍa media de turistas no residentes, según paso. Año {anio_ref}.")}


#Cuadro 223, Llegadas, gasto total, gasto por viajero, pernoctaciones y estadÍa media de turistas no residentes

#Insumo:

etie10_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref | p3_3==anio_ref-1 | p3_3==anio_ref-2) %>%
  group_by(p3_3) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE)) %>% 
  mutate(paso_final="TOTAL")



etie10 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref | p3_3==anio_ref-1 | p3_3==anio_ref-2) %>%
  group_by(p3_3,paso_final) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE)) %>% 
  rbind(etie10_total) %>% 
  arrange(p3_3, paso_final)%>%
  mutate(estadia=pernoctes/viajeros,
         gasto_tur=gasto/viajeros,
         gasto_diario=gasto/pernoctes) %>%
  ungroup() %>% 
  mutate(var_1=round((viajeros/lag(viajeros,6)-1),3),
         var_2=round((viajeros/lag(viajeros,12)-1),3),
         var_3=round((pernoctes/lag(pernoctes,6)-1),3),
         var_4=round((pernoctes/lag(pernoctes,12)-1),3),
         var_5=round((gasto/lag(gasto,6)-1),3),
         var_6=round((gasto/lag(gasto,12)-1),3),
         var_7=round((estadia/lag(estadia,6)-1),3),
         var_8=round((estadia/lag(estadia,12)-1),3),
         var_9=round((gasto_tur/lag(gasto_tur,6)-1),3),
         var_10=round((gasto_tur/lag(gasto_tur,12)-1),3),
         var_11=round((gasto_diario/lag(gasto_diario,6)-1),3),
         var_12=round((gasto_diario/lag(gasto_diario,12)-1),3)) %>%
  filter(p3_3==anio_ref) %>% 
  select(paso_final,viajeros,var_1,var_2,pernoctes,var_3,var_4,gasto,var_5,var_6,estadia,var_7,var_8,
         gasto_tur,var_9,var_10,gasto_diario,var_11,var_12)%>%
  mutate(paso_final=factor(paso_final,levels = c("TOTAL","Ezeiza y Aeroparque","Aep. Córdoba",
                                                 "Puerto de Buenos Aires","Aep. Mendoza",
                                                 "Cristo Redentor"))) %>% 
  arrange(paso_final)


#filtros para los años 2020:2022.


if (anio_ref==2021) {
  etie10 <- etie10 %>% select(1:2,5,8,11,14,17)
} else {
  etie10 <- etie10 %>% select(1:19)
}  


rm(etie10_total)

#Tabla:

etie10 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(paso_final = "Paso",
             viajeros="Turistas",
             pernoctes="Pernoctaciones",
             gasto="Gasto total",
             estadia="Estadía promedio (en noches)",
             gasto_tur="Gasto por turista (en USD)",
             gasto_diario="Gasto promedio diario (en USD)")  %>%
  fmt_number(columns = c(2:3), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_number(columns = c(4), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  fmt_number(columns = c(5:6), decimals = 1, dec_mark = ",", sep_mark = ".") %>% 
  tab_options(data_row.padding = px(3)) %>% 
  cols_width(columns = c(1) ~ px(160),
             columns = c(2:7) ~ px(120)) %>%
  tab_source_note(
    source_note = md("**Nota:** En el año 2021 se relevaron el trim 1 y 4 en Ezeiza y Aeroparque; y el trim 4 en el resto de los pasos.")) %>%
  tab_source_note(
    source_note = md("**Fuente:** Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = 1)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))


rm(etie10)

# Gráfico 2.2.2 Gasto total Gasto total de turistas no residentes según motivo del viaje, tipo de alojamiento utilizado y organización del viaje y variación interanual. Aerop. de Ezeiza, Aeroparque J. Newbery, Aerop. de Córdoba y Puerto de Bs. As. 


#```{r eti11, fig.cap=glue("Distribución del gasto total de turistas no residentes según motivo de viaje, por paso. Año {anio_ref}.")}

# Gasto total de no residentes por motivo de viaje

#Insumo

etie11_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo_viaje) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etie11 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo_viaje) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etie11_total) %>% 
  mutate(prop_=(round(prop_,3))*100)

rm(etie11_total)


#Gráfico en ggplot


ggplot(data=etie11 %>% mutate(motivo_viaje=factor(motivo_viaje,
                                                  levels = c("Visita de familiares o amigos",
                                                             "Vacaciones, ocio o recreación",
                                                             "Negocios, congreso o conferencia",
                                                             "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo_viaje,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)
            #,size=6
  ) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(text = element_text(family = "Encode Sans Normal"),
        plot.caption  = element_markdown(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "top")

rm(etie11)


#```{r eti12, fig.cap=glue("Distribución del gasto total de los turistas no residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes por tipo alojamiento 

#Insumo:

etie12_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etie12 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etie12_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etie12_total)

#Gráfico:

ggplot(data=etie12 %>% mutate(alojamiento=factor(alojamiento,
                                                 levels = c("Casa de familiares o amigos",
                                                            "Hotel de 4 y 5 estrellas",
                                                            "Hotel de 1, 2 o 3 estrellas",
                                                            "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=alojamiento,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(text = element_text(family = "Encode Sans Normal"),
        plot.caption  = element_markdown(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "top")

rm(etie12)


#```{r eti13, fig.cap=glue("Distribución del gasto total de los turistas no residentes según utilización de paquete turístico, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes según uso de paquete

#Insumo:

etie13_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etie13 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etie13_total) %>%
  mutate(paq=case_when(paq==0~"No utilizó paquete",
                       paq==1~"Utilizó paquete"))%>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etie13_total)

#Gráfico:

ggplot(data=etie13 %>% mutate(paq=factor(paq,
                                         levels = c("No utilizó paquete",
                                                    "Utilizó paquete")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=paq,
           label=glue("{prop_} %"),group=paq))+
  geom_col()+ 
  geom_label(fill="white",size=2.5,position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face = "bold"),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etie13)


#```{r eti14, fig.cap=glue("Distribución de los pernoctes no residentes según motivo de viaje, por paso. Año {anio_ref}.")}

#Pernoctaciones por alojamiento

#Insumo

etie14_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo_viaje) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etie14 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo_viaje) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etie14_total) %>% 
  mutate(prop_=(round(prop_,3))*100)

rm(etie14_total)


#Gráfico:

ggplot(data=etie14 %>% mutate(motivo_viaje=factor(motivo_viaje,
                                                  levels = c("Visita de familiares o amigos",
                                                             "Vacaciones, ocio o recreación",
                                                             "Negocios, congreso o conferencia",
                                                             "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo_viaje,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)
            #,size=6
  ) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(text = element_text(family = "Encode Sans Normal"),
        plot.caption  = element_markdown(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "top")

rm(etie14)


#```{r eti15, fig.cap=glue("Distribución de los pernoctes no residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes por tipo alojamiento 

#Insumo:

etie15_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etie15 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etie15_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etie15_total)

#Gráfico:

ggplot(data=etie15 %>% mutate(alojamiento=factor(alojamiento,
                                                 levels = c("Casa de familiares o amigos",
                                                            "Hotel de 4 y 5 estrellas",
                                                            "Hotel de 1, 2 o 3 estrellas",
                                                            "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=alojamiento,
           label=glue("{prop_} %")))+
  geom_col()+ 
  geom_text(position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(text = element_text(family = "Encode Sans Normal"),
        plot.caption  = element_markdown(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        legend.position = "top")

rm(etie15)


#```{r eti16, fig.cap=glue("Distribución de los pernoctes no residentes según utilización de paquete turístico, por paso. Año {anio_ref}.")}


#  Gasto total de no residentes según uso de paquete

#Insumo:

etie16_total <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etie16 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etie16_total) %>%
  mutate(paq=case_when(paq==0~"No utilizó paquete",
                       paq==1~"Utilizó paquete"))%>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etie16_total)

#Gráfico:

ggplot(data=etie16 %>% mutate(paq=factor(paq,
                                         levels = c("No utilizó paquete",
                                                    "Utilizó paquete")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=paq,
           label=glue("{prop_} %"),group=paq))+
  geom_col()+ 
  geom_label(fill="white",size=2.5,position = position_stack(vjust = .5)) +
  scale_fill_dnmye(reverse = TRUE)+
  scale_y_continuous(labels = function(x) paste0(x,"%"))+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face = "bold"),
    legend.title = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etie16)


# Cuadro 224 a 226, ya está armado con las tablas eti 10

#########################ACTIVIDADES REALIZADAS#############################


#```{r eti17, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Total de pasos relevados. Año {anio_ref}.")}


###TOTAL: sin diferenciar por paso

#Insumo:

etie17a <- eti_e_perfiles %>%
  filter(p3_3==anio_ref) %>%  
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso))%>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden)


ranking <- (etie17a$actividad)


#Gráfico:

ggplot(data=etie17a %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  #theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie17a)


# Gráfico 223 Actividades realizadas por turistas no residentes por origen, Distribución porcentual. Aerop. de Ezeiza, Aeroparque J. Newbery, Aerop de Córdoba y Puerto de Bs


#```{r eti18, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia.Ezeiza y Aeroparque, Año {anio_ref}.")}


#Ezeiza y aeroparque (TOTAL)

#Insumo:

etie18 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Ezeiza y Aeroparque") %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso))%>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden)


ranking <- (etie18$actividad)


#Gráfico

ggplot(data=etie18 %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie18)



#```{r eti19, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Ezeiza y Aeroparque, Año {anio_ref}.")}


#Ezeiza y aeroparque por origen

#Insumo:

etie19 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Ezeiza y Aeroparque") %>%
  group_by(orig_eya) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(1,17:30) %>% 
  pivot_longer(cols=c(2:15),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades"))

#Gráfico:

ggplot(data=etie19 %>% mutate (orig_eya=factor(orig_eya,levels =c("Bolivia",
                                                                  "Brasil","Chile",
                                                                  "Paraguay","Uruguay",
                                                                  "EE.UU y Canadá","Resto América",
                                                                  "Europa","Resto del Mundo")),
                               actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~orig_eya)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie19,ranking)


#```{r eti20, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia.Aeropuerto Internacional de Córdoba, Año {anio_ref}.")}

###Aeropuerto de Córdoba (TOTAL)

#Dato chequeado

etie20 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Córdoba") %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso)) %>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden) 

ranking <- (etie20$actividad)


#Gráfico:


ggplot(data=etie20 %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie20)


#```{r eti21, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Aeropuerto Internacional de Córdoba, Año {anio_ref}.")}

#CÓRDOBA desagregado por origen

etie21 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Córdoba") %>%
  group_by(orig_cor) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(1,17:30) %>% 
  pivot_longer(cols=c(2:15),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades"))

#Gráfico en ggplot:


ggplot(data=etie21 %>% mutate (orig_cor=factor(orig_cor,levels =c("Brasil","Chile",
                                                                  "EE.UU, Canadá y México","Resto de América",
                                                                  "Europa y resto del mundo")),
                               actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~orig_cor)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie21,ranking)

#```{r eti22, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Aeropuerto Internacional de Mendoza, Año {anio_ref}.")}

###MENDOZA

#TOTAL

#Insumo:

etie22 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Mendoza") %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso)) %>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden)

ranking <- (etie22$actividad)

#Gráfico:


ggplot(data=etie22 %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie22)


#```{r eti23, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Aeropuerto Internacional de Mendoza, Año {anio_ref}.")}

#MENDOZA POR ORIGEN


etie23 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Mendoza") %>%
  group_by(orig_mdz) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(1,17:30) %>% 
  pivot_longer(cols=c(2:15),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades"))



#Gráfico en ggplot:

ggplot(data=etie23 %>% mutate (orig_mdz=factor(orig_mdz,levels =c("Brasil",
                                                                  "Chile","EE.UU, Canadá",
                                                                  "Resto de América",
                                                                  "Europa y resto del mundo")),
                               actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~orig_mdz)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie23,ranking)


#```{r eti24, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia.Puerto de Buenos Aires, Año {anio_ref}.")}

#TOTAL

###PUERTO

#Insumo:

etie24 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Puerto de Buenos Aires") %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso))%>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden)

ranking <- (etie24$actividad)


#Gráfico:

ggplot(data=etie24 %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_text(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie24)


#```{r eti25, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Puerto de Buenos Aires, Año {anio_ref}.")}


#PUERTO POR ORIGEN

#Insumo.

etie25 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Puerto de Buenos Aires") %>%
  group_by(orig_puerto) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(1,17:30) %>% 
  pivot_longer(cols=c(2:15),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades"))


#Gráfico en ggplot:

ggplot(data=etie25 %>% mutate (orig_puerto=factor(orig_puerto,levels =c("Uruguay",
                                                                        "Resto del Mundo")),
                               actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~orig_puerto)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_text(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie25,ranking)


#```{r eti26, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Paso Internacional Cristo Redentor, Año {anio_ref}.")}


###CRISTO

#TOTAL

#Insumo:

etie26 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Cristo Redentor") %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(16:29) %>% 
  pivot_longer(cols=c(1:14),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades")) %>% 
  arrange((peso)) %>% 
  mutate(orden=row_number()) %>% 
  mutate(orden=as.numeric(orden)) %>% 
  mutate(orden=case_when(actividad=="Otras actividades"~0,
                         TRUE~orden)) %>% 
  arrange(orden) %>% 
  select(-orden)

ranking <- (etie26$actividad)

#Gráfico:

ggplot(data=etie26 %>% mutate(actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=20),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie26)


#```{r eti27, fig.cap=glue("Peso porcentual de las actividades realizadas según lugar de residencia. Puerto de Buenos Aires,Año {anio_ref}.")}

#CRISTO POR ORIGEN

#Insumo:

etie27 <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Cristo Redentor") %>%
  group_by(orig_cristo) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            aventura=sum(p39_1r*wpf,na.rm = TRUE),
            gastro=sum(p39_4r*wpf,na.rm = TRUE),
            idioma=sum(p39_7r*wpf,na.rm = TRUE),
            tmedicos=sum(p39_9r*wpf,na.rm = TRUE),
            tango=sum(p39_10r*wpf,na.rm = TRUE),
            urbanos=sum(p39_13r*wpf,na.rm = TRUE),
            compras=sum(p39_14r*wpf,na.rm = TRUE),
            edeportivos=sum(p39_15r*wpf,na.rm = TRUE),
            actnoct=sum(p39_16r*wpf,na.rm = TRUE),
            bajadif=sum(p39_17r*wpf,na.rm = TRUE),
            parquesnac=sum(p39_18r*wpf,na.rm = TRUE),
            nieve=sum(p39_19r*wpf,na.rm = TRUE),
            vino=sum(p39_20r*wpf,na.rm = TRUE),
            otras=sum(p39_111r*wpf,na.rm = TRUE)) %>% 
  mutate(paventura=aventura/viajeros,
         pgastro=gastro/viajeros,
         pidioma=idioma/viajeros,
         ptmedicos=tmedicos/viajeros,
         ptango=tango/viajeros,
         purbanos=urbanos/viajeros,
         pcompras=compras/viajeros,
         pedeportivos=edeportivos/viajeros,
         pactnoct=actnoct/viajeros,
         pbajadif=bajadif/viajeros,
         pparquesnac=parquesnac/viajeros,
         pnieve=nieve/viajeros,
         pvino=vino/viajeros,
         potras=otras/viajeros) %>% 
  select(1,17:30) %>% 
  pivot_longer(cols=c(2:15),
               names_to = "actividad",
               values_to="peso") %>% 
  mutate(peso=(round(peso,3))*100) %>% 
  mutate(actividad=case_when(actividad=="paventura"~"Tur. Aventura",
                             actividad=="pgastro"~"Act. Gastronómicas",
                             actividad=="pidioma"~"Aprendizaje español",
                             actividad=="ptmedicos"~"Trat. médicos",
                             actividad=="ptango"~"Tango",
                             actividad=="purbanos"~"Acts. lugares urbanos",
                             actividad=="pcompras"~"Compras",
                             actividad=="pedeportivos"~"Eventos deportivos /culturales",
                             actividad=="pactnoct"~"Actividades nocturnas",
                             actividad=="pbajadif"~"Act. baja dificultad en naturaleza",
                             actividad=="pparquesnac"~"Visita parques nacionales",
                             actividad=="pnieve"~"Acts. en la nieve",
                             actividad=="pvino"~"Acts. vinculadas al vino",
                             actividad=="potras"~"Otras actividades"))



#Gráfico:

ggplot(data=etie27 %>% mutate (orig_cristo=factor(orig_cristo,levels =c("Chile",
                                                                        "Resto del mundo")),
                               actividad=factor(actividad,levels = ranking)),
       aes(x=peso,y=actividad,fill=actividad,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~orig_cristo)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuenet: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie27,ranking)


#Cuadro 227 y 228: no hacer


#################CALIFICACIÓN####################


#```{r eti28, fig.cap=glue("Calificación promedio (sobre escala de 1 a 5) por paso, según rubro.Año {anio_ref}.")}

#Sumar todos las calificaciones de los calificaciones de 1 a 5.

tinterno <- eti_e_perfiles %>% 
  filter((p40_1>=1 & p40_1<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_1*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(tinterno=calif/nbase) %>% 
  select(1,4)

alojamiento <- eti_e_perfiles %>% 
  filter((p40_2>=1 & p40_2<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_2*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(alojamiento=calif/nbase) %>% 
  select(1,4)

gastro <- eti_e_perfiles %>% 
  filter((p40_3>=1 & p40_3<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_3*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(gastro=calif/nbase) %>% 
  select(1,4)

info <- eti_e_perfiles %>% 
  filter((p40_4>=1 & p40_4<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_4*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(info=calif/nbase) %>% 
  select(1,4)

higiene <- eti_e_perfiles %>% 
  filter((p40_5>=1 & p40_5<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_5*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(higiene=calif/nbase) %>% 
  select(1,4)

seguridad <- eti_e_perfiles %>% 
  filter((p40_6>=1 & p40_6<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_6*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(seguridad=calif/nbase) %>% 
  select(1,4)

estadia <- eti_e_perfiles %>% 
  filter((p40_7>=1 & p40_7<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_7*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(estadia=calif/nbase) %>% 
  select(1,4)

inout <- eti_e_perfiles %>% 
  filter((p40_8>=1 & p40_8<=5) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(calif=sum(p40_8*wpf,na.rm = TRUE),
            nbase=sum(viajeros*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(inout=calif/nbase) %>% 
  select(1,4)

calif <- left_join(tinterno,alojamiento)
calif <- left_join(calif,gastro)
calif <- left_join(calif,info)
calif <- left_join(calif,higiene)
calif <- left_join(calif,seguridad)
calif <- left_join(calif,estadia)
calif <- left_join(calif,inout)

etie28 <- calif %>% 
  pivot_longer(cols=c(2:9),
               names_to = "rubro",
               values_to="valor") %>% 
  mutate(rubro=case_when(rubro=="tinterno"~"Transporte interno",
                         rubro=="alojamiento"~"Alojamiento",
                         rubro=="gastro"~"Gastronomía",
                         rubro=="info"~"Información turística",
                         rubro=="higiene"~"Higiene",
                         rubro=="seguridad"~"Seguridad",
                         rubro=="estadia"~"Estadía en general",
                         rubro=="inout"~"In out del aeropuerto"),
         valor=round(valor,1)) 

para_ranking  <- etie28 %>% 
  filter(paso_final=="Total") %>% 
  arrange(valor)

ranking <- para_ranking$rubro

rm(tinterno,alojamiento,gastro,info,higiene,seguridad,estadia,inout,calif)


#Gráfico:

ggplot(data=etie28 %>% mutate (rubro=factor(rubro,levels = ranking),
                               paso_final=factor(paso_final,
                                                 levels = c("Total",
                                                            "Ezeiza y Aeroparque",
                                                            "Aep. Córdoba",
                                                            "Aep. Mendoza",
                                                            "Puerto de Buenos Aires",
                                                            "Cristo Redentor"))),
       aes(x=valor,y=rubro,fill=rubro,
           label=valor))+
  geom_col()+ 
  facet_wrap(~paso_final)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie28,ranking, para_ranking)


#################ASPECTOS INFLUYENTES####################

#Cuadro 2210, no se hace

# Gráfico 2.2.5 Aspectos que influyeron en la decisión de viaje de turistas no residentes hacia la Argentina según lugar de residencia habitual. 


#```{r eti29, fig.cap=glue("Peso porcentual de los aspectos que influyeron a la hora de elegir la Argentina. Año {anio_ref}.")}

#Insumo:

etie29 <- eti_e_perfiles %>% 
  filter(p17_1 %in% c(1,5,6,9) & p3_3==anio_ref) %>%
  group_by(paso_final) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pcio_cal=sum(p41a_1r*wpf,na.rm = TRUE),
            naturaleza=sum(p41a_2r*wpf,na.rm = TRUE),
            cultura=sum(p41a_3r*wpf,na.rm = TRUE),
            proximidad=sum(p41a_4r*wpf,na.rm = TRUE),
            gfriend=sum(p41a_5r*wpf,na.rm = TRUE),
            act_esp=sum(p41a_6r*wpf,na.rm = TRUE),
            otros=sum(p41a_71r*wpf,na.rm = TRUE),
            amigos=sum(p41a_8r*wpf,na.rm = TRUE),
            iva=sum(p41a_9r*wpf,na.rm = TRUE)) %>%
  ungroup() %>% 
  adorn_totals() %>% 
  mutate(pcio_cal=pcio_cal/viajeros,
         naturaleza=naturaleza/viajeros,
         cultura=cultura/viajeros,
         proximidad=proximidad/viajeros,
         gfriend=gfriend/viajeros,
         act_esp=act_esp/viajeros,
         otros=otros/viajeros,
         amigos=amigos/viajeros,
         iva=iva/viajeros) %>% 
  select(-viajeros) %>% 
  pivot_longer(cols=c(2:10),
               names_to = "aspecto",
               values_to="valor") %>% 
  mutate(aspecto=case_when(aspecto=="pcio_cal"~"Relación precio/calidad",
                           aspecto=="naturaleza"~"Diversidad naturaleza",
                           aspecto=="cultura"~"Valores culturales",
                           aspecto=="proximidad"~"Proximidad",
                           aspecto=="gfriend"~"País gay friendly",
                           aspecto=="act_esp"~"Actividades específicas del país",
                           aspecto=="otros"~"Otros",
                           aspecto=="amigos"~"Presencia amigos/flia",
                           aspecto=="iva"~"Reintegro del iva"),
         valor=(round(valor,3))*100) 


para_ranking  <- etie29 %>% 
  filter(paso_final=="Total") %>% 
  arrange(valor) %>% 
  mutate(secuencia=as.numeric(seq(1:9))) %>% 
  mutate(secuencia=case_when(aspecto=="Otros"~0,
                             TRUE~secuencia)) %>% 
  arrange(secuencia)

ranking <- para_ranking$aspecto

rm(para_ranking)

#Gráfico:

ggplot(data=etie29 %>% mutate (aspecto=factor(aspecto,levels = ranking),
                               paso_final=factor(paso_final,
                                                 levels = c("Total",
                                                            "Ezeiza y Aeroparque",
                                                            "Aep. Córdoba",
                                                            "Aep. Mendoza",
                                                            "Puerto de Buenos Aires",
                                                            "Cristo Redentor"))),
       aes(x=valor,y=aspecto,fill=aspecto,
           label=glue("{valor} %")))+
  geom_col()+ 
  facet_wrap(~paso_final)+
  geom_text(hjust = "inward") +
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  scale_fill_dnmye(reverse = TRUE)+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_text(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      size = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x=element_blank(),
    panel.background = element_blank())

rm(etie29,ranking)


#VER SI VALE LA PENA DESAGREGAR POR NACIÓN


##################DESTINOS VISITADOS POR PASO Y MERCADOS###############

#PERNOCTES POR DESTINOS VISITADOS

#```{r eti30, fig.cap=glue("Distribución de los pernoctes y turistas según destinos visitados.Ezeiza y Aeroparque, Año {anio_ref}.")}

###EZEIZA Y AEROPARQUE:

#Insumo:

etie30_p <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Ezeiza y Aeroparque") %>% 
  summarise(pern_caba=sum(noch1*wpf,na.rm = TRUE),
            pern_catl=sum(noch2*wpf,na.rm = TRUE),
            pern_pmadryn=sum(noch3*wpf,na.rm = TRUE),
            pern_ciguazu=sum(noch4*wpf,na.rm = TRUE),
            pern_pcord=sum(noch5*wpf,na.rm = TRUE),
            pern_psaltucjuj=sum(noch6*wpf,na.rm = TRUE),
            pern_pmza=sum(noch7*wpf,na.rm = TRUE),
            pern_ush=sum(noch8*wpf,na.rm = TRUE),
            pern_calafate=sum(noch9*wpf,na.rm = TRUE),
            pern_barvasm=sum(noch10*wpf,na.rm = TRUE),
            pern_rosario=sum(noch11*wpf,na.rm = TRUE),
            pern_antar=sum(noch12*wpf,na.rm = TRUE),
            pern_restopba=sum(noch13*wpf,na.rm = TRUE),
            pern_restonor=sum(noch14*wpf,na.rm = TRUE),
            pern_restocuyo=sum(noch15_sin_mza*wpf,na.rm = TRUE),
            pern_restopata=sum(noch16_sin_mza*wpf,na.rm = TRUE),
            pern_restolito=sum(noch17*wpf,na.rm = TRUE),
            total=sum(totaln*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:17),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="pern_caba"~"CABA",
                           destino=="pern_catl"~"Costa Atlántica",
                           destino=="pern_pmadryn"~"Puerto Madryn",
                           destino=="pern_ciguazu"~"Cataratas Iguazú",
                           destino=="pern_pcord"~"Pcia. Córdoba",
                           destino=="pern_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="pern_pmza"~"Pcia. Mendoza",
                           destino=="pern_ush"~"Ushuaia",
                           destino=="pern_calafate"~"Calafate",
                           destino=="pern_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="pern_rosario"~"Rosario",
                           destino=="pern_antar"~"Antártida",
                           destino=="pern_restopba"~"Resto PBA.",
                           destino=="pern_restonor"~"Resto Norte",
                           destino=="pern_restocuyo"~"Resto Cuyo",
                           destino=="pern_restopata"~"Resto Patagonia",
                           destino=="pern_restolito"~"Resto Litoral")) %>% 
  mutate(variable="Pernoctes") %>% 
  arrange(peso)


orden <- (etie30_p$destino)



etie30_v <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Ezeiza y Aeroparque") %>% 
  summarise(viaj_caba=sum(p21_11*wpf,na.rm = TRUE),
            viaj_catl=sum(p21_21*wpf,na.rm = TRUE),
            viaj_pmadryn=sum(p21_31*wpf,na.rm = TRUE),
            viaj_ciguazu=sum(p21_41*wpf,na.rm = TRUE),
            viaj_pcord=sum(p21_51*wpf,na.rm = TRUE),
            viaj_psaltucjuj=sum(p21_61*wpf,na.rm = TRUE),
            viaj_pmza=sum(p21_71*wpf,na.rm = TRUE),
            viaj_ush=sum(p21_81*wpf,na.rm = TRUE),
            viaj_calafate=sum(p21_91*wpf,na.rm = TRUE),
            viaj_barvasm=sum(p21_101*wpf,na.rm = TRUE),
            viaj_rosario=sum(p21_111*wpf,na.rm = TRUE),
            viaj_antar=sum(p21_121*wpf,na.rm = TRUE),
            viaj_restopba=sum(p21_131*wpf,na.rm = TRUE),
            viaj_restonor=sum(p21_141*wpf,na.rm = TRUE),
            viaj_restocuyo=sum(p21_151_sin_mza*wpf,na.rm = TRUE),
            viaj_restopata=sum(p21_161_sin_mza*wpf,na.rm = TRUE),
            viaj_restolito=sum(p21_171*wpf,na.rm = TRUE),
            total=sum(viajeros*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:17),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="viaj_caba"~"CABA",
                           destino=="viaj_catl"~"Costa Atlántica",
                           destino=="viaj_pmadryn"~"Puerto Madryn",
                           destino=="viaj_ciguazu"~"Cataratas Iguazú",
                           destino=="viaj_pcord"~"Pcia. Córdoba",
                           destino=="viaj_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="viaj_pmza"~"Pcia. Mendoza",
                           destino=="viaj_ush"~"Ushuaia",
                           destino=="viaj_calafate"~"Calafate",
                           destino=="viaj_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="viaj_rosario"~"Rosario",
                           destino=="viaj_antar"~"Antártida",
                           destino=="viaj_restopba"~"Resto PBA.",
                           destino=="viaj_restonor"~"Resto Norte",
                           destino=="viaj_restocuyo"~"Resto Cuyo",
                           destino=="viaj_restopata"~"Resto Patagonia",
                           destino=="viaj_restolito"~"Resto Litoral")) %>% 
  mutate(variable="Turistas")


etie30 <- etie30_p %>% 
  rbind(etie30_v)

#Gráfico:

ggplot(data=etie30 %>%mutate(destino = factor(destino,levels=orden)),
       aes(x=peso,y=destino,fill=destino,
           label=glue("{peso} %")))+
  facet_wrap(~variable)+
  geom_col()+ 
  geom_text(hjust = "inward") +
  scale_fill_dnmye(reverse = TRUE)+
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank())

rm(etie30,etie30_p,etie30_v)

#```{r eti31, fig.cap=glue("Distribución de los pernoctes y turistas según destinos visitados. Puerto de Buenos Aires Año {anio_ref}.")}

###PUERTO:

#Insumos:

etie31_p <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Puerto de Buenos Aires") %>% 
  summarise(pern_caba=sum(noch1*wpf,na.rm = TRUE),
            pern_catl=sum(noch2*wpf,na.rm = TRUE),
            pern_pmadryn=sum(noch3*wpf,na.rm = TRUE),
            pern_ciguazu=sum(noch4*wpf,na.rm = TRUE),
            pern_pcord=sum(noch5*wpf,na.rm = TRUE),
            pern_psaltucjuj=sum(noch6*wpf,na.rm = TRUE),
            pern_pmza=sum(noch7*wpf,na.rm = TRUE),
            pern_ush=sum(noch8*wpf,na.rm = TRUE),
            pern_calafate=sum(noch9*wpf,na.rm = TRUE),
            pern_barvasm=sum(noch10*wpf,na.rm = TRUE),
            pern_rosario=sum(noch11*wpf,na.rm = TRUE),
            pern_antar=sum(noch12*wpf,na.rm = TRUE),
            pern_restopba=sum(noch13*wpf,na.rm = TRUE),
            pern_restonor=sum(noch14*wpf,na.rm = TRUE),
            pern_restocuyo=sum(noch15_sin_mza*wpf,na.rm = TRUE),
            pern_restopata=sum(noch16_sin_mza*wpf,na.rm = TRUE),
            pern_restolito=sum(noch17*wpf,na.rm = TRUE),
            total=sum(totaln*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:17),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="pern_caba"~"CABA",
                           destino=="pern_catl"~"Costa Atlántica",
                           destino=="pern_pmadryn"~"Puerto Madryn",
                           destino=="pern_ciguazu"~"Cataratas Iguazú",
                           destino=="pern_pcord"~"Pcia. Córdoba",
                           destino=="pern_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="pern_pmza"~"Pcia. Mendoza",
                           destino=="pern_ush"~"Ushuaia",
                           destino=="pern_calafate"~"Calafate",
                           destino=="pern_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="pern_rosario"~"Rosario",
                           destino=="pern_antar"~"Antártida",
                           destino=="pern_restopba"~"Resto PBA.",
                           destino=="pern_restonor"~"Resto Norte",
                           destino=="pern_restocuyo"~"Resto Cuyo",
                           destino=="pern_restopata"~"Resto Patagonia",
                           destino=="pern_restolito"~"Resto Litoral")) %>% 
  mutate(variable="Pernoctes") %>% 
  arrange(peso)


orden <- (etie31_p$destino)


etie31_v <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Puerto de Buenos Aires") %>% 
  summarise(viaj_caba=sum(p21_11*wpf,na.rm = TRUE),
            viaj_catl=sum(p21_21*wpf,na.rm = TRUE),
            viaj_pmadryn=sum(p21_31*wpf,na.rm = TRUE),
            viaj_ciguazu=sum(p21_41*wpf,na.rm = TRUE),
            viaj_pcord=sum(p21_51*wpf,na.rm = TRUE),
            viaj_psaltucjuj=sum(p21_61*wpf,na.rm = TRUE),
            viaj_pmza=sum(p21_71*wpf,na.rm = TRUE),
            viaj_ush=sum(p21_81*wpf,na.rm = TRUE),
            viaj_calafate=sum(p21_91*wpf,na.rm = TRUE),
            viaj_barvasm=sum(p21_101*wpf,na.rm = TRUE),
            viaj_rosario=sum(p21_111*wpf,na.rm = TRUE),
            viaj_antar=sum(p21_121*wpf,na.rm = TRUE),
            viaj_restopba=sum(p21_131*wpf,na.rm = TRUE),
            viaj_restonor=sum(p21_141*wpf,na.rm = TRUE),
            viaj_restocuyo=sum(p21_151_sin_mza*wpf,na.rm = TRUE),
            viaj_restopata=sum(p21_161_sin_mza*wpf,na.rm = TRUE),
            viaj_restolito=sum(p21_171*wpf,na.rm = TRUE),
            total=sum(viajeros*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:17),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="viaj_caba"~"CABA",
                           destino=="viaj_catl"~"Costa Atlántica",
                           destino=="viaj_pmadryn"~"Puerto Madryn",
                           destino=="viaj_ciguazu"~"Cataratas Iguazú",
                           destino=="viaj_pcord"~"Pcia. Córdoba",
                           destino=="viaj_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="viaj_pmza"~"Pcia. Mendoza",
                           destino=="viaj_ush"~"Ushuaia",
                           destino=="viaj_calafate"~"Calafate",
                           destino=="viaj_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="viaj_rosario"~"Rosario",
                           destino=="viaj_antar"~"Antártida",
                           destino=="viaj_restopba"~"Resto PBA.",
                           destino=="viaj_restonor"~"Resto Norte",
                           destino=="viaj_restocuyo"~"Resto Cuyo",
                           destino=="viaj_restopata"~"Resto Patagonia",
                           destino=="viaj_restolito"~"Resto Litoral")) %>% 
  mutate(variable="Turistas")


etie31 <- etie31_p %>% 
  rbind(etie31_v)


#Gráfico:


ggplot(data=etie31 %>%mutate(destino = factor(destino,levels=orden)),
       aes(x=peso,y=destino,fill=destino,
           label=glue("{peso} %")))+
  facet_wrap(~variable)+
  geom_col()+ 
  geom_text(hjust = "inward") +
  scale_fill_dnmye(reverse = TRUE)+
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank())

rm(etie31,etie31_p,etie31_v)



#```{r eti32, fig.cap=glue("Distribución de los pernoctes y turistas según destinos visitados. Aeropuerto Internacional de Córdoba, Año {anio_ref}.")}

#AEROPUERTO DE CÓRDOBA:

#Insumos:

#Pernoctes

etie32_p <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Córdoba") %>% 
  summarise(pern_caba=sum(noch1*wpf,na.rm = TRUE),
            pern_psaltucjuj=sum(noch6*wpf,na.rm = TRUE),
            pern_antar=sum(noch12*wpf,na.rm = TRUE),
            pern_restonor=sum(noch14*wpf,na.rm = TRUE),
            pern_restocuyo=sum(noch15_sin_mza*wpf,na.rm = TRUE),
            pern_restolito=sum(noch17*wpf,na.rm = TRUE),
            pern_ccord=sum(noch19*wpf,na.rm = TRUE),
            pern_trasla=sum(noch20*wpf,na.rm = TRUE),
            pern_punilla=sum(noch21*wpf,na.rm = TRUE),
            pern_calamu=sum(noch22*wpf,na.rm = TRUE),
            pern_restocord=sum(noch23*wpf,na.rm = TRUE),
            pern_pba=sum(noch24*wpf,na.rm = TRUE),
            pern_patagonia=sum(noch28*wpf,na.rm = TRUE),
            pern_rosario=sum(noch11*wpf,na.rm = TRUE),
            pern_mza=sum(noch7*wpf,na.rm = TRUE),
            total=sum(totaln*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:15),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="pern_caba"~"CABA",
                           destino=="pern_psaltucjuj"~"Salta/Tuc./Jujuy",
                           destino=="pern_antar"~"Antártida",
                           destino=="pern_restonor"~"Resto Norte",
                           destino=="pern_restocuyo"~"Resto Cuyo",
                           destino=="pern_restolito"~"Resto Litoral",
                           destino=="pern_ccord"~"Córdoba (ciudad)",
                           destino=="pern_trasla"~"Traslasierra",
                           destino=="pern_punilla"~"Punilla",
                           destino=="pern_calamu"~"Calamuchita",
                           destino=="pern_restocord"~"Resto Pcia. Córdoba",
                           destino=="pern_pba"~"PBA",
                           destino=="pern_patagonia"~"Patagonia",
                           destino=="pern_rosario"~"Rosario",
                           destino=="pern_mza"~"Mendoza")) %>% 
  arrange(peso) %>% 
  mutate(variable="Pernoctes")


orden <- (etie32_p$destino)

#Viajeros

etie32_v <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Córdoba") %>% 
  summarise(viaj_caba=sum(p21_11*wpf,na.rm = TRUE),
            viaj_psaltucjuj=sum(p21_61*wpf,na.rm = TRUE),
            viaj_antar=sum(p21_121*wpf,na.rm = TRUE),
            viaj_restonor=sum(p21_141*wpf,na.rm = TRUE),
            viaj_restocuyo=sum(p21_151_sin_mza*wpf,na.rm = TRUE),
            viaj_restolito=sum(p21_171*wpf,na.rm = TRUE),
            viaj_ccord=sum(p21_191*wpf,na.rm = TRUE),
            viaj_trasla=sum(p21_201*wpf,na.rm = TRUE),
            viaj_punilla=sum(p21_211*wpf,na.rm = TRUE),
            viaj_calamu=sum(p21_221*wpf,na.rm = TRUE),
            viaj_restocord=sum(p21_231*wpf,na.rm = TRUE),
            viaj_pba=sum(p21_241*wpf,na.rm = TRUE),
            viaj_patagonia=sum(p21_281*wpf,na.rm = TRUE),
            viaj_rosario=sum(p21_111*wpf,na.rm = TRUE),
            viaj_mza=sum(p21_71*wpf,na.rm = TRUE),
            total=sum(viajeros*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:15),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="viaj_caba"~"CABA",
                           destino=="viaj_psaltucjuj"~"Salta/Tuc./Jujuy",
                           destino=="viaj_antar"~"Antártida",
                           destino=="viaj_restonor"~"Resto Norte",
                           destino=="viaj_restocuyo"~"Resto Cuyo",
                           destino=="viaj_restolito"~"Resto Litoral",
                           destino=="viaj_ccord"~"Córdoba (ciudad)",
                           destino=="viaj_trasla"~"Traslasierra",
                           destino=="viaj_punilla"~"Punilla",
                           destino=="viaj_calamu"~"Calamuchita",
                           destino=="viaj_restocord"~"Resto Pcia. Córdoba",
                           destino=="viaj_pba"~"PBA",
                           destino=="viaj_patagonia"~"Patagonia",
                           destino=="viaj_rosario"~"Rosario",
                           destino=="viaj_mza"~"Mendoza")) %>% 
  arrange(peso) %>% 
  mutate(variable="Viajeros")

#BASE TOTAL:

etie32 <- etie32_p %>% 
  rbind(etie32_v)


rm(etie32_p,etie32_v)

#Gráfico:


ggplot(data=etie32 %>%mutate(destino = factor(destino,levels=orden)),
       aes(x=peso,y=destino,fill=destino,
           label=glue("{peso} %")))+
  geom_col()+
  facet_wrap(~variable)+
  geom_text(hjust = "inward") +
  scale_fill_dnmye(reverse = TRUE)+
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_markdown(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank())

rm(etie32)


#```{r eti33, fig.cap=glue("Distribución de los pernoctes y turistas según destinos visitados. Aeropuerto Internacional de Mendoza, Año {anio_ref}.")}

###AEROPUERTO DE MENDOZA:

#Insumos:

#Pernoctes

etie33_p <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Mendoza") %>% 
  summarise(pern_caba=sum(noch1*wpf,na.rm = TRUE),
            pern_ciguazu=sum(noch4*wpf,na.rm = TRUE),
            pern_pcord=sum(noch5*wpf,na.rm = TRUE),
            pern_psaltucjuj=sum(noch6*wpf,na.rm = TRUE),
            pern_calafate=sum(noch9*wpf,na.rm = TRUE),
            pern_barvasm=sum(noch10*wpf,na.rm = TRUE),
            pern_rosario=sum(noch11*wpf,na.rm = TRUE),
            pern_restonor=sum(noch14*wpf,na.rm = TRUE),
            pern_restocuyo=sum(noch15_sin_mza*wpf,na.rm = TRUE),
            pern_restopata=sum(noch16_sin_mza*wpf,na.rm = TRUE),
            pern_restolito=sum(noch17*wpf,na.rm = TRUE),
            pern_cmza=sum(noch30*wpf,na.rm = TRUE),
            pern_srafael=sum(noch31*wpf,na.rm = TRUE),
            pern_malar=sum(noch32*wpf,na.rm = TRUE),
            pern_restopciamza=sum(noch33*wpf,na.rm = TRUE),
            pern_pciasanjuan=sum(noch34*wpf,na.rm = TRUE),
            pern_pciachubut=sum(noch35*wpf,na.rm = TRUE),
            pern_restoneuquen=sum(noch36*wpf,na.rm = TRUE),
            total=sum(totaln*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:18),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="pern_caba"~"CABA",
                           destino=="pern_ciguazu"~"Cataratas Iguazú",
                           destino=="pern_pcord"~"Pcia. Córdoba",
                           destino=="pern_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="pern_calafate"~"Calafate",
                           destino=="pern_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="pern_rosario"~"Rosario",
                           destino=="pern_restopba"~"Resto PBA.",
                           destino=="pern_restonor"~"Resto Norte",
                           destino=="pern_restocuyo"~"Resto Cuyo",
                           destino=="pern_restopata"~"Resto Patagonia",
                           destino=="pern_restolito"~"Resto Litoral",
                           destino=="pern_cmza"~"Mendoza (ciudad)",
                           destino=="pern_srafael"~"San Rafael",
                           destino=="pern_malar"~"Malargue/Leñas",
                           destino=="pern_restopciamza"~"Resto Pcia. Mdza.",
                           destino=="pern_pciasanjuan"~"Pcia. San Juan",
                           destino=="pern_pciachubut"~"Pcia. Chubut",
                           destino=="pern_restoneuquen"~"Resto Neuquén")) %>% 
  arrange(peso) %>% 
  mutate(variable="Pernoctes")


orden <- (etie33_p$destino)

#Viajeros:

etie33_v <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Aep. Mendoza") %>% 
  summarise(viaj_caba=sum(p21_11*wpf,na.rm = TRUE),
            viaj_ciguazu=sum(p21_41*wpf,na.rm = TRUE),
            viaj_pcord=sum(p21_51*wpf,na.rm = TRUE),
            viaj_psaltucjuj=sum(p21_61*wpf,na.rm = TRUE),
            viaj_calafate=sum(p21_91*wpf,na.rm = TRUE),
            viaj_barvasm=sum(p21_101*wpf,na.rm = TRUE),
            viaj_rosario=sum(p21_111*wpf,na.rm = TRUE),
            viaj_restonor=sum(p21_141*wpf,na.rm = TRUE),
            viaj_restocuyo=sum(p21_151_sin_mza*wpf,na.rm = TRUE),
            viaj_restopata=sum(p21_161_sin_mza*wpf,na.rm = TRUE),
            viaj_restolito=sum(p21_171*wpf,na.rm = TRUE),
            viaj_cmza=sum(p21_301*wpf,na.rm = TRUE),
            viaj_srafael=sum(p21_311*wpf,na.rm = TRUE),
            viaj_malar=sum(p21_321*wpf,na.rm = TRUE),
            viaj_restopciamza=sum(p21_331*wpf,na.rm = TRUE),
            viaj_pciasanjuan=sum(p21_341*wpf,na.rm = TRUE),
            viaj_pciachubut=sum(p21_351*wpf,na.rm = TRUE),
            viaj_restoneuquen=sum(p21_361*wpf,na.rm = TRUE),
            total=sum(viajeros*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:18),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="viaj_caba"~"CABA",
                           destino=="viaj_ciguazu"~"Cataratas Iguazú",
                           destino=="viaj_pcord"~"Pcia. Córdoba",
                           destino=="viaj_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="viaj_calafate"~"Calafate",
                           destino=="viaj_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="viaj_rosario"~"Rosario",
                           destino=="viaj_restopba"~"Resto PBA.",
                           destino=="viaj_restonor"~"Resto Norte",
                           destino=="viaj_restocuyo"~"Resto Cuyo",
                           destino=="viaj_restopata"~"Resto Patagonia",
                           destino=="viaj_restolito"~"Resto Litoral",
                           destino=="viaj_cmza"~"Mendoza (ciudad)",
                           destino=="viaj_srafael"~"San Rafael",
                           destino=="viaj_malar"~"Malargue/Leñas",
                           destino=="viaj_restopciamza"~"Resto Pcia. Mdza.",
                           destino=="viaj_pciasanjuan"~"Pcia. San Juan",
                           destino=="viaj_pciachubut"~"Pcia. Chubut",
                           destino=="viaj_restoneuquen"~"Resto Neuquén")) %>% 
  arrange(peso) %>% 
  mutate(variable="Viajeros")

#BASE TOTAL:

etie33 <- etie33_p %>% 
  rbind(etie33_v)


rm(etie33_p,etie33_v)


#Gráfico:

ggplot(data=etie33 %>%mutate(destino = factor(destino,levels=orden)),
       aes(x=peso,y=destino,fill=destino,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~variable)+
  geom_text(hjust = "inward") +
  scale_fill_dnmye(reverse = TRUE)+
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_text(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank())

rm(etie33)


#```{r eti34, fig.cap=glue("Distribución de los pernoctes y turistas según destinos visitados. Paso Internacional Cristo Resdentor, Año {anio_ref}.")}

###CRISTO REDENTOR:

#Insumos:

#Pernoctes

etie34_p <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Cristo Redentor") %>% 
  summarise(pern_caba=sum(noch1*wpf,na.rm = TRUE),
            pern_ciguazu=sum(noch4*wpf,na.rm = TRUE),
            pern_pcord=sum(noch5*wpf,na.rm = TRUE),
            pern_psaltucjuj=sum(noch6*wpf,na.rm = TRUE),
            pern_calafate=sum(noch9*wpf,na.rm = TRUE),
            pern_barvasm=sum(noch10*wpf,na.rm = TRUE),
            pern_rosario=sum(noch11*wpf,na.rm = TRUE),
            pern_restonor=sum(noch14*wpf,na.rm = TRUE),
            pern_restocuyo=sum(noch15_sin_mza*wpf,na.rm = TRUE),
            pern_restopata=sum(noch16_sin_mza*wpf,na.rm = TRUE),
            pern_restolito=sum(noch17*wpf,na.rm = TRUE),
            pern_cmza=sum(noch30*wpf,na.rm = TRUE),
            pern_srafael=sum(noch31*wpf,na.rm = TRUE),
            pern_malar=sum(noch32*wpf,na.rm = TRUE),
            pern_restopciamza=sum(noch33*wpf,na.rm = TRUE),
            pern_pciasanjuan=sum(noch34*wpf,na.rm = TRUE),
            pern_pciachubut=sum(noch35*wpf,na.rm = TRUE),
            pern_restoneuquen=sum(noch36*wpf,na.rm = TRUE),
            total=sum(totaln*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:18),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="pern_caba"~"CABA",
                           destino=="pern_ciguazu"~"Cataratas Iguazú",
                           destino=="pern_pcord"~"Pcia. Córdoba",
                           destino=="pern_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="pern_calafate"~"Calafate",
                           destino=="pern_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="pern_rosario"~"Rosario",
                           destino=="pern_restopba"~"Resto PBA.",
                           destino=="pern_restonor"~"Resto Norte",
                           destino=="pern_restocuyo"~"Resto Cuyo",
                           destino=="pern_restopata"~"Resto Patagonia",
                           destino=="pern_restolito"~"Resto Litoral",
                           destino=="pern_cmza"~"Mendoza (ciudad)",
                           destino=="pern_srafael"~"San Rafael",
                           destino=="pern_malar"~"Malargue/Leñas",
                           destino=="pern_restopciamza"~"Resto Pcia. Mdza.",
                           destino=="pern_pciasanjuan"~"Pcia. San Juan",
                           destino=="pern_pciachubut"~"Pcia. Chubut",
                           destino=="pern_restoneuquen"~"Resto Neuquén")) %>% 
  arrange(peso) %>% 
  mutate(variable="Pernoctes")


orden <- (etie34_p$destino)

#Viajeros:

etie34_v <- eti_e_perfiles %>%
  filter(p3_3==anio_ref,paso_final=="Cristo Redentor") %>% 
  summarise(viaj_caba=sum(p21_11*wpf,na.rm = TRUE),
            viaj_ciguazu=sum(p21_41*wpf,na.rm = TRUE),
            viaj_pcord=sum(p21_51*wpf,na.rm = TRUE),
            viaj_psaltucjuj=sum(p21_61*wpf,na.rm = TRUE),
            viaj_calafate=sum(p21_91*wpf,na.rm = TRUE),
            viaj_barvasm=sum(p21_101*wpf,na.rm = TRUE),
            viaj_rosario=sum(p21_111*wpf,na.rm = TRUE),
            viaj_restonor=sum(p21_141*wpf,na.rm = TRUE),
            viaj_restocuyo=sum(p21_151_sin_mza*wpf,na.rm = TRUE),
            viaj_restopata=sum(p21_161_sin_mza*wpf,na.rm = TRUE),
            viaj_restolito=sum(p21_171*wpf,na.rm = TRUE),
            viaj_cmza=sum(p21_301*wpf,na.rm = TRUE),
            viaj_srafael=sum(p21_311*wpf,na.rm = TRUE),
            viaj_malar=sum(p21_321*wpf,na.rm = TRUE),
            viaj_restopciamza=sum(p21_331*wpf,na.rm = TRUE),
            viaj_pciasanjuan=sum(p21_341*wpf,na.rm = TRUE),
            viaj_pciachubut=sum(p21_351*wpf,na.rm = TRUE),
            viaj_restoneuquen=sum(p21_361*wpf,na.rm = TRUE),
            total=sum(viajeros*wpf,na.rm = TRUE))%>% 
  pivot_longer(cols=c(1:18),
               names_to = "destino",
               values_to="valor") %>%
  mutate(peso=round(valor/total,3)*100) %>% 
  select(destino,peso) %>%
  mutate(destino=case_when(destino=="viaj_caba"~"CABA",
                           destino=="viaj_ciguazu"~"Cataratas Iguazú",
                           destino=="viaj_pcord"~"Pcia. Córdoba",
                           destino=="viaj_psaltucjuj"~"Salta, Tucumán o Jujuy",
                           destino=="viaj_calafate"~"Calafate",
                           destino=="viaj_barvasm"~"Bari./V. Angost./SM Andes",
                           destino=="viaj_rosario"~"Rosario",
                           destino=="viaj_restopba"~"Resto PBA.",
                           destino=="viaj_restonor"~"Resto Norte",
                           destino=="viaj_restocuyo"~"Resto Cuyo",
                           destino=="viaj_restopata"~"Resto Patagonia",
                           destino=="viaj_restolito"~"Resto Litoral",
                           destino=="viaj_cmza"~"Mendoza (ciudad)",
                           destino=="viaj_srafael"~"San Rafael",
                           destino=="viaj_malar"~"Malargue/Leñas",
                           destino=="viaj_restopciamza"~"Resto Pcia. Mdza.",
                           destino=="viaj_pciasanjuan"~"Pcia. San Juan",
                           destino=="viaj_pciachubut"~"Pcia. Chubut",
                           destino=="viaj_restoneuquen"~"Resto Neuquén")) %>% 
  arrange(peso) %>% 
  mutate(variable="Viajeros")

#BASE TOTAL:

etie34 <- etie34_p %>% 
  rbind(etie34_v)


rm(etie34_p,etie34_v)


#Gráfico:

ggplot(data=etie34 %>%mutate(destino = factor(destino,levels=orden)),
       aes(x=peso,y=destino,fill=destino,
           label=glue("{peso} %")))+
  geom_col()+ 
  facet_wrap(~variable)+
  geom_text(hjust = "inward") +
  scale_fill_dnmye(reverse = TRUE)+
  theme(legend.position='none')+
  labs(title = "",
       subtitle = glue(""),
       x = "",
       y = "",
       colour="",
       caption = "Fuente: Encuesta de Turismo Internacional (ETI) - (DNMyE e INDEC).")+
  theme_minimal()+
  theme(#text = element_text(family = "Encode Sans Normal",size=16),
    plot.caption  = element_text(hjust = 0),
    plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
    axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
    plot.title=element_text(hjust = 0,face="bold"),
    legend.title = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "grey",
                                      linewidth = 0.3,
                                      linetype = 4),
    panel.grid.minor.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x=element_blank())


rm(etie34)
