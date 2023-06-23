
####```{r include=FALSE}

library(tidyverse)
library(magrittr) #pipe
library(janitor)
library(lubridate)
library(haven)
#library(dplyr) #Group_by,summarise, lag, filter
library(glue)
library (gt)
library(comunicacion)
library(hrbrthemes) 
library(ggtext) 

#```

#```{r}
### Se defininen algunos parámetros

#anio de referencia de publicación
options(scipen = 9999)

anio_ref <- 2021 #eti

#```

#```{r}


eti_a <- herramientas::read_file_srv("/srv/DataDNMYE/eti/bases/eti_a_2014_2023.rds") %>% 
  filter(p3_3<=anio_ref & vis==2 & wpf>0)%>% 
  mutate(motivo=case_when(p17nuev==1 ~"Vacaciones/ocio/recreación",
                          p17nuev==2 ~"Visita flia/amigos",
                          p17nuev==3 ~"Negocios/congreso/conferencia",
                          p17nuev>=4 ~"Otros")) %>%
  mutate(alojamiento=case_when(p21_ta_publ==1~"Casa flia./amigos",
                               p21_ta_publ==2~"Hotel 1,2, y 3 estrellas",
                               p21_ta_publ==3~"Hotel 4 y 5 estrellas",
                               p21_ta_publ==4~"Otros")) %>% 
  mutate(paq=case_when(paq==0~"No utilizó paquete",
                       paq==1~"Utilizó paquete"))

eti_a_perfiles <- eti_a %>%
  filter((paso_final=="Ezeiza y Aeroparque" & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-01-01" | anio_trim=="2021-10-01"))) |
           (paso_final %in% c("Aep. Córdoba","Puerto de Buenos Aires","Aep. Mendoza","Cristo Redentor") & (p3_3<2020 | p3_3>2021 | (anio_trim=="2020-01-01" | anio_trim=="2021-10-01"))))


#```


#```{r etia1, fig.cap=glue("Salidas de turistas residentes por año según paso. Año {anio_ref}.")}

#Insumo:

etia1 <- eti_a %>% 
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

etia1 %>%
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
    locations = cells_body(rows = nrow(etia1))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(etia1)
    )) %>%  
  gtsave("imagenes/eoh/etia1.png")


rm(etia1)

#```

#```{r etia2, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Ezeiza y Aeroparque. Año {anio_ref}.")}

#Ezeiza y aeroparque (dato chequeado)

#Insumo:

etia2 <- eti_a_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Ezeiza y Aeroparque") %>% 
  group_by(p3_3,dest_eya) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (dest_eya=factor(dest_eya,levels =c("Total","Bolivia",
                                             "Brasil","Chile",
                                             "Paraguay","Uruguay",
                                             "EE.UU y Canadá","Resto América",
                                             "Europa","Resto del Mundo"))) %>% 
  arrange(dest_eya)

if (anio_ref==2021) {
  etia2 <- etia2 %>% select(dest_eya,t0,prop_)
} else {
  etia2 <- etia2 %>% select(dest_eya,t0,var_1,var_2,prop_)
}  


#Tabla:

etia2 %>%
  gt()%>%
  cols_label(dest_eya = "Destino",
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
  gt_theme_dnmye() %>%
  gtsave("imagenes/eti/etia2.png")


rm(etia2)


#```

#```{r eti3, fig.cap=glue("Llegada de turistas residentes según residencia habitual. Aeropuerto Internacional de Córdoba. Año {anio_ref}.")}

#AEP Córdoba (dato chequeado)

#Insumo:


etia3 <- eti_a_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Aep. Córdoba") %>% 
  group_by(p3_3,dest_cor) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros))%>%
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate(t0=case_when(is.na(t0)~0,
                      TRUE~t0)) %>% 
  mutate(prop_=prop.table(t0)) %>% 
  ungroup() %>% 
  adorn_totals()%>%
  mutate (dest_cor=factor(dest_cor,levels =c("Total","Brasil","Chile",
                                             "EE.UU y Canadá","Caribe","Resto América",
                                             "Europa y resto del mundo"))) %>% 
  arrange(dest_cor)



if (anio_ref==2021) {
  etia3 <- etia3 %>% select(dest_cor,t0,prop_)
} else {
  etia3 <- etia3 %>% select(dest_cor,t0,var_1,var_2,prop_)
}  



#Tabla GT ()

etia3 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(dest_cor = "Destino",
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
  gtsave("imagenes/eti/etia3.png")


rm(etia3)


#```

#```{r eti4, fig.cap=glue("Llegada de turistas no residentes según residencia habitual. Puerto de Buenos Aires. Año {anio_ref}.")}

# Puerto de buenos aires (dato chequeado)

#Insumo:

etia4 <- eti_a_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Puerto de Buenos Aires") %>% 
  group_by(p3_3,dest_puerto) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (dest_puerto=factor(dest_puerto,levels =c("Total","Uruguay",
                                                   "Resto del mundo"))) %>% 
  arrange(dest_puerto)

if (anio_ref==2021) {
  etia4 <- etia4 %>% select(dest_puerto,t0,prop_)
} else {
  etia4 <- etia4 %>% select(dest_puerto,t0,var_1,var_2,prop_)
}  


#Tabla:


etia4 %>%
  gt()%>%
  gt_theme_dnmye() %>% 
  cols_label(dest_puerto = "Destino",
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
  gtsave("imagenes/eti/etia4.png")

rm(etia4)

#```


#```{r etia5, fig.cap=glue("Llegada de turistas residentes según residencia habitual. Aeropuerto Internacional de Mendoza. Año {anio_ref}.")}

### Aep Mendoza (dato chequeado)

#Insumo:

etia5 <- eti_a_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Aep. Mendoza") %>% 
  group_by(p3_3,dest_mdza) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (dest_mdza=factor(dest_mdza,levels =c("Total","Brasil",
                                               "Chile","México y Caribe","EE.UU y Canadá",
                                               "Resto América",
                                               "Europa y resto del mundo"))) %>% 
  arrange(dest_mdza)

if (anio_ref==2021) {
  etia5 <- etia5 %>% select(dest_mdza,t0,prop_)
} else {
  etia5 <- etia5 %>% select(dest_mdza,t0,var_1,var_2,prop_)
}  

#Tabla:

etia5 %>%
  gt()%>% 
  cols_label(dest_mdza = "Destino",
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
  gt_theme_dnmye() %>%
  gtsave("imagenes/eti/etia5.png")

rm(etia5)

#```

#```{r etia6, fig.cap=glue("Llegada de turistas residentes según residencia habitual. Paso Internacional Cristo Redentor. Año {anio_ref}.")}

###Cristo Redentor (dato chequeado)

#Insumo:

etia6 <- eti_a_perfiles %>%
  filter(p3_3>=anio_ref-2 & paso_final=="Cristo Redentor") %>% 
  group_by(p3_3,dest_cristo) %>% 
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE)) %>% 
  pivot_wider(names_from = p3_3,
              values_from = c(viajeros)) %>% 
  rename(t0=4,t_1=3,t_2=2) %>% 
  mutate(prop_=prop.table(t0))%>% 
  ungroup() %>% 
  adorn_totals() %>%
  mutate(var_1=(t0/t_1)-1,
         var_2=(t0/t_2)-1) %>%
  mutate (dest_cristo=factor(dest_cristo,levels =c("Total","Chile",
                                                   "Resto del mundo"))) %>% 
  arrange(dest_cristo)


if (anio_ref==2021) {
  etia6 <- etia6 %>% select(dest_cristo,t0,prop_)
} else {
  etia6 <- etia6 %>% select(dest_cristo,t0,var_1,var_2,prop_)
}  

#Tabla:


etia6 %>%
  gt()%>% 
  cols_label(dest_cristo = "Destino",
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
  gt_theme_dnmye() %>%
  gtsave("imagenes/eti/etia6.png")


rm(etia6)

#```

#```{r etia7, fig.cap=glue("Distribución de los turistas residentes según motivo de viaje, por paso. Año {anio_ref}.")}

# Llegadas de turistas no residentes por motivo de viaje (dato chequeado)

#Insumo:

etia7_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etia7 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etia7_total) %>% 
  mutate(motivo=factor(motivo,levels = c("Visita flia/amigos","Vacaciones/ocio/recreación",
                                         "Negocios/congreso/conferencia","Otros")),
         prop_=round(prop_,3)*100)


rm(etia7_total)

#Gráfico:

ggplot(data=etia7 %>% mutate(motivo=factor(motivo,
                                           levels = c("Visita flia/amigos",
                                                      "Vacaciones/ocio/recreación",
                                                      "Negocios/congreso/conferencia",
                                                      "Otros")),
                             paso_final=factor(paso_final,
                                               levels = c("TOTAL",
                                                          "Ezeiza y Aeroparque",
                                                          "Aep. Córdoba",
                                                          "Aep. Mendoza",
                                                          "Puerto de Buenos Aires",
                                                          "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo,
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
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top")

rm(etia7)


#```

#```{r etia8, fig.cap=glue("Distribución de los turistas residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}

# Llegadas de turistas no residentes por tipo alojamiento (dato chequeado)

#Insumo:

etia8_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etia8 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etia8_total) %>% 
  mutate(prop_=round(prop_,3)*100) %>% 
  mutate(alojamiento=case_when(alojamiento=="Casa flia./amigos"~"Casa flia/amigos",
                               alojamiento=="Hotel 1,2, y 3 estrellas"~"Hotel 1/2/3 est.",
                               alojamiento=="Hotel 4 y 5 estrellas"~"Hotel 4/5 est.",
                               alojamiento=="Otros"~"Otros"))


rm(etia8_total)

#Gráfico:


ggplot(data=etia8 %>% mutate(alojamiento=factor(alojamiento,
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

rm(etia8)

#```

#```{r eti9, fig.cap=glue("Distribución de los turistas residentes según organización del viaje, por paso. Año {anio_ref}.")}


# Llegadas de turistas no residentes según USO DE PAQUETE

#Insumo:

etia9_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>% 
  mutate(paso_final="TOTAL")


etia9 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(viajeros)) %>% 
  select(-viajeros) %>%
  rbind(etia9_total) %>% 
  pivot_wider(names_from = paso_final,
              values_from = prop_)  %>% 
  select(paq,`TOTAL`,`Ezeiza y Aeroparque`,`Aep. Córdoba`,`Puerto de Buenos Aires`,
         `Aep. Mendoza`,`Cristo Redentor`) %>% 
  #mutate_all(~replace(., is.na(.), 0)) %>% 
  pivot_longer(cols=c(2:7),
               names_to = "paso_final",
               values_to="prop_") %>% 
  arrange(paso_final) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etia9_total)


#Gráfico:

ggplot(data=etia9 %>% mutate(paq=factor(paq,
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

rm(etia9)

#```


#```{r etia10, fig.cap=glue("Salidas de turistas, pernoctaciones, gasto total, gasto por viajero, estadÍa media de turistas residentes, según paso. Año {anio_ref}.")}

#Cuadro 223, Llegadas, gasto total, gasto por viajero, pernoctaciones y estadÍa media de turistas no residentes

#Insumo:

etia10_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref | p3_3==anio_ref-1 | p3_3==anio_ref-2) %>%
  group_by(p3_3) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE)) %>% 
  mutate(paso_final="TOTAL")



etia10 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref | p3_3==anio_ref-1 | p3_3==anio_ref-2) %>%
  group_by(p3_3,paso_final) %>%
  summarise(viajeros=sum(viajeros*wpf,na.rm = TRUE),
            pernoctes=sum(totaln*wpf,na.rm = TRUE),
            gasto=sum(gastoest2*wpf,na.rm = TRUE)) %>% 
  rbind(etia10_total) %>% 
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
  etia10 <- etia10 %>% select(1:2,5,8,11,14,17)
} else {
  etia10 <- etia10 %>% select(1:19)
}  


rm(etia10_total)

#Tabla:

etia10 %>%
  gt()%>%
  fmt_number(columns = c(5,7), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  gt_theme_dnmye() %>% 
  cols_label(paso_final = "Paso",
             viajeros="Turistas",
             pernoctes="Pernoctaciones",
             gasto="Gasto total",
             estadia="Estadía promedio (en noches)",
             gasto_tur="Gasto por turista (en USD)",
             gasto_diario="Gasto promedio diario (en USD)")  %>%
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
    )) %>%  
  gtsave("imagenes/eti/etia10.png") 


rm(etia10)

#```

#Seguir acá:

#```{r etia11, fig.cap=glue("Distribución del gasto total de turistas residentes según motivo de viaje, por paso. Año {anio_ref}.")}


# Gasto total de no residentes por motivo de viaje

#Insumo

etia11_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etia11 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etia11_total) %>% 
  mutate(prop_=(round(prop_,3))*100)

rm(etia11_total)


#Gráfico en ggplot


ggplot(data=etia11 %>% mutate(motivo=factor(motivo,
                                            levels = c("Visita flia/amigos",
                                                       "Vacaciones/ocio/recreación",
                                                       "Negocios/congreso/conferencia",
                                                       "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo,
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

rm(etia11)


#```{r etia12, fig.cap=glue("Distribución del gasto total de los turistas residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes por tipo alojamiento 

#Insumo:

etia12_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etia12 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etia12_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etia12_total)

#Gráfico:

ggplot(data=etia12 %>% mutate(alojamiento=factor(alojamiento,
                                                 levels = c("Casa flia./amigos",
                                                            "Hotel 4 y 5 estrellas",
                                                            "Hotel 1,2, y 3 estrellas",
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

rm(etia12)


#```

#```{r etia13, fig.cap=glue("Distribución del gasto total de los turistas no residentes según utilización de paquete turístico, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes según uso de paquete

#Insumo:

etia13_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>% 
  mutate(paso_final="TOTAL")


etia13 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(gasto=sum(gastoest2*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(gasto)) %>% 
  select(-gasto) %>%
  rbind(etia13_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etia13_total)

#Gráfico:

ggplot(data=etia13 %>% mutate(paq=factor(paq,
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

rm(etia13)


#```

#```{r etia14, fig.cap=glue("Distribución de los pernoctes no residentes según motivo de viaje, por paso. Año {anio_ref}.")}

#Pernoctaciones por alojamiento

#Insumo

etia14_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(motivo) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etia14 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,motivo) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etia14_total) %>% 
  mutate(prop_=(round(prop_,3))*100)

rm(etia14_total)


#Gráfico:

ggplot(data=etia14 %>% mutate(motivo=factor(motivo,
                                            levels = c("Visita flia/amigos",
                                                       "Vacaciones/ocio/recreación",
                                                       "Negocios/congreso/conferencia",
                                                       "Otros")),
                              paso_final=factor(paso_final,
                                                levels = c("TOTAL",
                                                           "Ezeiza y Aeroparque",
                                                           "Aep. Córdoba",
                                                           "Aep. Mendoza",
                                                           "Puerto de Buenos Aires",
                                                           "Cristo Redentor"))),
       aes(x=paso_final,y=prop_,fill=motivo,
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

rm(etia14)

#```

#```{r etia15, fig.cap=glue("Distribución de los pernoctes residentes según principal tipo de alojamiento, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes por tipo alojamiento 

#Insumo:

etia15_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(alojamiento) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etia15 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,alojamiento) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etia15_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etia15_total)

#Gráfico:

ggplot(data=etia15 %>% mutate(alojamiento=factor(alojamiento,
                                                 levels = c("Casa flia./amigos",
                                                            "Hotel 4 y 5 estrellas",
                                                            "Hotel 1,2, y 3 estrellas",
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

rm(etia15)

#```

#```{r eti16, fig.cap=glue("Distribución de los pernoctes no residentes según utilización de paquete turístico, por paso. Año {anio_ref}.")}

#  Gasto total de no residentes según uso de paquete

#Insumo:

etia16_total <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paq) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>% 
  mutate(paso_final="TOTAL")


etia16 <- eti_a_perfiles %>%
  filter(p3_3==anio_ref) %>% 
  group_by(paso_final,paq) %>%
  summarise(pernoctes=sum(totaln*wpf,na.rm = TRUE))%>%
  mutate(prop_=prop.table(pernoctes)) %>% 
  select(-pernoctes) %>%
  rbind(etia16_total) %>% 
  mutate(prop_=(round(prop_,3))*100)


rm(etia16_total)

#Gráfico:

ggplot(data=etia16 %>% mutate(paq=factor(paq,
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

rm(etia16)
