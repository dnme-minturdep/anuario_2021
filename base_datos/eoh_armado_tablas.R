#-----------------------EOH RECURSOS DATA ABIERTA---------------------#

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


### Se defininen algunos parámetros

#anio de referencia de publicación

anio_ref <- 2021



#-----------------------Se levanta la base---------------------#


eoh <- readRDS("/srv/DataDNMYE/eoh/bases/eoh.rds") %>% 
  filter(anio>=2010 & anio<=anio_ref)


# Cuadro 4.2.1 Pernoctaciones. Variación interanual y participación porcentual
#por condición de residencia según año. Años 2010-2015.

eoh1 <- eoh %>%
  group_by(anio) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE)) %>%
  mutate(varia_1=round((pern_totales/lag(pern_totales,1)-1),3),
         varia_2=round((pern_res/lag(pern_res,1)-1),3),
         varia_3=round((pern_nores/lag(pern_nores,1)-1),3),
         varia_1b=round((pern_totales/lag(pern_totales,2)-1),3),
         varia_2b=round((pern_res/lag(pern_res,2)-1),3),
         varia_3b=round((pern_nores/lag(pern_nores,2)-1),3),
         part_res=round(pern_res/pern_totales,3),
         part_nores=round(pern_nores/pern_totales,3),
         varia_1b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_1b),
         varia_2b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_2b),
         varia_3b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_3b))
if (anio_ref==2021) {
  eoh1 <- eoh1 %>% select(anio,pern_totales,varia_1,varia_1b,pern_res,varia_2,varia_2b,part_res,pern_nores,varia_3,varia_3b,part_nores)
} else {
  eoh1 <- eoh1 %>% select(anio,pern_totales,varia_1,pern_res,varia_2,part_res,pern_nores,varia_3,part_nores)
}



#Tabla GT (dato chequeado)

eoh1 %>%
  gt()%>% 
  cols_label(anio = md("**Año**"),
             pern_totales = md("**Pernoctaciones**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             pern_res = md("**Pernoctaciones**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             part_res = md("**% residentes sobre el Total**"),
             pern_nores = md("**Pernoctaciones**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"),
             part_nores = md("**% no residentes sobre el Total**"))  %>%
  fmt_number(columns = c(2,5,9), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:8,10:12), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,9) ~ px(120),
             columns = c(3,4,6:8,10:12) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS. 2010-{anio_ref}"),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_spanner(label = "Totales", columns = c(2:4)) %>% 
  tab_spanner(label = "Residentes", columns = c(5:8)) %>% 
  tab_spanner(label = "No residentes", columns = c(9:12)) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = nrow(eoh1))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(eoh1)
    ))


rm(eoh1)

# Cuadro 4.2.2 Viajeros hospedados. Variación interanual y participación porcentual
#por condición de residencia según año. Años 2010-2015.


eoh2 <- eoh %>%
  group_by(anio) %>% 
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>%
  mutate(varia_1=round((viaj_totales/lag(viaj_totales,1)-1),3),
         varia_2=round((viaj_res/lag(viaj_res,1)-1),3),
         varia_3=round((viaj_nores/lag(viaj_nores,1)-1),3),
         varia_1b=round((viaj_totales/lag(viaj_totales,2)-1),3),
         varia_2b=round((viaj_res/lag(viaj_res,2)-1),3),
         varia_3b=round((viaj_nores/lag(viaj_nores,2)-1),3),
         part_res=round(viaj_res/viaj_totales,3),
         part_nores=round(viaj_nores/viaj_totales,3),
         varia_1b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_1b),
         varia_2b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_2b),
         varia_3b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_3b)) 

if (anio_ref==2021) {
  eoh2 <- eoh2 %>% select(anio,viaj_totales,varia_1,varia_1b,viaj_res,varia_2,varia_2b,part_res,viaj_nores,varia_3,varia_3b,part_nores)
} else {
  eoh2 <- eoh2 %>% select(anio,viaj_totales,varia_1,viaj_res,varia_2,part_res,viaj_nores,varia_3,part_nores)
}

#Tabla GT (Dato chequeado)

eoh2 %>%
  gt()%>% 
  cols_label(anio = md("**Año**"),
             viaj_totales = md("**Viajeros**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             viaj_res = md("**Viajeros**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             part_res = md("**% residentes sobre el Total**"),
             viaj_nores = md("**Viajeros**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"),
             part_nores = md("**% no residentes sobre el Total**"))  %>%
  fmt_number(columns = c(2,5,9), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:8,10:12), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,9) ~ px(120),
             columns = c(3,4,6:8,10:12) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS. 2010-{anio_ref}"),
    title = toupper(md("__Viajeros. Variación interanual y participación porcentual por condición de residencia según año.__"))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_spanner(label = "Totales", columns = c(2:4)) %>% 
  tab_spanner(label = "Residentes", columns = c(5:8)) %>% 
  tab_spanner(label = "No residentes", columns = c(9:12)) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = nrow(eoh2))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(eoh2)
    )) %>% 
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) 


rm(eoh2)

# G421 Pernoctaciones y viajeros hospedados en establecimientos
#hoteleros y parahoteleros por condición de residencia. Años 2020-2021


eoh3 <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
  group_by(anio) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  pivot_longer(cols=c(pern_totales,pern_res,pern_nores,
                      viaj_totales,viaj_res,viaj_nores),
               names_to = "indicador",
               values_to="valor") %>%
  mutate(variable=case_when(str_detect(indicador, "pern")~"Pernoctes",
                            str_detect(indicador, "viaj")~"Viajeros"),
         residencia=case_when(str_detect(indicador, "_totales")~"Total",
                              str_detect(indicador, "_res")~"Residentes",
                              str_detect(indicador, "_nores")~"No residentes")) %>% 
  select(anio,variable,residencia,valor) %>% 
  mutate(valor=round(valor/1000000,1)) %>% 
  arrange(desc(anio))


# Gráfico en ggplot (Dato chequeado)

ggplot(data=eoh3 %>% 
         filter(residencia !="Total")
       %>% mutate(anio=factor(anio,levels = c(anio_ref,anio_ref-1,anio_ref-2))),aes(x=anio,
                                                                                    y=valor,
                                                                                    fill=residencia,
                                                                                    label=valor))+
  geom_col()+
  geom_text(position = position_stack(vjust = .5),size=8)+
  facet_wrap(~variable)+
  scale_fill_dnmye(palette="dicotomica")+
  theme_minimal()+
  labs(title = "",
       subtitle = glue("TOTAL PAIS. 2019-{anio_ref}"),
       x = "",
       y = "(millones)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face = "bold"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top", panel.border = element_rect(color="black",fill = NA),
        strip.text.x = element_text(size=14),
        text = element_text(size = 20))

rm(eoh3)

# Cuadro 4.2.3 Pernoctaciones según región de destino y condición
#de residencia. Variación interanual. Año 2015.

eoh4 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,region_de_destino) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(varia_1=round((pern_totales/lag(pern_totales,7)-1),3),
         varia_2=round((pern_res/lag(pern_res,7)-1),3),
         varia_3=round((pern_nores/lag(pern_nores,7)-1),3)) %>% 
  mutate(varia_1b=round((pern_totales/lag(pern_totales,14)-1),3),
         varia_2b=round((pern_res/lag(pern_res,14)-1),3),
         varia_3b=round((pern_nores/lag(pern_nores,14)-1),3)) %>%
  filter(anio==anio_ref) 

if (anio_ref==2021) {
  eoh4 <- eoh4 %>% select(region_de_destino,pern_totales,varia_1,varia_1b,pern_res,varia_2,varia_2b,pern_nores,varia_3,varia_3b)
} else {
  eoh4 <- eoh4 %>% select(region_de_destino,pern_totales,varia_1,pern_res,varia_2,pern_nores,varia_3)
}


#Tabla GT (Dato chequeado)

eoh4 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             pern_totales = md("**Totales**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             pern_res = md("**Residentes**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             pern_nores = md("**No residentes**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"))  %>%
  fmt_number(columns = c(2,5,8), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(120),
             columns = c(3,4,6,7,9,10) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS, {anio_ref}"),
    title = toupper(md("__Pernoctaciones según región de destino y condición de residencia__"))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))

rm(eoh4)

# Cuadro 4.2.4 Viajeros hospedados por año según región de destino y condición
#de residencia. Variación interanual. Año 2015

eoh5 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>%
  group_by(anio,region_de_destino) %>% 
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(varia_1=round((viaj_totales/lag(viaj_totales,7)-1),3),
         varia_2=round((viaj_res/lag(viaj_res,7)-1),3),
         varia_3=round((viaj_nores/lag(viaj_nores,7)-1),3),
         varia_1b=round((viaj_totales/lag(viaj_totales,14)-1),3),
         varia_2b=round((viaj_res/lag(viaj_res,14)-1),3),
         varia_3b=round((viaj_nores/lag(viaj_nores,14)-1),3)) %>%
  filter(anio==anio_ref) %>% 
  select(region_de_destino,viaj_totales,varia_1,varia_1b,viaj_res,varia_2,varia_2b,viaj_nores,varia_3,varia_3b)

if (anio_ref==2021) {
  eoh5 <- eoh5 %>% select(region_de_destino,viaj_totales,varia_1,varia_1b,viaj_res,varia_2,varia_2b,viaj_nores,varia_3,varia_3b)
} else {
  eoh5 <- eoh5 %>% select(region_de_destino,viaj_totales,varia_1,viaj_res,varia_2,viaj_nores,varia_3)
}

#Tabla GT (dato chequeado)

eoh5 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             viaj_totales = md("**Totales**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             viaj_res = md("**Residentes**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             viaj_nores = md("**No residentes**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"))  %>%
  fmt_number(columns = c(2,5,8), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(120),
             columns = c(3,4,6,7,9,10) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS, {anio_ref}"),
    title = toupper(md("__Viajeros hospedados por año según región de destino y condición de residencia__"))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))

rm(eoh5)

# Cuadro 4.2.5 p 146 Participación de viajeros hospedados por región de destino
#según condición de residencia. Variación interanual. Año 2015


eoh6 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1) %>%
  group_by(anio,region_de_destino) %>% 
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  mutate(dist_total=prop.table(viaj_totales),
         dist_res=prop.table(viaj_res),
         dist_nores=prop.table(viaj_nores)) %>% 
  select(-c(viaj_totales,viaj_res,viaj_nores)) %>%
  ungroup() %>% 
  mutate(variapp_1=(dist_total-lag(dist_total,7))*100,
         variapp_2=(dist_res-lag(dist_res,7))*100,
         variapp_3=(dist_nores-lag(dist_nores,7))*100) %>%
  filter(anio==anio_ref) %>% 
  select (region_de_destino,dist_total,variapp_1,
          dist_res,variapp_2,dist_nores,variapp_3)

# Tabla GT (dato chequeado)

eoh6 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             dist_total = md("**Total**"),
             variapp_1 = md("**Variación i.a.(en p.p)**"),
             dist_res = md("**Residentes**"),
             variapp_2 = md("**Variación i.a.(en p.p)**"),
             dist_nores = md("**No residentes**"),
             variapp_3 = md("**Variación i.a.(en p.p)**"))%>%
  fmt_number(columns = c(3,5,7), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(2,4,6), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = (2:7) ~ px(120)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS, {anio_ref}"),
    title = toupper(md("__Participación de viajeros hospedados por región de destino según condición de residencia__"))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_1),
      rows = variapp_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_1),
      rows = variapp_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_2),
      rows = variapp_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_2),
      rows = variapp_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_3),
      rows = variapp_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(variapp_3),
      rows = variapp_3 > 0
    )) 

rm(eoh6)

# Cuadro 4.2.6 p 147 Estadía promedio según región de destino y condición de residencia. Variación interanual. 


eoh7_total <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
  group_by(anio) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  mutate(region_de_destino="TOTAL PAÍS")


eoh7_total <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
  group_by(anio,region_de_destino) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  rbind(eoh7_total) %>% 
  arrange(anio,region_de_destino) %>% 
  mutate(em_total=pern_totales/viaj_totales,
         em_res=pern_res/viaj_res,
         em_nores=pern_nores/viaj_nores) %>% 
  select(anio,region_de_destino,em_total,em_res,em_nores) %>%
  ungroup() %>% 
  mutate(varia_1=round((em_total/lag(em_total,8)-1),3),
         varia_2=round((em_res/lag(em_res,8)-1),3),
         varia_3=round((em_nores/lag(em_nores,8)-1),3),
         varia_1b=round((em_total/lag(em_total,16)-1),3),
         varia_2b=round((em_res/lag(em_res,16)-1),3),
         varia_3b=round((em_nores/lag(em_nores,16)-1),3)) %>% 
  mutate(region_de_destino=factor(region_de_destino,
                                  levels =c("TOTAL PAÍS","Buenos Aires",
                                            "CABA","Córdoba",
                                            "Cuyo","Litoral","Norte","Patagonia"))) %>% 
  arrange(anio,region_de_destino) %>%
  filter(anio==anio_ref) %>% 
  mutate(em_total=round(em_total,1),
         em_res=round(em_res,1),
         em_nores=round(em_nores,1))

if (anio_ref==2021) {
  eoh7_total <- eoh7_total %>% select(region_de_destino,em_total,varia_1,varia_1b,em_res,varia_2,varia_2b,em_nores,varia_3,varia_3b)
} else {
  eoh7_total <- eoh7_total %>% select(region_de_destino,em_total,varia_1,em_res,varia_2,em_nores,varia_3)
}


#Tabla GT (dato chequeado)

#Estadía promedio según región de destino y condición de residencia.

eoh7_total %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             em_total = md("**Total**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             em_res = md("**Residentes**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             em_nores = md("**No residentes**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"))%>%
  fmt_number(columns = c(2,5,8), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(70),
             columns = c(3,4,6,7,9,10) ~ px(70),
             columns = c(1) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("SEGÚN REGIÓN, {anio_ref}"),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>%
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
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) 

rm(eoh7_total,eoh7)

#Gráfico eoh8 Estadía promedio de los viajeros por año,
#según condición de residencia 


eoh8 <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
  group_by(anio) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE),
            em_total=round(pern_totales/viaj_totales,2),
            em_res=round(pern_res/viaj_res,2),
            em_nores=round(pern_nores/viaj_nores,2)) %>% 
  select(anio,em_total,em_res,em_nores) %>% 
  pivot_longer(cols=c(em_total,em_res,em_nores),
               names_to = "indicador",
               values_to="valor") %>% 
  mutate(indicador=case_when(indicador=="em_total"~"Total",
                             indicador=="em_res"~"Residentes",
                             indicador=="em_nores"~"No Residentes"))

if (anio_ref==2021) {
  eoh8 <- eoh8 %>% filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2))
} else {
  eoh8 <- eoh8 %>% filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2))
}

#Gráfico en ggplot (dato chequeado)

#Estadía promedio de los viajeros por año, según condición de residencia.

ggplot(data = eoh8 %>% mutate(anio=factor(anio,levels = c(anio_ref,anio_ref-1,anio_ref-2)),
                              indicador=factor(indicador,levels = c("Total","Residentes","No Residentes"))),
       aes(y=valor,x=indicador,label=valor,fill=indicador))+
  facet_wrap(~anio)+
  geom_col()+
  geom_text(position = position_stack(vjust = 1.02),size=8)+ 
  scale_fill_manual(values = c("Residentes" = dnmye_colores("rosa"),
                               "No Residentes" = dnmye_colores("cian"),
                               "Total" = dnmye_colores("gris oscuro")))+
  labs(title = "",
       subtitle = glue("TOTAL PAIS. 2019-{anio_ref}."),
       x = "",
       y = "(noches)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face="bold"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position='none',
        text = element_text(size=20))


rm(eoh8)

#Gráfico 4.2.3 Estadía promedio de los viajeros hospedados
#por región de destino, según condición de residencia.


eoh9 <- eoh %>%
  filter(anio==anio_ref) %>% 
  group_by(region_de_destino) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE),
            em_total=round(pern_totales/viaj_totales,1),
            em_res=round(pern_res/viaj_res,1),
            em_nores=round(pern_nores/viaj_nores,1)) %>% 
  select(region_de_destino,em_total,em_res,em_nores) %>% 
  pivot_longer(cols=c(em_total,em_res,em_nores),
               names_to = "indicador",
               values_to="valor") %>% 
  mutate(indicador=case_when(indicador=="em_total"~"Total",
                             indicador=="em_res"~"Residentes",
                             indicador=="em_nores"~"No Residentes"))

#Tabla ggplot (dato chequeado)

ggplot(data = eoh9 %>% mutate(indicador=factor(indicador,levels = c("Total","Residentes","No Residentes"))), aes(y=valor,x=region_de_destino,label=valor,fill=indicador))+
  geom_bar(stat='identity',position="dodge")+
  geom_text(position = position_dodge(width = 1),
            size=8)+ 
  scale_fill_manual(values = c("Residentes" = dnmye_colores("rosa"),
                               "No Residentes" = dnmye_colores("cian"),
                               "Total" = dnmye_colores("gris oscuro")))+
  labs(title = "",
       subtitle = glue("Por región de destino, {anio_ref}."),
       x = "",
       y = "(noches)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face="bold"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=20),
        legend.position = "bottom")



###########ANÁLISIS POR TIPO DE ALOJAMIENTO


#Cuadro 4.2.7 Pernoctaciones por categorías según región de destino. Variación interanual. 

eoh10_total <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE)) %>%
  pivot_wider(names_from = categoria_del_hotel,
              values_from = pern_totales) %>%
  rename("1_2_est"="1 y 2 estrellas",
         "3_est"="3 estrellas/boutiques/aparts",
         "4_5_est"="4 y 5 estrellas",
         "para_hoteles"="Para Hoteles") %>% 
  mutate(total_hoteleros=sum(`1_2_est`,`3_est`,`4_5_est`),
         region_de_destino="Total País")


eoh10 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,region_de_destino,categoria_del_hotel) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE)) %>%
  pivot_wider(names_from = categoria_del_hotel,
              values_from = pern_totales) %>%
  rename("1_2_est"="1 y 2 estrellas",
         "3_est"="3 estrellas/boutiques/aparts",
         "4_5_est"="4 y 5 estrellas",
         "para_hoteles"="Para Hoteles") %>% 
  mutate(total_hoteleros=sum(`1_2_est`,`3_est`,`4_5_est`)) %>% 
  rbind(eoh10_total) %>%
  arrange(anio,region_de_destino) %>% 
  ungroup() %>% 
  mutate(varia_4=round((`1_2_est`/lag(`1_2_est`,8)-1),3),
         varia_3=round((`3_est`/lag(`3_est`,8)-1),3),
         varia_2=round((`4_5_est`/lag(`4_5_est`,8)-1),3),
         varia_5=round((`para_hoteles`/lag(`para_hoteles`,8)-1),3),
         varia_1=round((total_hoteleros/lag(total_hoteleros,8)-1),3),
         varia_4b=round((`1_2_est`/lag(`1_2_est`,16)-1),3),
         varia_3b=round((`3_est`/lag(`3_est`,16)-1),3),
         varia_2b=round((`4_5_est`/lag(`4_5_est`,16)-1),3),
         varia_5b=round((`para_hoteles`/lag(`para_hoteles`,16)-1),3),
         varia_1b=round((total_hoteleros/lag(total_hoteleros,16)-1),3)) %>%
  filter(anio==anio_ref) %>% 
  mutate(region_de_destino=factor(region_de_destino,
                                  levels =c("Total País","CABA","Buenos Aires",
                                            "Córdoba",
                                            "Cuyo","Litoral","Norte","Patagonia"))) %>% 
  arrange(region_de_destino) 


if (anio_ref==2021) {
  eoh10 <- eoh10 %>% select(region_de_destino,total_hoteleros,varia_1,varia_1b,`4_5_est`,varia_2,varia_2b,`3_est`,
                            varia_3,varia_3b,`1_2_est`,varia_4,varia_4b,para_hoteles,varia_5,varia_5b)
} else {
  eoh10 <- eoh10 %>% select(region_de_destino,total_hoteleros,varia_1,`4_5_est`,varia_2,`3_est`,
                            varia_3,`1_2_est`,varia_4,para_hoteles,varia_5)
} 

#Tabla GT (dato chequeado)

#Pernoctaciones por categoría, según región de destino. (MEJORAR)

eoh10 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             total_hoteleros = md("**TOTAL HOTE LEROS**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             `4_5_est` = md("**Hoteles 4 y 5 est.**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             `3_est` = md("**Hotel 3 est., bout., apart**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"),
             `1_2_est` = md("**Hotel 1 y 2 est.**"),
             varia_4 = md("**var % i.a**"),
             varia_4b = md("**var % vs 2019**"),
             para_hoteles = md("**PARA HOTE LEROS**"),
             varia_5 = md("**var % i.a**"),
             varia_5b = md("**var % vs 2019**"))%>%
  fmt_number(columns = c(2,5,8,11,14), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10,12,13,15,16), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8,11,14) ~ px(60),
             columns = c(3,4,6,7,9,10,12,13,15,16) ~ px(50),
             columns = c(1) ~ px(80)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b > 0
    ))%>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(
      rows = c(1)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    ))%>% 
  data_color(columns = c(total_hoteleros,para_hoteles),
             colors = dnmye_colores("gris claro"))




#Cuadro 4.2.8 Viajeros por categoría según región de destino. Variación interanual. 

eoh11_total <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE)) %>%
  pivot_wider(names_from = categoria_del_hotel,
              values_from = viaj_totales) %>%
  rename("1_2_est"="1 y 2 estrellas",
         "3_est"="3 estrellas/boutiques/aparts",
         "4_5_est"="4 y 5 estrellas",
         "para_hoteles"="Para Hoteles") %>% 
  mutate(total_hoteleros=sum(`1_2_est`,`3_est`,`4_5_est`),
         region_de_destino="Total País")


eoh11 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,region_de_destino,categoria_del_hotel) %>% 
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE)) %>%
  pivot_wider(names_from = categoria_del_hotel,
              values_from = viaj_totales) %>%
  rename("1_2_est"="1 y 2 estrellas",
         "3_est"="3 estrellas/boutiques/aparts",
         "4_5_est"="4 y 5 estrellas",
         "para_hoteles"="Para Hoteles") %>% 
  mutate(total_hoteleros=sum(`1_2_est`,`3_est`,`4_5_est`)) %>% 
  rbind(eoh11_total) %>%
  arrange(anio,region_de_destino) %>% 
  ungroup() %>% 
  mutate(varia_4=round((`1_2_est`/lag(`1_2_est`,8)-1),3),
         varia_3=round((`3_est`/lag(`3_est`,8)-1),3),
         varia_2=round((`4_5_est`/lag(`4_5_est`,8)-1),3),
         varia_5=round((`para_hoteles`/lag(`para_hoteles`,8)-1),3),
         varia_1=round((total_hoteleros/lag(total_hoteleros,8)-1),3),
         varia_4b=round((`1_2_est`/lag(`1_2_est`,16)-1),3),
         varia_3b=round((`3_est`/lag(`3_est`,16)-1),3),
         varia_2b=round((`4_5_est`/lag(`4_5_est`,16)-1),3),
         varia_5b=round((`para_hoteles`/lag(`para_hoteles`,16)-1),3),
         varia_1b=round((total_hoteleros/lag(total_hoteleros,16)-1),3)) %>%
  filter(anio==anio_ref)  %>% 
  mutate(region_de_destino=case_when(region_de_destino=="Total País"~"TOTAL PAÍS",
                                     TRUE~region_de_destino)) %>%  
  mutate(region_de_destino=factor(region_de_destino,
                                  levels =c("TOTAL PAÍS","CABA","Buenos Aires",
                                            "Córdoba",
                                            "Cuyo","Litoral","Norte","Patagonia"))) %>% 
  arrange(region_de_destino) %>% 
  mutate(`4_5_est`=round(`4_5_est`,0),
         `3_est`=round(`3_est`,0),
         `1_2_est`=round(`1_2_est`,0),
         `para_hoteles`=round(`para_hoteles`,0))


# select(region_de_destino,total_hoteleros,varia_1,`4_5_est`,varia_2,`3_est`,
#        varia_3,`1_2_est`,varia_4,para_hoteles,varia_5) 

if (anio_ref==2021) {
  eoh11 <- eoh11 %>% select(region_de_destino,total_hoteleros,varia_1,varia_1b,`4_5_est`,varia_2,varia_2b,`3_est`,
                            varia_3,varia_3b,`1_2_est`,varia_4,varia_4b,para_hoteles,varia_5,varia_5b)
} else {
  eoh11 <- eoh11 %>% select(region_de_destino,total_hoteleros,varia_1,`4_5_est`,varia_2,`3_est`,
                            varia_3,`1_2_est`,varia_4,para_hoteles,varia_5)
}

#Tabla GT (dato chequeado)

#Viajeros por categoría, según región de destino.

eoh11 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             total_hoteleros = md("**TOTAL HOTE LEROS**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             `4_5_est` = md("**Hoteles 4 y 5 est.**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             `3_est` = md("**Hotel 3 est., bout., apart**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"),
             `1_2_est` = md("**Hotel 1 y 2 est.**"),
             varia_4 = md("**var % i.a**"),
             varia_4b = md("**var % vs 2019**"),
             para_hoteles = md("**PARA HOTE LES**"),
             varia_5 = md("**var % i.a**"),
             varia_5b = md("**var % vs 2019**"))%>%
  fmt_number(columns = c(2,5,8,11,14), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10,12,13,15,16), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8,11,14) ~ px(60),
             columns = c(3,4,6,7,9,10,12,13,15,16) ~ px(50),
             columns = c(1) ~ px(80)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b > 0
    ))%>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(
      rows = c(1)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = 1
    )) %>% 
  data_color(columns = c(total_hoteleros,para_hoteles),
             colors = dnmye_colores("gris claro")) 

#Gráfico 4.2.4 Pernoctaciones y viajeros hospedados en establecimientos hoteleros
#y parahoteleros por categoría del establecimiento.


eoh12 <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1)| anio==(anio_ref-2)) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(Pernoctes=round(sum(p5_t2*wr,na.rm = TRUE)/1000000,1),
            Viajeros=round(sum(p5_t1*wr,na.rm = TRUE)/1000000,1)) %>%
  pivot_longer(cols=c(Pernoctes,Viajeros),
               names_to = "indicador",
               values_to="valor") %>% 
  mutate(categoria_del_hotel=case_when(categoria_del_hotel=="3 estrellas/boutiques/aparts"~"3 est/bout/apart",
                                       TRUE~categoria_del_hotel))

if (anio_ref==2021) {
  eoh12 <- eoh12 %>% filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2))
} else {
  eoh12 <- eoh12 %>% filter(anio==anio_ref | anio==(anio_ref-1))
}


# Gráfico ggplot (dato chequeado)

#Pernoctaciones y viajeros hospedados en establecimientos hoteleros y parahoteleros por categoría del establecimiento.

ggplot(data = eoh12%>% 
         mutate(anio=factor(anio,levels = c(anio_ref,anio_ref-1,anio_ref-2)),
                categoria_del_hotel=factor(categoria_del_hotel,
                                           levels = c("4 y 5 estrellas","3 est/bout/apart",
                                                      "1 y 2 estrellas","Para Hoteles"))),
       aes(x=categoria_del_hotel, y=valor,label=valor,fill=anio))+
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = 1),
            size=8,
            vjust = -0.5,
            hjust = 0.8)+
  facet_wrap(~indicador)+
  labs(title = "",
       subtitle = glue("TOTAL PAIS. {anio_ref-2}-{anio_ref}."),
       x = "",
       y = "(millones)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro")),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro")),
        plot.title=element_text(hjust = 0,face="bold"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0),
        legend.position = "bottom",
        text=element_text(size=20))+
  scale_fill_manual(values = c(dnmye_colores("rosa"),
                               dnmye_colores("cian"),
                               dnmye_colores("purpura")))+
  scale_y_continuous(limits = c(0, (max(eoh12$valor))+2))

#Cuadro 4.2.9 Pernoctes y viajeros hospedados por categoría según condición
#de residencia. Variación interanual. 

eoh13_hoteles <- eoh %>%
  filter((anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) & categoria_del_hotel!="Para Hoteles") %>% 
  group_by(anio) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  mutate(categoria_del_hotel="Total Hoteleros")


eoh13 <- eoh %>%
  filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=round(sum(p5_tn2*wr,na.rm = TRUE),0),
            viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=round(sum(p5_tn1*wr,na.rm = TRUE),0)) %>% 
  rbind(eoh13_hoteles) %>% 
  arrange(anio,categoria_del_hotel) %>%
  ungroup() %>% 
  mutate(varia_1=round((pern_totales/lag(pern_totales,5)-1),3),
         varia_2=round((pern_res/lag(pern_res,5)-1),3),
         varia_3=round((pern_nores/lag(pern_nores,5)-1),3),
         varia_4=round((viaj_totales/lag(viaj_totales,5)-1),3),
         varia_5=round((viaj_res/lag(viaj_res,5)-1),3),
         varia_6=round((viaj_nores/lag(viaj_nores,5)-1),3),
         varia_1b=round((pern_totales/lag(pern_totales,10)-1),3),
         varia_2b=round((pern_res/lag(pern_res,10)-1),3),
         varia_3b=round((pern_nores/lag(pern_nores,10)-1),3),
         varia_4b=round((viaj_totales/lag(viaj_totales,10)-1),3),
         varia_5b=round((viaj_res/lag(viaj_res,10)-1),3),
         varia_6b=round((viaj_nores/lag(viaj_nores,10)-1),3)) %>% 
  filter(anio==anio_ref)%>%
  mutate(categoria_del_hotel=case_when(categoria_del_hotel=="Total Hoteleros"~"TOTAL HOTELEROS",
                                       categoria_del_hotel=="3 estrellas/boutiques/aparts"~"3 est/bout/apart",
                                       categoria_del_hotel=="Para Hoteles"~"PARA HOTELEROS",
                                       TRUE~categoria_del_hotel)) %>% 
  mutate(categoria_del_hotel=factor(categoria_del_hotel,
                                    levels =c("TOTAL HOTELEROS","4 y 5 estrellas","3 est/bout/apart",
                                              "1 y 2 estrellas","PARA HOTELEROS"))) %>% 
  arrange(categoria_del_hotel)

if (anio_ref==2021) {
  eoh13 <- eoh13 %>% select(categoria_del_hotel,pern_totales,varia_1,varia_1b,pern_res,varia_2,varia_2b,pern_nores,varia_3,varia_3b,
                            viaj_totales,varia_4,varia_4b,viaj_res,varia_5,varia_5b,viaj_nores,varia_6,varia_6b)
} else {
  eoh13 <- eoh13 %>% select(categoria_del_hotel,pern_totales,varia_1,pern_res,varia_2,pern_nores,varia_3,
                            viaj_totales,varia_4,viaj_res,varia_5,viaj_nores,varia_6)
}



#Cuadro en ggt (dato chequeado) (MEJORAR)

#Pernoctes y viajeros hospedados, por categoría según condición de residencia.

eoh13 %>% 
  gt()%>% 
  cols_label(categoria_del_hotel = md("**Categoría**"),
             pern_totales = md("**Totales**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             pern_res = md("**Residentes**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             pern_nores = md("**No residentes**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"),
             viaj_totales = md("**Totales**"),
             varia_4 = md("**var % i.a**"),
             varia_4b = md("**var % vs 2019**"),
             viaj_res = md("**Residentes**"),
             varia_5 = md("**var % i.a**"),
             varia_5b = md("**var % vs 2019**"),
             viaj_nores = md("**No residentes**"),
             varia_6 = md("**var % i.a**"),
             varia_6b = md("**var % vs 2019**"))%>%
  fmt_number(columns = c(2,5,8,11,14,17), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6,7,9,10,12,13,15,16,18,19), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8,11,14) ~ px(50),
             columns = c(3,4,6,7,9,10,12,13,15,16) ~ px(50),
             columns = c(1) ~ px(60)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("TOTAL PAÍS, {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4),
      rows = varia_4 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_4b),
      rows = varia_4b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5),
      rows = varia_5 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_5b),
      rows = varia_5b > 0
    ))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_6),
      rows = varia_6 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_6),
      rows = varia_6 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_6b),
      rows = varia_6b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_6b),
      rows = varia_6b > 0
    ))%>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(
      rows = c(1,5)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = c(1,5)
    )) %>% 
  tab_spanner(label = "PERNOCTES", columns = c(2:10)) %>% 
  tab_spanner(label = "VIAJEROS", columns = c(11:19))


#Cuadro eoh14 Cantidad de establecimientos promedio, plazas disponibles y ocupación de plazas.


eoh14 <- eoh %>%
  filter(anio>=2010 & anio<=anio_ref) %>% 
  group_by(anio) %>% 
  summarise(establecimientos=round(sum(establecimientos*wr,na.rm = TRUE)/12,0),
            plazas_disp=sum(plazas_disp*wr,na.rm = TRUE),
            pernoctes=sum(p5_t2*wr,na.rm = TRUE)) %>% 
  mutate(establecimientos=case_when(anio==2010~6370,
                                    anio==2011~5842,
                                    TRUE~establecimientos),
         plazas_disp=case_when(anio==2010~140669330,
                               anio==2011~138582276,
                               TRUE~plazas_disp)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>% 
  mutate(varia_1=round((establecimientos/lag(establecimientos,1)-1),3),
         varia_2=round((plazas_disp/lag(plazas_disp,1)-1),3),
         varia_3=(top-lag(top,1))*100,
         varia_1b=round((establecimientos/lag(establecimientos,2)-1),3),
         varia_2b=round((plazas_disp/lag(plazas_disp,2)-1),3),
         varia_3b=(top-lag(top,2))*100) %>%
  mutate(varia_1b=case_when(anio==2021~varia_1b,
                            TRUE~NA_real_),
         varia_2b=case_when(anio==2021~varia_2b,
                            TRUE~NA_real_),
         varia_3b=case_when(anio==2021~varia_3b,
                            TRUE~NA_real_)) %>% 
  mutate(varia_1=case_when(anio==2010~0.006,
                           TRUE~varia_1),
         varia_2=case_when(anio==2010~0.018,
                           TRUE~varia_2),
         varia_3=case_when(anio==2010~13.0,
                           TRUE~varia_3))

if (anio_ref==2021) {
  eoh14 <- eoh14 %>% select(anio,establecimientos,varia_1,varia_1b,plazas_disp,varia_2,varia_2b,top,varia_3,varia_3b)
} else {
  eoh14 <- eoh14 %>% select(anio,establecimientos,varia_1,plazas_disp,varia_2,top,varia_3)
}


#Tabla GT (dato chequeado)

#Cantidad de establecimientos promedio, plazas disponibles y ocupación de plazas.

eoh14 %>%
  gt()%>% 
  cols_label(anio = md("**Año**"),
             establecimientos = md("**Establecimientos (prom. mensual)**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             plazas_disp = md("**Plazas disponibles**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             top = md("**Tasa de ocup. en plazas**"),
             varia_3 = md("**var i.a (en p.p.)**"),
             varia_3b = md("**var vs 2019 (en p.p.)**"))  %>%
  fmt_number(columns = c(2,5), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_number(columns = c(9:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:8), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(100),
             columns = c(3,4,6:8,9,10) ~ px(70)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS. 2010-{anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))%>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = nrow(eoh14))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(eoh14)
    ))



#Cuadro 4.2.11 Cantidad de establecimientos promedio, plazas disponibles y ocupación de plazas
#por región de destino


eoh15 <- eoh %>%
  filter(anio==anio_ref |anio==anio_ref-1 |anio==anio_ref-2) %>% 
  group_by(anio,region_de_destino) %>% 
  summarise(establecimientos=round(sum(establecimientos*wr,na.rm = TRUE)/12,0),
            plazas_disp=sum(plazas_disp*wr,na.rm = TRUE),
            pernoctes=sum(p5_t2*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>%
  ungroup() %>% 
  mutate(varia_1=round((establecimientos/lag(establecimientos,7)-1),3),
         varia_2=round((plazas_disp/lag(plazas_disp,7)-1),3),
         varia_3=(top-lag(top,7)),
         varia_1b=round((establecimientos/lag(establecimientos,14)-1),3),
         varia_2b=round((plazas_disp/lag(plazas_disp,14)-1),3),
         varia_3b=(top-lag(top,14))) %>%
  filter(anio==anio_ref)

if (anio_ref==2021) {
  eoh15 <- eoh15 %>% select(region_de_destino,establecimientos,varia_1,varia_1b,plazas_disp,varia_2,varia_2b,top,varia_3,varia_3b)
} else {
  eoh15 <- eoh15 %>% select(region_de_destino,establecimientos,varia_1,plazas_disp,varia_2,top,varia_3)
}

#Tabla GT (dato chequeado)

#Cantidad de establecimientos promedio, plazas disponibles y ocupación de plazas, según región de destino.

eoh15 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             establecimientos = md("**Establecimientos (prom. mensual)**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             plazas_disp = md("**Plazas disponibles**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             top = md("**Tasa de ocup. en plazas**"),
             varia_3 = md("**var i.a. (en p.p.)**"),
             varia_3b = md("**var vs 2019 (en p.p.)**"))%>%
  fmt_number(columns = c(2,5), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(120),
             columns = c(3,4,6,7,9,10) ~ px(80),
             columns = c(1) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}"),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))

#Cuadro 4.2.12 Cantidad de establecimientos promedio, plazas disponibles
#y tasa de ocupación por categoría.   

eoh16_hoteleros <- eoh %>%
  filter((anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) & categoria_del_hotel!="Para Hoteles") %>%  
  group_by(anio) %>% 
  summarise(establecimientos=sum(establecimientos*wr,na.rm = TRUE)/12,
            plazas_disp=sum(plazas_disp*wr,na.rm = TRUE),
            pernoctes=sum(p5_t2*wr,na.rm = TRUE)) %>% 
  mutate(categoria_del_hotel="Total Hoteleros")


eoh16 <- eoh %>%
  filter(anio==anio_ref |anio==anio_ref-1 |anio==anio_ref-2) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(establecimientos=sum(establecimientos*wr,na.rm = TRUE)/12,
            plazas_disp=sum(plazas_disp*wr,na.rm = TRUE),
            pernoctes=sum(p5_t2*wr,na.rm = TRUE)) %>%
  rbind(eoh16_hoteleros) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>%
  arrange(anio,categoria_del_hotel) %>% 
  ungroup() %>% 
  mutate(varia_1=round((establecimientos/lag(establecimientos,5)-1),3),
         varia_2=round((plazas_disp/lag(plazas_disp,5)-1),3),
         varia_3=(top-lag(top,5)),
         varia_1b=round((establecimientos/lag(establecimientos,10)-1),3),
         varia_2b=round((plazas_disp/lag(plazas_disp,10)-1),3),
         varia_3b=(top-lag(top,10))) %>%
  mutate(categoria_del_hotel=case_when(categoria_del_hotel=="3 estrellas/boutiques/aparts"~"3 est/bout/apart",
                                       TRUE~categoria_del_hotel)) %>% 
  filter(anio==anio_ref) %>%mutate(categoria_del_hotel=factor(categoria_del_hotel,
                                                              levels =c("Total Hoteleros","4 y 5 estrellas","3 est/bout/apart",
                                                                        "1 y 2 estrellas","Para Hoteles"))) %>% 
  arrange(categoria_del_hotel)

if (anio_ref==2021) {
  eoh16 <- eoh16 %>% select(categoria_del_hotel,establecimientos,varia_1,varia_1b,plazas_disp,varia_2,varia_2b,top,varia_3,varia_3b)
} else {
  eoh16 <- eoh16 %>% select(categoria_del_hotel,establecimientos,varia_1,plazas_disp,varia_2,top,varia_3)
}


#Tabla GT (dato chequeado)

#Cantidad de establecimientos promedio, plazas disponibles y ocupación de plazas, según región de destino.

eoh16 %>%
  gt()%>% 
  cols_label(categoria_del_hotel = md("**Categoría**"),
             establecimientos = md("**Establecimientos (prom. mensual)**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             plazas_disp = md("**Plazas disponibles**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             top = md("**Tasa de ocup. en plazas**"),
             varia_3 = md("**var en p.p.**"),
             varia_3b = md("**var en p.p.**"))%>%
  fmt_number(columns = c(2,5), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(120),
             columns = c(3,4,6,7,9,10) ~ px(80),
             columns = c(1) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = c(-1)) %>%
  cols_align(
    align = "left",
    columns = c(1)) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}"),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))%>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = c(1,5))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = c(1,5)
    ))



##########################################HASTA ACÁ LLEGA LA INFO DEL ANUARIO################

##############################################################################################


##########TABLAS NUEVAS:



##########Tasas de ocupación en plazas en FDS


eoh17 <- eoh %>%
  filter(anio>=2018 & anio<=anio_ref) %>% 
  group_by(anio) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>%
  mutate(top=case_when(is.na(top)~0,
                       TRUE~top)) %>%
  ungroup() %>% 
  mutate(varia_1=(top-lag(top,1))) %>% 
  mutate(varia_1b=case_when(anio==2021~top-lag(top,2)))

if (anio_ref==2021) {
  eoh17 <- eoh17 %>% select(anio,top,varia_1,varia_1b)
} else {
  eoh17 <- eoh17 %>% select(anio,top,varia_1)
}


#Tabla GT (dato chequeado)

#Tasa de ocupación en plazas en fines de semana (FDS*).

eoh17 %>%
  gt()%>% 
  cols_label(anio = md("**Año**"),
             top = md("**Tasa de ocupación en plazas en FDS**"),
             varia_1 = md("**var en p.p.**"),
             varia_1b = md("**var en p.p vs 2019**"))  %>%
  fmt_percent(columns = c(2:4), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2) ~ px(100),
             columns = c(3,4) ~ px(70)) %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("TOTAL PAIS. 2018-{anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    ))%>%
  sub_missing(
    columns = everything(),
    missing_text = "///")%>% 
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) %>%
  tab_style(
    style = cell_fill(dnmye_colores("gris claro")),
    locations = cells_body(rows = nrow(eoh17))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight =  "bold")
    ),
    locations = cells_body(rows = nrow(eoh17)
    )) %>%
  tab_source_note(
    source_note = "(///) Dato que no corresponde presentar debido a la naturaleza de las cosas") %>%
  tab_source_note(
    source_note = "(*) Datos correspondientes a los días viernes y sábados del mes") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))


#Tasas de ocupación en plazas en fds por categoría 2018-2021


eoh18_a <- eoh %>%
  filter(anio>=2018 & anio<=anio_ref) %>%
  group_by(anio) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>% 
  mutate(categoria_del_hotel="TOTAL")


eoh18 <- eoh %>%
  filter(anio>=2018 & anio<=anio_ref) %>% 
  group_by(anio,categoria_del_hotel) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>% 
  rbind(eoh18_a) %>%
  ungroup() %>% 
  select(anio,categoria_del_hotel,top) %>% 
  mutate(categoria_del_hotel=factor(categoria_del_hotel,levels = c("TOTAL","4 y 5 estrellas",
                                                                   "3 estrellas/boutiques/aparts",
                                                                   "1 y 2 estrellas","Para Hoteles"))) %>% 
  arrange(anio,categoria_del_hotel) %>%
  mutate(varia_1=(top-lag(top,5))) %>% 
  mutate(varia_1b=case_when(anio==2021~top-lag(top,10))) %>% 
  filter(anio==2021)



if (anio_ref==2021) {
  eoh18 <- eoh18 %>% select(categoria_del_hotel,top,varia_1,varia_1b)
} else {
  eoh18 <- eoh18 %>% select(categoria_del_hotel,top,varia_1)
}



#Tabla GT (chequeado)

#Tasa de ocupación en plazas en fines de semana, según categoría.

eoh18 %>%
  gt()%>% 
  cols_label(categoria_del_hotel = md("**Categoría**"),
             top = md("**Tasa de ocupación en plazas en FDS**"),
             varia_1 = md("**var en p.p.**"),
             varia_1b = md("**var en p.p vs 2019**"))  %>%
  fmt_percent(columns = c(2:4), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2) ~ px(120),
             columns = c(3,4) ~ px(100)) %>% 
  cols_align(
    align = "center",
    columns = everything()) %>% 
  cols_align(
    align = "left",
    columns = 1) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    ))%>%
  sub_missing(
    columns = everything(),
    missing_text = "///")%>% 
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) %>%
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
  tab_source_note(
    source_note = "(*) Datos correspondientes a los días viernes y sábados del mes") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))

#Tasas de ocupación en plazas en fds por región 2018-2021


eoh19_a <- eoh %>%
  filter(anio>=2018 & anio<=anio_ref) %>%
  group_by(anio) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>% 
  mutate(region_de_destino="TOTAL")


eoh19 <- eoh %>%
  filter(anio>=2018 & anio<=anio_ref) %>% 
  group_by(anio,region_de_destino) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>% 
  rbind(eoh19_a) %>%
  ungroup() %>% 
  select(anio,region_de_destino,top) %>% 
  mutate(region_de_destino=factor(region_de_destino,levels = c("TOTAL","Buenos Aires",
                                                               "CABA","Córdoba","Cuyo",
                                                               "Litoral","Norte","Patagonia"))) %>% 
  arrange(anio,region_de_destino) %>%
  mutate(varia_1=(top-lag(top,8))) %>% 
  mutate(varia_1b=case_when(anio==2021~top-lag(top,16))) %>% 
  filter(anio==2021)


if (anio_ref==2021) {
  eoh19 <- eoh19 %>% select(region_de_destino,top,varia_1,varia_1b)
} else {
  eoh19 <- eoh19 %>% select(region_de_destino,top,varia_1)
}


#Tabla GT (dato chequeado)

# Tasa de ocupación en plazas en fines de semana (*), según región de destino.

eoh19 %>%
  gt()%>% 
  cols_label(region_de_destino = md("**Región**"),
             top = md("**Tasa de ocupación en plazas en FDS**"),
             varia_1 = md("**var en p.p.**"),
             varia_1b = md("**var en p.p vs 2019**"))  %>%
  fmt_percent(columns = c(2:4), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2) ~ px(120),
             columns = c(3,4) ~ px(100)) %>% 
  cols_align(
    align = "center",
    columns = everything()) %>% 
  cols_align(
    align = "left",
    columns = 1) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    ))%>%
  sub_missing(
    columns = everything(),
    missing_text = "///")%>% 
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) %>%
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
  tab_source_note(
    source_note = "(*) Datos correspondientes a los días viernes y sábados del mes") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))


#Tasa de ocupación en plazas por mes


eoh20 <- eoh %>%
  filter(anio==anio_ref |anio==anio_ref-1 |anio==anio_ref-2) %>% 
  group_by(indice_de_tiempo) %>% 
  summarise(establecimientos=round(sum(establecimientos*wr,na.rm = TRUE),0),
            plazas_disp=sum(plazas_disp*wr,na.rm = TRUE),
            pernoctes=sum(p5_t2*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>%
  ungroup() %>% 
  mutate(varia_1=round((establecimientos/lag(establecimientos,12)-1),3),
         varia_2=round((plazas_disp/lag(plazas_disp,12)-1),3),
         varia_3=(top-lag(top,12))*100,
         varia_1b=round((establecimientos/lag(establecimientos,24)-1),3),
         varia_2b=round((plazas_disp/lag(plazas_disp,24)-1),3),
         varia_3b=(top-lag(top,24))*100) %>% 
  mutate(varia_1=case_when(is.infinite(varia_1)~NA_real_,
                           TRUE~varia_1),
         varia_2=case_when(is.infinite(varia_2)~NA_real_,
                           TRUE~varia_2),
         varia_3=case_when(is.infinite(varia_3)~NA_real_,
                           TRUE~varia_3)) %>%  
  filter(year(indice_de_tiempo)==anio_ref) %>% 
  mutate(mes=month(indice_de_tiempo)) %>% 
  mutate(mes=case_when(mes==1~"Ene",mes==2~"Feb",mes==3~"Mar",mes==4~"Abr",
                       mes==5~"May",mes==6~"Jun",mes==7~"Jul",mes==8~"Ago",
                       mes==9~"Sep",mes==10~"Oct",mes==11~"Nov",mes==12~"Dic")) 

if (anio_ref==2021) {
  eoh20 <- eoh20 %>% select(mes,establecimientos,varia_1,varia_1b,plazas_disp,varia_2,varia_2b,top,varia_3,varia_3b)
} else {
  eoh20 <- eoh20 %>% select(mes,establecimientos,varia_1,plazas_disp,varia_2,top,varia_3)
}


#CUADROS (dato chequeado)

# Cantidad de establecimientos, plazas disponibles y ocupación de plazas, por mes.

eoh20 %>%
  gt()%>% 
  cols_label(mes= md("**Mes**"),
             establecimientos = md("**Establecimientos**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             plazas_disp = md("**Plazas disponibles**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             top = md("**Tasa de ocup. en plazas**"),
             varia_3 = md("**var p.p**"),
             varia_3b = md("**var p.p vs 2019**"))  %>%
  fmt_number(columns = c(2,5), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_number(columns = c(9:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3,4,6:8), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(120),
             columns = c(3,4,6:8,9,10) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  )%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>%  
  cols_align(
    align = "left",
    columns = 1)  %>%
  tab_source_note(
    source_note = "(///) Dato que no corresponde presentar debido a la naturaleza de las cosas") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))



#Tasa de ocupación en plazas en fds por mes


eoh21 <- eoh %>%
  filter(anio==anio_ref |anio==anio_ref-1 |anio==anio_ref-2) %>% 
  group_by(indice_de_tiempo) %>% 
  summarise(plazas_disp=sum(plazas_dispfds*wr,na.rm = TRUE),
            pernoctes=sum(p1_33*wr,na.rm = TRUE)) %>% 
  mutate(top=(pernoctes/plazas_disp)) %>%
  ungroup() %>% 
  mutate(varia_2=round((plazas_disp/lag(plazas_disp,12)-1),3),
         varia_3=(top-lag(top,12)),
         varia_2b=round((plazas_disp/lag(plazas_disp,24)-1),3),
         varia_3b=(top-lag(top,24))) %>% 
  mutate(varia_2=case_when(is.infinite(varia_2)~NA_real_,
                           TRUE~varia_2),
         varia_3=case_when(is.infinite(varia_3)~NA_real_,
                           TRUE~varia_3)) %>%  
  filter(year(indice_de_tiempo)==anio_ref) %>% 
  mutate(mes=month(indice_de_tiempo)) %>% 
  mutate(mes=case_when(mes==1~"Ene",mes==2~"Feb",mes==3~"Mar",mes==4~"Abr",
                       mes==5~"May",mes==6~"Jun",mes==7~"Jul",mes==8~"Ago",
                       mes==9~"Sep",mes==10~"Oct",mes==11~"Nov",mes==12~"Dic")) 

if (anio_ref==2021) {
  eoh21 <- eoh21 %>% select(mes,top,varia_3,varia_3b)
} else {
  eoh21 <- eoh21 %>% select(mes,top,varia_3)
}


#CUADRO (dato chequeado)

# Tasas de ocupación en plazas en fines de semana (*),por mes.

eoh21 %>%
  gt()%>% 
  cols_label(mes= md("**Mes**"),
             top = md("**Tasa de ocup. en plazas**"),
             varia_3 = md("**var p.p**"),
             varia_3b = md("**var p.p vs 2019**"))  %>%
  fmt_percent(columns = c(2:4), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2) ~ px(120),
             columns = c(3,4) ~ px(100),
             columns = c(1) ~ px(100)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}."),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = "(*) Datos correspondientes a los días viernes y sábados del mes") %>%
  tab_source_note(
    source_note = "(///) Dato que no corresponde presentar debido a la naturaleza de las cosas") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))%>%  
  cols_align(
    align = "left",
    columns = 1) %>%  
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) 




##########ESTACIONALIDAD DE LOS DATOS ##################

#Pernoctaciones según residencia por mes


eoh22 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,indice_de_tiempo) %>%
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(varia_1=(pern_totales/lag(pern_totales,12)-1),
         varia_2=(pern_res/lag(pern_res,12)-1),
         varia_3=(pern_nores/lag(pern_nores,12)-1),
         varia_1b=(pern_totales/lag(pern_totales,24)-1),
         varia_2b=(pern_res/lag(pern_res,24)-1),
         varia_3b=(pern_nores/lag(pern_nores,24)-1),
         varia_1b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_1b),
         varia_2b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_2b),
         varia_3b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_3b)) %>%
  filter(anio==anio_ref) %>% 
  mutate(varia_1=case_when(is.infinite(varia_1)~NA_real_,
                           TRUE~varia_1),
         varia_2=case_when(is.infinite(varia_2)~NA_real_,
                           TRUE~varia_2),
         varia_3=case_when(is.infinite(varia_3)~NA_real_,
                           TRUE~varia_3))%>% 
  mutate(mes=month(indice_de_tiempo)) %>% 
  mutate(mes=case_when(mes==1~"Ene",mes==2~"Feb",mes==3~"Mar",mes==4~"Abr",
                       mes==5~"May",mes==6~"Jun",mes==7~"Jul",mes==8~"Ago",
                       mes==9~"Sep",mes==10~"Oct",mes==11~"Nov",mes==12~"Dic")) %>% 
  select(mes,everything(),-c(anio,indice_de_tiempo))



#Selección de variaciones vs 2019 para el anuario 2021.

if (anio_ref==2021) {
  eoh22 <- eoh22 %>% select(mes,pern_totales,varia_1,varia_1b,pern_res,varia_2,varia_2b,pern_nores,varia_3,varia_3b)
} else {
  eoh22 <- eoh22 %>% select(mes,pern_totales,varia_1,pern_res,varia_2,pern_nores,varia_3)
}


#TABLA GT (dato chequeado)






#Viajeros según residencia por mes

eoh23 <- eoh %>%
  filter(anio==anio_ref | anio==anio_ref-1 | anio==anio_ref-2) %>% 
  group_by(anio,indice_de_tiempo) %>%
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(varia_1=(viaj_totales/lag(viaj_totales,12)-1),
         varia_2=(viaj_res/lag(viaj_res,12)-1),
         varia_3=(viaj_nores/lag(viaj_nores,12)-1),
         varia_1b=(viaj_totales/lag(viaj_totales,24)-1),
         varia_2b=(viaj_res/lag(viaj_res,24)-1),
         varia_3b=(viaj_nores/lag(viaj_nores,24)-1),
         varia_1b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_1b),
         varia_2b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_2b),
         varia_3b=case_when(anio != anio_ref~NA_real_,
                            TRUE~varia_3b)) %>%
  filter(anio==anio_ref) %>% 
  mutate(varia_1=case_when(is.infinite(varia_1)~NA_real_,
                           TRUE~varia_1),
         varia_2=case_when(is.infinite(varia_2)~NA_real_,
                           TRUE~varia_2),
         varia_3=case_when(is.infinite(varia_3)~NA_real_,
                           TRUE~varia_3))%>% 
  mutate(mes=month(indice_de_tiempo)) %>% 
  mutate(mes=case_when(mes==1~"Ene",mes==2~"Feb",mes==3~"Mar",mes==4~"Abr",
                       mes==5~"May",mes==6~"Jun",mes==7~"Jul",mes==8~"Ago",
                       mes==9~"Sep",mes==10~"Oct",mes==11~"Nov",mes==12~"Dic")) %>% 
  select(mes,everything(),-c(anio,indice_de_tiempo))



#Selección de variaciones vs 2019 para el anuario 2021.

if (anio_ref==2021) {
  eoh23 <- eoh23 %>% select(mes,viaj_totales,varia_1,varia_1b,viaj_res,varia_2,varia_2b,viaj_nores,varia_3,varia_3b)
} else {
  eoh23 <- eoh23 %>% select(mes,viaj_totales,varia_1,viaj_res,varia_2,viaj_nores,varia_3)
}



#TABLA GT (dato chequeado)

#Viajeros según residencia, por mes.

eoh23 %>%
  gt()%>% 
  cols_label(mes= md("**Mes**"),
             viaj_totales = md("**Viajeros totales**"),
             varia_1 = md("**var p.p**"),
             varia_1b = md("**var p.p vs 2019**"),
             viaj_res = md("**Viajeros residentes**"),
             varia_2 = md("**var p.p**"),
             varia_2b = md("**var p.p vs 2019**"),
             viaj_nores = md("**Viajeros no residentes**"),
             varia_3 = md("**var p.p**"),
             varia_3b = md("**var p.p vs 2019**"))  %>%
  fmt_number(columns = c(2,5,8), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(3:4,6:7,9:10), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(2,5,8) ~ px(100),
             columns = c(3:4,6:7,9:10) ~ px(80),
             columns = c(1) ~ px(80)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("AÑO {anio_ref}"),
    title = toupper(md(""))
  ) %>%
  tab_source_note(
    source_note = "(///) Dato que no corresponde presentar debido a la naturaleza de las cosas") %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    ))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    ))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    ))%>%  
  cols_align(
    align = "left",
    columns = 1) %>%  
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) 



#Pernoctaciones según residencia por región de destino por mes (MUCHA INFORMACIÓN)


eoh24 <- eoh %>%
  filter(anio==anio_ref) %>% 
  group_by(region_de_destino,indice_de_tiempo) %>%
  summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
            pern_res=sum(p5_tr2*wr,na.rm = TRUE),
            pern_nores=sum(p5_tn2*wr,na.rm = TRUE)) %>% 
  pivot_longer(cols=c(pern_totales,pern_res,pern_nores),
               names_to = "indicador",
               values_to="valor") %>% 
  mutate(indicador=case_when(indicador=="pern_totales"~"Totales",
                             indicador=="pern_res"~"Residentes",
                             indicador=="pern_nores"~"No residentes"))

#Ggplot para pernoctaciones totales con facet por región (dato chequeado)

ggplot(data=eoh24 %>% filter(indicador !="Totales"),aes(x=indice_de_tiempo,y=valor/1000000,color=indicador))+
  geom_line(linewidth=1,alpha = 0.4)+
  geom_point(size=2,alpha = 0.8) +
  facet_wrap(~region_de_destino)+
  labs(title = "",
       subtitle = glue("AÑO {anio_ref}."),
       x = "",
       y = "(en millones)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro"), size=15),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro"),size = 15),
        plot.title=element_text(hjust = 0,face="bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color="black",fill = NA),
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.3,
                                          linetype = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text=element_text(face='bold', size=15, hjust=0, color='black')) + 
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b",
               expand = c(0,10))


#Viajeros según residencia por región de destino por mes

viaj_2 <- eoh %>%
  filter(anio>=anio_ref-2) %>% 
  group_by(indice_de_tiempo,region_de_destino) %>%
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(varia_1=(viaj_totales/lag(viaj_totales,84)-1),
         varia_2=(viaj_res/lag(viaj_res,84)-1),
         varia_3=(viaj_nores/lag(viaj_nores,84)-1),
         varia_1b=(viaj_totales/lag(viaj_totales,168)-1),
         varia_2b=(viaj_res/lag(viaj_res,168)-1),
         varia_3b=(viaj_nores/lag(viaj_nores,168)-1)) %>% 
  mutate(varia_1=case_when(is.infinite(varia_1)~NA_real_,
                           TRUE~varia_1),
         varia_2=case_when(is.infinite(varia_2)~NA_real_,
                           TRUE~varia_2),
         varia_3=case_when(is.infinite(varia_3)~NA_real_,
                           TRUE~varia_3)) %>% 
  filter(year(indice_de_tiempo)==anio_ref) %>% 
  mutate(mes=month(indice_de_tiempo)) %>%
  select(mes,region_de_destino,viaj_totales,varia_1,varia_1b,viaj_res,varia_2,varia_2b,viaj_nores,varia_3,varia_3b)


#Cuadro GT (dato chequeado, pero es mucha información)

viaj_2 %>%
  gt(groupname_col = "region_de_destino")%>% 
  cols_label(mes = md("**Mes**"),
             viaj_totales = md("**Totales**"),
             varia_1 = md("**var % i.a**"),
             varia_1b = md("**var % vs 2019**"),
             viaj_res = md("**Residentes**"),
             varia_2 = md("**var % i.a**"),
             varia_2b = md("**var % vs 2019**"),
             viaj_nores = md("**No residentes**"),
             varia_3 = md("**var % i.a**"),
             varia_3b = md("**var % vs 2019**"))  %>%
  fmt_number(columns = c(3,6,9), decimals = 0, dec_mark = ",", sep_mark = ".") %>%
  fmt_percent(columns = c(4:5,7:8,10:11), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
  tab_options(table.font.size = 12,
              data_row.padding = px(3),
              column_labels.font.size = 14,
              column_labels.font.weight = "bold") %>% 
  cols_width(columns = c(3,6,9) ~ px(120),
             columns = c(4:5,7:8,10:11) ~ px(100),
             columns = c(1) ~ px(180)) %>%
  sub_missing(
    columns = everything(),
    missing_text = "///") %>% 
  cols_align(
    align = "center",
    columns = everything()) %>%
  tab_header(
    subtitle = glue("{anio_ref}"),
    title = toupper(md("__Viajeros por condición de residencia y región de destino.__"))
  ) %>%
  tab_source_note(
    source_note = md(
      "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1),
      rows = varia_1 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_1b),
      rows = varia_1b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2),
      rows = varia_2 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_2b),
      rows = varia_2b > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3),
      rows = varia_3 > 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("rosa"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b <= 0
    )) %>% 
  tab_style(
    style = list(
      cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
    ),
    locations = cells_body(
      columns = c(varia_3b),
      rows = varia_3b > 0
    )) %>% 
  opt_all_caps() %>% 
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.subtitle.font.size = 12,
    heading.align = "left",
    column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
    column_labels.border.bottom.width= px(3)) 

#Gráfico en ggplot


eoh25 <- eoh %>%
  filter(anio==anio_ref) %>% 
  group_by(region_de_destino,indice_de_tiempo) %>%
  summarise(viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
            viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
            viaj_nores=sum(p5_tn1*wr,na.rm = TRUE)) %>% 
  pivot_longer(cols=c(viaj_totales,viaj_res,viaj_nores),
               names_to = "indicador",
               values_to="valor") %>% 
  mutate(indicador=case_when(indicador=="viaj_totales"~"Totales",
                             indicador=="viaj_res"~"Residentes",
                             indicador=="viaj_nores"~"No residentes"))

#Ggplot para viajeros totales con facet por región (dato chequeado)

ggplot(data=eoh25 %>% filter(indicador !="Totales"),aes(x=indice_de_tiempo,y=valor/1000000,color=indicador))+
  geom_line(linewidth=1,alpha = 0.4)+
  geom_point(size=2,alpha = 0.8)+
  facet_wrap(~region_de_destino)+
  labs(title = "",
       subtitle = glue("AÑO {anio_ref}."),
       x = "",
       y = "(en millones)",
       colour="",
       caption = "Fuente: Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC).")+
  theme(plot.caption  = element_text(hjust = 0),
        plot.subtitle=element_text(colour = dnmye_colores("gris oscuro"),size=15),
        axis.title.y = element_text(colour = dnmye_colores("gris oscuro"),size=15),
        plot.title=element_text(hjust = 0,face="bold"),
        legend.title = element_blank(),
        legend.position = "bottom", panel.border = element_rect(color="black",fill = NA),
        panel.grid.major.y = element_line(color = "grey",
                                          linewidth = 0.3,
                                          linetype = 4),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text=element_text(face='bold', size=15, hjust=0, color='black')) + 
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b",
               expand = c(0,10))



#Estadía Promedio según residencia por región de destino por mes (mucha información)
# 
# 
# em_1 <- eoh %>%
#   filter(anio==anio_ref | anio==(anio_ref-1) | anio==(anio_ref-2)) %>% 
#   group_by(indice_de_tiempo,region_de_destino) %>% 
#   summarise(pern_totales=sum(p5_t2*wr,na.rm = TRUE),
#             pern_res=sum(p5_tr2*wr,na.rm = TRUE),
#             pern_nores=sum(p5_tn2*wr,na.rm = TRUE),
#             viaj_totales=sum(p5_t1*wr,na.rm = TRUE),
#             viaj_res=sum(p5_tr1*wr,na.rm = TRUE),
#             viaj_nores=sum(p5_tn1*wr,na.rm = TRUE),
#             em_total=pern_totales/viaj_totales,
#             em_res=pern_res/viaj_res,
#             em_nores=pern_nores/viaj_nores) %>% 
#   ungroup() %>% 
#   select(indice_de_tiempo,region_de_destino,em_total,em_res,em_nores)%>%
#   mutate(varia_1=(em_total/lag(em_total,84)-1),
#          varia_2=(em_res/lag(em_res,84)-1),
#          varia_3=(em_nores/lag(em_nores,84)-1),
#          varia_1b=(em_total/lag(em_total,168)-1),
#          varia_2b=(em_res/lag(em_res,168)-1),
#          varia_3b=(em_nores/lag(em_nores,168)-1)) %>% 
#   mutate(varia_1=case_when(is.infinite(varia_1)~NA_real_,
#                            TRUE~varia_1),
#          varia_2=case_when(is.infinite(varia_2)~NA_real_,
#                            TRUE~varia_2),
#          varia_3=case_when(is.infinite(varia_3)~NA_real_,
#                            TRUE~varia_3)) %>% 
#   filter(year(indice_de_tiempo)==anio_ref) %>% 
#   mutate(mes=month(indice_de_tiempo)) %>%
#   select(mes,region_de_destino,em_total,varia_1,varia_1b,em_res,
#          varia_2,varia_2b,em_nores,varia_3,varia_3b)
# 
# #Cuadro GT (dato chequeado)
# 
# em_1 %>%
#   gt(groupname_col = "region_de_destino")%>% 
#   cols_label(mes = md("**Mes**"),
#              em_total = md("**Totales**"),
#              varia_1 = md("**var % i.a**"),
#              varia_1b = md("**var % vs 2019**"),
#              em_res = md("**Residentes**"),
#              varia_2 = md("**var % i.a**"),
#              varia_2b = md("**var % vs 2019**"),
#              em_nores = md("**No residentes**"),
#              varia_3 = md("**var % i.a**"),
#              varia_3b = md("**var % vs 2019**"))  %>%
#   fmt_number(columns = c(3,6,9), decimals = 1, dec_mark = ",", sep_mark = ".") %>%
#   fmt_percent(columns = c(4:5,7:8,10:11), decimals = 1, dec_mark = ",", sep_mark = ".")%>%
#   tab_options(table.font.size = 12,
#               data_row.padding = px(3),
#               column_labels.font.size = 14,
#               column_labels.font.weight = "bold") %>% 
#   cols_width(columns = c(3,6,9) ~ px(120),
#              columns = c(4:5,7:8,10:11) ~ px(100),
#              columns = c(1) ~ px(180)) %>%
#   sub_missing(
#     columns = everything(),
#     missing_text = "///") %>% 
#   cols_align(
#     align = "center",
#     columns = everything()) %>%
#   tab_header(
#     subtitle = glue("{anio_ref}"),
#     title = toupper(md("__Estadía promedio por condición de residencia y región de destino.__"))
#   ) %>%
#   tab_source_note(
#     source_note = md(
#       "**Fuente:** Encuesta de Ocupación Hotelera (EOH) - (DNMyE e INDEC)."))%>%
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_1),
#       rows = varia_1 <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_1),
#       rows = varia_1 > 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_1b),
#       rows = varia_1b <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_1b),
#       rows = varia_1b > 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_2),
#       rows = varia_2 <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_2),
#       rows = varia_2 > 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_2b),
#       rows = varia_2b <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_2b),
#       rows = varia_2b > 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_3),
#       rows = varia_3 <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_3),
#       rows = varia_3 > 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("rosa"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_3b),
#       rows = varia_3b <= 0
#     )) %>% 
#   tab_style(
#     style = list(
#       cell_text(color = dnmye_colores("azul verde"),weight =  "bold")
#     ),
#     locations = cells_body(
#       columns = c(varia_3b),
#       rows = varia_3b > 0
#     )) %>% 
#   opt_all_caps() %>% 
#   tab_options(
#     column_labels.border.top.width = px(3),
#     column_labels.border.top.color = "transparent",
#     table.border.top.color = "transparent",
#     table.border.bottom.color = "transparent",
#     heading.subtitle.font.size = 12,
#     heading.align = "left",
#     column_labels.border.bottom.color = dnmye_colores("gris oscuro"),
#     column_labels.border.bottom.width= px(3)) 
# 

##################VER SI VALE LA PENA SUMAR CUADROS TAN LARGOS#################

#Pernoctaciones según residencia por categoría por mes


#Viajeros según residencia por categoría por mes

#Estadía Promedio según residencia por categoría por mes


#Tasa de ocupación en plazas en fds región por mes

#Tasa de ocupación en plazas en fds por categoría por mes

##############################################################################

