#--------------------

# José Ángel Torrens Hernández
# 175021 | Ciencia Política

#--------------------

# Cargar librerías

if(!require(pacman)) install.packages(pacman)
pacman::p_load(dplyr, ggplot2, tidyr, janitor, haven, tidyverse, lubridate, 
      stringr, hrbrthemes, ggrepel, ggspatial, sf, ggmap, cowplot, 
      googleway, lwgeom, ggpubr, ggthemes, readxl, readr, tidyverse,
      tidyr, units, viridis, wesanderson, foreign,
      labelled, stargazer)

options(spicen = 9999999)

# #--------------------

# Cargar catálogo de claves del INEGI

catalogo <- read.csv2("input/pop/catalogo.csv", sep = ",", encoding = "UTF-8")

catalogo <- catalogo %>% 
  select(c(CVE_ENT, NOM_ENT, NOM_ABR, CVE_MUN, NOM_MUN))

catalogo <- catalogo %>%
  distinct()

catalogo <- catalogo %>%
  mutate(codigo = sprintf("%02d%03d", CVE_ENT, CVE_MUN))

estados <- catalogo %>%
  select(-c(CVE_MUN, NOM_MUN, codigo)) %>%
  distinct() %>%
  rename(prov = NOM_ENT)

municipios <- catalogo %>%
  select(-c(CVE_ENT, NOM_ABR)) %>%
  distinct() %>%
  rename(municipio = NOM_MUN) %>%
  rename(prov = NOM_ENT)

# Cargar bases de datos -- Latin American Public Opinion Project LAPOP Lab

lp <- read.spss("input/lapop/MEX_merge_2004-2023_LAPOP_AmericasBarometer_v1.0_w.sav", to.data.frame = TRUE)

# Seleccionar variables - LAPOP
lp <- lp %>% 
  mutate(municipio = case_when(year == 2004 ~ municipio04,
                               year == 2006 ~ municipio06,
                               year == 2008 ~ municipio08,
                               year == 2010 ~ municipio10,
                               TRUE ~ municipio),
         prov = case_when(is.na(prov) ~ prov1t, TRUE ~ prov)) %>% 
  select(c(pais, year, estratopri, prov, municipio, municipio1t,
           vic1ext, q2,
           b21a, b32, ing4, it1, pn4))

lp <- lp %>%
  mutate(prov = recode(prov,
                       "Veracruz" = "Veracruz de Ignacio de la Llave",
                       "Michoacán" = "Michoacán de Ocampo",
                       "Estado de México" = "México",
                       "Coahuila" = "Coahuila de Zaragoza",
                       "Distrito Federal" = "Ciudad de México"),
         prov = as.character(prov))

lp <- left_join(lp, estados, by = "prov") %>% 
      relocate(NOM_ABR, .after = prov) %>% 
      relocate(CVE_ENT, .before = prov)

lp <- lp %>%
  mutate(municipio = as.character(municipio),
         cve_mun = case_when(
             str_detect(municipio, "^[0-9]+$") ~ substr(municipio, nchar(municipio)-2, nchar(municipio)),
             TRUE ~ str_extract(municipio, "[0-9]+")),
         cve_mun = as.numeric(cve_mun),
         cve_mun = str_pad(cve_mun, width = 3, side = "left", pad = "0"),
         CVE_ENT = str_pad(CVE_ENT, width = 2, side = "left", pad = "0"))

lp <- lp %>%
  mutate(municipio = case_when(
    str_detect(municipio, "[0-9]") ~ NA_character_,
    TRUE ~ municipio
  ))

lp <- lp %>%
  mutate(municipio1t = as.character(municipio1t),
         municipio = ifelse(is.na(municipio), municipio1t, municipio)) %>% 
  select(-municipio1t)

lp <- lp %>%
  mutate(CVE_ENT = as.numeric(CVE_ENT),
         cve_mun = as.numeric(cve_mun),
         cve_mun = ifelse(is.na(cve_mun), NA, 
                          sprintf("%02d%03d", CVE_ENT, cve_mun))) %>% 
  relocate(cve_mun, .after = municipio)

names_lp <- as.character(sort(unique(lp$municipio)))
names_inegi <- sort(unique(municipios$municipio))

summary(names_lp %in% names_inegi)

recodificar <- names_lp[(names_lp %in% names_inegi)==FALSE]

recodificar <- data.frame(recodificar)

# write.csv(recodificar, "output/recodificar.csv", row.names = FALSE, fileEncoding = "UTF-8")

recodificados <- c("Álamo Temapache", "Álvaro Obregón", "Álvaro Obregón", "Acajete", 
                   "Acambay de Ruíz Castañeda", "Acatlán", "Acuña", "Álamos", "Aldama", 
                   "Allende", "Allende", "Allende", "Ángel Albino Corzo", "Arteaga", "Atotonilco el Alto", 
                   "Atoyac de Álvarez", "Azoyú", "Benito Juárez", "Benito Juárez", "Benito Juárez", 
                   "Castaños", "Chiautla", "Chilapa de Álvarez", "Cintalapa de Figueroa", "Comondú", 
                   "Corregidora", "Cuauhtémoc", "Cuauhtémoc", "Dolores Hidalgo Cuna de la Independencia Nacional", 
                   "Emiliano Zapata", "Emiliano Zapata", "Emiliano Zapata", "Francisco I. Madero", "General Escobedo", 
                   "General Terán", "General Zuazua", "Guadalupe", "Juchitán de Zaragoza", "Hidalgo", "Hidalgo", 
                   "Huimilpan", "Rayón", "Izúcar de Matamoros", "Jilotepec", "Juárez", "Juárez", "La Masica", 
                   "La Paz", "Matamotos", "Medellín de Bravo", "Múzquiz", "Minatitlán", "Mixco", "Morelos", 
                   "Peñamiller", "Puerto Peñasco", "Rosario", "San Fernando", "Santa Catarina", 
                   "Silao de la Victoria", "Teoloyucan", "Tequila", "Tierra Blanca", "Tlajomulco de Zúñiga", 
                   "Tlaltizapán de Zapata", "San Pedro Tlaquepaque", "Tonalá", "La Trinidad Vista Hermosa", 
                   "Tuxpan", "Venustiano Carranza", "Venustiano Carranza", "Villa de Álvarez", "Villa de Álvarez", 
                   "Villa Nueva", "Zapotán el Grande", "Zaragoza", "Zaragoza", "Zimatlán de Álvarez", "Zitácuaro")

recodificar$recodificados <- recodificados

recodificar <- recodificar %>% 
  rename(municipio = recodificar)

lp <- lp %>%  # Recodificar los nombres
  mutate(municipio = recode(municipio,
                            "Alvaro Obregón" = "Álvaro Obregón",
                            "CORREGIDORA" = "Corregidora",
                            "Matamoros_duplicated_101999" = "Matamoros",
                            "Zacualpan" = "Zacualpan de Amilpas",
                            "Zapotlán El Grande" = "Zapotlán el Grande"),
         municipio = as.character(municipio))

lp <- left_join(lp, municipios, by = c("prov", "municipio")) %>%
  mutate(cve_mun = case_when(is.na(cve_mun) ~ codigo, TRUE ~ cve_mun)) %>%
  select(-c(CVE_MUN, codigo))

lp <- left_join(lp, recodificar, by = "municipio")

lp <- left_join(lp, municipios, by = c("prov", "recodificados" = "municipio"))

lp <- lp %>% 
  mutate(cve_mun = case_when(is.na(cve_mun) ~ codigo, TRUE ~ cve_mun)) %>%
  select(-c(recodificados, CVE_MUN, codigo))

lp <- lp %>% filter(!is.na(cve_mun)) %>% 
  select(-municipio)

lp <- left_join(lp, municipios, by = c("prov", "cve_mun" = "codigo"))

lp <- lp %>% filter(!is.na(municipio)) %>% 
  select(-CVE_MUN) %>% 
  clean_names()

lp <- lp %>%
  mutate(year = as.character(year),
         year = as.numeric(year)) %>% 
  filter(year > 2008)

lp <- lp %>% 
  select(c(pais, estratopri, cve_ent, prov, municipio, cve_mun, year, q2,
           vic1ext, b21a, b32, ing4, it1, pn4))

lp <- lp %>% 
  mutate(vic1ext = gsub("No", 0, vic1ext),
         vic1ext = gsub("Sí", 1, vic1ext))

lp <- lp %>% 
  mutate(b21a = gsub("1", "Nada", b21a),
         b21a = gsub("2", "Muy poco", b21a),
         b21a = gsub("3", "Poco", b21a),
         b21a = gsub("4", "Algo", b21a),
         b21a = gsub("5", "Moderado", b21a),
         b21a = gsub("6", "Bastante", b21a),
         b21a = gsub("7", "Mucho", b21a))

lp <- lp %>% 
  mutate(b32 = gsub("1", "Nada", b32),
         b32 = gsub("2", "Muy poco", b32),
         b32 = gsub("3", "Poco", b32),
         b32 = gsub("4", "Algo", b32),
         b32 = gsub("5", "Moderado", b32),
         b32 = gsub("6", "Bastante", b32),
         b32 = gsub("7", "Mucho", b32))

lp <- lp %>% 
  mutate(ing4 = gsub("1", "Muy en desacuerdo", ing4),
         ing4 = gsub("2", "En desacuerdo", ing4),
         ing4 = gsub("3", "Ligeramente en desacuerdo", ing4),
         ing4 = gsub("4", "Ni en acuerdo ni en desacuerdo", ing4),
         ing4 = gsub("5", "Ligeramente en de acuerdo", ing4),
         ing4 = gsub("6", "De acuerdo", ing4),
         ing4 = gsub("7", "Muy de acuerdo", ing4))
  
#--------------------
# Recodificar variables

lp$vic1ext = factor(lp$vic1ext, levels = c("0", "1"), ordered = TRUE) 
 
lp$b21a = factor(lp$b21a, levels = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho"), ordered = TRUE) 
lp$b32 = factor(lp$b32, levels = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho"), ordered = TRUE) 

lp$ing4 = factor(lp$ing4, levels = c("Muy en desacuerdo", "En desacuerdo", "Ligeramente en desacuerdo", 
                                "Ni en acuerdo ni en desacuerdo", "Ligeramente en de acuerdo", 
                                "De acuerdo", "Muy de acuerdo"), ordered = TRUE)
 
lp$it1 = factor(lp$it1, levels = c("Muy confiable", "Algo confiable", "Poco confiable", "Nada confiable"), ordered = TRUE) 
 
lp$pn4 = factor(lp$pn4, levels = c("Muy satisfecho(a)", "Satisfecho(a)", "Insatisfecho(a)", "Muy insatisfecho(a)"), ordered = TRUE) 
 
lp$year = as.numeric(lp$year) 

rm(catalogo, estados, municipios, recodificar, names_inegi, names_lp, recodificados)

#--------------------
# Guardar base

write.csv(lp, "output/lp.csv", row.names = FALSE) # Export csv