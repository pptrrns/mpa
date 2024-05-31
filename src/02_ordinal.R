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
      labelled, stargazer, MASS, ggeffects, ordinal)

options(spicen = 9999999)

#--------------------

# Cargar bases de datos

lp <- read.csv2("output/lp.csv", sep = ",")

# Recodificar variables ~ datos LAPOP

# Victimización

lp$vic1ext = factor(lp$vic1ext, levels = c("0", "1"), ordered = TRUE) 

# ¿Hasta qué punto tiene usted confianza en el presidente?

lp$b21a = factor(lp$b21a, levels = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho"), ordered = TRUE) 

# ¿Hasta qué punto tiene usted confianza en su municipalidad?

lp$b32 = factor(lp$b32, levels = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho"), ordered = TRUE) 

# ¿Diría que la gente de su comunidad/barrio/área/vecindad es muy confiable, algo confiable, poco confiable o nada confiable?

lp$it1 = factor(lp$it1, levels = c("Muy confiable", "Algo confiable", "Poco confiable", "Nada confiable"), ordered = TRUE) 

# ¿Usted diría que está muy satisfecho(a), satisfecho(a), insatisfecho(a) o muy insatisfecho(a) con la forma en que la democracia funciona en (país)?

lp$pn4 = factor(lp$pn4, levels = c("Muy satisfecho(a)", "Satisfecho(a)", "Insatisfecho(a)", "Muy insatisfecho(a)"), ordered = TRUE) 

lp$year = factor(lp$year) # Año

lp$q2 = as.character(lp$q2) # Edad

lp$q2 = as.numeric(lp$q2) 

# Recursos

tema <- theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 8),
              axis.title.y = element_text(size = 9), 
              legend.text = element_text(size = 8),
              strip.text = element_text(face = "bold", size = 10))

tema2 <- theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 8),
              axis.title.y = element_text(size = 9), 
              legend.text = element_text(size = 8),
              strip.text = element_text(face = "bold", size = 10))

#--------------------

# Modelo ~ confianza presidente

m_pre <- clm(b21a ~ vic1ext + year, link = "logit", data = lp)

# Modelo ~ confianza municipal

m_mun <- clm(b32 ~ vic1ext + year, link = "logit", data = lp)

# Modelo ~ confianza interpersonal

m_peer <- clm(it1 ~ vic1ext + year, link = "logit", data = lp)

# Modelo ~ satisfacción con la democracia

m_dem <- clm(pn4 ~ vic1ext + year, link = "logit", data = lp)

# Raw estimates

stargazer(m_pre, m_mun, m_peer, m_dem, type = "text")

# Odd ratios

odds_pre = exp(coef(m_pre))
odds_mun = exp(coef(m_mun))
odds_peer = exp(coef(m_peer))
odds_dem = exp(coef(m_dem))

stargazer(m_pre, m_mun, m_peer, m_dem, type = "latex",
          coef = list(odds_pre, odds_mun, odds_peer, odds_dem),
          p.auto = FALSE)

# Thresholds

thresholds_pre <- coef(m_pre)[grep("\\|", names(coef(m_pre)))]
thresholds_mun <- coef(m_mun)[grep("\\|", names(coef(m_mun)))]
thresholds_peer <- coef(m_peer)[grep("\\|", names(coef(m_peer)))]
thresholds_dem <- coef(m_dem)[grep("\\|", names(coef(m_dem)))]

stargazer(thresholds_pre,
          type = "text", 
          title = "Threshold Coefficients: Confianza en el presidente")

stargazer(thresholds_mun,
          type = "text", 
          title = "Threshold Coefficients: Confianza en la municipalidad")

stargazer(thresholds_peer,
          type = "text", 
          title = "Threshold Coefficients: Confianza interpersonal")

stargazer(thresholds_dem, 
          type = "text", 
          title = "Threshold Coefficients: Satisfacción con la democracia")

#--------------------

training = data.frame(ggpredict(m_pre, terms = c("vic1ext", "year"))) 

training$x = factor(training$x)

levels(training$x) = c("No víctima", "Víctima") 

training$response.level = factor(training$response.level) 

levels(training$response.level) = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho")

colnames(training)[c(1, 5)] = c("Victimizacion", "likert") 

training$group = as.character(training$group) 

training$group = as.numeric(training$group) 

training %>%
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\nen el presidente")
  
ggsave("plot_pres.png", width = 8, height = 5, dpi = 400) 

training %>%
  filter(Victimizacion == "Víctima") %>% 
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\nen el presidente")

ggsave("plot_pres_02.png", width = 8, height = 5, dpi = 400)  

training %>%
  ggplot() + 
  geom_point(aes(x = Victimizacion, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = Victimizacion, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  facet_wrap(~group) +
  tema2 +
  labs(col = "Confianza\nen el presidente")

ggsave("plot_pres_03.png", width = 8, height = 5, dpi = 400)  

#--------------------

training = data.frame(ggpredict(m_mun, terms = c("vic1ext", "year"))) 

training$x = factor(training$x)

levels(training$x) = c("No víctima", "Víctima") 

training$response.level = factor(training$response.level) 

levels(training$response.level) = c("Nada", "Muy poco", "Poco", "Algo", "Moderado", "Bastante", "Mucho")

colnames(training)[c(1, 5)] = c("Victimizacion", "likert") 

training$group = as.character(training$group) 

training$group = as.numeric(training$group) 

training %>%
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\nen la municipalidad")

ggsave("plot_mun.png", width = 8, height = 5, dpi = 400) 

training %>%
  filter(Victimizacion == "Víctima") %>% 
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\nen la municipalidad")

ggsave("plot_mun_02.png", width = 8, height = 5, dpi = 400)  

training %>%
  ggplot() + 
  geom_point(aes(x = Victimizacion, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = Victimizacion, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  facet_wrap(~group) +
  tema2 +
  labs(col = "Confianza\nen la municipalidad")

ggsave("plot_mun_03.png", width = 8, height = 5, dpi = 400)  

#--------------------

training = data.frame(ggpredict(m_peer, terms = c("vic1ext", "year"))) 

training$x = factor(training$x)

levels(training$x) = c("No víctima", "Víctima") 

training$response.level = factor(training$response.level) 

levels(training$response.level) = c("Muy confiable", "Algo confiable", "Poco confiable", "Nada confiable")

colnames(training)[c(1, 5)] = c("Victimizacion", "likert") 

training$group = as.character(training$group) 

training$group = as.numeric(training$group) 

training %>%
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\ninterpersonal")

ggsave("plot_peer.png", width = 8, height = 5, dpi = 400) 

training %>%
  filter(Victimizacion == "Víctima") %>% 
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Confianza\ninterpersonal")

ggsave("plot_peer_02.png", width = 8, height = 5, dpi = 400)  

training %>%
  ggplot() + 
  geom_point(aes(x = Victimizacion, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = Victimizacion, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  facet_wrap(~group) +
  tema2 +
  labs(col = "Confianza\ninterpersonal")

ggsave("plot_peer_03.png", width = 8, height = 5, dpi = 400)  

#--------------------

training = data.frame(ggpredict(m_dem, terms = c("vic1ext", "year"))) 

training$x = factor(training$x)

levels(training$x) = c("No víctima", "Víctima") 

training$response.level = factor(training$response.level) 

levels(training$response.level) = c("Muy satisfecho(a)", "Satisfecho(a)", "Insatisfecho(a)", "Muy insatisfecho(a)") 

colnames(training)[c(1, 5)] = c("Victimizacion", "likert") 

training$group = as.character(training$group) 

training$group = as.numeric(training$group) 

training %>%
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion) +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Satisfacción\nDemocracia")

ggsave("plot_dem.png", width = 8, height = 5, dpi = 400) 

training %>%
  filter(Victimizacion == "Víctima") %>% 
  ggplot() + 
  geom_point(aes(x = group, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = group, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  geom_errorbar(aes(group,
                    ymin = conf.low,
                    ymax = conf.high,
                    col = likert), width = 0.15, alpha = .5) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  geom_vline(xintercept = 2018,
             linetype = "dashed", color = "black", alpha = 0.4) +
  facet_wrap(~Victimizacion, scales = "free_y") +
  scale_x_continuous(breaks = seq(2010, 2023, by = 1)) +
  tema +
  labs(col = "Satisfacción\nDemocracia")

ggsave("plot_dem_02.png", width = 8, height = 5, dpi = 400)  

training %>%
  ggplot() + 
  geom_point(aes(x = Victimizacion, y = predicted, col = likert), size = 2) +  
  geom_line(aes(x = Victimizacion, y = predicted, col = likert, group = likert),  
            alpha = 0.3, linewidth = .3) +
  theme_bw() +
  labs(y = "Predicción de la Probabilidad",
       x = "") +
  facet_wrap(~group) +
  tema2 +
  labs(col = "Satisfacción\nDemocracia")

ggsave("plot_dem_03.png", width = 8, height = 5, dpi = 400)  