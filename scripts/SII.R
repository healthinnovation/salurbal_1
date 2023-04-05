library(biscale)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(sf)
library(ggpubr)
library(table1)
library(ggstatsplot)
library(innovar)
library(ggmosaic)
library(readr)
library(readxl)
library(broom)
source("utils2.R")

#1. Import data -----------

sn_argentina <- read_excel("Data/AR_L2_5_ANALYTIC_09022022.xlsx")
sn_brasil <- read_excel("Data/BR_L2_5_ANALYTIC_09022022.xlsx")
sn_chile <- read_excel("Data/CL_L3_ANALYTIC_09022022.xlsx")
sn_colombia <- read_excel("Data/CO_L3_ANALYTIC_09022022.xlsx")
sn_costarica <- read_excel("Data/CR_L2_5_ANALYTIC_09022022.xlsx")
sn_guatemala <- read_excel("Data/GT_L3_ANALYTIC_09022022.xlsx")
sn_mexico <- read_excel("Data/MX_L3_ANALYTIC_09022022.xlsx")
sn_panama <- read_excel("Data/PA_L3_ANALYTIC_09022022.xlsx")

bogota <- st_read("Procesado/Bogota.gpkg") %>% 
  select("SALID3", "geom")
buenosaires <- st_read("Procesado/Buenos aires.gpkg") %>% 
  select("SALID2_5", "geom")
guatemala <- st_read("Procesado/Guatemala.gpkg") %>% 
  select("SALID3", "geom")
mexico <- st_read("Procesado/Mexico.gpkg") %>% 
  select("salid3", "geom")
panama <- st_read("Procesado/Panama.gpkg") %>% 
  select("salid3", "geom")
sanjose <- st_read("Procesado/San jose.gpkg") %>% 
  select("SALID2_5", "geom")
santiago <- st_read("Procesado/Santiago.gpkg") %>% 
  select("salid3", "geom")
saopaolo <- st_read("Procesado/Sao paolo.gpkg") %>% 
  select("SALID2_5", "geom")

bogota <- merge(bogota, sn_colombia,
                by.x="SALID3", by.y="SALID3", all=F)

buenosaires <- merge(buenosaires, sn_argentina,
                     by.x="SALID2_5", by.y="SALID2_5", all=F)

guatemala <- merge(guatemala, sn_guatemala,
                   by.x="SALID3", by.y="SALID3", all=F)

mexico <- merge(mexico, sn_mexico,
                by.x="salid3", by.y="SALID3", all=F)

panama <- merge(panama, sn_panama,
                by.x="salid3", by.y="SALID3", all=F)

sanjose <- merge(sanjose, sn_costarica,
                 by.x="SALID2_5", by.y="SALID2_5", all=F)

santiago <- merge(santiago, sn_chile,
                  by.x="salid3", by.y="SALID3", all=F)

saopaolo <- merge(saopaolo, sn_brasil,
                  by.x="SALID2_5", by.y="SALID2_5", all=F)

#2. SII analysis ----------

edson <- function(x, pais = "peru"){
  s1 <- summary(x)
  df_coef <- as.data.frame(s1$coefficients)
  rank <- df_coef[["Estimate"]][5]
  confid <- tidy(x, conf.int = T)
  conf.low <- confid[["conf.low"]][5]
  conf.high <- confid[["conf.high"]][5]
  table_model <- data.frame(rank = rank,
                            conf.low = conf.low,
                            conf.high = conf.high,
                            country = pais)
  return(table_model)
}

##2.1 argentina ----

buenosaires$PM2.5 <- (buenosaires$APSPM25MEAN2000L3 +
  buenosaires$APSPM25MEAN2001L3 +
  buenosaires$APSPM25MEAN2002L3 +
  buenosaires$APSPM25MEAN2003L3 +
  buenosaires$APSPM25MEAN2004L3 +
  buenosaires$APSPM25MEAN2005L3 +
  buenosaires$APSPM25MEAN2006L3 +
  buenosaires$APSPM25MEAN2007L3 +
  buenosaires$APSPM25MEAN2008L3 +
  buenosaires$APSPM25MEAN2009L3 +
  buenosaires$APSPM25MEAN2010L3 +
  buenosaires$APSPM25MEAN2011L3 +
  buenosaires$APSPM25MEAN2012L3 +
  buenosaires$APSPM25MEAN2013L3 +
  buenosaires$APSPM25MEAN2014L3 +
  buenosaires$APSPM25MEAN2015L3 +
  buenosaires$APSPM25MEAN2016L3 +
  buenosaires$APSPM25MEAN2017L3 +
  buenosaires$APSPM25MEAN2018L3 +
  buenosaires$APSPM25MEAN2019L3 +
  buenosaires$APSPM25MEAN2020L3)/21

buenosaires <- buenosaires %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

argentina_total <- sum(buenosaires$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_argentina_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
         data = buenosaires)

rank_lm_sii_argentina_attending <- edson(sii_argentina_attending, "argentina")

#primary school

sii_argentina_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                              data = buenosaires)

rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "argentina")

#high school

sii_argentina_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                              data = buenosaires)

rank_lm_sii_argentina_high <- edson(sii_argentina_high, "argentina")

#university school

sii_argentina_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                              data = buenosaires)

rank_lm_sii_argentina_university <- edson(sii_argentina_university, "argentina")

##2.2 brasil ----

saopaolo$PM2.5 <- (saopaolo$APSPM25MEAN2000L3 +
                        saopaolo$APSPM25MEAN2001L3 +
                        saopaolo$APSPM25MEAN2002L3 +
                        saopaolo$APSPM25MEAN2003L3 +
                        saopaolo$APSPM25MEAN2004L3 +
                        saopaolo$APSPM25MEAN2005L3 +
                        saopaolo$APSPM25MEAN2006L3 +
                        saopaolo$APSPM25MEAN2007L3 +
                        saopaolo$APSPM25MEAN2008L3 +
                        saopaolo$APSPM25MEAN2009L3 +
                        saopaolo$APSPM25MEAN2010L3 +
                        saopaolo$APSPM25MEAN2011L3 +
                        saopaolo$APSPM25MEAN2012L3 +
                        saopaolo$APSPM25MEAN2013L3 +
                        saopaolo$APSPM25MEAN2014L3 +
                        saopaolo$APSPM25MEAN2015L3 +
                        saopaolo$APSPM25MEAN2016L3 +
                        saopaolo$APSPM25MEAN2017L3 +
                        saopaolo$APSPM25MEAN2018L3 +
                        saopaolo$APSPM25MEAN2019L3 +
                        saopaolo$APSPM25MEAN2020L3)/21

saopaolo <- saopaolo %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

brasil_total <- sum(saopaolo$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_brasil_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                              data = saopaolo)

rank_lm_sii_brasil_attending <- edson(sii_brasil_attending, "brasil")

#primary school

sii_brasil_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                            data = saopaolo)

rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "brasil")

#high school

sii_brasil_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                              data = saopaolo)

rank_lm_sii_brasil_high <- edson(sii_brasil_high, "brasil")

#university school

sii_brasil_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                               data = saopaolo)

rank_lm_sii_brasil_university <- edson(sii_brasil_university, "brasil")

##2.3 chile ----

santiago$PM2.5 <- (santiago$APSPM25MEAN2000L3 +
                     santiago$APSPM25MEAN2001L3 +
                     santiago$APSPM25MEAN2002L3 +
                     santiago$APSPM25MEAN2003L3 +
                     santiago$APSPM25MEAN2004L3 +
                     santiago$APSPM25MEAN2005L3 +
                     santiago$APSPM25MEAN2006L3 +
                     santiago$APSPM25MEAN2007L3 +
                     santiago$APSPM25MEAN2008L3 +
                     santiago$APSPM25MEAN2009L3 +
                     santiago$APSPM25MEAN2010L3 +
                     santiago$APSPM25MEAN2011L3 +
                     santiago$APSPM25MEAN2012L3 +
                     santiago$APSPM25MEAN2013L3 +
                     santiago$APSPM25MEAN2014L3 +
                     santiago$APSPM25MEAN2015L3 +
                     santiago$APSPM25MEAN2016L3 +
                     santiago$APSPM25MEAN2017L3 +
                     santiago$APSPM25MEAN2018L3 +
                     santiago$APSPM25MEAN2019L3 +
                     santiago$APSPM25MEAN2020L3)/21

santiago <- santiago %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

chile_total <- sum(santiago$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_chile_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = santiago)

rank_lm_sii_chile_attending <- edson(sii_chile_attending, "chile")

#primary school

sii_chile_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = santiago)

rank_lm_sii_chile_primary <- edson(sii_chile_primary, "chile")

#high school

sii_chile_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                      data = santiago)

rank_lm_sii_chile_high <- edson(sii_chile_high, "chile")

#university school

sii_chile_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                            data = santiago)

rank_lm_sii_chile_university <- edson(sii_chile_university, "chile")

##2.4 colombia ----

bogota$PM2.5 <- (bogota$APSPM25MEAN2000L3 +
                     bogota$APSPM25MEAN2001L3 +
                     bogota$APSPM25MEAN2002L3 +
                     bogota$APSPM25MEAN2003L3 +
                     bogota$APSPM25MEAN2004L3 +
                     bogota$APSPM25MEAN2005L3 +
                     bogota$APSPM25MEAN2006L3 +
                     bogota$APSPM25MEAN2007L3 +
                     bogota$APSPM25MEAN2008L3 +
                     bogota$APSPM25MEAN2009L3 +
                     bogota$APSPM25MEAN2010L3 +
                     bogota$APSPM25MEAN2011L3 +
                     bogota$APSPM25MEAN2012L3 +
                     bogota$APSPM25MEAN2013L3 +
                     bogota$APSPM25MEAN2014L3 +
                     bogota$APSPM25MEAN2015L3 +
                     bogota$APSPM25MEAN2016L3 +
                     bogota$APSPM25MEAN2017L3 +
                     bogota$APSPM25MEAN2018L3 +
                     bogota$APSPM25MEAN2019L3 +
                     bogota$APSPM25MEAN2020L3)/21

bogota <- bogota %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

colombia_total <- sum(bogota$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_colombia_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = bogota)

rank_lm_sii_colombia_attending <- edson(sii_colombia_attending, "colombia")

#primary school

sii_colombia_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = bogota)

rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "colombia")

#high school

sii_colombia_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                      data = bogota)

rank_lm_sii_colombia_high <- edson(sii_colombia_high, "colombia")

#university school

sii_colombia_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                            data = bogota)

rank_lm_sii_colombia_university <- edson(sii_colombia_university, "colombia")

##2.5 costarica ----

sanjose$PM2.5 <- (sanjose$APSPM25MEAN2000L3 +
                     sanjose$APSPM25MEAN2001L3 +
                     sanjose$APSPM25MEAN2002L3 +
                     sanjose$APSPM25MEAN2003L3 +
                     sanjose$APSPM25MEAN2004L3 +
                     sanjose$APSPM25MEAN2005L3 +
                     sanjose$APSPM25MEAN2006L3 +
                     sanjose$APSPM25MEAN2007L3 +
                     sanjose$APSPM25MEAN2008L3 +
                     sanjose$APSPM25MEAN2009L3 +
                     sanjose$APSPM25MEAN2010L3 +
                     sanjose$APSPM25MEAN2011L3 +
                     sanjose$APSPM25MEAN2012L3 +
                     sanjose$APSPM25MEAN2013L3 +
                     sanjose$APSPM25MEAN2014L3 +
                     sanjose$APSPM25MEAN2015L3 +
                     sanjose$APSPM25MEAN2016L3 +
                     sanjose$APSPM25MEAN2017L3 +
                     sanjose$APSPM25MEAN2018L3 +
                     sanjose$APSPM25MEAN2019L3 +
                     sanjose$APSPM25MEAN2020L3)/21

sanjose <- sanjose %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

costarica_total <- sum(sanjose$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_costarica_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = sanjose)

rank_lm_sii_costarica_attending <- edson(sii_costarica_attending, "costa rica")

#primary school

sii_costarica_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = sanjose)

rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "costa rica")

#high school

sii_costarica_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                      data = sanjose)

rank_lm_sii_costarica_high <- edson(sii_costarica_high, "costa rica")

#university school

sii_costarica_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                            data = sanjose)

rank_lm_sii_costarica_university <- edson(sii_costarica_university, "costa rica")

##2.6 guatemala ----

guatemala$PM2.5 <- (guatemala$APSPM25MEAN2000L3 +
                     guatemala$APSPM25MEAN2001L3 +
                     guatemala$APSPM25MEAN2002L3 +
                     guatemala$APSPM25MEAN2003L3 +
                     guatemala$APSPM25MEAN2004L3 +
                     guatemala$APSPM25MEAN2005L3 +
                     guatemala$APSPM25MEAN2006L3 +
                     guatemala$APSPM25MEAN2007L3 +
                     guatemala$APSPM25MEAN2008L3 +
                     guatemala$APSPM25MEAN2009L3 +
                     guatemala$APSPM25MEAN2010L3 +
                     guatemala$APSPM25MEAN2011L3 +
                     guatemala$APSPM25MEAN2012L3 +
                     guatemala$APSPM25MEAN2013L3 +
                     guatemala$APSPM25MEAN2014L3 +
                     guatemala$APSPM25MEAN2015L3 +
                     guatemala$APSPM25MEAN2016L3 +
                     guatemala$APSPM25MEAN2017L3 +
                     guatemala$APSPM25MEAN2018L3 +
                     guatemala$APSPM25MEAN2019L3 +
                     guatemala$APSPM25MEAN2020L3)/21

guatemala <- guatemala %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

guatemala_total <- sum(guatemala$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_guatemala_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = guatemala)

rank_lm_sii_guatemala_attending <- edson(sii_guatemala_attending, "guatemala")

#primary school

sii_guatemala_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = guatemala)

rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "guatemala")

#high school

sii_guatemala_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                      data = guatemala)

rank_lm_sii_guatemala_high <- edson(sii_guatemala_high, "guatemala")

#university school

sii_guatemala_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                            data = guatemala)

rank_lm_sii_guatemala_university <- edson(sii_guatemala_university, "guatemala")

##2.7 mexico ----

mexico$PM2.5 <- (mexico$APSPM25MEAN2000L3 +
                     mexico$APSPM25MEAN2001L3 +
                     mexico$APSPM25MEAN2002L3 +
                     mexico$APSPM25MEAN2003L3 +
                     mexico$APSPM25MEAN2004L3 +
                     mexico$APSPM25MEAN2005L3 +
                     mexico$APSPM25MEAN2006L3 +
                     mexico$APSPM25MEAN2007L3 +
                     mexico$APSPM25MEAN2008L3 +
                     mexico$APSPM25MEAN2009L3 +
                     mexico$APSPM25MEAN2010L3 +
                     mexico$APSPM25MEAN2011L3 +
                     mexico$APSPM25MEAN2012L3 +
                     mexico$APSPM25MEAN2013L3 +
                     mexico$APSPM25MEAN2014L3 +
                     mexico$APSPM25MEAN2015L3 +
                     mexico$APSPM25MEAN2016L3 +
                     mexico$APSPM25MEAN2017L3 +
                     mexico$APSPM25MEAN2018L3 +
                     mexico$APSPM25MEAN2019L3 +
                     mexico$APSPM25MEAN2020L3)/21

mexico <- mexico %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5))

mexico_total <- sum(mexico$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_mexico_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = mexico)

rank_lm_sii_mexico_attending <- edson(sii_mexico_attending, "mexico")

#primary school

sii_mexico_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = mexico)

rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "mexico")

##2.8 panama ----

panama$PM2.5 <- (panama$APSPM25MEAN2000L3 +
                     panama$APSPM25MEAN2001L3 +
                     panama$APSPM25MEAN2002L3 +
                     panama$APSPM25MEAN2003L3 +
                     panama$APSPM25MEAN2004L3 +
                     panama$APSPM25MEAN2005L3 +
                     panama$APSPM25MEAN2006L3 +
                     panama$APSPM25MEAN2007L3 +
                     panama$APSPM25MEAN2008L3 +
                     panama$APSPM25MEAN2009L3 +
                     panama$APSPM25MEAN2010L3 +
                     panama$APSPM25MEAN2011L3 +
                     panama$APSPM25MEAN2012L3 +
                     panama$APSPM25MEAN2013L3 +
                     panama$APSPM25MEAN2014L3 +
                     panama$APSPM25MEAN2015L3 +
                     panama$APSPM25MEAN2016L3 +
                     panama$APSPM25MEAN2017L3 +
                     panama$APSPM25MEAN2018L3 +
                     panama$APSPM25MEAN2019L3 +
                     panama$APSPM25MEAN2020L3)/21

panama <- panama %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(high_quantile_c = ntile(CNSMINHS_L3, 5)) %>% 
  mutate(university_quantile_c = ntile(CNSMINUN_L3, 5))

panama_total <- sum(panama$CNSPOPL3, na.rm = T) #Create total population value

#attending school

sii_panama_attending <- lm(PM2.5~factor(attending_quantile_c), weights = CNSPOPL3,
                           data = panama)

rank_lm_sii_panama_attending <- edson(sii_panama_attending, "panama")

#primary school

sii_panama_primary <- lm(PM2.5~factor(primary_quantile_c), weights = CNSPOPL3,
                         data = panama)

rank_lm_sii_panama_primary <- edson(sii_panama_primary, "panama")

#high school

sii_panama_high <- lm(PM2.5~factor(high_quantile_c), weights = CNSPOPL3,
                      data = panama)

rank_lm_sii_panama_high <- edson(sii_panama_high, "panama")

#university school

sii_panama_university <- lm(PM2.5~factor(university_quantile_c), weights = CNSPOPL3,
                            data = panama)

rank_lm_sii_panama_university <- edson(sii_panama_university, "panama")

#3. SII Graphics----------------------------------

## Forest plot GG -------------------
rank_lm_sii_attending <- rbind(rank_lm_sii_argentina_attending,
                                               rank_lm_sii_brasil_attending,
                                               rank_lm_sii_chile_attending,
                                               rank_lm_sii_colombia_attending,
                                               rank_lm_sii_costarica_attending,
                                               rank_lm_sii_guatemala_attending,
                                               rank_lm_sii_mexico_attending,
                                               rank_lm_sii_panama_attending) %>% 
  mutate(exposure = "School attendance (any school, 15 to 17 yo)")

rank_lm_sii_primary <- rbind(rank_lm_sii_argentina_primary,
                               rank_lm_sii_brasil_primary,
                               rank_lm_sii_chile_primary,
                               rank_lm_sii_colombia_primary,
                               rank_lm_sii_costarica_primary,
                               rank_lm_sii_guatemala_primary,
                               rank_lm_sii_mexico_primary,
                               rank_lm_sii_panama_primary) %>% 
  mutate(exposure = "Primary school attainment (all ages)")

rank_lm_sii_high <- rbind(rank_lm_sii_argentina_high,
                               rank_lm_sii_brasil_high,
                               rank_lm_sii_chile_high,
                               rank_lm_sii_colombia_high,
                               rank_lm_sii_costarica_high,
                               rank_lm_sii_guatemala_high,
                               rank_lm_sii_panama_high) %>% 
  mutate(exposure = "High school attainment (all ages)")

rank_lm_sii_university <- rbind(rank_lm_sii_argentina_university,
                               rank_lm_sii_brasil_university,
                               rank_lm_sii_chile_university,
                               rank_lm_sii_colombia_university,
                               rank_lm_sii_costarica_university,
                               rank_lm_sii_guatemala_university,
                               rank_lm_sii_panama_university) %>% 
  mutate(exposure = "University attainment (all ages)")

rank_lm_sii <- rbind(rank_lm_sii_attending,
                     rank_lm_sii_primary,
                     rank_lm_sii_high,
                     rank_lm_sii_university) %>% 
  mutate(exposure_r = factor(exposure,levels = c(
    "School attendance (any school, 15 to 17 yo)",
    "Primary school attainment (all ages)",
    "High school attainment (all ages)",
    "University attainment (all ages)"
  )) ) %>% 
  mutate(country_r = fct_rev(country))


ggplot(rank_lm_sii, aes(x=rank, y=country_r, color=country_r)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (population weighted) of education by country", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure_r, ncol=2)

ggsave("rank_lm_sii.png",
       last_plot(),
       dpi = 300,
       bg = "white",
       width = 9,
       height = 6)

# A. gg_barplot -----------------------------------------------------------
gg_barplot <- function(x,nrow = 1){
  g0 <- x %>% 
    ggplot(
      aes(
        x = bi_class,
        y = total,
        fill = bi_class
      )
    ) + 
    geom_bar(stat = 'identity') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    labs(x = "", y = "")
  
  if(nrow == 1){
    g1 <- g0 + 
      scale_fill_manual(values = c("#AE384C","#BA7A8F","#CADECF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  else if(nrow == 2){ 
    g1 <- g0 + 
      scale_fill_manual(values = c("#75304A","#7F688A","#87A1C7")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  } else {
    g1 <- g0 + 
      scale_fill_manual(values = c("#3D2847","#425785","#4785BF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  return(g1)
} 

# B. Biscale color palette - customized -----------------------------------

bi_pal2 <- function (pal, dim = 3, preview = TRUE) 
{
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (pal %in% c("Brown", "DkBlue", "DkCyan", "DkViolet", "GrPink") == 
      FALSE) {
    stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (is.logical(preview) == FALSE) {
    stop("A logical scalar must be supplied for 'preview'. Please provide either 'TRUE' or 'FALSE'.")
  }
  if (preview == TRUE) {
    out <- bi_legend(pal = pal, dim = dim, size = 16)
  }
  else if (preview == FALSE) {
    if (pal == "DkViolet") {
      out <- pal_dkviolet2(n = dim)
    }
    else if (pal == "GrPink") {
      out <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      out <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      out <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      out <- pal_brown(n = dim)
    }
  }
  return(out)
}

pal_dkviolet <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#3F2949", # high x, high y
      "1-2" = "#4885C1", # low x, high y
      "2-1" = "#AE3A4E", # high x, low y
      "1-1" = "#CABED0" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#CADECF", # high x, high y
      "2-3" = "#BA7A8F",
      "1-3" = "#AE384C", # low x, high y
      "3-2" = "#87A1C7",
      "2-2" = "#7F688A", # medium x, medium y
      "1-2" = "#75304A",
      "3-1" = "#4785BF", # high x, low y
      "2-1" = "#425785",
      "1-1" = "#3D2847" # low x, low y
    )
    
  }
  return(out)
}

# gray pink palette
pal_grpink <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#574249", # high x, high y
      "1-2" = "#64ACBE", # low x, high y
      "2-1" = "#C85A5A", # high x, low y
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#574249", # high x, high y
      "2-3" = "#627F8C",
      "1-3" = "#64ACBE", # low x, high y
      "3-2" = "#985356",
      "2-2" = "#AD9EA5", # medium x, medium y
      "1-2" = "#B0D5DF",
      "3-1" = "#C85A5A", # high x, low y
      "2-1" = "#E4ACAC",
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  }
  
  # return output
  return(out)
  
}


bi_legend <- function(pal, dim = 3, xlab, ylab, size = 10, flip_axes = FALSE, rotate_pal = FALSE, pad_width = NA, pad_color = '#ffffff'){
  
  # global binding
  bi_class = bi_fill = x = y = NULL
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (missing(xlab) == TRUE){
    xlab <- "x var "
  }
  
  if (is.character(xlab) == FALSE){
    stop("The 'xlab' argument must be a character string.")
  }
  
  if (missing(ylab) == TRUE){
    ylab <- "y var "
  }
  
  if (is.character(ylab) == FALSE){
    stop("The 'ylab' argument must be a character string.")
  }
  
  if (is.numeric(size) == FALSE){
    stop("The 'size' argument must be a numeric value.")
  }
  
  # nse
  xQN <- rlang::quo_name(rlang::enquo(xlab))
  yQN <- rlang::quo_name(rlang::enquo(ylab))
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # create tibble for plotting
  x <- dplyr::tibble(
    bi_class = names(x),
    bi_fill = x
  )
  
  # reformat
  leg <- tidyr::separate(x, bi_class, into = c("x", "y"), sep = "-")
  leg <- dplyr::mutate(leg, x = as.integer(x), y = as.integer(y))
  
  # create ggplot2 legend object
  legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = leg, mapping = ggplot2::aes(x = x, y = y, fill = bi_fill), lwd = pad_width, col = pad_color) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = substitute(paste(xQN, ""%->%"")), y = substitute(paste(yQN, ""%->%""))) +
    bi_theme() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = size)) +
    ggplot2::coord_fixed()
  
  # return output
  return(legend)
  
}
bi_theme <- function(base_family = "sans", base_size = 24, bg_color = "#ffffff", font_color = "#000000", ...) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      
      # text defaults
      text = ggplot2::element_text(color = font_color),
      
      # remove all axes
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      
      # add a grid that blends into plot background
      panel.grid.major = ggplot2::element_line(color = bg_color, size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # background colors
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      
      # borders and margins
      plot.margin = ggplot2::unit(c(.5, .5, .2, .5), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(-.1, 0.2, .2, 0.2), "cm"),
      
      # titles
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, color = font_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = font_color,
                                            margin = ggplot2::margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                            face = "bold", debug = FALSE),
      legend.title = ggplot2::element_text(color = font_color),
      legend.text = ggplot2::element_text(hjust = 0, color = font_color),
      
      # captions
      plot.caption = ggplot2::element_text(size = ggplot2::rel(.6), hjust = .5,
                                           margin = ggplot2::margin(t = 0.2, b = 0, unit = "cm"),
                                           color = font_color),
      ...
    )
  
}

bi_scale_fill2 <- function(pal, dim = 3, flip_axes = FALSE, rotate_pal = FALSE, ...){
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis' or supply a custom palette created with 'bi_pal_custom()'.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # apply to ggplot object
  ggplot2::scale_fill_manual(values = x, ...)
  
}

# C. Maps biscale ---------------------------------------------------------
gg_bimap <- function(data, xlim, ylim){
  g0 <- data %>% 
    ggplot() + 
    geom_sf(
      data = dep,
      lwd = 0.5,
      fill = "#d9d9d9",
      show.legend = FALSE,
      color = "white"
    ) +
    geom_sf(
      data = data,
      lwd = 0.0,
      aes(fill = bi_class),
      show.legend = FALSE
    ) +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) +
    bi_scale_fill2(pal = "DkViolet", dim = 3) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    theme_bw()  +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) + 
    facet_grid(.~title) + 
    theme(
      strip.background = element_rect(
        colour = "black",
        fill = "white")
    )
}

#==========================================================================

# 3. Bogota attending -----------------------------------------------
library(janitor)
data1 %>% 
  tabyl(bi_class)

data1 <- bi_class(
  bogota,
  x = CNSST1517L3,
  y = APSPM25MEAN2018L3,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSST1517L3)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 4. Buenos aires attending -----------------------------------------------

data2 <- bi_class(
  buenosaires,
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 5. Guatemala attending -----------------------------------------------

data3 <- bi_class(
  guatemala,
  x = CNSST1517L3,
  y = APSPM25MEAN2002L3,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 6. Panama attending -----------------------------------------------

data4 <- bi_class(
  panama,
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 7. San jose attending -----------------------------------------------

data5 <- bi_class(
  sanjose,
  x = CNSST1517L3,
  y = APSPM25MEAN2011L3,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 8. Santiago attending -----------------------------------------------

data6 <- bi_class(
  santiago,
  x = CNSST1517L3,
  y = APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 9. Sao paolo attending -----------------------------------------------

data7 <- bi_class(
  saopaolo,
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 10. Mexico attending -----------------------------------------------

data8 <- bi_class(
  mexico,
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data8 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = mexico,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data8,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end8 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

end9 <- ggdraw() +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    scale = 1
  ) +
  theme_bw()

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end8, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", "H. Mexico city (Mexico", ""),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_attending.png",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_attending.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)

# 3. Bogota primary -----------------------------------------------

data1 <- bi_class(
  bogota,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2018L3,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSMINPR_L3)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 4. Buenos aires primary -----------------------------------------------

data2 <- bi_class(
  buenosaires,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 5. Guatemala primary -----------------------------------------------

data3 <- bi_class(
  guatemala,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2002L3,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 6. Panama primary -----------------------------------------------

data4 <- bi_class(
  panama,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 7. San jose primary -----------------------------------------------

data5 <- bi_class(
  sanjose,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2011L3,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 8. Santiago primary -----------------------------------------------

data6 <- bi_class(
  santiago,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 9. Sao paolo primary -----------------------------------------------

data7 <- bi_class(
  saopaolo,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 10. Mexico primary -----------------------------------------------

data8 <- bi_class(
  mexico,
  x = CNSMINPR_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data8 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of primary school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of primary school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = mexico,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data8,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end8 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

end9 <- ggdraw() +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    scale = 1
  ) +
  theme_bw()

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end8, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", "H. Mexico city (Mexico", ""),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_primary.png",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_primary.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)

# 3. Bogota high -----------------------------------------------

data1 <- bi_class(
  bogota,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2018L3,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSMINHS_L3)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 4. Buenos aires high -----------------------------------------------

data2 <- bi_class(
  buenosaires,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 5. Guatemala high -----------------------------------------------

data3 <- bi_class(
  guatemala,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2002L3,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 6. Panama high -----------------------------------------------

data4 <- bi_class(
  panama,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 7. San jose high -----------------------------------------------

data5 <- bi_class(
  sanjose,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2011L3,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 8. Santiago high -----------------------------------------------

data6 <- bi_class(
  santiago,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 9. Sao paolo high -----------------------------------------------

data7 <- bi_class(
  saopaolo,
  x = CNSMINHS_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of high school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of high school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

end9 <- ggdraw() +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    scale = 1
  ) +
  theme_bw()

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", ""),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_high.png",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_high.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)

# 3. Bogota university -----------------------------------------------

data1 <- bi_class(
  bogota,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2018L3,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSMINUN_L3)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 4. Buenos aires university -----------------------------------------------

data2 <- bi_class(
  buenosaires,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 5. Guatemala university -----------------------------------------------

data3 <- bi_class(
  guatemala,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2002L3,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 6. Panama university -----------------------------------------------

data4 <- bi_class(
  panama,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 7. San jose university -----------------------------------------------

data5 <- bi_class(
  sanjose,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2011L3,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 8. Santiago university -----------------------------------------------

data6 <- bi_class(
  santiago,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2017L3,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 9. Sao paolo university -----------------------------------------------

data7 <- bi_class(
  saopaolo,
  x = CNSMINUN_L3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of university attendance →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of university attendance → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

end9 <- ggdraw() +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    scale = 1
  ) +
  theme_bw()

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", ""),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_university.png",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_university.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)

#================================================================
#Merging all countries databases ==============

buenosaires$country <- "argentina"
buenosaires$ID <- buenosaires$SALID2_5
buenosaires$Population <- as.numeric(buenosaires$SPTPOPL3)
buenosaires$Water <- as.numeric(buenosaires$CNSWATNETL3)
buenosaires$Sewage <- as.numeric(buenosaires$CNSSEWANYL3)
buenosaires$Floors <- as.numeric(buenosaires$CNSFLOORL3)
buenosaires$Overcrowding <- as.numeric(buenosaires$CNSCROWD3RML3)
buenosaires$Unemployment <- as.numeric(buenosaires$CNSUNEMPL3)
buenosaires$Labor_force <- as.numeric(buenosaires$CNSLABPARTL3)
buenosaires$School_attending <- as.numeric(buenosaires$CNSST1517L3)
buenosaires$Primary_education <- as.numeric(buenosaires$CNSMINPR_L3)
buenosaires$High_education <- as.numeric(buenosaires$CNSMINHS_L3)
buenosaires$University_education <- as.numeric(buenosaires$CNSMINUN_L3)
df_argentina <- buenosaires %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)

saopaolo$country <- "brasil"
saopaolo$ID <- saopaolo$SALID2_5
saopaolo$Population <- as.numeric(saopaolo$SPTPOPL3)
saopaolo$Water <- as.numeric(saopaolo$CNSWATNETL3)
saopaolo$Sewage <- as.numeric(saopaolo$CNSSEWANYL3)
saopaolo$Floors <- NA
saopaolo$Overcrowding <- as.numeric(saopaolo$CNSCROWD3RML3)
saopaolo$Unemployment <- as.numeric(saopaolo$CNSUNEMPL3)
saopaolo$Labor_force <- as.numeric(saopaolo$CNSLABPARTL3)
saopaolo$School_attending <- as.numeric(saopaolo$CNSST1517L3)
saopaolo$Primary_education <- as.numeric(saopaolo$CNSMINPR_L3)
saopaolo$High_education <- as.numeric(saopaolo$CNSMINHS_L3)
saopaolo$University_education <- as.numeric(saopaolo$CNSMINUN_L3)
df_brasil <- saopaolo %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
santiago$country <- "chile"
santiago$ID <- santiago$salid3
santiago$Population <- as.numeric(santiago$SPTPOPL3)
santiago$Sewage <- NA
santiago$Water <- as.numeric(santiago$CNSWATNETL3)
santiago$Floors <- as.numeric(santiago$CNSFLOORL3)
santiago$Overcrowding <- as.numeric(santiago$CNSCROWD25BRL3)
santiago$Unemployment <- as.numeric(santiago$CNSUNEMPL3)
santiago$Labor_force <- as.numeric(santiago$CNSLABPARTL3)
santiago$School_attending <- as.numeric(santiago$CNSST1517L3)
santiago$Primary_education <- as.numeric(santiago$CNSMINPR_L3)
santiago$High_education <- as.numeric(santiago$CNSMINHS_L3)
santiago$University_education <- as.numeric(santiago$CNSMINUN_L3)
df_chile <- santiago %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
bogota$country <- "colombia"
bogota$ID <- bogota$SALID3
bogota$Population <- as.numeric(bogota$SPTPOPL3)
bogota$Water <- as.numeric(bogota$CNSWATNETL3)
bogota$Sewage <- as.numeric(bogota$CNSSEWANYL3)
bogota$Floors <- as.numeric(bogota$CNSFLOORL3)
bogota$Overcrowding <- as.numeric(bogota$CNSCROWD3RML3)
bogota$Unemployment <- as.numeric(bogota$CNSUNEMPL3)
bogota$Labor_force <- as.numeric(bogota$CNSLABPARTL3)
bogota$School_attending <- as.numeric(bogota$CNSST1517L3)
bogota$Primary_education <- as.numeric(bogota$CNSMINPR_L3)
bogota$High_education <- as.numeric(bogota$CNSMINHS_L3)
bogota$University_education <- as.numeric(bogota$CNSMINUN_L3)
df_colombia <- bogota %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
sanjose$country <- "costarica"
sanjose$ID <- sanjose$SALID2_5
sanjose$Population <- as.numeric(sanjose$SPTPOPL3)
sanjose$Water <- as.numeric(sanjose$CNSWATNETL3)
sanjose$Sewage <- as.numeric(sanjose$CNSSEWANYL3)
sanjose$Floors <- as.numeric(sanjose$CNSFLOORL3)
sanjose$Overcrowding <- as.numeric(sanjose$CNSCROWD3RML3)
sanjose$Unemployment <- as.numeric(sanjose$CNSUNEMPL3)
sanjose$Labor_force <- as.numeric(sanjose$CNSLABPARTL3)
sanjose$School_attending <- as.numeric(sanjose$CNSST1517L3)
sanjose$Primary_education <- as.numeric(sanjose$CNSMINPR_L3)
sanjose$High_education <- as.numeric(sanjose$CNSMINHS_L3)
sanjose$University_education <- as.numeric(sanjose$CNSMINUN_L3)
df_costarica <- sanjose %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
guatemala$country <- "guatemala"
guatemala$ID <- guatemala$SALID3
guatemala$Population <- as.numeric(guatemala$SPTPOPL3)
guatemala$Water <- as.numeric(guatemala$CNSWATNETL3)
guatemala$Sewage <- as.numeric(guatemala$CNSSEWANYL3)
guatemala$Floors <- as.numeric(guatemala$CNSFLOORL3)
guatemala$Overcrowding <- as.numeric(guatemala$CNSCROWD3RML3)
guatemala$Unemployment <- as.numeric(guatemala$CNSUNEMPL3)
guatemala$Labor_force <- as.numeric(guatemala$CNSLABPARTL3)
guatemala$School_attending <- as.numeric(guatemala$CNSST1517L3)
guatemala$Primary_education <- as.numeric(guatemala$CNSMINPR_L3)
guatemala$High_education <- as.numeric(guatemala$CNSMINHS_L3)
guatemala$University_education <- as.numeric(guatemala$CNSMINUN_L3)
df_guatemala <- guatemala %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
mexico$country <- "mexico"
mexico$ID <- mexico$salid3
mexico$Population <- as.numeric(mexico$SPTPOPL3)
mexico$Water <- NA
mexico$Sewage <- as.numeric(mexico$CNSSEWANYL3)
mexico$Floors <- as.numeric(mexico$CNSFLOORL3)
mexico$Overcrowding <- NA
mexico$Unemployment <- as.numeric(mexico$CNSUNEMPL3)
mexico$Labor_force <- as.numeric(mexico$CNSLABPARTL3)
mexico$School_attending <- as.numeric(mexico$CNSST1517L3)
mexico$Primary_education <- as.numeric(mexico$CNSMINPR_L3)
mexico$High_education <- NA
mexico$University_education <- NA
mexico$high_quantile_c <- NA
mexico$university_quantile_c <- NA
df_mexico <- mexico %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)
panama$country <- "panama"
panama$ID <- panama$salid3
panama$Population <- as.numeric(panama$SPTPOPL3)
panama$Water <- as.numeric(panama$CNSWATNETL3)
panama$Sewage <- as.numeric(panama$CNSSEWANYL3)
panama$Floors <- as.numeric(panama$CNSFLOORL3)
panama$Overcrowding <- as.numeric(panama$CNSCROWD3RML3)
panama$Unemployment <- as.numeric(panama$CNSUNEMPL3)
panama$Labor_force <- as.numeric(panama$CNSLABPARTL3)
panama$School_attending <- as.numeric(panama$CNSST1517L3)
panama$Primary_education <- as.numeric(panama$CNSMINPR_L3)
panama$High_education <- as.numeric(panama$CNSMINHS_L3)
panama$University_education <- as.numeric(panama$CNSMINUN_L3)
df_panama <- panama %>% 
  st_drop_geometry() %>% 
  select(ID,
         country,
         Population,
         Water,
         Sewage,
         Floors,
         Overcrowding,
         Unemployment,
         Labor_force,
         School_attending,
         Primary_education,
         High_education,
         University_education,
         attending_quantile_c,
         primary_quantile_c,
         high_quantile_c,
         university_quantile_c,
         PM2.5)

df_final <- rbind(df_argentina,
                  df_brasil,
                  df_chile,
                  df_colombia,
                  df_costarica,
                  df_guatemala,
                  df_mexico,
                  df_panama) %>% 
  mutate(attending_quantile_t = ntile(School_attending, 5),
         primary_quantile_t = ntile(Primary_education, 5),
         high_quantile_t = ntile(High_education, 5),
         university_quantile_t = ntile(University_education, 5))

table1(~ Population +
         Water +
         Sewage +
         Floors +
         Overcrowding +
         Unemployment +
         Labor_force +
         School_attending +
         Primary_education +
         High_education +
         University_education +
         PM2.5 | country,
       data = df_final,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

library(lme4)
library(jtools)
library(broom.mixed)
m1 <- lmer(formula = PM2.5 ~ 1 + (1|country) +
       Water +
       Sewage +
       Floors +
       Overcrowding +
       Unemployment +
       Labor_force +
       School_attending +
       Primary_education, 
     data=df_final,
     na.action=na.exclude)
summary(m1)
summ(m1, confin)

tidy(m1, conf.int=T)


VarCorr(m1)
RandomEffects <- as.data.frame(VarCorr(m1))
RandomEffects
ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between

library(qgcomp)
library(ggplot2)
library(purrr)
library(tidyverse)

Xnm <- c(
  'Water','Sewage','Floors','Overcrowding','Unemployment','Labor_force',
  'School_attending','Primary_education'
)

system.time(qc.fit <- qgcomp.noboot(df_final$PM2.5~.,dat=df1[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
qc.fit

df1 <- df_final %>% 
  filter(country =="argentina" | country =="colombia" | country =="costarica" | country =="guatemala" | country =="panama")

r4 <- df1 %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(PM2.5~.,dat=.x[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(negw=map(.x=qcfit,
                  .f=~.x$neg.weights)) %>%
  mutate(dataexport=map(.x=negw,
                        .f=~enframe(.x))) %>% 
  select(country, dataexport) %>% 
  unnest()

r4$value <- r4$value*-1

r4 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=country)) +
  geom_col(position = "dodge")

r5 <- df1 %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(PM2.5~.,dat=.x[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(posgw=map(.x=qcfit,
                   .f=~.x$pos.weights)) %>%
  mutate(dataexport=map(.x=posgw,
                        .f=~enframe(.x))) %>% 
  select(country, dataexport) %>% 
  unnest()

r5 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=country)) +
  geom_col(position = "dodge")

r6 <- rbind(r4, r5)

library(ggsci)

ggplot(r6)+
  geom_linerange(aes(x = country, ymin = 0, ymax = value, colour = country), 
                 position = position_dodge(width = 1),
                 size=0.9,
                 show.legend = T, ylab = "") +
  geom_point(aes(x = country, y = value, color = country),
             position = position_dodge(width = 1),
             size=3.3,
             show.legend = T, ylab = "") +
  coord_flip() +
  scale_color_innova("jama") +
  scale_fill_innova("jama") +
  theme_bw() +
  theme(axis.text.y=element_blank()) +
  xlab("Variables included in the Quantile g-computation analysis") +
  ylab("Scaled effect size on PM2.5 concentration") +
  geom_hline(yintercept = 0) +
  guides(scale = "none") +
  facet_grid(rows = "name", switch = "y")

ggsave("llolipop_scaled_qgcomp_PM2.5.png", last_plot(), width = 6, heigh = 9, bg = "white",dpi = 300)


library(qgcomp)
library(ggplot2)
library(purrr)
library(tidyverse)

Xnm <- c(
 'Unemployment','Labor_force',
  'School_attending','Primary_education'
)

system.time(qc.fit <- qgcomp.noboot(df_final$PM2.5~.,dat=df1[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
qc.fit

df1 <- df_final %>% 
  filter(country =="argentina" | country =="colombia" | country =="costarica" | country =="guatemala" | country =="panama" | country =="chile" | country =="mexico" | country =="brasil")

r4 <- df1 %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(PM2.5~.,dat=.x[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(negw=map(.x=qcfit,
                  .f=~.x$neg.weights)) %>%
  mutate(dataexport=map(.x=negw,
                        .f=~enframe(.x))) %>% 
  select(country, dataexport) %>% 
  unnest()

r4$value <- r4$value*-1

r4 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=country)) +
  geom_col(position = "dodge")

r5 <- df1 %>% 
  group_by(country) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(PM2.5~.,dat=.x[,c(Xnm, 'PM2.5')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(posgw=map(.x=qcfit,
                   .f=~.x$pos.weights)) %>%
  mutate(dataexport=map(.x=posgw,
                        .f=~enframe(.x))) %>% 
  select(country, dataexport) %>% 
  unnest()

r5 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=country)) +
  geom_col(position = "dodge")

r6 <- rbind(r4, r5)

library(ggsci)
library(innovar)

ggplot(r6)+
  geom_linerange(aes(x = country, ymin = 0, ymax = value, colour = country), 
                 position = position_dodge(width = 1),
                 size=0.9,
                 show.legend = T, ylab = "") +
  geom_point(aes(x = country, y = value, color = country),
             position = position_dodge(width = 1),
             size=3.3,
             show.legend = T, ylab = "") +
  coord_flip() +
  scale_color_innova("jama") +
  scale_fill_innova("jama") +
  theme_bw() +
  theme(axis.text.y=element_blank()) +
  xlab("Variables included in the Quantile g-computation analysis") +
  ylab("Scaled effect size on PM2.5 concentration") +
  geom_hline(yintercept = 0) +
  guides(scale = "none") +
  facet_grid(rows = "name", switch = "y")

ggsave("llolipop_scaled_qgcomp_PM2.5_allcountries.png", last_plot(), width = 6, heigh = 9, bg = "white",dpi = 300)

p1 <- ggplot(df_final, aes(x = country, y = School_attending)) +
  geom_boxplot(oulier.shape = NA, # remove outliers from plot
               outlier.colour = NA, 
               outlier.size = 0,) +
  scale_fill_discrete(name = "Number of Gears") +
  ggtitle("Boxplots of Mileage by Number of Cylinders and Gears") +
  xlab("Number of Cylinders") +
  ylab("Mileage (mpg)")

p2 <- ggplot(df_final, aes(x = country, y = PM2.5)) +
  geom_boxplot(oulier.shape = NA, # remove outliers from plot
               outlier.colour = NA, 
               outlier.size = 0,) +
  scale_fill_discrete(name = "Number of Gears") +
  ggtitle("Boxplots of Mileage by Number of Cylinders and Gears") +
  xlab("Number of Cylinders") +
  ylab("Mileage (mpg)")


g = ggplot_gtable(ggplot_build(p1))
g = p1 + p2

p1 <- ggplot(data = df_final, aes(factor(country), color = factor(country)))
p1 <- p1 + geom_boxplot(aes(country, School_attending), position = position_dodge(width = 0), width = 0.5, alpha = 0.5, stat="identity") +scale_fill_manual(values = c("red","blue"),labels = c("maternal","paternal"),name = "parental allele")+scale_colour_manual(values = c("red","blue"),labels = c("maternal","paternal"),name = "parental allele")
p1 <- p1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'), legend.position = "none")+theme(strip.background=element_rect(fill="white"))

ggplot(df_final, aes(country, School_attending, fill=country)) +
  geom_boxplot(position=position_dodge(width=1), size = .5, outlier.colour = "black", color="black") +
  facet_wrap(~ country)

ggplot(df_final, aes(x = factor(country), y = School_attending)) +
  geom_boxplot() +
  xlab("Number of Cylinders") +
  ylab("Mileage (mpg)") +
  ggtitle("Boxplot of Mileage by Number of Cylinders") +
  facet_grid(. ~ country, scales = "free_y", switch = "y") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Number of Gears"))

ggplot(df_final, aes(x = factor(country))) +
  geom_boxplot(aes(y = School_attending), color = "#106b53", outlier.shape = NA) +
  xlab("") +
  ylab("") +
  scale_y_continuous(name = "Mileage (mpg)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "School attandance (all ages)"),
                     name = "") +
  ggtitle("") +
  theme_bw() +
  coord_flip()

ggsave("box_school.pdf", plot = last_plot(), width = 5,height = 4)

ggplot(df_final, aes(x = factor(country))) +
  geom_boxplot(aes(y = PM2.5), color = "#106b53", outlier.shape = NA) +
  xlab("") +
  ylab("") +
  scale_y_continuous(name = "Mileage (mpg)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "PM2.5 μm/m3"),
                     name = "") +
  ggtitle("") +
  theme_bw() +
  coord_flip()

ggsave("box_pm25.pdf", plot = last_plot(), width = 5,height = 4)

ggplot(df_final, aes(x = factor(country))) +
  geom_boxplot(aes(y = PM2.5), color = "#106b53", outlier.shape = NA) +
  xlab("") +
  ylab("") +
  scale_y_continuous(name = "Mileage (mpg)") +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "PM2.5 μm/m3"),
                     name = "") +
  ggtitle("") +
  theme_bw() +
  coord_flip() +
  facet_wrap("country")

ggsave("box_facet.pdf", plot = last_plot(), width = 14,height = 11)

