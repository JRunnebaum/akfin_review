#These figures have been adjusted from what is in the report so 
#they are presenting the same scale and size

#libraries----
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr) #group_by()
library(knitr)
library(tidyr) #drop_na()
library(png)
library(grDevices)
library(EnvStats)
library(ggplot2)
library(extrafont)

#theme----
# windowsFonts(Times=windowsFont("Calibri"))

theme_sleek <- function(base_size = 12, base_family = "Calibri") {
  half_line <- base_size/2
  theme_light(base_size = 12, base_family = "Calibri") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      # axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      legend.title = element_blank(),
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
}
theme_set(theme_sleek())
options(scipen = 999) #turns off scientific notation

#global_objects ----
AVGING_YEARS <- data.frame(2013, 2014, 2015, 2016, 2017)


##############################################
#bring in sampling data
##############################################

#region_i_sampling_data----
region_i <- read.csv("data/Region_I/groundfish_ port_sampling_seak_2013_2017.csv")

region_i <- region_i %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(species_use = case_when(species_code == 110 ~ "cod",
                                 species_code == 710 ~ "sablefish",
                                 species_code == 130 ~ "lingcod",
                                 species_code == 145 ~ "yelloweye",
                                 species_code == 142 ~ "black_rockfish",
                                 species_code == 212 ~ "hagfish",
                                 species_code == 202 ~ "hagfish"))%>% 
  group_by(year, mgmt_area, species_use) %>% 
  summarise(samples = n()) %>% 
  mutate(samples = as.numeric(samples)) 


#Cod Samples
cod_region_i <- region_i %>%
  filter(species_use == "cod") %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "NSEI" ~ 2750,
                            mgmt_area == "NSEI" ~ 550)) %>% 
  mutate(target = as.numeric(target))


#Sablefish Samples
sablefish_region_i <- region_i %>%
  filter(species_use == "sablefish") %>% 
  droplevels() %>% 
  filter(mgmt_area == "NSEI" | mgmt_area == "SSEI") %>% 
  mutate(target = case_when(mgmt_area == "NSEI" ~ 1500,
                            mgmt_area == "SSEI" ~ 550))


#Lingcod Samples
lingcod_region_i <- region_i %>%
  filter(species_use == "lingcod") %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "IBS" ~ 550,
                            mgmt_area == "EYKT" ~ 550,
                            mgmt_area == "NSEO" ~ 550,
                            mgmt_area == "CSEO" ~ 550,
                            mgmt_area == "SSEOC" ~ 550,
                            mgmt_area == "NSEI" ~ 550,
                            mgmt_area == "SSEIW" ~ 550))



#Yelloweye Samples
yelloweye_region_i <- region_i %>%
  filter(species_use == "yelloweye") %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "EYKT" ~ 550,
                            mgmt_area == "NSEO" ~ 550,
                            mgmt_area == "CSEO" ~ 550,
                            mgmt_area == "SSEO" ~ 550,
                            mgmt_area == "NSEI" ~ 550,
                            mgmt_area == "SSEI" ~ 550))


#Black rockfish samples
black_region_i <- region_i %>%
  filter(species_use == "black_rockfish") %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "IBS" ~ 550,
                            mgmt_area == "EYKT" ~ 550,
                            mgmt_area == "NSEO" ~ 550,
                            mgmt_area == "CSEO" ~ 550,
                            mgmt_area == "SSEOC" ~ 550,
                            mgmt_area == "NSEI" ~ 550,
                            mgmt_area == "SSEIW" ~ 550))


#Hagfish samples
hagfish_region_i <- region_i %>%
  filter(species_use == "hagfish") %>% 
  droplevels() %>% 
  mutate(target = 550)


#region_ii_sampling_data----
region_ii <- read.csv("data/Region_II/region_ii_samples.csv")

region_ii <- region_ii %>% 
  filter(year %in% AVGING_YEARS) %>% 
  mutate(year = as.factor(year))

#Cod Samples
cod_region_ii <- region_ii %>%
  filter(species_code == 110) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550)) %>% 
  mutate(target = as.numeric(target))

#Pollock Samples
pollock_region_ii <- region_ii %>%
  filter(species_code == 270) %>% 
  droplevels() %>%
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 0))


#Sablefish Samples
sablefish_region_ii <- region_ii %>%
  filter(species_code == 710) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))

#Lingcod Samples
lingcod_region_ii <- region_ii %>%
  filter(species_code == 130) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))

#Black rockfish samples
black_region_ii <- region_ii %>%
  filter(species_code == 142) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 0))

#Yelloweye Samples
yelloweye_region_ii <- region_ii %>%
  filter(species_code == 145) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))

#Quillback Samples
quillback_region_ii <- region_ii %>%
  filter(species_code == 147) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))

#Rougheye Samples
rougheye_region_ii <- region_ii %>%
  filter(species_code == 151) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))

#Shortraker Samples
shortraker_region_ii <- region_ii %>%
  filter(species_code == 152) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "PWS" ~ 550,
                            mgmt_area == "CI" ~ 550))



#region_iv_sampling_data----
region_iv <- read.csv("data/Region_IV/yearly_samples.csv")

region_iv <- region_iv %>% 
  filter(year %in% AVGING_YEARS) %>% 
  mutate(year = as.factor(year)) %>% 
  dplyr::rename(species_code = species) %>% 
  mutate(species = case_when(species_code == 110 ~ "cod",
                             species_code == 710 ~ "sablefish",
                             species_code == 130 ~ "lingcod",
                             species_code == 142 ~ "black_rockfish",
                             species_code == 173 ~ "dark_rockfish")) 


#Cod Samples
cod_region_iv <- region_iv %>%
  filter(species_code == 110) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "K" ~ 1000,
                            mgmt_area == "L" ~ 500,
                            mgmt_area == "M" ~ 500)) %>% 
  mutate(target = as.numeric(target))

#Sablefish Samples
sablefish_region_iv <- region_iv %>%
  filter(species_code == 710) %>% 
  droplevels() 

#Lingcod Samples
lingcod_region_iv <- region_iv %>%
  filter(species_code == 130) %>% 
  droplevels() 

#Black rockfish samples
black_region_iv <- region_iv %>%
  filter(species_code == 142) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "K" ~ 1000,
                            mgmt_area == "L" ~ 1000,
                            mgmt_area == "M" ~ 1000)) %>% 
  mutate(target = as.numeric(target))

#Dark rockfish samples
dark_region_iv <- region_iv %>%
  filter(species_code == 173) %>% 
  droplevels() %>% 
  mutate(target = case_when(mgmt_area == "K" ~ 500,
                            mgmt_area == "L" ~ 500,
                            mgmt_area == "M" ~ 500)) %>% 
  mutate(target = as.numeric(target))




#pacific_cod_sampleing_plots




#region_i_plots----
cod_region_i_plot <- ggplot(data = cod_region_i)+
  geom_point(aes(x = year, y = samples, color = year, shape = mgmt_area), size = 3)+
  geom_hline(yintercept = 1000)+
  geom_hline(yintercept = 500)+
  ylim(0,5000) +
  ylab("Number of Samples\n")+
  xlab("Year\n")+
  ggtitle("Region I Pacific Cod Port Sampling") 
ggsave(plot = cod_region_i_plot, file = paste0("figures/region_i_pcod_samples.png"),
       height = 6, width = 4, units = "in")

#region_ii_plots----
cod_region_ii_plot <- ggplot(data = cod_region_ii)+
  geom_point(aes(x = year, y = samples, color = year, shape = mgmt_area), size = 3)+
  geom_hline(yintercept = 1000)+
  geom_hline(yintercept = 500)+
  ylim(0,5000) +
  # scale_fill_grey() + 
  # scale_color_grey() +
  ylab("Number of Samples\n")+
  xlab("Year\n")+
  ggtitle("Region II Pacific Cod Port Sampling") 
ggsave(plot = cod_region_ii_plot, file = paste0("figures/region_ii_pcod_samples.png"),
       height = 6, width = 4, units = "in")

#region_iv_plots----
cod_region_iv_plot <- ggplot(data = cod_region_iv)+
  geom_point(aes(x = year, y = samples, color = year, shape = mgmt_area), size = 3)+
  geom_hline(yintercept = 1000)+
  geom_hline(yintercept = 500)+
  ylim(0,5000) +
  # scale_fill_grey() + 
  # scale_color_grey() +
  ylab("Number of Samples\n")+
  xlab("Year\n")+
  ggtitle("Region IV Pacific Cod Port Sampling") 
ggsave(plot = cod_region_iv_plot, file = paste0("figures/region_iv_pcod_samples.png"),
       height = 6, width = 4, units = "in")


#region_i_cod_landings----
region_i_samp <- read.csv("data/Region_I/groundfish_ port_sampling_seak_2013_2017.csv")

region_i_cod <- region_i_samp %>% 
  filter(year %in% AVGING_YEARS) %>%
  filter(species_code == 110) %>% 
  mutate(date = mdy(date)) %>% 
  mutate(julian = yday(date)) %>% 
  droplevels() %>% 
  select(year, julian, mgmt_area) %>% 
  mutate(type = "sample")
  # group_by(date, mgmt_area) %>%
  # summarise_all(funs(sum)) 

region_i_land <- read.csv("data/Region_I/pacific_cod.csv")

region_i_land <- region_i_land %>% 
  mutate(date_landed = mdy(date_landed),
         year = year(date_landed)) %>% 
  filter(year %in% AVGING_YEARS) %>%
  mutate(julian = yday(date_landed)) %>% 
  select(year, julian, mgmt_area, pounds) %>% 
  group_by(julian, pounds) %>%
  summarise_all(funs(mean)) 
  # select(year, julian, mgmt_area) %>% 
  # mutate(type = "landing")
  
cod_i <- full_join(region_i_land, region_i_cod) %>% 
  group_by(julian, type) %>%
  summarise_all(funs(sum)) 


ggplot()+
  geom_bar(data = cod_i, aes(x = julian, fill = type))

