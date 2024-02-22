# Jared Adam
# started 1/18/2024

# three years of ce2 micros in corn
    # 2021, 2022, and 2023
 # two years of corn and bean micros 
    # 2022 and 2023
# two timings for each

# packages ####
library(tidyverse)
library(vegan)
library(lme4)
library(performance)
library(lmtest)
library(MASS)
library(plotly)
library(ggpmisc)

# data ####
micros <- CE2_counts
micros

corn_cc <- PSA_corn_cc_biomass
corn_cc

bean_cc <- beans_cc_biomasss
bean_cc

yield <- PSA_PA_yield
yield

bean_yield <- PA_PSA_beans_yield_all

# cleaning ####
colnames(micros)
unique(micros$date)

?distinct
# eliminating the duplicated rows
micros_ready <- micros %>% 
  mutate_at(vars(4:41), as.numeric) %>% 
  group_by(date, crop, plot) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  ungroup() %>% 
  print(n = Inf)

# need to add treatment in now
micros_set <- micros_ready %>% 
  mutate(plot = replace(plot, plot == 507, 502)) %>% # there is a sneaky 507 plot number
  mutate(trt = case_when(plot %in% c(101,203,304,401,503) ~ 'Check',
                         plot %in% c(102,201,303,402,502) ~ 'Green',
                         plot %in% c(103,204,302,403,501) ~ 'Brown',
                         plot %in% c(104,202,301,404,504) ~ 'Gr-Br')) %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  mutate_at(vars(43), as.factor) %>% 
  filter(!row_number() %in% c(46,47,71,83)) %>% # these rows are all NA, so when I replace with 0, they become all 0 and then vegdist cannot function. removing them early
  replace(is.na(.),0) %>% 
  print(n = Inf)
# check to make sure these changes worked
colnames(micros_set)
unique(micros_set$trt)
unique(micros_set$plot)
which(micros_set$trt == 'NA')
which(micros_set$trt == 'Green')
class(micros_set$trt)

# scores ####
micros_set
colnames(micros_set)
#1. aggregate columns
  # e.g., mites into one column 

aggregate_micros <- micros_set %>% 
  mutate(mites = Orb + Norb,
          hemiptera = hemip + Enich + Coccomorpha,
          adult = a_dipt + lep + sipoopter, 
          coleop_1 = AC + OAC) %>% 
  dplyr::select(-Orb, -Norb, -hemip, -Enich, -pod, -ento, -sym, -AC, -OAC, 
                -a_dipt, -Coccomorpha, -lep, -sipoopter) %>% 
  rename(dip_5 = 'Dip>5',
         dip_20 = 'Dip<5',
         chil_10 = 'Chil>5',
         chil_20 = 'Chil<5',
         zygentoma = archaeognatha) %>% 
  rename_with(tolower)
colnames(aggregate_micros)

micro_scores <- aggregate_micros %>% 
  mutate(mite_score = if_else(mites >= 1, 20, 0),
         pro_score = if_else(protura >= 1, 20,0),
         dip_score = if_else(diplura >= 1, 20, 0),
         hemip_score = if_else(hemiptera >= 1, 1, 0), #1 unless cicada larvae 
         thrips_score = if_else(thrips >= 1, 1, 0),
         coleop_score = if_else(coleop_1 >= 1, 1, 0),
         hymen_score = if_else(hymen >= 1, 1, 0),
         formic_score = if_else(formicid >= 1, 5, 0), 
         beetle_larv__score = if_else(cl >= 1, 10, 0),
         other_fly_larv_score = if_else(ol >= 1, 10, 0),
         spider_score = if_else(spider >= 1, 5, 0),
         pseudo_score = if_else(pseu >= 1, 20, 0), 
         isop_score = if_else(iso >= 1, 10, 0), 
         chil_10_score = if_else(chil_10 >= 1, 10, 0),
         chil_20_score = if_else(chil_20 >= 1, 20, 0),
         diplo_20_score = if_else(dip_20 >= 1, 20, 0),
         diplo_5_score = if_else(dip_5 >= 1, 5, 0),
         symph_score = if_else(simphyla >= 1, 20, 0), 
         col_20_score = if_else(col_20 >= 1, 20, 0),
         col_10_score = if_else(col_10 >= 1, 10, 0), 
         col_6_score = if_else(col_6 >= 1, 6, 0),
         col_4_score = if_else(col_4 >= 1, 4, 0),
         adult_score = if_else(adult >= 1, 1, 0),
         psocop_score = if_else(psocodea >= 1, 1, 0),
         pauropod_score = if_else(pauropoda >= 1, 20, 0),
         dermaptera_score = if_else(dermaptera >= 1, 1, 0),
         zygentoma_score = if_else(zygentoma >= 1, 10, 0)) %>% 
   dplyr::select(-mites, -protura, -diplura, -hemiptera, -thrips, -coleop_1, -hymen,
          -formicid, -cl, -ol, -spider, -pseu, -iso, -chil_10, -chil_20,-dip_20,-dip_5, -simphyla,
          -col_20, -col_10, -col_6, -col_4, -adult, -pauropoda, -annelid, -psocodea, -dermaptera,
          -zygentoma)
colnames(micro_scores)

### 
##
#
(27.1/(sqrt(1))) # is this correct? this is how it is done in the pipe

mean_scores <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                              plot %in% c(201,202,203,204) ~ 2, 
                              plot %in% c(301,302,303,304) ~ 3, 
                              plot %in% c(401,402,403,404) ~ 4,
                              plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  dplyr::group_by(date, trt, crop) %>% 
  dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% #plyr has summarize
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, form = "%Y"),
         year = as.factor(year),
         date = as.factor(date)) %>% 
  print(n = Inf)
colnames(mean_scores)
unique(mean_scores$date)

overall_fig <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  dplyr::group_by(trt, crop) %>% 
  dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% 
  print(n = Inf)

# done here for now 
# overall corn fig
corn_trt_year_score <- micro_scores %>%
  filter(crop == "corn") %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, form = "%Y"),
         year = as.factor(year),
         date = as.factor(date)) %>%
  dplyr::group_by(year, trt) %>% 
    dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% 
  print(n = Inf)

# score stats ####
# all them 

total_ks_crop <- kruskal.test(avg ~ crop, data = mean_scores)
total_ks_crop


# beans 

bean_scores <- mean_scores %>% 
  filter(crop == "beans")
b_22_scores <- bean_scores %>% 
  filter(year == "2022")
b_23_scores <- bean_scores %>% 
  filter(year == "2023")

b_22_d <- kruskal.test(avg ~ date, data = b_22_scores)
b_22_d
# date = 0.02092
b_22_trt <- kruskal.test(avg ~ trt, data = b_22_scores)
b_22_trt

b_23_d <- kruskal.test(avg ~ date, data = b_23_scores)
b_23_d
b_23_trt <- kruskal.test(avg ~ trt, data = b_23_scores)
b_23_trt

b_year <- kruskal.test(avg ~ year, data = bean_scores)
b_year

# corn

# 2021 is only corn
micros_21 <- filter(mean_scores, year == '2021') %>% 
  mutate(timing = case_when( date == "2021-07-01"~ "1",
                             date == "2021-09-01" ~ "2")) %>% 
  mutate(timing = as.factor(timing))
corn_21_d <- kruskal.test(avg ~ date, data  = micros_21)
corn_21_d
corn_21_trt <- kruskal.test(avg ~ trt, data = micros_21)
corn_21_trt

corn_scores <- mean_scores %>% 
  filter(crop == "corn")
c_22_scores <- corn_scores %>% 
  filter(year == "2022")
c_23_scores <- corn_scores %>% 
  filter(year == "2023")

corn_22_d <- kruskal.test(avg ~ date, data  = c_22_scores)
corn_22_d
corn_22_trt <- kruskal.test(avg ~ trt, data = c_22_scores)
corn_22_trt
corn_23_d <- kruskal.test(avg ~ date, data  = c_23_scores)
corn_23_d
corn_23_trt <- kruskal.test(avg ~ trt, data = c_23_scores)
corn_23_trt

corn_year <- kruskal.test(avg ~ year, data = corn_scores)
corn_year
pairwise.wilcox.test(corn_scores$avg, corn_scores$year, p.adjust.method = "BH")
# 0.0011 
# 2021 - 23 = 0.00047
# 2021 - 22 = 0.0028


# score plots ####

ggplot(overall_fig, aes(x = trt, y = avg, fill = crop))+
  geom_bar(stat = 'identity', position = "dodge")+
  scale_fill_manual(values = c( "#1B9E77","#D95F02"),
                    name = "Crop", labels = c("Soybean", "Corn"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  geom_errorbar(aes(x = trt, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Overall average QBS scores by crop",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Average QBS scores")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# ggplot(filter(mean_scores, crop == "corn"), aes(x = trt, y = avg, fill = date))+
#   geom_bar(stat = 'identity', position = 'dodge')+
#   facet_wrap(~date)+
#   geom_errorbar(aes(x = trt, ymin = avg-se, ymax = avg+se))+
#   labs(title = "Overall corn mean QBS scores x year", 
#        x = "Treatment",
#        y = "Average QBS scores")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

# corn by year plot 
corn_trt_year_score

ggplot(corn_trt_year_score, aes(x = year, y = avg, fill = factor(trt, levels = c("Check", "Brown", "Green", "Gr-Br"))))+
  geom_bar(stat = 'identity', position = "dodge")+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"),
                    name = "Treatment")+
  geom_errorbar(aes(x = year, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Corn: Overall average QBS scores by crop",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Average QBS scores")+
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 0.8, y = 110, label = "a", size = 8)+
  annotate("text", x = 1.8, y = 65, label = "b", size = 8)+
  annotate("text", x = 2.8, y = 65, label = "b", size = 8)+
  annotate("text", x = .8, y = 115, label = "p = 0.0011", size = 8)
  


# 2021
# micros_21 <- filter(mean_scores, date %in% c("9/1/2021", "7/1/2021"))
unique(mean_scores$date)
micros_21 <- filter(mean_scores, year == '2021') %>% 
  mutate(timing = case_when( date == "2021-07-01"~ "1",
                             date == "2021-09-01" ~ "2")) %>% 
  mutate(timing = as.factor(timing))
unique(micros_21$crop)

ggplot(filter(micros_21, crop == "corn"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  facet_wrap(~date)+
  ggtitle("Corn: Average QBS scores")+
  labs(subtitle = "Year: 2021",
       x = "Treatment",
       y = "Average QBS score")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, linewidth=1.3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 2022
# micros_22 <- filter(mean_scores, date %in% c('6/22/2022', '9/23/2022'))
micros_22 <- filter(mean_scores, year == "2022")
unique(micros_22$date)


# dat_text <- data.frame(
#   avg = 100, 
#   lab = "*",
#   date = factor("2022-06-22", levels = c("2022-06-22", "2022-09-23")))

ggplot(filter(micros_22, crop == "beans"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  facet_wrap(~factor(date, c("2022-06-22", "2022-09-23")))+
  ggtitle("Soybean: Average QBS scores")+
  labs(subtitle = "Year: 2022",
       x = "Treatment",
       y = "Average QBS score")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(filter(micros_22, crop == "corn"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  facet_wrap(~factor(date, c("2022-06-22", "2022-09-23")))+
  ggtitle("Corn: Average QBS scores")+
  labs(subtitle = "Year: 2022",
       x = "Treatment",
       y = "Average QBS score")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 2023
# micros_23 <- filter(mean_scores, date %in% c('7/18/2023','11/4/2023'))
micros_23 <- filter(mean_scores, year == "2023")
unique(micros_23$date)


ggplot(filter(micros_23, crop == "beans"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  facet_wrap(~factor(date, c("2023-07-18","2023-11-04")))+
  ggtitle("Soybean: Average QBS scores")+
  labs(subtitle = "Year: 2023",
       x = "Treatment",
       y = "Average QBS score")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(filter(micros_23, crop == "corn"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  facet_wrap(~factor(date, c("2023-07-18","2023-11-04")))+
  ggtitle("Corn: Average QBS scores")+
  labs(subtitle = "Year: 2023",
       x = "Treatment",
       y = "Average QBS score")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())##
###

# CC biomass stats ####
# cc biomass time 
ccc_clean <- corn_cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_biomass_g),
            cc_sd = sd(cc_biomass_g),
            cc_se = cc_sd/sqrt(n()))
unique(bean_cc$trt)
bcc_clean <- bean_cc %>% 
  mutate_at(vars(1:2), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g)) %>% 
  mutate(trt = factor(trt, levels= c("br", "grbr", "gr"))) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_g), 
            cc_sd = sd(cc_g),
            cc_se = cc_sd /sqrt(n()))

# bean stats
bcc_ <- bean_cc %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  group_by(year, plot, trt) %>% 
  summarise(mean = mean(cc_g))
bbc_aov <- aov(mean ~ year, data = bcc_)
hist(residuals(bbc_aov))
summary(bbc_aov)
TukeyHSD(bbc_aov)

bcc_22 <- bcc_ %>% 
  filter(year == "2022")
bbc_22_aov <- aov(mean ~ trt, data = bcc_22)
hist(residuals(bbc_22_aov))
summary(bbc_22_aov)
TukeyHSD(bbc_22_aov)

bcc_23 <- bcc_ %>% 
  filter(year == "2023")
bbc_23_aov <- aov(mean ~ trt, data = bcc_23)
hist(residuals(bbc_23_aov))
summary(bbc_23_aov)
TukeyHSD(bbc_23_aov)

# corn stats
cc_ <- corn_cc %>%
  filter(trt != "check") %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year, plot, trt) %>% 
  summarise(mean = mean(cc_biomass_g))
cc_aov <- aov(mean ~ year, data = cc_)
hist(residuals(cc_aov))
summary(cc_aov)
TukeyHSD(cc_aov)
# 21-22 diff, 21-23 diff, 22-23 no diff

cc_21 <- cc_ %>% 
  filter(year == "2021")
cc_21_aov <- aov(mean ~ trt, data = cc_21)
hist(residuals(cc_21_aov))
summary(cc_21_aov)
TukeyHSD(cc_21_aov)
# all diff


cc_22 <- cc_ %>% 
  filter(year == "2022")
cc_22_aov <- aov(mean ~ trt, data = cc_22)
hist(residuals(cc_22_aov))
summary(cc_22_aov)
TukeyHSD(cc_22_aov)
# all diff

cc_23 <- cc_ %>% 
  filter(year == "2023")
cc_23_aov <- aov(mean ~ trt, data = cc_23)
hist(residuals(cc_23_aov))
summary(cc_23_aov)
TukeyHSD(cc_23_aov)
#grbr-br = diff, gr - br = diff

# cc biomass plots ####
bcc_
ggplot(bcc_, aes(x = trt, y = mean, fill = trt))+
  geom_violin()+
  geom_jitter()+
  scale_x_discrete(limits=c("br", "gr", "grbr"),
                   labels = c("Brown", "Green", "GrBr"))+  
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "#7570B3"))+
  facet_wrap(~year, scales = "free")+
  labs(title = "Soybean: Cover crop biomass",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       y = "Average cover crop biomass")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bcc_clean
ggplot(bcc_clean, aes(x = trt, y = cc_mean, fill = trt))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_x_discrete(limits=c("br", "gr", "grbr"),
                   labels = c("Brown", "Green", "GrBr"))+  
  scale_fill_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  facet_wrap(~year)+
  geom_errorbar(aes(x = trt, ymin = cc_mean-cc_se, ymax = cc_mean+cc_se), width=0.4, 
                colour="black", alpha=0.9, linewidth=1.3)+
  labs(title = "Soybean: Cover crop biomass",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       y = "Average cover crop biomass")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(cc_, aes(x = trt, y = mean, fill = trt))+
  geom_violin()+
  geom_jitter()+
  scale_x_discrete(limits=c("brown", "green", "gr-br"),
                   labels = c("Brown", "Green", "GrBr"))+  
  scale_fill_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  facet_wrap(~year, scales = "free")+
  labs(title = "Corn: Cover crop biomass",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Average cover crop biomass")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(filter(ccc_clean, trt != 'check'), aes(x = trt, y = cc_mean, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  scale_x_discrete(limits=c("brown", "green", "gr-br"),
                   labels = c("Brown", "Green", "GrBr"))+
  facet_wrap(~year)+
  geom_errorbar(aes(x = trt, ymin = cc_mean-cc_se, ymax = cc_mean+cc_se), width=0.4, 
                colour="black", alpha=0.9, linewidth=1.3)+
  labs(title = "Corn: Cover crop biomass",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Average cover crop biomass")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  


  

# corn micros only
# corn_micros <- mean_scores %>% 
#   filter(crop %in% 'corn') %>% 
#   print(n = Inf)
# unique(corn_micros$crop)

# total micros x year 
# corn_micros_yr <- corn_micros %>% 
#   ungroup() %>% 
#   mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
#   mutate(year = format(date, "%Y")) %>% 
#   relocate(year) %>% 
#   dplyr::select(-date) %>% 
#   group_by(year, trt) %>% 
#   summarise(year_mean = mean(avg))

# the above code worked, but i think it was wrong 1/19/2024
# # took the avg of an avg
# #let's check this

###
##
#

# mirco scores x cc biomass ####

# corn # 

colnames(test)
corn_micros_yr <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year, total_score) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt, crop) %>% 
  summarise(mean_score_yr = mean(total_score)) %>% 
  filter(crop == 'corn') %>% 
  dplyr::select(-crop) %>% 
  print(n = Inf)

# cbind now
new_micro_cc <- cbind(corn_micros_yr, ccc_clean)
new_micro_cc <- new_micro_cc %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1) %>% 
  rename(trt = trt...2) %>% 
  dplyr::select(-trt...5) %>% 
  relocate(year, trt) %>% 
  filter(trt != "Check")


cc_score_cc_aov <- glm(mean_score_yr ~ cc_mean, data = new_micro_cc)
summary(cc_score_cc_aov)
hist(residuals(cc_score_cc_aov))


ggplot(new_micro_cc, aes(x = cc_mean, y = mean_score_yr))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Corn: QBS Scores ~ CC biomass",
       subtitle = "Years: 2021-2023",
       y = 'Average QBS scores',
       x = 'Average cc biomass (g)')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 35, y = 115, label = "p = 0.001583 **", size = 8)


ggplot(filter(new_micro_cc, year == "2021"), aes(x = cc_mean , y = mean_score_yr))+
  geom_point(aes(color = trt), size = 6)+
  geom_smooth(method = 'lm', se = FALSE) + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  # geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "2021 Corn: Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')

ggplot(filter(new_micro_cc, year == "2022"), aes(x = cc_mean , y = mean_score_yr))+
  geom_point(aes(color = trt), size = 6)+
  geom_smooth(method = 'lm', se = FALSE) + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  # geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "2022 Corn: Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')

ggplot(filter(new_micro_cc, year == "2023"), aes(x = cc_mean , y = mean_score_yr))+
  geom_point(aes(color = trt), size = 6)+
  geom_smooth(method = 'lm', se = FALSE) + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  # geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "2023 Corn: Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')


###
##
#
#
##
###

# beans #
bcc_clean 

micro_scores
unique(micro_scores$crop)
bean_micros_yr <- micro_scores %>% 
relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year, total_score) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt, crop) %>% 
  summarise(mean_score_yr = mean(total_score)) %>% 
  filter(crop == 'beans') %>% 
  dplyr::select(-crop) %>% 
  filter(trt != "Check") %>% 
  print(n = Inf)

new_micro_bcc <- cbind(bcc_clean, bean_micros_yr) %>% 
  rename(trt = trt...2,
         year = year...1) %>% 
  dplyr::select(-trt...7,
                -year...6) %>% 
  mutate(trt = case_when(trt == "br" ~ "Brown",
                         trt == "grbr" ~ "Gr-Br",
                         trt == "gr" ~ "Green"))

ggplot(new_micro_bcc, aes(x = cc_mean, y = mean_score_yr))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Soybean: QBS Scores ~ CC biomass",
       subtitle = "Years: 2022-2023",
       y = 'Average QBS scores',
       x = 'Average cc biomass (g)')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bb_score_cc_ <- glm(mean_score_yr ~ cc_mean, data = new_micro_bcc)
summary(bb_score_cc_)
hist(residuals(bb_score_cc_))

ggplot(new_micro_bcc, aes(x = mean_score_yr, y = cc_mean, label = year))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm') + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "Bean Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')

ggplot(filter(new_micro_bcc, year == "2022"), aes(x = cc_mean , y = mean_score_yr))+
  geom_point(aes(color = trt), size = 6)+
  geom_smooth(method = 'lm', se = FALSE) + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  # geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "2022 Bean: Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')

ggplot(filter(new_micro_bcc, year == "2023"), aes(x = cc_mean , y = mean_score_yr))+
  geom_point(aes(color = trt), size = 6)+
  geom_smooth(method = 'lm', se = FALSE) + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  # geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "2023 Bean: Micro scores x CC biomass and year", 
       y = 'Average micro scores',
       x = 'Average cc biomass (g)')

# micro abundance x cc biomass ####

# corn

micros_set
colnames(micros_set)
unique(micros_set$crop)
corn_micro_totals <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  filter(crop == 'corn') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt) %>% 
  summarise(avg_abund = mean(total_abund))

new_total_cc <- cbind(ccc_clean, corn_micro_totals)
new_total_cc <- new_total_cc %>% 
  rename(year = year...1, 
         trt = trt...7) %>% 
  dplyr::select(-year...6, -trt...2)

?glm
cc_ccXabund <- glm(cc_mean ~ avg_abund,
                   data = new_total_cc)
hist(residuals(cc_ccXabund))
summary(cc_ccXabund)

ggplot(filter(new_total_cc, trt != 'Check'), aes(x = avg_abund, y = cc_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Corn: CC biomass ~ Micro abundance",
       subtitle = "Years: 2021-2023",
       y = 'Average cc biomass (g)',
       x = 'Average micro abundance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 24),
            plot.subtitle = element_text(size = 18),
            axis.line = element_line(size = 1.25),
            axis.ticks = element_line(size = 1.25),
            axis.ticks.length = unit(.25, "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
  annotate("text", x = 17, y = 260, label = "p = 0.00229 **", size = 8)

cc_abund_cc <- glm(avg_abund ~ cc_mean,
                   data = new_total_cc)
hist(residuals(cc_abund_cc))
summary(cc_abund_cc)

ggplot(filter(new_total_cc, trt != 'Check'), aes(x = cc_mean, y = avg_abund))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Corn: Micro abundance ~ CC biomass",
       subtitle = "Years: 2021-2023",
       y = 'Average micro abundance',
       x = 'Average cc biomass (g)')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 35, y = 90, label = "p = 0.00229 **", size = 8)

# beans 

micros_set
colnames(micros_set)
unique(micros_set$crop)
bean_micro_totals <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  filter(crop == 'beans') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt) %>% 
  summarise(avg_abund = mean(total_abund)) %>% 
  filter(trt != "Check")

bcc_clean
new_total_bcc <- cbind(bcc_clean, bean_micro_totals)
new_total_bcc <- new_total_bcc %>% 
  rename(year = year...1, 
         trt = trt...2) %>% 
  dplyr::select(-year...6, -trt...7) %>% 
  mutate(trt = case_when(trt == "br" ~ "Brown",
                         trt == "grbr" ~ "Gr-Br",
                         trt == "gr" ~ "Green"))

?glm
bb_abund_cc <- glm(avg_abund ~ cc_mean,
                     data = new_total_bcc)
hist(residuals(bb_abund_cc))
summary(bb_abund_cc)

ggplot(new_total_bcc, aes(x = avg_abund, y = cc_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Soybean: CC biomass ~ Micro abundance ",
       subtitle = "Years: 2022-2023",
       y = 'Average cc biomass (g)',
       x = 'Average micro abundance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(new_total_bcc, aes(x = cc_mean, y = avg_abund))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+
  labs(title = "Soybean: Micro abundance ~ CC biomass",
       subtitle = "Years: 2022-2023",
       y = 'Average micro abundance',
       x = 'Average cc biomass (g)')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
###
##
#

# yield x micro abundance ####

# Corn 

yield
colnames(yield)

yield$plot <- gsub('-[0-9.]','', yield$plot) # remove - and all numbers following
test_b <- filter(yield, trt == 'Brown' & year == '2021') %>% 
  summarise(mean = mean(bu_ac))
test_g <- filter(yield, trt == 'Green' & year == '2021') %>% 
  summarise(mean = mean(bu_ac))

yield_clean <- yield %>% 
  dplyr::select(-block, -trt_num, -crop) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, trt) %>% 
  summarise(lb_pass_mean = mean(lb_pass_moisture), 
         lb_ac_mean = mean(lb_ac),
         bu_ac_mean = mean(bu_ac)) %>% 
  mutate(year = as.factor(year)) %>%
  print(n = Inf)

test_b <- filter(yield, trt == 'Brown' & year == '2021') %>% 
  summarise(brown_mean = mean(bu_ac)) %>% 
  print(n = Inf)
test_g <- filter(yield, trt == 'Green' & year == '2021') %>% 
  summarise(green_mean = mean(bu_ac))%>% 
  print(n = Inf)

# micros 
corn_micro_totals

new_yield_abundance <- cbind(corn_micro_totals, yield_clean)
new_yield_abundance <- new_yield_abundance %>% 
  rename(year = year...4, 
         trt = trt...2) %>% 
  dplyr::select(-year...1, - trt...5) %>% 
  relocate(year, trt) %>% 
  print( n = Inf)

?glm


cc_yield_yield <- glm(bu_ac_mean ~ avg_abund,
                      data = new_yield_abundance)
hist(residuals(cc_yield_yield))
summary(cc_yield_yield)

ggplot(new_yield_abundance, aes(x = avg_abund, y = bu_ac_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  labs(title = "Corn: Yield ~ Micro abundance",
       subtitle = "Years: 2021-2023",
       y = 'Average bu/ac',
       x = 'Average micro abundance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 18, y = 240, label = "p = 0.001999 **", size = 8)

cc_abund_yield <- glm(avg_abund ~ bu_ac_mean,
                   data = new_yield_abundance)
hist(residuals(cc_abund_yield))
summary(cc_abund_yield)

ggplot(new_yield_abundance, aes(x = bu_ac_mean, y = avg_abund))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02","#E7298A" ,"#7570B3", "#1B9E77"))+
  labs(title = "Corn: Micro abundance ~ Yield",
       subtitle = "Years: 2021-2023",
       y = 'Average micro abundance',
       x = 'Average bu/ac')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 104, y = 64, label = "p = 0.002 **", size = 8)

# Beans
bean_yield$plot <- gsub('-[0-9.]','', bean_yield$plot) # remove - and all numbers following


by_clean <- bean_yield %>% 
  dplyr::select(-block, -trt_num) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, trt) %>% 
  summarise(lb_pass_mean = mean(lb_pass_moisture), 
            lb_ac_mean = mean(lb_ac),
            bu_ac_mean = mean(bu_ac)) %>% 
  print(n = Inf)
by_clean<- by_clean[3:10,]
  
# micros 
bean_micro_totals_YIELD <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  filter(crop == 'beans') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt) %>% 
  summarise(avg_abund = mean(total_abund))

bean_micro_totals_YIELD

bean_yield_abundance <- cbind(bean_micro_totals_YIELD, by_clean)
bean_yield_abundance <- bean_yield_abundance %>% 
  rename(year = year...4, 
         trt = trt...2) %>% 
  dplyr::select(-year...1, - trt...5) %>% 
  relocate(year, trt) %>% 
  print( n = Inf)


bb_yield_abund <- glm(bu_ac_mean ~ avg_abund,
                      data = bean_yield_abundance)
hist(residuals(bb_yield_abund))
summary(bb_yield_abund)

ggplot(bean_yield_abundance, aes(x = avg_abund, y = bu_ac_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  labs(title = "Soybean: Yield ~ Micro abundance",
       subtitle = "Years: 2022-2023",
       y = 'Average bu/ac',
       x = 'Average micro abundance')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

bb_abund_yield <- glm(avg_abund ~ bu_ac_mean,
                      data = bean_yield_abundance)
hist(residuals(bb_abund_yield))
summary(bb_abund_yield)

ggplot(bean_yield_abundance, aes(x = bu_ac_mean, y = avg_abund))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02","#E7298A" ,"#7570B3", "#1B9E77"))+
  labs(title = "Soybean: Micro abundance ~ Yield",
       subtitle = "Years: 2022-2023",
       y = 'Average micro abundance',
       x = 'Average bu/ac')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# yield x micro scores ####

# beans
# need check

bean_micro_scores_YIELD <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year, total_score) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt, crop) %>% 
  summarise(mean_score_yr = mean(total_score)) %>% 
  filter(crop == 'beans') %>% 
  dplyr::select(-crop) %>% 
  print(n = Inf)

by_clean <- bean_yield %>% 
  dplyr::select(-block, -trt_num) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, trt) %>% 
  summarise(lb_pass_mean = mean(lb_pass_moisture), 
            lb_ac_mean = mean(lb_ac),
            bu_ac_mean = mean(bu_ac)) %>% 
  print(n = Inf)
by_clean<- by_clean[3:10,]

bean_yield_scores <- cbind(bean_micro_scores_YIELD, by_clean)
bean_yield_scores <- bean_yield_scores %>% 
  rename(year = year...4, 
         trt = trt...2) %>% 
  dplyr::select(-year...1, - trt...5) %>% 
  relocate(year, trt) %>% 
  print( n = Inf)


bb_scoresXyield <- glm(bu_ac_mean ~ mean_score_yr, data = bean_yield_scores)
hist(residuals(bb_scoresXyield))
summary(bb_scoresXyield)


ggplot(bean_yield_scores, aes(x = mean_score_yr, y = bu_ac_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  labs(title = "Soybean: Yield ~ QBS Scores",
       subtitle = "Years: 2022-2023",
       y = 'Average bu/ac',
       x = 'Average QBS scores')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# corn 
# need check

corn_micro_scores_YIELD <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, crop, plot, trt, block) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year, total_score) %>% 
  dplyr::select(-date) %>% 
  group_by(year, trt, crop) %>% 
  summarise(mean_score_yr = mean(total_score)) %>% 
  filter(crop == 'corn') %>% 
  dplyr::select(-crop) %>% 
  print(n = Inf)

yield_clean <- yield %>% 
  dplyr::select(-block, -trt_num, -crop) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, trt) %>% 
  summarise(lb_pass_mean = mean(lb_pass_moisture), 
            lb_ac_mean = mean(lb_ac),
            bu_ac_mean = mean(bu_ac)) %>% 
  mutate(year = as.factor(year)) %>%
  print(n = Inf)

corn_yield_scores <- cbind(corn_micro_scores_YIELD, yield_clean)
corn_yield_scores <- corn_yield_scores %>% 
  rename(year = year...4, 
         trt = trt...2) %>% 
  dplyr::select(-year...1, - trt...5) %>% 
  relocate(year, trt) %>% 
  print( n = Inf)

cc_scoresXyield <- glm(bu_ac_mean ~ mean_score_yr, data = corn_yield_scores)
hist(residuals(cc_scoresXyield))
summary(cc_scoresXyield)

ggplot(corn_yield_scores, aes(x = mean_score_yr, y = bu_ac_mean))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
  scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  labs(title = "Corn: Yield ~ QBS Scores",
       subtitle = "Years: 2021-2023",
       y = 'Average bu/ac',
       x = 'Average QBS scores')+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  annotate("text", x = 40, y = 245, label = "p = 0.000664 ***", size = 8)


# permanova ####
# need different values here
# eliminate score columns 
micros_set

perm_micros <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year))
colnames(perm_micros)

permed_micros <- perm_micros %>% 
  mutate(colembola = col_20 + col_10 + col_6 + col_4,
         Diplopod = `Dip>5` + `Dip<5`,
         Chilopoda = `Chil>5` + `Chil<5`,
         Hemiptera = Enich + hemip) %>% 
  dplyr::select(-japy, - camp, -`Dip>5`, -`Dip<5`, -`Chil>5`, -`Chil<5`,
                -col_20, -col_10, -col_6, -col_4, -Enich, -hemip, -sym, -pod, -ento, -Iso) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(vars(5:32), as.numeric) %>% 
  mutate(trt = as.factor(trt))
# %>% 
#   filter(crop == 'beans')
colnames(permed_micros)
# tt <- permed_micros 
# tt[apply(tt[,-6], 1, function(x) !all(x==0))]
# tt[rowSums(tt[, -5])>0, ]


# perm 

perm_pops <- permed_micros[6:32]

perm_dist <- vegdist(perm_pops, "bray")


perm_1 <- adonis2(perm_dist ~ trt, permutations = 999, method = "bray", data = permed_micros)
perm_1

perm_2 <- adonis2(perm_dist ~ crop, permutations =  999, method = "bray", data = permed_micros)
perm_2

perm_3 <- adonis2(perm_dist ~ year + crop, permutations = 999, method = "bray", data = permed_micros)
perm_3

# I want to look at permnanova between corn 22 and beans 23
pc <- permed_micros %>% 
  filter(crop == "corn" & year == "2022")
pb <- permed_micros %>% 
  filter(crop == "beans" & year == "2023")
pc_pb <- rbind(pc, pb)
pc_pb_pops <- pc_pb[6:32]

pc_pb_dist <- vegdist(pc_pb_pops, method = "bray")
p1_cb <- adonis2(pc_pb_dist ~ year + trt, permutations = 999, method = "bray", data = pc_pb)
p1_cb

# nmds ####
# all data 

# 2/12/24: issue here
nmds1 <- metaMDS(perm_pops, k=3)
stressplot(nmds1)



# troubhle shooting code ####
#seeking complete.cases
y <- subset(micros_set, !complete.cases(micros_set))
y
z <- subset(perm_micros, !complete.cases(perm_micros))
z
w <- subset(permed_micros, !complete.cases(permed_micros))
w
x <- subset(perm_pops, !complete.cases(perm_pops))
x

#removing rows will only 0s
zeros_away <- permed_micros[rowSums(permed_micros[,5:32])>0,]
zeros_away <- perm_pops[rowSums(perm_pops[])>0,]

# only zeros? ISO was all zeros, checked here and then removed above 
which(colSums(permed_micros!=0) == 0)
which(rowSums(permed_micros[5:32]!=0) == 0)


# seeking negative values
neg_data <- permed_micros[-(1:190), -(5:31)]
neg_data

neg_beans <- filter(permed_micros, crop == "beans" & year == "2023") 
her <- neg_beans %>% filter(plot %in% c("104","303"))

com_cases <- complete.cases(permed_micros[6:32]) #: this gets rid of everything becuase it FDUCKEDUEDX
# empty cell warning message 

