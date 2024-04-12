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
library(multcomp)
library(emmeans)

# data ####
micros <- CE2_counts
micros

cc <- PSA_corn_cc_biomass


bcc <- beans_cc_biomasss


wield <- wallace_yield_cb

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
  # filter(!row_number() %in% c(46,47,71,83)) %>% # these rows are all NA, so when I replace with 0, they become all 0 and then vegdist cannot function. removing them early
  replace(is.na(.),0) %>% 
  print(n = Inf)
# check to make sure these changes worked
colnames(micros_set)
unique(micros_set$trt)
unique(micros_set$plot)
which(micros_set$trt == 'NA')
which(micros_set$trt == 'Green')
class(micros_set$trt)

# need to divide the 2021 data / 3 to standardize them 
subset_21 <- micros_set %>% 
  filter(date %in% c('7/1/2021', '9/1/2021')) %>% 
  arrange(date, plot) %>% 
  mutate_if(is.numeric, ~ ./3) %>% 
  print(n = Inf)
unique(test$date)


micro_other <- micros_set %>% 
  filter(date != '7/1/2021', date != '9/1/2021')
unique(micro_other$date)

# brining back the og name from above 
micros_set <- rbind(micro_other, subset_21) %>% 
  arrange(crop,date, plot) %>% 
  print(n = Inf)

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

micro_score_model <- micro_scores %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_score = dplyr::select(.,5:33) %>% 
           rowSums(na.rm = TRUE)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, form = "%Y"),
         year = as.factor(year),
         date = as.factor(date)) %>% 
  relocate(date, crop, plot, trt, block, year) %>% 
  mutate_at(vars(1:6), as.factor)


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
  dplyr::group_by(year) %>% 
    dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% 
  print(n = Inf)

# overall beans fig
unique(micro_scores$crop)
beans_trt_year_score <- micro_scores %>%
  filter(crop == "beans") %>% 
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
  dplyr::group_by(year) %>% 
  dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% 
  print(n = Inf)

# score stats ####



micro_score_model
m1 <- glm.nb(total_score ~ trt + date, data = micro_score_model)
hist(residuals(m1))
summary(m1)
cld(emmeans(m1, ~ date), Letters = letters)

# date       emmean     SE  df asymp.LCL asymp.UCL .group
# 2022-09-23   3.35 0.0926 Inf      3.17      3.53  a    
# 2021-09-01   3.58 0.1295 Inf      3.33      3.84  ab   
# 2023-07-18   3.72 0.0911 Inf      3.54      3.90  ab   
# 2023-11-04   3.95 0.0904 Inf      3.77      4.13   b   
# 2022-06-22   3.99 0.0903 Inf      3.82      4.17   b   
# 2021-07-01   4.07 0.1477 Inf      3.78      4.36   b   





# not using this but will leave it here for meow
# colnames(micro_scores)
# 
# #mixed model for avg score x trt? 
# micro_score_model
# p <- glmer(total_score ~ trt + 
#              (1|year/crop),
#            family = poisson,
#            data = micro_score_model)
# nb <- glmer.nb(total_score ~ trt + date +
#                  (1|year),
#                data = micro_score_model)
# 
# lrtest(p, nb)
# 
# hist(residuals(nb))
# 
# # model with block = singular. I removed block
# m0 <- glmer.nb(total_score ~ 
#                  (1|year/crop), 
#                data = micro_score_model)
# m1 <- glmer.nb(total_score ~ trt+
#                  (1|year/crop), 
#                data = micro_score_model)
# m2 <- glmer.nb(total_score ~ trt + date +
#                  (1|crop), 
#                data = micro_score_model)
# # m3 <- glmer.nb(total_score ~ trt*date +
# #                  (1|year/crop), 
# #                data = micro_score_model)
# 
# anova(m0, m1, m2, m3)
# hist(residuals(m2))
# summary(m2)
# r2_nakagawa(m3)
# # Conditional R2: 0.411
# # Marginal R2: 0.411
# 
# all_emm <- cld(emmeans(m3, ~trt + date), Letters = letters)
# # high levels of variation among year and trt

all_aov <- aov(total_score ~ year, data = micro_score_model)
TukeyHSD(all_aov)
hist(residuals(all_aov))
# $year
# diff       lwr       upr     p adj
# 2022-2021 -4.203571 -15.64270  7.235559 0.6611223
# 2023-2021  1.033929 -10.40520 12.473059 0.9751895
# 2023-2022  5.237500  -3.68719 14.162190 0.3501438

corn_aov_df <- filter(micro_score_model, crop == 'corn')
corn_aov <- aov(total_score ~ year, data = corn_aov_df)
TukeyHSD(corn_aov)
hist(residuals(corn_aov))
# $year
# diff       lwr      upr     p adj
# 2022-2021 -0.3785714 -12.74795 11.99081 0.9970907
# 2023-2021 -1.5035714 -13.87295 10.86581 0.9551049
# 2023-2022 -1.1250000 -13.07496 10.82496 0.9728177

beans_aov_df <- filter(micro_score_model, crop == 'beans')
beans_aov <- aov(total_score ~ year, data = beans_aov_df)
TukeyHSD(beans_aov)
hist(residuals(beans_aov))
# $year
# diff       lwr      upr     p adj
# 2023-2022 11.6 0.1879175 23.01208 0.0464333

####
###
##
#

# individual year models 

corn_only <- micro_score_model %>% 
  filter(crop == 'corn')

#2021 
# no trt significance, do not need it here
corn_1 <- corn_only %>% 
  filter(year == '2021')
corn_1_mod <- MASS::glm.nb(total_score ~ date, data = corn_1)
summary(corn_1_mod)
hist(residuals(corn_1_mod))
corn_1_mode_df <- cld(emmeans(corn_1_mod, ~date), Letters = letters)
# date       emmean     SE  df asymp.LCL asymp.UCL .group
# 2021-09-01   3.59 0.0924 Inf      3.41      3.77  a    
# 2021-07-01   4.06 0.1034 Inf      3.86      4.27   b  

#2022
corn_2 <- corn_only %>% 
  filter(year == '2022')
corn_2_mod <- MASS::glm.nb(total_score ~ date, data = corn_2)
summary(corn_2_mod)
hist(residuals(corn_2_mod))
corn_2_mod_df <- cld(emmeans(corn_2_mod, ~date), Letters = letters)

# date       emmean    SE  df asymp.LCL asymp.UCL .group
# 2022-09-23   3.52 0.139 Inf      3.25      3.79  a    
# 2022-06-22   4.04 0.136 Inf      3.77      4.30   b   

corn_2_plot <- corn_2 %>% 
  group_by(trt, date) %>% 
  summarise(mean = mean(total_score),
            sd = sd(total_score),
            n = n(), 
            se = sd/sqrt(n)) 

# just looking here
ggplot(corn_2_plot, aes(x = trt, y = mean))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  facet_wrap(~date)

#2023
corn_3 <- corn_only %>% 
  filter(year == '2023')
corn_3_mod <- MASS::glm.nb(total_score ~ trt + date, data = corn_3)
summary(corn_3_mod)
hist(residuals(corn_3_mod))
cld(emmeans(corn_3_mod, ~trt + date), Letters = letters)
# trt   date       emmean    SE  df asymp.LCL asymp.UCL .group
# Brown 2023-07-18   3.43 0.164 Inf      3.11      3.75  a    
# Check 2023-07-18   3.57 0.162 Inf      3.25      3.89  a    
# Brown 2023-11-04   3.65 0.163 Inf      3.33      3.97  a    
# Gr-Br 2023-07-18   3.70 0.161 Inf      3.39      4.02  a    
# Check 2023-11-04   3.79 0.162 Inf      3.48      4.11  a    
# Green 2023-07-18   3.92 0.160 Inf      3.60      4.23  a    
# Gr-Br 2023-11-04   3.93 0.161 Inf      3.61      4.24  a    
# Green 2023-11-04   4.14 0.159 Inf      3.83      4.45  a    


# beans
beans_only <- micro_score_model %>% 
  filter(crop == 'beans')

#2022
beans_1 <- beans_only %>% 
  filter(year == '2022')
beans_1_mod <- MASS::glm.nb(total_score ~ trt + date, data = beans_1)
summary(beans_1_mod)
hist(residuals(beans_1_mod))
beans_1_mod_df <- cld(emmeans(beans_1_mod, ~trt + date), Letters = letters)
# trt   date       emmean    SE  df asymp.LCL asymp.UCL .group
# Green 2022-09-23   3.05 0.297 Inf      2.47      3.64  a    
# Check 2022-09-23   3.14 0.297 Inf      2.56      3.72  a    
# Brown 2022-09-23   3.24 0.296 Inf      2.66      3.82  a    
# Gr-Br 2022-09-23   3.25 0.296 Inf      2.67      3.84  a    
# Green 2022-06-22   3.81 0.295 Inf      3.23      4.39  a    
# Check 2022-06-22   3.90 0.295 Inf      3.32      4.47  a    
# Brown 2022-06-22   4.00 0.294 Inf      3.42      4.58  a    
# Gr-Br 2022-06-22   4.01 0.294 Inf      3.43      4.59  a 
beans_1_plot <- beans_1 %>% 
  group_by(trt, date) %>% 
  summarise(mean = mean(total_score),
            sd = sd(total_score),
            n = n(), 
            se = sd/sqrt(n)) 

# just looking here
ggplot(beans_1_plot, aes(x = trt, y = mean))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  facet_wrap(~date)


#2023
beans_2 <- beans_only %>% 
  filter(year == '2023')
beans_2_mod <- MASS::glm.nb(total_score ~ trt + date, data = beans_2)
summary(beans_2_mod)
hist(residuals(beans_2_mod))
cld(emmeans(beans_2_mod, ~trt+date), Letters = letters)
# trt   date       emmean    SE  df asymp.LCL asymp.UCL .group
# Gr-Br 2023-07-18   3.66 0.180 Inf      3.31      4.01  a    
# Brown 2023-07-18   3.78 0.179 Inf      3.43      4.13  a    
# Green 2023-07-18   3.80 0.179 Inf      3.45      4.15  a    
# Check 2023-07-18   3.80 0.179 Inf      3.45      4.16  a    
# Gr-Br 2023-11-04   3.91 0.179 Inf      3.56      4.26  a    
# Brown 2023-11-04   4.03 0.179 Inf      3.68      4.38  a    
# Green 2023-11-04   4.05 0.179 Inf      3.70      4.40  a    
# Check 2023-11-04   4.05 0.179 Inf      3.70      4.40  a


# 
##
###
####


# score plots ####

ggplot(overall_fig, aes(x = trt, y = avg, fill = crop))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  scale_fill_manual(values = c( "#1B9E77","#D95F02"),
                    name = "Crop", labels = c("Soybean", "Corn"))+
  scale_x_discrete(limits = c("Check", "Brown", "Gr-Br", "Green"),
                   labels=c('No CC', '14-28 DPP', '3-7 DPP', '1-3 DAP'))+
  geom_errorbar(aes(x = trt, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Overall Average QBS Scores x Crop and Treatment",
       subtitle = "Years: Corn, 2021-2023. Beans, 2022-2023",
       x = "Treatment",
       y = "Average QBS scores",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  theme(legend.position = 'bottom',
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# corn by year plot 
corn_trt_year_score

ggplot(corn_trt_year_score, aes(x = year, y = avg, fill = year))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = year, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_brewer(palette = 'Dark2')+
  labs(title = "Corn: Overall average QBS Scores x Year",
       subtitle = "Years: 2021-2023",
       x = "Year",
       y = "Average QBS scores")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())


# 2021 corn
corn_1_plot <- corn_1 %>% 
  group_by(date) %>% 
  summarise(mean = mean(total_score),
            sd = sd(total_score),
            n = n(), 
            se = sd/sqrt(n)) 

ggplot(corn_1_plot, aes(x = date, y = mean, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_manual(values = c("#E7298A", "#7570B3"))+
  scale_x_discrete(labels = c('1 July 2021', '1 September 2021'))+
  labs(
    title = 'Corn 2021 AVG QBS x Date',
    x = 'Sampling Date', 
    y = 'Average QBS Score'
  )+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate('text', x = 1, y = 67, label = 'a', size = 10)+
  annotate('text' , x = 2, y = 67, label = 'b', size = 10)

# 2022 corn 
corn_2_plot <- corn_2 %>% 
  group_by(date) %>% 
  summarise(mean = mean(total_score),
            sd = sd(total_score),
            n = n(), 
            se = sd/sqrt(n)) 

# just looking here
ggplot(corn_2_plot, aes(x = date, y = mean, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_manual(values = c("#E7298A", "#7570B3"))+
  scale_x_discrete(labels = c('22 June 2022', '23 September 2022'))+
  labs(
    title = 'Corn 2022 AVG QBS x Date',
    x = 'Sampling Date', 
    y = 'Average QBS Score'
  )+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate('text', x = 1, y = 65, label = 'a', size = 10)+
  annotate('text' , x = 2, y = 65, label = 'b', size = 10)







# beans by year plot 
ggplot(beans_trt_year_score, aes(x = year, y = avg, fill = year))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = year, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_brewer(palette = 'Dark2')+
  labs(title = "Soybean: Overall average QBS Scores x Year",
       subtitle = "Years: 2022-2023",
       x = "Year",
       y = "Average QBS scores")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# both of these are from 2022. What happened that year?

beans_1_mod_df

ggplot(beans_1_mod_df, aes(x = trt, y = emmean, fill = trt))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  facet_wrap(~date)+
  scale_fill_manual(values = c("#E7298A", "#D95F02" ,"#7570B3", "#1B9E77"))+
  scale_x_discrete(limits = c('Check', 'Brown', "Gr-Br", 'Green'),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Soybean: Emmean of QBS Scores x Date and Treatment",
       subtitle = "Year: 2022",
       x = "Treatment",
       y = "Emmean QBS Scores")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 28))+
geom_text(aes(x = trt, y = 4.8, label = trimws(.group)), size = 10, color = "black")














# extra plots from all years and crops. Only really the ones where there are differences

corn_2_mod_df

ggplot(corn_2_mod_df, aes(x = date, y = emmean, fill = date))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02" ,"#7570B3", "#1B9E77"))+
  scale_x_discrete(limits = c('Check', 'Brown', "Gr-Br", 'Green'),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  geom_errorbar(aes(ymin = emmean-SE, ymax = emmean+SE), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Corn: Emmean of QBS Scores x Date and Treatment",
       subtitle = "Year: 2022",
       x = "Treatment",
       y = "Emmean QBS Scores")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 28))+
  geom_text(aes(x = trt, y = 4.8, label = trimws(.group)), size = 10, color = "black")



# 2021
unique(mean_scores$date)
micros_21 <- filter(mean_scores, year == '2021') %>% 
  mutate(timing = case_when( date == "2021-07-01"~ "1",
                             date == "2021-09-01" ~ "2")) %>% 
  mutate(timing = as.factor(timing))
unique(micros_21$crop)

ggplot(filter(micros_21, crop == "corn"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02" ,"#7570B3", "#1B9E77"))+
  scale_x_discrete(limits = c('Check', 'Brown', "Gr-Br", 'Green'),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~date)+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="black", alpha=0.9, linewidth=1.3)+
  labs(title = "Corn: Average QBS scores",
    subtitle = "Year: 2021",
       x = "Treatment",
       y = "Average QBS score",
    caption = "DPP: Days pre plant
DAP: Days after plant")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# 2022
# micros_22 <- filter(mean_scores, date %in% c('6/22/2022', '9/23/2022'))
micros_22 <- filter(mean_scores, year == "2022")
unique(micros_22$date)


ggplot(filter(micros_22, crop == "beans"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02" ,"#7570B3", "#1B9E77"))+
  scale_x_discrete(limits = c('Check', 'Brown', "Gr-Br", 'Green'),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  geom_errorbar(aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                  colour="black", alpha=0.9, linewidth=1.3)+
  facet_wrap(~factor(date, c("2022-06-22", "2022-09-23")))+
  labs(title = "Soybean: Average QBS scores",
    subtitle = "Year: 2022",
       x = "Treatment",
       y = "Average QBS score",
    caption = "DPP: Days pre plant
DAP: Days after plant")+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

ggplot(filter(micros_22, crop == "corn"), aes(x = trt, y = avg, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  scale_fill_manual(values = c("#E7298A", "#D95F02" ,"#7570B3", "#1B9E77"))+
  scale_x_discrete(limits = c('Check', 'Brown', "Gr-Br", 'Green'),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
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

# beans 
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
#corn
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

# corn cc biomass time ####

cc_clean <- cc %>% # this is in grams
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_biomass_g),
            cc_sd = sd(cc_biomass_g),
            cc_se = cc_sd/sqrt(n())) 

# this is for the micro regressions
cc_mg_clean <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  mutate(mg_ha = cc_biomass_g*0.04)%>% 
  group_by(year, trt) %>% 
  summarise(mean = mean(mg_ha),
            sd = sd(mg_ha),
            se = sd/sqrt(n())) %>% 
  print(n = Inf)



# over all bar
cc_mg_plot <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# bar by year 
cc_year_plot <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt, year) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# model and boxplot
cc_mg_model <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot,block) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  filter(trt != "check")


cc0 <- lmer(mg_ha~
              (1|year/block),
            data = cc_mg_model)
summary(cc0)

cc1 <- lmer(mg_ha ~ trt +
              (1|year/block),
            data = cc_mg_model)
summary(cc1)

cc_em <- emmeans(cc1, ~trt)
pwpm(cc_em)
cld(cc_em, Letters = letters)
# trt   emmean  SE   df lower.CL upper.CL .group
# brown   1.96 1.8 2.09    -5.48     9.41  a    
# gr-br   3.77 1.8 2.09    -3.67    11.22   b   
# green   6.44 1.8 2.09    -1.01    13.88    c 
cc_mg_model %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

ca <- aov(mg_ha ~ year, data = cc_mg_model)
TukeyHSD(ca)

# $year
# diff       lwr        upr     p adj
# 2022-2021 -4.36412 -6.378883 -2.3493571 0.0000133
# 2023-2021 -5.97080 -7.985563 -3.9560371 0.0000000
# 2023-2022 -1.60668 -3.621443  0.4080829 0.1408407

# 2021
c21 <- filter(cc_mg_model, year == "2021")
cc21 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c21)
summary(cc21)

cc21_em <- emmeans(cc21, ~trt)
pwpm(cc21_em)
cld(cc21_em, Letters = letters)
# trt   emmean    SE df lower.CL upper.CL .group
# brown   4.08 0.485 12     3.03     5.14  a    
# gr-br   6.81 0.485 12     5.75     7.87   b   
# green  11.61 0.485 12    10.55    12.67    c  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_21_mean <- c21 %>% 
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>%
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  mutate(Year = c(2021, 2021,2021)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)


# 2022
c22 <- filter(cc_mg_model, year == "2022")
cc22 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c22)
summary(c22)

cc22_em <- emmeans(cc22, ~trt)
pwpm(cc22_em)
cld(cc22_em, Letters = letters)
# trt   emmean    SE   df lower.CL upper.CL .group
# brown   1.17 0.163 11.2    0.807     1.52  a    
# gr-br   2.76 0.163 11.2    2.399     3.12   b   
# green   5.49 0.163 11.2    5.133     5.85    c  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_22_mean <- c22 %>%  
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))%>% 
  mutate(Year = c(2022, 2022,2022)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

# 2023
c23 <- filter(cc_mg_model, year == "2023")
cc23 <- lmer(mg_ha ~ trt +
               (1|block),
             data = c23)
summary(cc23)

cc23_em <- emmeans(cc23, ~trt)
pwpm(cc23_em)
cld(cc23_em, Letters = letters)
# trt   emmean    SE   df lower.CL upper.CL .group
# brown  0.639 0.145 11.5    0.322    0.956  a    
# gr-br  1.745 0.145 11.5    1.428    2.062   b   
# green  2.212 0.145 11.5    1.895    2.529   b  

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
corn_23_mean <- c23 %>%   
  ungroup() %>% 
  mutate(trt = case_when(trt == 'brown' ~ '14-28 DPP',
                         trt == 'green' ~ '1-3 DAP',
                         trt == 'gr-br' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))%>% 
  mutate(Year = c(2023, 2023, 2023)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)
# corn cc plots ####
cc_clean
ggplot(filter(cc_clean, trt != "check"), aes(x = trt, y = cc_mean, fill = trt))+
  facet_wrap(~year)+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DPP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.75)+
  geom_errorbar( aes(x=trt, ymin=cc_mean-cc_se, ymax=cc_mean+cc_se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Mean cover crop (g/m2)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#over all bar
ggplot(filter(cc_mg_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Mean Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 2.8, label = "a", size = 10)+
  annotate("text", x = 2, y = 4.9, label = "b", size = 10)+
  annotate("text", x = 3, y = 7.9, label = "c", size = 10)

# bar by year 
ggplot(filter(cc_year_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  facet_wrap(~year)+
  labs(title = "Corn: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# overall box
ggplot(filter(cc_mg_model, trt != "check"), aes(x = trt, y = mg_ha, fill = trt))+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 2)+
  labs(title = "Corn: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 7, label = "a", size = 10)+
  annotate("text", x = 2, y = 9, label = "b", size = 10)+
  annotate("text", x = 3, y = 14, label = "c", size = 10)


###

# bean cc biomass time ####

bcc
bcc_start <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_g),
            cc_sd = sd(cc_g),
            cc_se = cc_sd/sqrt(n())) 

# this is for the micro regressions
bcc_mg_clean <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  mutate(mg_ha = cc_g*0.04)%>% 
  group_by(year, trt) %>% 
  summarise(mean = mean(mg_ha),
            sd = sd(mg_ha),
            se = sd/sqrt(n())) %>% 
  print(n = Inf)


# all data
bcc_mg_plot <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# bar by year 
bcc_year_plot <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(trt, year) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

# model and boxplot
bcc_mg_model <- bcc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_g = as.numeric(cc_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  mutate(block = case_when(plot %in% c("101", '102', '103','104') ~ 1,
                           plot %in% c('201', '202', '203' ,'204') ~ 2, 
                           plot %in% c('301', '302', '303', '304') ~ 3,
                           plot %in% c('401', '402', '403', '404') ~ 4, 
                           plot %in% c('501', '502', '503', '504') ~ 5)) %>%
  mutate(block = as.factor(block)) %>% 
  print( n = Inf)
unique(bcc_mg_model$block)

bc0 <- lmer(mg_ha~
              (1|year/block),
            data = bcc_mg_model)
summary(bc0)

bc1 <- lmer(mg_ha ~ trt +
              (1|year/block),
            data = bcc_mg_model)
summary(bc1)

bcc_em <- emmeans(bc1, ~trt)
pwpm(bcc_em)
cld(bcc_em, Letters = letters)
# trt  emmean    SE   df lower.CL upper.CL .group
# br    0.634 0.136 7.36    0.316    0.951  a    
# grbr  1.779 0.136 7.36    1.461    2.097   b   
# gr    2.153 0.136 7.36    1.835    2.471   b   

bcc_mg_model %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))
# <fct> <dbl> <dbl> <int>  <dbl>
#   1 br    0.634 0.233    10 0.0737
# 2 gr    2.15  0.561    10 0.178 
# 3 grbr  1.78  0.428    10 0.135

ba <- aov(mg_ha ~ year, data = bcc_mg_model)
TukeyHSD(ba)

# $year
# diff        lwr       upr    p adj
# 2023-2022 0.04613333 -0.5450744 0.6373411 0.874153

bean_year_cc <- bcc_mg_model %>% 
  group_by(year) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))
# year   mean    sd     n    se
# <fct> <dbl> <dbl> <int> <dbl>
#   1 2022   1.50 0.976    15 0.252
# 2 2023   1.54 0.544    15 0.140



# 2022
b22 <- filter(bcc_mg_model, year == "2022")
bcc22 <- lmer(mg_ha ~ trt +
                (1|block),
              data = b22)
summary(bcc22)

bcc22_em <- emmeans(bcc22, ~trt)
pwpm(bcc22_em)
cld(bcc22_em, Letters = letters)
# trt  emmean    SE df lower.CL upper.CL .group
# br    0.419 0.179 12   0.0284    0.809  a    
# grbr  1.522 0.179 12   1.1312    1.912   b   
# gr    2.556 0.179 12   2.1656    2.947    c   

trt_ord <- c("14-28 DPP", "3-7 DPP", "1-3 DAP")
b22_table <- b22 %>% 
  ungroup() %>% 
  mutate(trt = case_when(trt == 'br' ~ '14-28 DPP',
                         trt == 'gr' ~ '1-3 DAP',
                         trt == 'grbr' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))   %>% 
  mutate(Year = c(2022, 2022, 2022)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

# trt    mean     sd     n     se
# <fct> <dbl>  <dbl> <int>  <dbl>
#   1 br    0.419 0.0668     5 0.0299
# 2 gr    2.56  0.531      5 0.237 
# 3 grbr  1.52  0.442      5 0.198 


# 2023
b23 <- filter(bcc_mg_model, year == "2023")
bcc23 <- lmer(mg_ha ~ trt +
                (1|block),
              data = b23)
summary(bcc23)

bcc23_em <- emmeans(bcc23, ~trt)
pwpm(bcc23_em)
cld(bcc23_em, Letters = letters)
# trt  emmean     SE df lower.CL upper.CL .group
# br    0.848 0.0704 12    0.695     1.00  a    
# gr    1.750 0.0704 12    1.597     1.90   b   
# grbr  2.037 0.0704 12    1.883     2.19    c    

b23_table <- b23 %>%
  ungroup() %>% 
  mutate(trt = case_when(trt == 'br' ~ '14-28 DPP',
                         trt == 'gr' ~ '1-3 DAP',
                         trt == 'grbr' ~ '3-7 DPP',
                         .default = as.character(trt))) %>% 
  mutate(trt = factor(trt, levels = trt_ord)) %>% 
  group_by(trt) %>% 
  summarise(mean = mean(mg_ha), 
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))   %>% 
  mutate(Year = c(2023, 2023, 2023)) %>% 
  relocate(Year, trt) %>% 
  mutate_at(vars(1:2), as.factor)

# bean cc plots ####

#over all bar
ggplot(filter(bcc_mg_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(limits = c('br', 'grbr', 'gr'),
                   labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02","#1B9E77" , "#7570B3"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Soybean: Mean Cover Crop Biomass x Treatment",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 1.5, label = "a", size = 10)+
  annotate("text", x = 2, y = 2.2, label = "b", size = 10)+
  annotate("text", x = 3, y = 3, label = "c", size = 10)

# bar by year 
ggplot(filter(bcc_year_plot, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  scale_x_discrete(limits = c('br', 'grbr', 'gr'),
                   labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02","#1B9E77", "#7570B3"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  facet_wrap(~year)+
  labs(title = "Soybean: Cover Crop Biomass x Treatment",
       # subtitle = "Years: 2022-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Mean" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# overall box
ggplot(filter(bcc_mg_model, trt != "check"), aes(x = trt, y = mg_ha, fill = trt))+
  scale_x_discrete(limits = c("br", "grbr", "gr"),
                   labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02", "#1B9E77", "#7570B3"))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 2)+
  labs(title = "Soybean: Cover Crop Biomass x Treatment",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote("Biomass" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate("text", x = 1, y = 1.25, label = "a", size = 10)+
  annotate("text", x = 2, y = 2.5, label = "b", size = 10)+
  annotate("text", x = 3, y = 3.5, label = "b", size = 10)


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
new_micro_cc <- cbind(corn_micros_yr, cc_mg_clean)
new_micro_cc <- new_micro_cc %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1) %>% 
  rename(trt = trt...2) %>% 
  dplyr::select(-trt...5) %>% 
  relocate(year, trt) %>% 
  filter(trt != "Check")


corn_cc_micro <- glm(mean_score_yr ~ mean, data = new_micro_cc)
summary(corn_cc_micro)
hist(residuals(corn_cc_micro))


ggplot(new_micro_cc, aes(x = mean, y = mean_score_yr))+
  geom_point(aes(color = trt),size = 6)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
                     labels = c("No CC", "14-28 DPP", "1-3 DAP"))+
  labs(title = "Corn: QBS Scores ~ Average CC Biomass",
       subtitle = "Years: 2021-2023",
       y = 'Average QBS scores',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  xlab(bquote('Biomass'(Mg/ha^-1)))+
  guides(color = guide_legend("Treatment"))+
  annotate("text", x = 1.8, y = 115, label = "p value < 0.001", size = 12, fontface = 'italic')+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
 

# did not look at these on 3/29/2024
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
bcc_mg_clean 

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
  filter(trt != "Check")  %>% 
  print(n = Inf)

bcc_mg_clean <- bcc_mg_clean %>% 
  arrange(year, factor(trt, levels = c('br', 'grbr', 'gr')))

new_micro_bcc <- cbind(bcc_mg_clean, bean_micros_yr) %>% 
  rename(trt = trt...2,
         year = year...1) %>% 
  dplyr::select(-trt...7,
                -year...6) %>% 
  mutate(trt = case_when(trt == "br" ~ "Brown",
                         trt == "grbr" ~ "Gr-Br",
                         trt == "gr" ~ "Green"))

bb_score_cc_ <- glm(mean_score_yr ~ mean, data = new_micro_bcc)
summary(bb_score_cc_)
hist(residuals(bb_score_cc_))


ggplot(new_micro_bcc, aes(x = mean, y = mean_score_yr))+
  geom_point(aes(color = trt),size = 8)+
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
                     labels = c("No CC", "14-28 DPP", "1-3 DAP"))+
  labs(title = "Soybean: QBS Scores ~ Average CC Biomass",
       subtitle = "Years: 2022-2023",
       y = 'Average QBS scores',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  xlab(bquote('Biomass'(Mg/ha^-1)))+
  guides(color = guide_legend("Treatment"))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))



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

new_total_cc <- cbind(cc_mg_clean, corn_micro_totals)
new_total_cc <- new_total_cc %>% 
  rename(year = year...1, 
         trt = trt...7) %>% 
  dplyr::select(-year...6, -trt...2) %>% 
  filter(trt !=  'Check')


cc_abund_cc <- glm(avg_abund ~ mean  ,
                   data = new_total_cc)
hist(residuals(cc_abund_cc))
summary(cc_abund_cc)

ggplot(new_total_cc, aes(x = mean, y = avg_abund))+
  geom_point(aes(color = trt),size = 8)+
  geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"))+ 
  annotate("text", x = 1.7, y = 85, label = "p value < 0.01", size = 12, fontface = 'italic')+
  labs(title = "Corn: Micro Abundance ~ Average CC Biomass",
       subtitle = "Years: 2021-2023",
       y = 'Average micro abundance',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  xlab(bquote('Biomass'(Mg/ha^-1)))+
  guides(color = guide_legend("Treatment",
       caption = "DPP: Days pre plant
DAP: Days after plant"))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# I have also done cc ~ micros. It does not make sense bc there would be an interaction. 
# this formula does not tell us anything. Stick with micros ~ cc


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

bcc_mg_clean
new_total_bcc <- cbind(bcc_mg_clean, bean_micro_totals)
new_total_bcc <- new_total_bcc %>% 
  rename(year = year...1, 
         trt = trt...2) %>% 
  dplyr::select(-year...6, -trt...7) %>% 
  mutate(trt = case_when(trt == "br" ~ "Brown",
                         trt == "grbr" ~ "Gr-Br",
                         trt == "gr" ~ "Green"))


bb_abund_cc <- glm(avg_abund ~ mean,
                     data = new_total_bcc)
hist(residuals(bb_abund_cc))
summary(bb_abund_cc)

ggplot(new_total_bcc, aes(x = mean, y = avg_abund))+
  geom_point(aes(color = trt),size = 8)+
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
                     labels = c("No CC", "14-28 DPP", "1-3 DAP"))+
  labs(title = "Soybean: Micro Abundance ~ Average CC Biomass",
       subtitle = "Years: 2022-2023",
       y = 'Average QBS scores',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  xlab(bquote('Biomass'(Mg/ha^-1)))+
  guides(color = guide_legend("Treatment"))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
##
#

# yield x micro abundance ####

# Corn 
colnames(wield)
cield <- wield %>% 
  dplyr::select(Year, YieldBuac, Plot, Block, Crop, CC) %>% 
  mutate(YieldBuac = case_when(YieldBuac == "na" ~ NA, 
                               .default = as.character(YieldBuac))) %>% 
  na.omit() %>% 
  print(n = Inf)


corn <- filter(cield, Crop == "corn") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop,
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

corn_avg <- corn %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

ggplot(corn_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

corn %>% 
  group_by(cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

# beans 
unique(cield$Crop)
unique(cield$Year)
beans <- filter(cield, Crop == "soybean") %>% 
  arrange(Year, Plot, Block) %>% 
  rename(year = Year, 
         yieldbuac = YieldBuac, 
         plot = Plot, 
         block = Block, 
         crop = Crop, 
         cc = CC) %>% 
  mutate(yieldbuac = as.numeric(yieldbuac)) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate(cc = case_when(cc == "14-21 DPP" ~ "14-28 DPP", 
                        .default = as.factor(cc))) %>% 
  mutate_at(vars(3:6),as.factor) %>%
  print(n = Inf)

beans_avg <- beans %>% 
  group_by(year, cc) %>% 
  summarise(mean = mean(yieldbuac), 
            sd = sd(yieldbuac), 
            n = n(), 
            se = sd / sqrt(n))

ggplot(beans_avg, aes(x = cc, y = mean, fill = cc))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se))+
  scale_x_discrete(limits = c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  facet_wrap(~year)

####
###
##
#

# micros 
corn_micro_totals
corn_avg <- corn_avg %>% 
  arrange(year, factor(cc, levels = c('14-28 DPP', 'No CC', '3-7 DPP', '1-3 DAP')))

new_yield_abundance <- cbind(corn_micro_totals, corn_avg)
new_yield_abundance <- new_yield_abundance %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1, -trt) %>% 
  relocate(year, cc) %>% 
  print( n = Inf)

?glm


cc_yield_yield <- glm(mean ~ avg_abund,
                      data = new_yield_abundance)
hist(residuals(cc_yield_yield))
summary(cc_yield_yield)

cc_abund_yield <- glm(avg_abund ~ mean,
                   data = new_yield_abundance)
hist(residuals(cc_abund_yield))
summary(cc_abund_yield)

ggplot(new_yield_abundance, aes(x = avg_abund, y = mean))+
  geom_point(aes(color = cc),size = 8)+
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(limits = c('No CC', '14-28 DPP', '3-7 DPP', '1-3 DAP'),
                     values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  ylab(bquote("Mean"(bu / ac ^-1)))+
  labs(title = "Corn: Yield ~ Average Micro Abundance",
       subtitle = "Years: 2021-2023",
       x = 'Average micro abundance',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  guides(color = guide_legend('Treatment'))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
# Beans
# micros 
#other df for bean micros does not have the check trt
bean_micro_totals_w.check <- micros_set %>% 
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
  arrange(year, factor(trt, levels = c('Green', 'Brown', 'Gr-Br', 'Check')))

bean_micro_totals_w.check

bean_yield_abundance <- cbind(bean_micro_totals_w.check, beans_avg)
bean_yield_abundance <- bean_yield_abundance %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1, - trt) %>% 
  relocate(year, cc) %>% 
  print( n = Inf)


bb_yield_abund <- glm(mean ~ avg_abund,
                      data = bean_yield_abundance)
hist(residuals(bb_yield_abund))
summary(bb_yield_abund)

ggplot(bean_yield_abundance, aes(x = avg_abund, y = mean))+
  geom_point(aes(color = cc),size = 8)+
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(limits = c('No CC', '14-28 DPP', '3-7 DPP', '1-3 DAP'),
                     values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
  ylab(bquote("Mean"(bu / ac ^-1)))+
  labs(title = "Soybean: Yield ~ Average Micro Abundance",
       subtitle = "Years: 2022-2023",
       x = 'Average micro abundance',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  guides(color = guide_legend('Treatment'))+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# yield x micro scores ####

# beans
# need check

bean_micro_scores_w.check <- micro_scores %>% 
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
  arrange(year, factor(trt, levels = c('Green', 'Brown', 'Gr-Br', 'Check'))) %>% 
  print(n = Inf)

beans_avg


bean_yield_scores <- cbind(bean_micro_scores_w.check, beans_avg)
bean_yield_scores <- bean_yield_scores %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1, -trt) %>% 
  relocate(year, cc) %>% 
  print( n = Inf)


bb_scoresXyield <- glm(mean ~ mean_score_yr, data = bean_yield_scores)
hist(residuals(bb_scoresXyield))
summary(bb_scoresXyield)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)    25.6681    15.6734   1.638    0.153
# mean_score_yr   0.5905     0.3464   1.704    0.139
# 
# (Dispersion parameter for gaussian family taken to be 30.50276)
# 
# Null deviance: 271.63  on 7  degrees of freedom
# Residual deviance: 183.02  on 6  degrees of freedom
# AIC: 53.744
# 
# Number of Fisher Scoring iterations: 2


# ggplot(bean_yield_scores, aes(x = mean_score_yr, y = bu_ac_mean))+
#   geom_point(aes(color = trt, shape = year),size = 6)+
#   #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
#   stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
#   scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
#   labs(title = "Soybean: Yield ~ QBS Scores",
#        subtitle = "Years: 2022-2023",
#        y = 'Average bu/ac',
#        x = 'Average QBS scores')+
#   theme(legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 24),
#         plot.subtitle = element_text(size = 18),
#         axis.line = element_line(size = 1.25),
#         axis.ticks = element_line(size = 1.25),
#         axis.ticks.length = unit(.25, "cm"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())

# corn 


corn_micros_yr <- corn_micros_yr %>% 
  arrange(year, factor(trt, levels = c('Green', 'Brown', 'Gr-Br', 'Check')))
corn_avg

corn_yield_scores <- cbind(corn_micros_yr, corn_avg)
corn_yield_scores <- corn_yield_scores %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1, - trt) %>% 
  relocate(year, cc) %>% 
  print( n = Inf)

cc_scoresXyield <- glm(mean ~ mean_score_yr, data = corn_yield_scores)
hist(residuals(cc_scoresXyield))
summary(cc_scoresXyield)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   127.7610    14.2641   8.957 4.32e-06 ***
#   mean_score_yr   0.1038     0.2306   0.450    0.662    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 271.811)
# 
# Null deviance: 2773.2  on 11  degrees of freedom
# Residual deviance: 2718.1  on 10  degrees of freedom
# AIC: 105.13
# 
# Number of Fisher Scoring iterations: 2





# ggplot(corn_yield_scores, aes(x = mean_score_yr, y = bu_ac_mean))+
#   geom_point(aes(color = trt, shape = year),size = 6)+
#   geom_smooth(method = 'lm', color = "black", size = 1.5) + 
#   stat_poly_eq(label.x = "left", label.y = "top", size = 8)+
#   scale_color_manual(values = c("#D95F02", "#E7298A","#7570B3", "#1B9E77"))+
#   labs(title = "Corn: Yield ~ QBS Scores",
#        subtitle = "Years: 2021-2023",
#        y = 'Average bu/ac',
#        x = 'Average QBS scores')+
#   theme(legend.title = element_blank(),
#         legend.text = element_text(size = 14),
#         axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 24),
#         plot.subtitle = element_text(size = 18),
#         axis.line = element_line(size = 1.25),
#         axis.ticks = element_line(size = 1.25),
#         axis.ticks.length = unit(.25, "cm"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


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
         Insects = Enich + hemip + AC + CL + AC + OAC + Psocodea+ Thrips+ sipoopter+hymen+ archaeognatha
         + dermaptera + lep+ Formicid + Coccomorpha+a_dipt + OL,
         mites = Orb + Norb,
         non_insect = spider + Pseu + Annelid+ protura + simphyla+pauropoda +Diplura +`Chil>5` + `Chil<5`+`Dip>5` + `Dip<5`) %>% 
  dplyr::select(-japy, - camp, -`Dip>5`, -`Dip<5`, -`Chil>5`, -`Chil<5`,
                -col_20, -col_10, -col_6, -col_4, -Enich, -hemip, -sym, -pod, -ento, -Iso) %>% 
  dplyr::select(-Orb, -Norb, -spider, -Pseu, -CL, -OL, -OAC, -a_dipt, -AC, - Coccomorpha, -Psocodea, -Thrips, -simphyla, 
                -pauropoda, -sipoopter, -Annelid, -protura, -hymen, -Formicid, -archaeognatha, -dermaptera, -lep, -Diplura) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(vars(6:9), as.numeric) %>% 
  mutate(trt = as.factor(trt)) %>% 
  group_by(year, crop, date, trt) %>% 
  summarise(
    col = sum(colembola),
    ins = sum(Insects),
    mites = sum(mites), 
    non_insect = sum(non_insect)
  ) %>% 
  mutate(date = as.factor(date))


colnames(permed_micros)




# perm 

perm_pops <- permed_micros[5:8]

perm_dist <- vegdist(perm_pops, "bray")


perm_1 <- adonis2(perm_dist ~ trt, permutations = 999, method = "bray", data = permed_micros)
perm_1

perm_2 <- adonis2(perm_dist ~ crop, permutations =  999, method = "bray", data = permed_micros)
perm_2

# this one. Factorial 10 for permutations because I shrank the df so much 
perm_3 <- adonis2(perm_dist ~ year + date + crop, permutations = factorial(10), method = "bray", data = permed_micros)
perm_3
# Df SumOfSqs      R2      F    Pr(>F)    
# year      2   1.3320 0.27930 9.3514 1.102e-06 ***
# date      3   0.9222 0.19339 4.3166  0.000264 ***
# crop      1   0.1645 0.03450 2.3105  0.075247 .  
# Residual 33   2.3502 0.49281                     
# Total    39   4.7689 1.00000    



# comparing populations of the legacy years
# 2021- 2022
# can i do this? The collection methods changed a bit 
# more samples in 2021?


# 2022 - 2023
pc <- permed_micros %>% 
    filter(crop == "corn" & year == "2022")
pb <- permed_micros %>% 
    filter(crop == "beans" & year == "2023")
pc_pb <- rbind(pc, pb) %>% 
  mutate(date = as.factor(date))
pc_pb_pops <- pc_pb[5:8]

pc_pb_dist <- vegdist(pc_pb_pops, method = "bray")
p1_cb <- adonis2(pc_pb_dist ~ year + date + trt, permutations = factorial(10), method = "bray", data = pc_pb)
p1_cb

# Df SumOfSqs      R2      F   Pr(>F)   
# year      1  0.37589 0.25361 8.4090 0.005206 **
# date      2  0.59521 0.40158 6.6576 0.003338 **
# trt       3  0.10874 0.07337 0.8109 0.555276   
# Residual  9  0.40231 0.27144                   
# Total    15  1.48216 1.00000    



# nmds ####
# all data 

# k = 3 is better than 2
nmds1 <- metaMDS(perm_pops, k=3)
nmds1$stress
stressplot(nmds1)

# plot 

scores <- scores(nmds1, display = 'sites')
year_scores <- cbind(as.data.frame(scores), year = permed_micros$year)

# species_df <- as.data.frame(scores(nmds1, dispaly = 'species'))
# species_df$species <- rownames(species_df)

plot <- plot_ly(year_scores, x = ~NMDS1, y = ~ NMDS2, z = ~NMDS3, color = ~year)
plot <- plot %>% 
  add_markers()
plot


# total population plots ####


# trouble shooting code ####
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

