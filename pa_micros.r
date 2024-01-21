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

# data ####
micros <- CE2_counts
micros

cc <- PSA_PAcc_biomass
cc

yield <- PSA_PA_yield
yield

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
  print(n = Inf)
colnames(mean_scores)
unique(mean_scores$date)

ggplot(mean_scores, aes(x = trt, y = avg, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~date)

# 2021
micros_21 <- filter(mean_scores, date %in% c("9/1/2021", "7/1/2021"))

ggplot(micros_21, aes(x = trt, y = avg, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~date)+
  ggtitle("2021")+
  geom_errorbar( aes(x=trt, ymin=avg-se, ymax=avg+se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)

# 2022
micros_22 <- filter(mean_scores, date %in% c('6/22/2022', '9/23/2022'))

ggplot(micros_22, aes(x = trt, y = avg, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~date + crop)+
  ggtitle("2022")+
  geom_errorbar( aes(x=trt, ymin=avg-sd, ymax=avg+sd), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)

# 2023
micros_23 <- filter(mean_scores, date %in% c('7/18/2023','11/4/2023'))

ggplot(micros_23, aes(x = trt, y = avg, fill = date))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~date + crop)+
  ggtitle("2023")+
  geom_errorbar( aes(x=trt, ymin=avg-sd, ymax=avg+sd), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)

#
##
###

# CC biomass ####
# cc biomass time 
cc_clean <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_biomass_g),
            cc_sd = sd(cc_biomass_g),
            cc_se = cc_sd/sqrt(n()))

ggplot(filter(cc_clean, trt != 'check'), aes(x = cc_mean, y = trt, fill = trt))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(~year)+
  coord_flip()

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
new_micro_cc <- cbind(corn_micros_yr, cc_clean)
new_micro_cc <- new_micro_cc %>% 
  rename(year = year...4) %>% 
  dplyr::select(-year...1) %>% 
  rename(trt = trt...2) %>% 
  dplyr::select(-trt...5) %>% 
  relocate(year, trt)

ggplot(filter(new_micro_cc, trt != 'Check'), aes(x = mean_score_yr, y = cc_mean, label = year))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm') + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "Micro scores x CC biomass and year", 
       y = 'Average cc biomass (g)',
       x = 'Average micro scores')

###
##
#

# micro pops x cc biomass ####
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

new_total_cc <- cbind(cc_clean, corn_micro_totals)
new_total_cc <- new_total_cc %>% 
  rename(year = year...1, 
         trt = trt...7) %>% 
  dplyr::select(-year...6, -trt...2)

ggplot(filter(new_total_cc, trt != 'Check'), aes(x = avg_abund, y = cc_mean, label = year))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm') + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "Micro abundance x CC biomass and year", 
       y = 'Average cc biomass (g)',
       x = 'Average micro abundance')


###
##
#

# yield x micro pops ####
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

ggplot(filter(new_yield_abundance, trt != 'Check'), aes(x = bu_ac_mean, y = avg_abund, label = year))+
  geom_point(aes(color = trt, shape = year),size = 6)+
  geom_smooth(method = 'lm') + 
  scale_color_manual(values = c('brown', 'tan', 'green'))+
  geom_text(vjust = -1, aes(fontface = 'bold'))+
  guides(shape = FALSE)+
  labs(title = "Micro abundance x yield and year", 
       y = 'Average micro abundance',
       x = 'Average bu/ac')

# models
# need different dfs

# need to rethink these dfs for the model 1/20/2024
# this needs to be average population by year match the yield df
micro_model_df <- micros_set %>%
  filter(crop == 'corn') %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2, 
                           plot %in% c(301,302,303,304) ~ 3, 
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  relocate(date, block) %>% 
  print(n = Inf)

#yield_model_df
yield %>% 
  dplyr::select(-trt_num, -crop) %>% 
  mutate(trt = as.factor(trt)) %>% 
  mutate(year = as.factor(year)) %>%
  group_by(year, trt) %>% 
  summarise(lb_pass_mean = mean(lb_pass_moisture), 
            lb_ac_mean = mean(lb_ac),
            bu_ac_mean = mean(bu_ac)) %>% 
  mutate(year = as.factor(year)) %>%
  print(n = Inf)
  
  
  


# #ymp1 <- glmer.nb(bu_ac_mean ~ avg_abund + (1|year), 
#              data = new_yield_abundance)

###
##
#

# permanova ####
# need different values here
# eliminate score columns 
micros_set
