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
library(ggrepel)
library(flextable)

# data ####
micros <- CE2_counts
micros

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

date <- '7/1/2021'
crop <- 'corn'
plot <- c('102', '103', '203', '303', '502')
missing_date <- data.frame(date, crop, plot)
missing <- as_tibble(missing_date) %>% 
  mutate(plot = as.double(plot))

micros_next <- as_tibble(dplyr::bind_rows(missing, micros_ready)) %>% 
  print(n = Inf)

# need to add  treatment in now
micros_set <- micros_next %>% 
  mutate(plot = replace(plot, plot == 507, 502)) %>% # there is a sneaky 507 plot number
  mutate(trt = case_when(plot %in% c(101,203,304,401,503) ~ 'Check',
                         plot %in% c(102,201,303,402,502) ~ 'Green',
                         plot %in% c(103,204,302,403,501) ~ 'Brown',
                         plot %in% c(104,202,301,404,504) ~ 'Gr-Br')) %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  mutate_at(vars(43), as.factor) %>% 
  # filter(!row_number() %in% c(46,47,71,83)) %>% # these rows are all NA, so when I replace with 0, they become all 0 and then vegdist cannot function. removing them early
  replace(is.na(.),0) %>% 
  arrange(date, crop, plot) %>% 
  print(n = Inf)


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

# bringing back the og name from above 
micros_set <- rbind(micro_other, subset_21) %>% 
  arrange(crop,date, plot) %>% 
  print(n = Inf)


# figure of all groups for the paper
table <- micros_set %>% 
  mutate(Acari = Orb + Norb,
         'Hemi-Eudpahic Collembola' = col_10 + col_6 + sym,
         'Eudaphic Collembola' = col_4 +ento,
         'Epigeic Collembola' = col_20 + pod,
         Diplura = Diplura + japy + camp,
         Hemiptera = Enich + hemip, 
         ) %>% 
  rename(Protura = protura, 
         Pauropoda = pauropoda, 
         Thysanoptera = Thrips,
         Siponoptera = sipoopter,
         Archaeognatha = archaeognatha,
         'Coleoptera larvae' = CL,
         'Other larvae' = OL,
         'Carabidae' = AC,
         'Other Coleoptera' = OAC,
         Diptera = a_dipt,
         Spider = spider,
         'Diplopoda < 5mm' = 'Dip<5',
         'Diplopoda > 5mm' = 'Dip>5',
         'Chilopoda < 5mm' = 'Chil<5',
         'Chilopoda > 5mm' = 'Chil>5',
         Isopoda = Iso,
         Hymenoptera = hymen,
         Dermaptera = dermaptera,
         Lepidoptera = lep
  ) %>% 
  dplyr::select(-Orb, -Norb, -col_10, -col_6, -Diplura, -japy, -camp, -Enich, -hemip,
                -col_20, -col_4, -sym, -pod, -ento)

# how many micros did I id for this?
table %>% 
  pivot_longer(
    cols = where(is.numeric)
  )  %>% 
  summarise(total = sum(value)) %>% 
  print(n = Inf)
#  3709

# for the paper

paper <- table %>% 
  pivot_longer(
    cols = where(is.numeric)
  ) %>% 
  group_by(crop, name) %>% 
  summarise(total = sum(value)) %>% 
  mutate(crop = case_when(crop == 'corn' ~ "Corn",
                          crop == 'beans' ~ "Soybean")) %>% 
  print(n = Inf)

paper <- paper %>% 
  pivot_wider(names_from = name,
            values_from = total,
            values_fn = list(family = length))



fp <- flextable(paper) %>% 
  set_header_labels(fo,
                    values = list(
                      crop = 'Crop',
                      name = 'Group',
                      total = 'Total count'
                    ))
fp <- theme_zebra(fp)
autofit(fp) %>% 
  save_as_docx(path = 'ce2Totalmicros.docx')

# abundance ####
# overall model 

abundance_model <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate_at(vars(1:2),as.factor)
##
#

# for the crop models 
c.abund_model <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  filter(crop == 'corn') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate_at(vars(1:2),as.factor)

b.abund_model <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(total_abund = dplyr::select(.,5:43) %>% 
           rowSums(na.rm = TRUE)) %>% 
  dplyr::select(date, crop, plot, trt, total_abund) %>% 
  filter(crop == 'beans') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate_at(vars(1:2),as.factor)
##
#


# for the comparisons 
micros_set
colnames(micros_set)
unique(micros_set$crop)
# corn
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


# beans
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

# abundance stats ####
abundance_m <- abundance_model %>% 
  mutate(block = case_when(
    str_detect(plot, '10') ~ '1',
    str_detect(plot, '20') ~ '2',
    str_detect(plot, '30') ~ '3',
    str_detect(plot, '40') ~ '4',
    str_detect(plot, '50') ~ '5'
  )) %>% 
  mutate(block = as.factor(block))

nb <- glmer.nb(total_abund ~ trt*crop +
                 (1|block/plot),
               data = abundance_m)

p <- glmer(total_abund ~ trt*crop +
                 (1|block/plot),
               data = abundance_m,
           family = poisson)

g <- lmer(total_abund ~ trt*crop +
                 (1|block/plot),
               data = abundance_m)

anova( nb, p, g)


m0 <- glmer.nb(total_abund ~
                 (crop|block/plot),
               data = abundance_m)

m1 <- glmer.nb(total_abund ~ trt +
                 (crop|block/plot),
               data = abundance_m)

m2 <- glmer.nb(total_abund ~ trt+crop +
                 (crop|block/plot),
               data = abundance_m)

m3 <- glmer.nb(total_abund ~ trt*crop +
                 (crop|block/plot),
               data = abundance_m)

hist(residuals(m3))
summary(m3)

anova(m0, m1, m2 ,m3)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# m0    4 1594.7 1607.9 -793.34   1586.7                       
# m1    7 1597.5 1620.5 -791.73   1583.5 3.2202  3    0.35891  
# m2    8 1596.1 1622.5 -790.03   1580.1 3.3908  1    0.06556 .
# m3   11 1600.4 1636.7 -789.19   1578.4 1.6866  3    0.63991

cld(emmeans(m3, ~crop), Letters = letters)
# crop  emmean     SE  df asymp.LCL asymp.UCL .group
# beans   2.78 0.1036 Inf      2.58      2.98  a    
# corn    3.03 0.0839 Inf      2.87      3.20  a  
# 


# abundance plots ####
overall_abundance <- abundance_model %>% 
  group_by(crop, trt) %>% 
  summarise(mean = mean(total_abund), 
            sd = sd(total_abund), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(crop) %>% 
  print(n = Inf)

ggplot(overall_abundance, aes(x = trt, y = mean, fill = crop))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  scale_fill_manual(values = c( "#1B9E77","#D95F02"),
                    name = "Crop", labels = c("Soybean", "Corn"))+
  scale_x_discrete(limits = c("Check", "Brown", "Gr-Br", "Green"),
                   labels=c('No CC', '14-28 DPP', '3-7 DPP', '1-3 DAP'))+
  geom_errorbar(aes(x = trt, ymin = mean-se, ymax = mean+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Overall Average abundance x Crop and Treatment",
       subtitle = "Years: Corn, 2021-2023. Beans, 2022-2023",
       x = "Treatment",
       y = "Average abundance",
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

##
#

overall_a.cb <- abundance_model %>% 
  group_by(crop) %>% 
  summarise(mean = mean(total_abund), 
            sd = sd(total_abund), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(crop) %>% 
  print(n = Inf)

ggplot(overall_a.cb, aes(x = crop, y = mean, fill = crop))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  scale_x_discrete(labels = c('beans' = 'Soybean', 'corn' = 'Corn'))+
  scale_fill_manual(values = c( "#1B9E77","#D95F02"),
                    name = "Crop", labels = c("Soybean", "Corn"))+
  geom_errorbar(aes(x = crop, ymin = mean-se, ymax = mean+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  labs(title = "Overall Average abundance x Crop",
       subtitle = "Years: Corn, 2021-2023. Beans, 2022-2023",
       x = "Crop",
       y = "Average abundance")+
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
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  annotate('text', x = 1, y = 24, label = 'a', size = 10)+
  annotate('text', x = 2, y = 24, label = 'b', size = 10)

corn_abundance <- abundance_model %>% 
  filter(crop == 'corn') %>% 
  group_by(crop, year) %>% 
  summarise(mean = mean(total_abund), 
            sd = sd(total_abund), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(year) %>% 
  print(n = Inf)

ggplot(corn_abundance, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_brewer(palette = 'Dark2')+
  labs(
    title = 'Corn abund x year',
    x = 'Year', 
    y = 'Average abundance'
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
  annotate('text', x = 1, y = 39, label = 'a', size = 10)+
  annotate('text' , x = 2, y = 39, label = 'b', size = 10)+
  annotate('text' , x = 3, y = 39, label = 'a', size = 10)
##
#

bean_abundance <- abundance_model %>% 
  filter(crop == 'beans') %>% 
  group_by(crop, year) %>% 
  summarise(mean = mean(total_abund), 
            sd = sd(total_abund), 
            n = n(), 
            se = sd/sqrt(n)) %>% 
  arrange(year) %>% 
  print(n = Inf)

ggplot(bean_abundance, aes(x = year, y = mean, fill = year))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar(aes(ymin = mean - se, ymax = mean +se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_brewer(palette = 'Dark2')+
  labs(
    title = 'bean abund x year',
    x = 'Year', 
    y = 'Average abundance'
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
  annotate('text', x = 1, y = 25, label = 'a', size = 10)+
  annotate('text' , x = 2, y = 25, label = 'b', size = 10)


# abundance legacy ####


legacy_abund_1 <- rbind(corn_21_a, corn_22_a, beans_22_a, beans_23_a) %>% 
  mutate(exp = case_when(year == '2021' & crop == 'corn' ~ 1,
                         year == '2022' & crop == 'beans' ~ 1,
                         year == '2022' & crop == 'corn' ~ 2,
                         year == '2023' & crop == 'beans' ~ 2),
         exp = as.factor(exp)) %>%
  mutate(block = case_when(plot %in% c('101', '102', '103', '104') ~ 1,
                           plot %in% c('201', '202', '203', '204') ~ 2,
                           plot %in% c('301', '302', '303', '304') ~ 3,
                           plot %in% c('401', '402', '403', '404') ~ 4,
                           plot %in% c('501', '502', '503', '504') ~5),
         block = as.factor(block)) %>% 
  print(n = Inf)

# models with random effects of plot and/ or block are singular
l0 <- glm(total_abund ~ 0,
      data = legacy_abund_1)

l1 <- glm(total_abund ~ crop, 
          data = legacy_abund_1)

l2 <- glm(total_abund ~ crop + exp, 
          data = legacy_abund_1)

l3 <- glm(total_abund ~ crop*exp,
          data = legacy_abund_1)

anova(l0,l1,l2,l3)

summary(l3)
hist(residuals(l3))
cld(emmeans(l3, ~crop*exp), Letters= letters)
# crop  exp emmean  SE  df lower.CL upper.CL .group
# beans 1     11.7 2.9 156     5.99     17.5  a    
# corn  1     15.2 2.9 156     9.48     21.0  a    
# beans 2     21.0 2.9 156    15.27     26.7  a    
# corn  2     32.7 2.9 156    26.97     38.4   b 


# scores ####
micros_set
colnames(micros_set)
#1. aggregate columns
  # e.g., mites into one column 

aggregate_micros <- micros_set %>% 
  mutate(mites = Orb + Norb,
          hemiptera = hemip + Enich + Coccomorpha,
          adult = a_dipt + lep + sipoopter, 
          coleop_1 = AC + OAC, 
         col_20 = col_20 + pod,
         col_10 = col_10 + sym,
         col_6 = col_6 + ento) %>% 
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

#overall average by crop
micro_scores %>%
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
  dplyr::group_by( crop) %>% 
  dplyr::summarise(avg = mean(total_score), 
                   sd = sd(total_score),
                   se = sd/sqrt(n())) %>% 
  print(n = Inf)
  


# table of this for the paper
overall <- flextable(overall_fig)
overall <- theme_zebra(overall)
autofit(overall) %>% 
  save_as_docx(path = 'overall_scores.docx')


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

nb <- glmer.nb(total_score ~ trt*crop +
                 (crop|block/plot), 
               data = micro_score_model)

p <- glmer(total_score ~ trt*crop +
             (crop|block/plot), 
           data = micro_score_model,
           family = poisson)
g <- lmer(total_score ~ trt*crop +
            (crop|block/plot), 
          data = micro_score_model)

anova(nb, p, g)

s0 <- glmer.nb(total_score ~ 
                 (crop|block/plot), 
               data = micro_score_model)

s1 <- glmer.nb(total_score ~ trt +
                 (crop|block/plot), 
               data = micro_score_model)

s2 <- glmer.nb(total_score ~ trt+crop +
                 (crop|block/plot), 
               data = micro_score_model)

s3 <- glmer.nb(total_score ~ trt*crop +
                 (crop|block/plot), 
               data = micro_score_model)

anova(s0, s1, s2, s3)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# s0    8 1934.3 1960.7 -959.14   1918.3                     
# s1   11 1939.2 1975.5 -958.60   1917.2 1.0782  3     0.7823
# s2   12 1941.0 1980.6 -958.52   1917.0 0.1511  1     0.6975
# s3   15 1946.3 1995.8 -958.17   1916.3 0.7074  3     0.8715

# 
##
###
####


# score plots ####

ggplot(micro_score_model, aes(x=trt, y=total_score, fill = trt))+
  geom_boxplot(alpha = 0.7)+
  geom_point()+
  facet_grid(~factor(crop, levels=c('corn','beans'), labels = c('Corn', 'Soybeans')))+
  scale_x_discrete(limits = c('Check','Brown','Gr-Br', 'Green'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#D95F02" ,"#E7298A","#7570B3", "#1B9E77"))+
  labs(title = 'Micro scores w line',
       x = ' Treatment termination', 
       y = 'QBS-ar scores / treatment')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        strip.text = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  ylim(NA,120)+
  scale_y_continuous(breaks = c(0,30,60,93.7))+
  annotate('segment', x = 0, xend = 5, y = 93.7, yend = 93.7, linewidth = 1.5, color = 'red')

ggplot(micro_score_model, aes(x=trt, y=total_score, fill = trt))+
  geom_boxplot(alpha = 0.7)+
  geom_point()+
  facet_grid(~factor(crop, levels=c('corn','beans'), labels = c('Corn', 'Soybeans')))+
  scale_x_discrete(limits = c('Check','Brown','Gr-Br', 'Green'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c( "#D95F02" ,"#E7298A","#7570B3", "#1B9E77"))+
  labs(title = 'Micro scores no line',
       x = ' Treatment termination', 
       y = 'QBS-ar scores / treatment')+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        strip.text = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))+
  ylim(NA,120)+
  scale_y_continuous(breaks = c(0,30,60,90))


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

# NOT SIG as of 4/15/2024
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
    x = 'Sampling date', 
    y = 'Average QBS score'
  )+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# NOT SIG as of 4/14/2024
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
    x = 'Sampling date', 
    y = 'Average QBS score'
  )+
  theme(legend.position = 'none',
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

# NOT SIG as of 4/14/2024
# beans by year plot 
ggplot(beans_trt_year_score, aes(x = year, y = avg, fill = year))+
  geom_bar(stat = 'identity', position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = year, ymin = avg-se, ymax = avg+se), 
                position = position_dodge(0.9),
                width = 0.4, linewidth = 1.3)+
  scale_fill_manual(values = c("#E7298A", "#7570B3"))+
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

# score legacy ####

legacy_score_1 <- rbind(corn_1, corn_2, beans_1, beans_2) %>% 
  dplyr::select(date, crop, plot, trt, block, year, total_score) %>% 
  mutate(exp = case_when(year == '2021' & crop == 'corn' ~ 1,
                         year == '2022' & crop == 'beans' ~ 1,
                         year == '2022' & crop == 'corn' ~ 2,
                         year == '2023' & crop == 'beans' ~ 2),
         exp = as.factor(exp)) %>%
  mutate(block = case_when(plot %in% c('101', '102', '103', '104') ~ 1,
                           plot %in% c('201', '202', '203', '204') ~ 2,
                           plot %in% c('301', '302', '303', '304') ~ 3,
                           plot %in% c('401', '402', '403', '404') ~ 4,
                           plot %in% c('501', '502', '503', '504') ~5),
         block = as.factor(block)) %>% 
  print(n = Inf)

s0 <- glmer(total_score ~ (1|exp/plot),
            data = legacy_score_1)

s1 <- glmer(total_score ~ crop + (1|exp/plot),
            data = legacy_score_1)

s2 <- glmer(total_score ~ crop + exp + (1|exp/plot),
            data = legacy_score_1)

s3 <- glmer(total_score ~ crop*exp + (1|exp/plot),
              data = legacy_score_1)


anova(s0,s1,s2,s3)
# npar    AIC    BIC  logLik deviance  Chisq      Df  Pr(>Chisq) 
# s2    6 1503.7 1522.1 -745.85   1491.7 4.5857  1    0.03224 *

summary(s3)
hist(residuals(s3))

cld(emmeans(s3, ~ crop + exp), Letters= letters)
# crop  exp emmean  SE   df lower.CL upper.CL .group
# corn  1     39.9 7.6 1285     25.0     54.8  a    
# beans 1     47.5 7.6 1285     32.6     62.4  a    
# beans 2     52.1 7.6 1285     37.2     67.0  a    
# corn  2     57.7 7.6 1285     42.8     72.6  a 

# corn cc biomass time ####

cc_new <- wield %>% 
  dplyr::select(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  arrange(Year, Crop) %>% 
  distinct(Year, Crop, CC, Block, Plot, Rye.kgha) %>% 
  mutate(Mg_ha = Rye.kgha*0.001) %>% 
  mutate(CC = case_when(
    CC == '1-3 DAP' ~ 'green', 
    CC == '14-21 DPP' ~ 'early', 
    CC == '3-7 DPP' ~ 'late', 
    CC == 'No CC' ~ 'check'
  )) %>% 
  filter(CC != 'check') %>% 
  mutate_at(vars(1:5), as.factor) %>% 
  print(n = Inf)

corn_cc <- cc_new %>% 
  filter(Crop == 'corn') %>% 
  group_by(Year, CC) %>% 
  summarise(mean = mean(Mg_ha),
            sd = sd(Mg_ha),
            se = sd/sqrt(n())) %>% 
  print(n = Inf)
no_cc <- data.frame(Year = c('2021', '2022', '2023'),
                    CC = c('Check', 'Check', 'Check'),
                    mean = c(0,0,0),
                    sd = c(0,0,0),
                    se = c(0,0,0)
                    )

conr_cc_new <- rbind(corn_cc, no_cc) %>% 
  mutate(CC= case_when(
    CC == 'early' ~ 'Gr-Br',
    CC == 'late' ~ 'Brown',
    CC == 'green' ~ 'Green',
    CC == 'Check' ~ 'Check'
  )) %>% 
  arrange(CC)

soy_cc <- cc_new %>% 
  filter(Crop == 'soybean') %>% 
  group_by(Year, CC) %>% 
  summarise(mean = mean(Mg_ha),
            sd = sd(Mg_ha),
            se = sd/sqrt(n())) %>% 
  print(n = Inf)

soy_cc_new <- rbind(soy_cc, no_cc)%>% 
  mutate(CC= case_when(
    CC == 'early' ~ 'Gr-Br',
    CC == 'late' ~ 'Brown',
    CC == 'green' ~ 'Green',
    CC == 'Check' ~ 'Check'
  ))


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
  arrange(trt) %>% 
  print(n = Inf)

# cbind now
new_micro_cc <- cbind(corn_micros_yr, conr_cc_new) %>% 
  filter(trt != 'Check') %>% 
  print(n = Inf)
  

corn_cc_micro <- glm(mean_score_yr ~ mean, data = new_micro_cc)
summary(corn_cc_micro)
hist(residuals(corn_cc_micro))



ggplot(new_micro_cc, aes(x =  mean_score_yr, y =mean))+
  geom_point(aes(color = trt),size = 6)+
  #geom_smooth(method = 'lm', color = "black", size = 1.5) + 
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
                     labels = c("Early terminated", "Late terminated", "Planting green"))+
  labs(title = "Corn: QBS Scores ~ Average CC Biomass",
       subtitle = "Years: 2021-2023",
       x = 'Average QBS scores')+
  ylab(bquote('Biomass'(Mg/ha^-1)))+
  guides(color = guide_legend("Treatment"))+
  # annotate("text", x = 1.8, y = 115, label = "p value < 0.001", size = 12, fontface = 'italic')+
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





# ggplot(new_micro_cc, aes(x = mean, y = mean_score_yr))+
#   geom_point(aes(color = trt),size = 6)+
#   # geom_smooth(method = 'lm', color = "black", size = 1.5) + 
#   stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
#   scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
#                      labels = c("No CC", "14-28 DPP", "1-3 DAP"))+
#   labs(title = "Corn: QBS Scores ~ Average CC Biomass",
#        subtitle = "Years: 2021-2023",
#        y = 'Average QBS scores',
#        caption = "DPP: Days pre plant
# DAP: Days after plant")+
#   xlab(bquote('Biomass'(Mg/ha^-1)))+
#   guides(color = guide_legend("Treatment"))+
#   # annotate("text", x = 1.8, y = 115, label = "p value < 0.001", size = 12, fontface = 'italic')+
#   theme(legend.position = "bottom",
#         legend.key.size = unit(.50, 'cm'),
#         legend.title = element_text(size = 24),
#         legend.text = element_text(size = 24),
#         axis.text.x = element_text(size=26),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 28),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
 

# # did not look at these on 3/29/2024 or 4/12/2024
# ggplot(filter(new_micro_cc, year == "2021"), aes(x = cc_mean , y = mean_score_yr))+
#   geom_point(aes(color = trt), size = 6)+
#   geom_smooth(method = 'lm', se = FALSE) + 
#   scale_color_manual(values = c('brown', 'tan', 'green'))+
#   # geom_text(vjust = -1, aes(fontface = 'bold'))+
#   guides(shape = FALSE)+
#   labs(title = "2021 Corn: Micro scores x CC biomass and year", 
#        y = 'Average micro scores',
#        x = 'Average cc biomass (g)')
# 
# ggplot(filter(new_micro_cc, year == "2022"), aes(x = cc_mean , y = mean_score_yr))+
#   geom_point(aes(color = trt), size = 6)+
#   geom_smooth(method = 'lm', se = FALSE) + 
#   scale_color_manual(values = c('brown', 'tan', 'green'))+
#   # geom_text(vjust = -1, aes(fontface = 'bold'))+
#   guides(shape = FALSE)+
#   labs(title = "2022 Corn: Micro scores x CC biomass and year", 
#        y = 'Average micro scores',
#        x = 'Average cc biomass (g)')
# 
# ggplot(filter(new_micro_cc, year == "2023"), aes(x = cc_mean , y = mean_score_yr))+
#   geom_point(aes(color = trt), size = 6)+
#   geom_smooth(method = 'lm', se = FALSE) + 
#   scale_color_manual(values = c('brown', 'tan', 'green'))+
#   # geom_text(vjust = -1, aes(fontface = 'bold'))+
#   guides(shape = FALSE)+
#   labs(title = "2023 Corn: Micro scores x CC biomass and year", 
#        y = 'Average micro scores',
#        x = 'Average cc biomass (g)')
# 
# 
# ###
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


ggplot(new_micro_bcc, aes(x = mean_score_yr, y = mean))+
  geom_point(aes(color = trt),size = 8)+
  stat_poly_eq(label.x = "left", label.y = "top", size = 12)+
  scale_color_manual(values = c("#D95F02", "#7570B3", "#1B9E77"),
                     labels = c("14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = "Soybean: QBS Scores ~ Average CC Biomass",
       subtitle = "Years: 2022-2023",
       x = 'Average QBS scores',
       caption = "DPP: Days pre plant
DAP: Days after plant")+
  ylab(bquote('Biomass'(Mg/ha^-1)))+
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

# corn: DF from abundance 
corn_micro_totals
cc_mg_clean
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

# beans: DF from abundance 
bean_micro_totals
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

# the values are total counts, not sores
micros_set

perm_micros <- micros_set %>% 
  relocate(date, crop, plot, trt) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year))
colnames(perm_micros)


test <- perm_micros %>% 
  mutate(Collembola = col_20 + col_10 + col_6 + col_4,
         Insect = Enich + hemip + AC + CL + AC + OAC + Psocodea+ Thrips+ sipoopter+hymen+ archaeognatha
         + dermaptera + lep+ Formicid + Coccomorpha+a_dipt + OL,
         Acari = Orb + Norb,
         'Non-insect' = spider + Pseu + Annelid+ protura + simphyla+pauropoda +Diplura +`Chil>5` + `Chil<5`+`Dip>5` + `Dip<5`) %>% 
  dplyr::select(-japy, - camp, -`Dip>5`, -`Dip<5`, -`Chil>5`, -`Chil<5`,
                -col_20, -col_10, -col_6, -col_4, -Enich, -hemip, -sym, -pod, -ento, -Iso) %>% 
  dplyr::select(-Orb, -Norb, -spider, -Pseu, -CL, -OL, -OAC, -a_dipt, -AC, - Coccomorpha, -Psocodea, -Thrips, -simphyla, 
                -pauropoda, -sipoopter, -Annelid, -protura, -hymen, -Formicid, -archaeognatha, -dermaptera, -lep, -Diplura) %>% 
  replace(is.na(.),0) %>% 
  mutate_at(vars(6:9), as.numeric) %>% 
  mutate(trt = as.factor(trt)) %>%   
  mutate(date = as.factor(date))



test_new <- test %>% 
  rowwise() %>% 
  filter(sum(c(Collembola, Insect, Acari, `Non-insect`)) != 0)
test_pops <- test_new[6:9]

huh <- vegdist(test_pops, method = 'bray')
perm_3 <- adonis2(huh ~ year * date * crop, permutations = 999, method = "bray", data = test_new)
perm_3
#           Df  SumOfSqs      R2       F    Pr(>F)    
# year        2    1.834 0.04896  5.8044  0.001 ***
# date        3    3.427 0.09150  7.2314  0.001 ***
# crop        1    0.570 0.01522  3.6090  0.009 ** 
# year:crop   1    1.688 0.04506 10.6839  0.001 ***
# date:crop   2    1.501 0.04007  4.7504  0.001 ***
# Residual  180   28.437 0.75918                   
# Total     189   37.458 1.00000 

nmds0 <- metaMDS(test_pops, k = 2)
stressplot(nmds0)
nmds0$stress
# 0.1524051

scores <- scores(nmds0, display = 'sites')
scrs <- cbind(as.data.frame(scores), date = test_new$date)
scrs <- cbind(as.data.frame(scrs), crop = test_new$crop)


functional_scores <- as.data.frame(scores(nmds0, 'species'))
functional_scores$species <- rownames(functional_scores)


ggplot(data = scrs, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = crop, shape = crop), size = 3)+
  scale_shape_discrete(labels = c('Soybean', 'Corn'))+
  scale_fill_brewer(palette = 'Dark2', labels = c('1 July 2021', '1 September 2021', '22 June 2022', '23 September 2022',
                                 '18 July 2023', '4 November 2023'))+
  scale_color_brewer(date, palette = 'Dark2')+
  stat_ellipse(geom = 'polygon', aes(group = date, color = date, fill = date), alpha = 0.3)+
  # geom_segment(data = functional_scores, aes( x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #              arrow = arrow(), color = 'grey10', lwd = 0.7)+
  # geom_text_repel(data = functional_scores, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = 'both',
  #                 segment.size = 0.25)+
  annotate("label", x = -.5, y=1.5, label ="Stress = 0.15, k = 2", size = 10)+
  coord_equal()+
  theme_bw()+
  labs(title = "NMDS by date x crop", 
       shape = 'Crop',
       fill = 'Sampling date')+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 28),
        legend.position = 'right',
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1.5, 'cm')
  )+
  guides(size = 'none', color = 'none', fill = guide_legend(override.aes = list(size = 10)))


# 2021 - 2022
pc_1 <- test_new %>% 
  filter(crop == 'corn' & year ==  '2021') %>% 
  print(n = Inf)
pb_1 <- test_new %>% 
  filter(crop == 'beans' & year == '2022') %>% 
  print(n = Inf)
pc1_pb1 <- rbind(pc_1, pb_1)

pc1_pb1_pops <- pc1_pb1[6:9]
pc1_pb1_dist <- vegdist(pc1_pb1_pops, method = 'bray')
p1_pc1_pb2 <- adonis2(pc1_pb1_dist ~ year + date + trt, permutations = factorial(10), method = 'bray', data = pc1_pb1)
p1_pc1_pb2

# Df SumOfSqs      R2      F    Pr(>F)    
# year      1   1.0571 0.08099 7.4880 2.728e-05 ***
#   date      2   2.1595 0.16545 7.6484 2.756e-07 ***
#   trt       3   0.6598 0.05055 1.5578     0.101    
# Residual 65   9.1763 0.70302                     
#Total    71  13.0527 1.00000 

nmds1 <- metaMDS(pc1_pb1_pops, k = 2)
stressplot(nmds1)
nmds1$stress
# 0.1524051

scores <- scores(nmds1, display = 'sites')
scrs_2122 <- cbind(as.data.frame(scores), date = pc1_pb1$date)
scrs_2122 <- cbind(as.data.frame(scrs_2122), crop = pc1_pb1$crop)

functional_scores1 <- as.data.frame(scores(nmds1, 'species'))
functional_scores1$species <- rownames(functional_scores1)

ggplot(data = scrs_2122, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = crop, shape = crop), size =3)+
  scale_shape_discrete(labels = c('Soybean', 'Corn'))+
  scale_color_brewer( palette = 'Dark2')+
  scale_fill_brewer(palette = 'Dark2', labels = c('1 July 2021', '1 September 2021', '22 June 2022', '23 September 2022'))+
  stat_ellipse(geom = 'polygon', aes(group = date, color = date, fill = date), alpha = 0.3)+
  # geom_segment(data = functional_scores1, aes( x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #              arrow = arrow(), color = 'grey10', lwd = 1)+
  # geom_text_repel(data = functional_scores1, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = 'both',
  #                 segment.size = 0.25)+
annotate("label", x = -.5, y=1.5, label ="Stress = 0.15, k = 2", size = 10)+
  coord_equal()+
  theme_bw()+
  labs(title = "NMDS Legacy 21-22",
       shape = 'Crop',
       fill = 'Sampling date')+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 28),
        legend.position = 'right',
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1.5, 'cm')
  )+
  guides(size = 'none', color = 'none', fill = guide_legend(override.aes = list(size = 10)))

# 2022 - 2023
pc <- test_new %>% 
    filter(crop == "corn" & year == "2022")
pb <- test_new %>% 
    filter(crop == "beans" & year == "2023")

pc_pb <- rbind(pc, pb) 

pc_pb_pops <- pc_pb[6:9]

pc_pb_dist <- vegdist(pc_pb_pops, method = "bray")
p1_cb <- adonis2(pc_pb_dist ~ year + date + trt, permutations = factorial(10), method = "bray", data = pc_pb)
p1_cb

# Df SumOfSqs      R2      F    Pr(>F)    
# year      1   1.1525 0.07934 7.7637 6.614e-05 ***
#   date      2   2.4554 0.16903 8.2698 2.756e-07 ***
#   trt       3   0.3779 0.02601 0.8485    0.5933    
# Residual 71  10.5403 0.72561                     
# Total    77  14.5261 1.00000 

nmds2 <- metaMDS(pc_pb_pops, k = 2)
stressplot(nmds2)
nmds2$stress
# 0.1245803

scores <- scores(nmds2, display = 'sites')
scrs_2223 <- cbind(as.data.frame(scores), date = pc_pb$date)
scrs_2223 <- cbind(as.data.frame(scrs_2223), crop = pc_pb$crop)

functional_scores2 <- as.data.frame(scores(nmds2, 'species'))
functional_scores2$species <- rownames(functional_scores2)

ggplot(data = scrs_2223, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = crop, shape = crop), size =3)+
  scale_shape_discrete(labels = c('Soybean', 'Corn'))+
  scale_color_brewer( palette = 'Dark2')+
  scale_fill_brewer(palette = 'Dark2', labels = c('22 June 2022', '23 September 2022',
                                                  '18 July 2023', '4 November 2023'))+
  stat_ellipse(geom = 'polygon', aes(group = date, color = date, fill = date), alpha = 0.3)+
  # geom_segment(data = functional_scores2, aes( x = 0, xend = NMDS1, y = 0, yend = NMDS2),
  #              arrow = arrow(), color = 'grey10', lwd = 1)+
  # geom_text_repel(data = functional_scores2, aes(x = NMDS1, y = NMDS2, label = species), cex = 8, direction = 'both',
  #                 segment.size = 0.25)+
  annotate("label", x = -.5, y=1.2, label ="Stress = 0.13, k = 2", size = 10)+
  coord_equal()+
  theme_bw()+
  labs(title = "NMDS Legacy 22-23",
       fill = 'Sampling date',
       shape = 'Crop',
       )+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_blank(), # remove x-axis labels
        axis.title.y = element_blank(), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        plot.title = element_text(size = 28),
        legend.position = 'right',
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.key.size = unit(1.5, 'cm'))+
  guides(size = 'none', color = 'none', fill = guide_legend(order = 1), fill = guide_legend(override.aes = list(size = 10)))

# 4/12/2024: not using this anymore, this made the df too small
# permed_micros <- perm_micros %>% 
#   mutate(colembola = col_20 + col_10 + col_6 + col_4,
#          Insects = Enich + hemip + AC + CL + AC + OAC + Psocodea+ Thrips+ sipoopter+hymen+ archaeognatha
#          + dermaptera + lep+ Formicid + Coccomorpha+a_dipt + OL,
#          mites = Orb + Norb,
#          non_insect = spider + Pseu + Annelid+ protura + simphyla+pauropoda +Diplura +`Chil>5` + `Chil<5`+`Dip>5` + `Dip<5`) %>% 
#   dplyr::select(-japy, - camp, -`Dip>5`, -`Dip<5`, -`Chil>5`, -`Chil<5`,
#                 -col_20, -col_10, -col_6, -col_4, -Enich, -hemip, -sym, -pod, -ento, -Iso) %>% 
#   dplyr::select(-Orb, -Norb, -spider, -Pseu, -CL, -OL, -OAC, -a_dipt, -AC, - Coccomorpha, -Psocodea, -Thrips, -simphyla, 
#                 -pauropoda, -sipoopter, -Annelid, -protura, -hymen, -Formicid, -archaeognatha, -dermaptera, -lep, -Diplura) %>% 
#   replace(is.na(.),0) %>% 
#   mutate_at(vars(6:9), as.numeric) %>% 
#   mutate(trt = as.factor(trt)) %>% 
#   group_by(year, crop, date, trt) %>% 
#   summarise(
#     col = sum(colembola),
#     ins = sum(Insects),
#     mites = sum(mites), 
#     non_insect = sum(non_insect)
#   ) %>% 
#   mutate(date = as.factor(date))
# 
# 
# colnames(permed_micros)
# 
# perm_pops <- permed_micros[5:8]
# 
# perm_dist <- vegdist(perm_pops, "bray")
# 
# 
# perm_1 <- adonis2(perm_dist ~ trt, permutations = 999, method = "bray", data = permed_micros)
# perm_1
# 
# perm_2 <- adonis2(perm_dist ~ crop, permutations =  999, method = "bray", data = permed_micros)
# perm_2
# 
# # this one. Factorial 10 for permutations because I shrank the df so much 
# perm_3 <- adonis2(perm_dist ~ year + date + crop, permutations = 999, method = "bray", data = permed_micros)
# perm_3
# # Df SumOfSqs      R2      F    Pr(>F)    
# # year      2   0.5580 0.13967 3.9173  0.001725 ** 
# #   date      3   0.9222 0.23085 4.3166 9.425e-05 ***
# #   crop      1   0.1645 0.04119 2.3105  0.068170 .  
# # Residual 33   2.3502 0.58829                     
# # Total    39   3.9949 1.00000    



# nmds ####
# all data 

nmds1 <- metaMDS(perm_pops, k=2)
nmds1$stress
stressplot(nmds1)
# 0.1225953

scores <- scores(nmds1, display = 'sites')
scrs <- cbind(as.data.frame(scores), date = permed_micros$date)

functional_scores <- as.data.frame(scores(nmds1, 'species'))
functional_scores$species <- rownames(functional_scores)


ggplot(data = scrs, aes(x = NMDS1, y = NMDS2))+
  geom_point(aes(color = date))+
  stat_ellipse(geom = 'polygon', aes(group = date, color = date, fill = date), alpha = 0.3)
  


geom_segment(data = functional_scores, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25,"cm")),
               color = 'grey10', lwd = 0.7)


?geom_segment


plot(nmds1)
# plot 

scores <- scores(nmds1, display = 'sites')
year_scores <- cbind(as.data.frame(scores), year = permed_micros$year)

# species_df <- as.data.frame(scores(nmds1, dispaly = 'species'))
# species_df$species <- rownames(species_df)

plot <- plot_ly(year_scores, x = ~NMDS1, y = ~ NMDS2, z = ~NMDS3, color = ~year)
plot <- plot %>% 
  add_markers()
plot


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

