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
