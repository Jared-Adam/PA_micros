# Jared  Adam 
# looking at soil mositure 

# packages #####

# data ####
moisture <- ce2_master_moisture_sheet


# wrangling ####
moisture <- moisture %>%
  mutate(date = case_when(date == 'Jul-21' ~ '7/1/2021',
         .default = as.character(date))) 
moisture_2021 <- moisture %>% 
  mutate(date = as.Date(date, '%m/%d/%Y'),
         year = format(date, '%Y')) %>% 
  filter(year == '2021') %>% 
  group_by(date, crop, plot, year) %>%
  summarise(moisture = mean(moisture)) %>% 
  relocate(date, crop, plot, moisture, year) %>% 
  print(n = Inf)

moisture_2223 <- moisture %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y")) %>% 
  filter(year != 2021) %>% 
  print(n = Inf)

moisture_all <- rbind(moisture_2021, moisture_2223) %>% 
  mutate(trt = case_when(plot %in% c('101', '203', '304', '401', '503') ~ '1', 
                         plot %in% c('102', '201', '303', '402', '502')~ '3',
                         plot %in% c('103', '204', '302', '403', '501') ~ '2', 
                         plot %in% c('104', '202', '301', '404', '504') ~ '4')) %>% 
  print(n = Inf)

# stats ####
m1 <- aov(moisture ~ trt, data = moisture_all)
TukeyHSD(m1)
hist(residuals(m1))
# $trt
# diff        lwr      upr     p adj
# 2-1  0.1737120 -2.2492919 2.596716 0.9977186
# 3-1  1.7228672 -0.7001367 4.145871 0.2566928
# 4-1  0.9323415 -1.4906624 3.355345 0.7512539
# 3-2  1.5491552 -0.8738487 3.972159 0.3495455
# 4-2  0.7586295 -1.6643744 3.181633 0.8490819
# 4-3 -0.7905256 -3.2135296 1.632478 0.8326577