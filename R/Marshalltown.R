library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# IHA Data ----

peer.hosp <- iha %>% 
  filter(Couunty %in% c('Black Hawk', 'Grundy', 'Hardin', 'Jasper', 'Poweshiek', 'Story'))

marshalltown <- iha %>%
  filter(Hospital == 'UnityPoint Health-Marshalltown')

# Total Births ----
peer.births <- peer.hosp %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  filter(Total_Births_2011 > 0) %>% 
  gather(variable, value, -Hospital)
marshall.births <- marshalltown %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.births, aes(x = variable, y = value, group = Hospital), color = '#A7A9AC', size = 2) +
  geom_line(data = marshall.births, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 1600), labels = c('0 Births', '500', '1,000', '1,500 Births')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL BIRTHS ARE DOWN AT UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = 'Total births in a year, compared to area hospitals',
       caption = 'Area hospitals include Covenant Medical Center, Grinnell Regional Medical Center,\nHansen Family Hospital, Mary Greeley Medical Center, Skiff Medical Center, and\nUnityPoint Health - Allen Hospital                                                                                                                       Source: Iowa Hospital Assocation') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 5.5, y = 600, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 5.5, y = 600, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold')

# Total Emergency Department Visits ----
peer.ed <- peer.hosp %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  filter(Emergency_Department_Visits_2011 > 0) %>% 
  gather(variable, value, -Hospital)
marshall.ed <- marshalltown %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.ed, aes(x = variable, y = value, group = Hospital), color = '#A7A9AC', size = 2) +
  geom_line(data = marshall.ed, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 40000), labels = c('0 Visits', '10,000', '20,000', '30,000', '40,000 Visits')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL EMERGENCY DEPARTMENT VISITS ARE STABLE\nAT UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = 'Total emergency department visits in a year, compared to area hospitals',
       caption = 'Area hospitals include Covenant Medical Center, Grinnell Regional Medical Center,\nHansen Family Hospital, Mary Greeley Medical Center, Skiff Medical Center, and\nUnityPoint Health - Allen Hospital                                                                                                                       Source: Iowa Hospital Assocation') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 1.5, y = 17000, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 1.5, y = 17000, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold')

# Total Outpatient Surgeries ----
peer.op <- peer.hosp %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  filter(Total_Outpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
marshall.op <- marshalltown %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.op, aes(x = variable, y = value, group = Hospital), color = 'grey60', size = 2) +
  geom_line(data = marshall.op, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 20000), labels = c('0 Surgeries', '5,000', '10,000', '15,000', '20,000 Surgeries')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL OUTPATIENT SURGERIES REMAIN STABLE\nAT UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = 'Total outpatient surgeries in a year, compared to area hospitals',
       caption = 'Area hospitals include Covenant Medical Center, Grinnell Regional Medical Center,\nHansen Family Hospital, Mary Greeley Medical Center, Skiff Medical Center, and\nUnityPoint Health - Allen Hospital                                                                                                                       Source: Iowa Hospital Assocation') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 5.5, y = 5250, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 5.5, y = 5250, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold')

# Total Inpatient Surgeries ----
peer.ip <- peer.hosp %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  filter(Total_Inpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
marshall.ip <- marshalltown %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.ip, aes(x = variable, y = value, group = Hospital), color = 'grey60', size = 2) +
  geom_line(data = marshall.ip, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 3000), labels = c('0 Surgeries', '1,000', '2,000', '3,000 Surgeries')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL INPATIENT SURGERIES ARE DOWN\nAT UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = 'Total inpatient surgeries in a year, compared to area hospitals',
       caption = 'Area hospitals include Covenant Medical Center, Grinnell Regional Medical Center,\nHansen Family Hospital, Mary Greeley Medical Center, Skiff Medical Center, and\nUnityPoint Health - Allen Hospital                                                                                                                       Source: Iowa Hospital Assocation') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 2, y = 950, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2, y = 950, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold')

# Medicare Data ----
m.town <- medicare_data %>% 
  filter(Hospital == 'CENTRAL IOWA HEALTHCARE')

# Door to Diag Eval ----
d2d.eval <- medicare_data %>% 
  filter(Hospital != 'CENTRAL IOWA HEALTHCARE') %>% 
  select(Hospital:Door_To_Diag_Eval_2013) %>% 
  gather(variable, value, -Hospital)

d2d.avg <- d2d.eval %>%
  group_by(variable) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  mutate(Hospital = 'avg')

m.town.d2d <- m.town %>% 
  select(Hospital:Door_To_Diag_Eval_2013) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = d2d.eval, aes(x = variable, y = value, group = Hospital), color = '#A7A9AC', size = 2, alpha = .25) +
  geom_line(data = m.town.d2d, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  geom_line(data = d2d.avg, aes(x = variable, y = value, group = Hospital), color = '#6D6E71', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(breaks = c(seq(0,50,10)), labels = c('0 minutes', '10', '20', '30', '40', '50 minutes')) +
  scale_x_discrete(labels = c('2013', '2014', '2015', '2016')) + 
  labs(title = 'UNITYPOINT HEALTH - MARSHALLTOWN HAS ONE OF THE SLOWEST\nEMERGENCY DEPARTMENT DOOR-TO-DIAGNOSTIC EVALUATION TIMES\nIN THE STATE OF IOWA',
       subtitle = 'Average (median) time patients spent in the emergency department before they were seen by a healthcare professional',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2.5, y = 31, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2.5, y = 31, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold') +
  annotate('label', x = 2.5, y = 19, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2.5, y = 19, label = 'Iowa Hospital Average', color = '#6D6E71', size = 5, fontface = 'bold')

# Room Quietness ----
all.quiet <- medicare_data %>% 
  filter(Hospital != 'CENTRAL IOWA HEALTHCARE') %>% 
  select(Hospital, Always_Quiet_Night_2016:Usually_Quiet_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)))

m.town.quiet <- m.town %>% 
  select(Hospital, Always_Quiet_Night_2016:Usually_Quiet_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         cat = qdap::beg2char(variable, "_", 2),
         cat = factor(cat, levels = c('Always_Quiet', 'Usually_Quiet', 'Sometimes_Quiet')))

ggplot(m.town.quiet, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c('#A7A9AC', '#6D6E71', '#E84B37')) +
  #scale_fill_manual(values = color_invision_new_primary) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0%', '20', '40', '60', '80', '100%')) +
  scale_x_continuous(breaks = c(seq(2009, 2015, 1))) + 
  labs(title = 'ROOM NOISE RATINGS FLUCTUATE AT UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = '"During this hospital stay, how often was the area around your room quiet at night?"\nPercent of all patient responses, categorized into always quiet, usually quiet, and sometimes or never quiet ratings',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2015, y = 65, label = 'ALWAYS', fill = '#A7A9AC', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2015, y = 65, label = 'ALWAYS', color = 'grey80', size = 5, fontface = 'bold') +
  annotate('label', x = 2015, y = 25, label = 'USUALLY', fill = '#6d6E71', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2015, y = 25, label = 'USUALLY', color = 'grey65', size = 5, fontface = 'bold') +
  annotate('label', x = 2015, y = 5, label = 'SOMETIMES\nOR\nNEVER', fill = '#E84B37', color = NA, size = 3, fontface = 'bold') +
  annotate('text', x = 2015, y = 5, label = 'SOMETIMES\nOR\nNEVER', color = 'darkred', size = 3, fontface = 'bold')

# Hospital Rating ----
m.town.rating <- m.town %>% 
  select(Hospital, Hospital_Rating_Low_2016:Hospital_Rating_High_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         cat = qdap::beg2char(variable, "_", 3),
         cat = factor(cat, levels = c('Hospital_Rating_High', 'Hospital_Rating_Medium', 'Hospital_Rating_Low')))

ggplot(m.town.rating, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c('#A7A9AC', '#6D6E71', '#E84B37')) +
  #scale_fill_manual(values = color_invision_new_primary) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0%', '20', '40', '60', '80', '100%')) +
  scale_x_continuous(breaks = c(seq(2009, 2015, 1))) + 
  labs(title = 'UNITYPOINT HEALTH - MARSHALLTOWN HOSPITAL RATINGS ARE GETTING WORSE',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patient responses, categorized into high (9+), medium (7-8), and low (<6) ratings',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2015, y = 65, label = 'HIGH', fill = '#A7A9AC', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2015, y = 65, label = 'HIGH', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2015, y = 25, label = 'MEDIUM', fill = '#6d6E71', color = NA, size = 6.5, fontface = 'bold') +
  annotate('text', x = 2015, y = 25, label = 'MEDIUM', color = 'grey65', size = 6.5, fontface = 'bold') +
  annotate('label', x = 2015, y = 5, label = 'LOW', fill = '#E84B37', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2015, y = 5, label = 'LOW', color = 'darkred', size = 7, fontface = 'bold')

# Recommendation ----
m.town.recommend <- m.town %>% 
  select(Hospital, Recommend_Yes_2016:Recommend_No_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         cat = qdap::beg2char(variable, "_", 2),
         cat = factor(cat, levels = c('Recommend_Yes', 'Recommend_Probably', 'Recommend_No')))

# Marketing Colors
ggplot(m.town.recommend, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c('#A7A9AC', '#6D6E71', '#E84B37')) +
  #scale_fill_manual(values = rev(condition_charts)) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0%', '20', '40', '60', '80', '100%')) +
  scale_x_continuous(breaks = c(seq(2009, 2015, 1))) + 
  labs(title = 'PATIENTS ARE STARTING TO BECOME LESS CONFIDENT TO\nRECOMMEND UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = '"Would you recommend this hospital to your friends and family?"\nPercent of all patient responses who would definitely (Yes), probably, or not (No) recommend the facility',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2015, y = 75, label = 'YES', fill = '#A7A9AC', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2015, y = 75, label = 'YES', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2015, y = 25, label = 'PROBABLY', fill = '#6d6E71', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2015, y = 25, label = 'PROBABLY', color = 'grey65', size = 5, fontface = 'bold') +
  annotate('label', x = 2015, y = 3.5, label = 'NO', fill = '#E84B37', color = NA, size = 6, fontface = 'bold') +
  annotate('text', x = 2015, y = 3.5, label = 'NO', color = 'darkred', size = 6, fontface = 'bold')

# Condition Charts Colors
ggplot(m.town.recommend, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = rev(condition_charts)) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0%', '20', '40', '60', '80', '100%')) +
  scale_x_continuous(breaks = c(seq(2009, 2015, 1))) + 
  labs(title = 'PATIENTS ARE STARTING TO BECOME LESS CONFIDENT TO\nRECOMMEND UNITYPOINT HEALTH - MARSHALLTOWN',
       subtitle = '"Would you recommend this hospital to your friends and family?"\nPercent of all patient responses who would definitely (Yes), probably, or not (No) recommend the facility',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2015, y = 75, label = 'YES', fill = '#C3F227', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2015, y = 75, label = 'YES', color = 'darkolivegreen3', size = 7, fontface = 'bold') +
  annotate('label', x = 2015, y = 25, label = 'PROBABLY', fill = '#FFC200', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2015, y = 25, label = 'PROBABLY', color = 'tan2', size = 5, fontface = 'bold') +
  annotate('label', x = 2015, y = 3.5, label = 'NO', fill = '#E84B37', color = NA, size = 6, fontface = 'bold') +
  annotate('text', x = 2015, y = 3.5, label = 'NO', color = 'darkred', size = 6, fontface = 'bold')
