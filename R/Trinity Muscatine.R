library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(qdap)

# IHA Data ----

peer.hosp.ia <- Iowa_Healthcare_Data %>% 
  filter(Couunty %in% c('Henry', 'Washington'))
peer.hosp.il <- Illinois_Healthcare_Data %>%
  filter(Hospital %in% c('Genesis Medical Center Aledo', 'OSF Saint Luke Medical Center', 'OSF Holy Family Medical Center', 'Morrison Community Hospital', 'Hammond Henry Hospital'))

musc <- Iowa_Healthcare_Data %>%
  filter(Hospital == 'UnityPoint Health-Trinity Muscatine')

# Total Births ----
peer.births.ia <- peer.hosp.ia %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  #filter(Total_Births_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.births.il <- peer.hosp.il %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  #filter(Total_Births_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.births <- rbind(peer.births.ia, peer.births.il) %>% 
  mutate(value = as.numeric(value))
musc.births <- musc %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.births, aes(x = variable, y = value, group = Hospital), color = '#A7A9AC', size = 2) +
  geom_line(data = musc.births, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 400), labels = c('0 births', '100', '200', '300', '400 births')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL BIRTHS ARE DOWN AT UNITYPOINT HEALTH - TRINITY MUSCATINE,\nBUT REMAIN HIGHER THAN PEERS',
       subtitle = 'Total births in a year, compared to area hospitals',
       caption = 'Area hospitals include Washington County Hospital & Clinics,\nHenry County Health Center, Genesis Medical Center Aledo,\nHammond Henry Hospital, Morrison Community Hospital,\nOSF Saint Luke Medical Center, and OSF Holy Family Medical Center               Source: Iowa Hospital Assocation, Illinois Dept. of Public Health') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 1.5, y = 355, label = 'UnityPoint Health -\nTrinity Muscatine', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 1.5, y = 355, label = 'UnityPoint Health -\nTrinity Muscatine', color = '#E84B37', size = 5, fontface = 'bold')

# Total Emergency Department Visits ----
peer.ed.ia <- peer.hosp.ia %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  #filter(Emergency_Department_Visits_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.ed.il <- peer.hosp.il %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  #filter(Emergency_Department_Visits_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.ed <- rbind(peer.ed.ia, peer.ed.il) %>% 
  mutate(value = as.numeric(value))
musc.ed <- musc %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.ed, aes(x = variable, y = value, group = Hospital), color = '#A7A9AC', size = 2) +
  geom_line(data = musc.ed, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 20000), labels = c('0 visits', '5,000', '10,000', '15,000', '20,000 visits')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL EMERGENCY DEPARTMENT VISITS ARE HIGHEST\nIN THE AREA AND GROWING',
       subtitle = 'Total emergency department visits in a year, compared to area peer hospitals',
       caption = 'Area hospitals include Washington County Hospital & Clinics,\nHenry County Health Center, Genesis Medical Center Aledo,\nHammond Henry Hospital, Morrison Community Hospital,\nOSF Saint Luke Medical Center, and OSF Holy Family Medical Center               Source: Iowa Hospital Assocation, Illinois Dept. of Public Health') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 1.5, y = 17000, label = 'UnityPoint Health -\nTrinity Muscatine', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 1.5, y = 17000, label = 'UnityPoint Health -\nTrinity Muscatine', color = '#E84B37', size = 5, fontface = 'bold')

# Total Outpatient Surgeries ----
peer.op.ia <- peer.hosp.ia %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  #filter(Total_Outpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.op.il <- peer.hosp.il %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  #filter(Total_Outpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.op <- rbind(peer.op.ia, peer.op.il) %>% 
  mutate(value = as.numeric(value))
musc.op <- musc %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.op, aes(x = variable, y = value, group = Hospital), color = 'grey60', size = 2) +
  geom_line(data = musc.op, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 3000), labels = c('0 Surgeries', '1,000', '2,000', '3,000 surgeries')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL OUTPATIENT SURGERIES HAVE STABILIZED\nAFTER A SIGNIFICANT DROP',
       subtitle = 'Total outpatient surgeries in a year, compared to area peer hospitals',
       caption = 'Area hospitals include Washington County Hospital & Clinics,\nHenry County Health Center, Genesis Medical Center Aledo,\nHammond Henry Hospital, Morrison Community Hospital,\nOSF Saint Luke Medical Center, and OSF Holy Family Medical Center               Source: Iowa Hospital Assocation, Illinois Dept. of Public Health') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 1.5, y = 2900, label = 'UnityPoint Health -\nTrinity Muscatine', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 1.5, y = 2900, label = 'UnityPoint Health -\nTrinity Muscatine', color = '#E84B37', size = 5, fontface = 'bold')

# Total Inpatient Surgeries ----
peer.ip.ia <- peer.hosp.ia %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  #filter(Total_Inpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.ip.il <- peer.hosp.il %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  #filter(Total_Inpatient_Surgeries_2011 > 0) %>% 
  gather(variable, value, -Hospital)
peer.ip <- rbind(peer.ip.ia, peer.ip.il) %>% 
  mutate(value = as.numeric(value))
musc.ip <- musc %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital)

ggplot() +
  geom_line(data = peer.ip, aes(x = variable, y = value, group = Hospital), color = 'grey60', size = 2) +
  geom_line(data = musc.ip, aes(x = variable, y = value, group = Hospital), color = '#E84B37', size = 2) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 600), labels = c('0 surgeries', '200', '400', '600 surgeries')) +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) + 
  labs(title = 'TOTAL INPATIENT SURGERIES HAVE STABILIZED\nAFTER A SIGNIFICANT DROP',
       subtitle = 'Total inpatient surgeries in a year, compared to area peer hospitals',
       caption = 'Area hospitals include Washington County Hospital & Clinics,\nHenry County Health Center, Genesis Medical Center Aledo,\nHammond Henry Hospital, Morrison Community Hospital,\nOSF Saint Luke Medical Center, and OSF Holy Family Medical Center               Source: Iowa Hospital Assocation, Illinois Dept. of Public Health') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1, hjust = 0)) +
  annotate('label', x = 5.5, y = 320, label = 'UnityPoint Health -\nMarshalltown', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 5.5, y = 320, label = 'UnityPoint Health -\nMarshalltown', color = '#E84B37', size = 5, fontface = 'bold')

# Medicare Data ----
m.town <- ia_medicare_data %>% 
  filter(Hospital == 'TRINITY MUSCATINE')

# Door to Diag Eval ----
d2d.eval.ia <- ia_medicare_data %>% 
  filter(Hospital != 'TRINITY MUSCATINE') %>% 
  select(Hospital:Door_To_Diag_Eval_2013) %>% 
  gather(variable, value, -Hospital)
d2d.eval.il <- il_medicare_data %>% 
  select(`Hospital Name`,Door_To_Diag_Eval_2016:Door_To_Diag_Eval_2013) %>% 
  rename(Hospital = `Hospital Name`) %>% 
  gather(variable, value, -Hospital)
d2d.eval <- rbind(d2d.eval.ia, d2d.eval.il) %>% 
  mutate(value = as.numeric(value))

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
  scale_y_continuous(limits = c(0,200), breaks = c(seq(0,200,50)), labels = c('0 minutes', '50', '100', '150', '200 minutes')) +
  scale_x_discrete(labels = c('2013', '2014', '2015', '2016')) + 
  labs(title = 'AT UNITYPOINT HEALTH - TRINITY MUSCATINE, DOOR-TO-DIAGNOSTIC\nEVALUATION TIMES ARE FASTER THAN THE IOWA-ILLINOIS AVERAGE',
       subtitle = 'Average (median) time patients spent in the emergency department before they were seen by a healthcare professional',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2.5, y = 6, label = 'UnityPoint Health -\nTrinity Muscatine', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2.5, y = 6, label = 'UnityPoint Health -\nTrinity Muscatine', color = '#E84B37', size = 5, fontface = 'bold') +
  annotate('label', x = 2.5, y = 35, label = 'Iowa-Illinois Hospital Average', fill = '#F0F0F0', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2.5, y = 35, label = 'Iowa-Illinois Hospital Average', color = '#6D6E71', size = 5, fontface = 'bold')

# Room Quietness ----
all.quiet.ia <- ia_medicare_data %>% 
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
  scale_x_continuous(breaks = c(seq(2009, 2016, 1))) + 
  labs(title = 'ROOM NOISE RATINGS FLUCTUATE AT UNITYPOINT HEALTH - TRINITY MUSCATINE',
       subtitle = '"During this hospital stay, how often was the area around your room quiet at night?"\nPercent of all patient responses, categorized into always quiet, usually quiet, and sometimes or never quiet ratings',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2016, y = 65, label = 'ALWAYS', fill = '#A7A9AC', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2016, y = 65, label = 'ALWAYS', color = 'grey80', size = 5, fontface = 'bold') +
  annotate('label', x = 2016, y = 25, label = 'USUALLY', fill = '#6d6E71', color = NA, size = 5, fontface = 'bold') +
  annotate('text', x = 2016, y = 25, label = 'USUALLY', color = 'grey65', size = 5, fontface = 'bold') +
  annotate('label', x = 2016, y = 5, label = 'SOMETIMES\nOR\nNEVER', fill = '#E84B37', color = NA, size = 3, fontface = 'bold') +
  annotate('text', x = 2016, y = 5, label = 'SOMETIMES\nOR\nNEVER', color = 'darkred', size = 3, fontface = 'bold')

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
  scale_x_continuous(breaks = c(seq(2009, 2016, 1))) + 
  labs(title = 'UNITYPOINT HEALTH - TRINITY MUSCATINE HAS THE MOST\nLOW AND MEDIUM RATINGS IN THE STATE OF IOWA',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patient responses, categorized into high (9+), medium (7-8), and low (<6) ratings',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2016, y = 65, label = 'HIGH', fill = '#A7A9AC', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2016, y = 65, label = 'HIGH', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2016, y = 25, label = 'MEDIUM', fill = '#6d6E71', color = NA, size = 6, fontface = 'bold') +
  annotate('text', x = 2016, y = 25, label = 'MEDIUM', color = 'grey65', size = 6, fontface = 'bold') +
  annotate('label', x = 2016, y = 5, label = 'LOW', fill = '#E84B37', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2016, y = 5, label = 'LOW', color = 'darkred', size = 7, fontface = 'bold')

# Low Ratings Analysis vs. All IA Hospitals
m.town.low <- ia_medicare_data %>% 
  filter(Hospital == 'TRINITY MUSCATINE') %>% 
  select(Hospital, Hospital_Rating_Low_2016:Hospital_Rating_Low_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "low") 

ia.low.avg <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Low_2016:Hospital_Rating_Low_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         Hospital = "A",
         rating = "low") %>% 
  group_by(Hospital, year) %>% 
  summarize(value = mean(value, na.rm = TRUE))

ia.low.all <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Low_2016:Hospital_Rating_Low_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "low")
  
ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Low_2016:Hospital_Rating_Low_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1))) %>% 
  ggplot(aes(x = year, y = value, group = Hospital)) +
  geom_line(show_guide = FALSE, alpha = 0.05, size = 2) +
  geom_line(data = m.town.low, aes(x = year, y = value, color = Hospital, group = Hospital), size = 2, color = primary_marketing[3]) +
  geom_line(data = ia.low.avg, aes(x = year, y = value, group = Hospital), color = primary_marketing[1], size = 2) +
  scale_y_continuous(labels = c('0 percent', '5', '10', '15', '20 percent'), breaks = c(seq(0,20,5))) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none') +
  annotate('label', label = 'UnityPoint Health -\nTrinity Muscatine', x = 2015.5 , y = 12, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'UnityPoint Health -\nTrinity Muscatine', x = 2015.5, y = 12, color = primary_marketing[3], fontface = 'bold') +
  annotate('label', label = 'Iowa Hospital Average', x = 2015.5 , y = 4.5, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'Iowa Hospital Average', x = 2015.5, y = 4.5, color = primary_marketing[1], fontface = 'bold') +
  labs(title = 'UNITYPOINT HEALTH - TRINITY MUSCATINE HAS THE MOST\nLOW RATINGS IN THE STATE OF IOWA',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hopsital during your stay?"\nPercent of all patients who gave a rating of "6" or lower (low)',
       caption = 'Source: Medicare HCAHPS Survey, 2011-2016')

# Medium Ratings Analysis vs. All IA Hospitals
m.town.med <- ia_medicare_data %>% 
  filter(Hospital == 'TRINITY MUSCATINE') %>% 
  select(Hospital, Hospital_Rating_Medium_2016:Hospital_Rating_Medium_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "medium") 

ia.med.avg <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Medium_2016:Hospital_Rating_Medium_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         Hospital = "A",
         rating = "medium") %>% 
  group_by(Hospital, year) %>% 
  summarize(value = mean(value, na.rm = TRUE))

ia.med.all <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Medium_2016:Hospital_Rating_Medium_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "medium")

ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Medium_2016:Hospital_Rating_Medium_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1))) %>% 
  ggplot(aes(x = year, y = value, group = Hospital)) +
  geom_line(show_guide = FALSE, alpha = 0.05, size = 2) +
  geom_line(data = m.town.med, aes(x = year, y = value, color = Hospital, group = Hospital), size = 2) +
  geom_line(data = ia.med.avg, aes(x = year, y = value, group = Hospital), color = 'black', size = 2) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none')

ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_Medium_2016:Hospital_Rating_Medium_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1))) %>% 
  ggplot(aes(x = year, y = value, group = Hospital)) +
  geom_line(show_guide = FALSE, alpha = 0.05, size = 2) +
  geom_line(data = m.town.med, aes(x = year, y = value, color = Hospital, group = Hospital), size = 2, color = primary_marketing[3]) +
  geom_line(data = ia.med.avg, aes(x = year, y = value, group = Hospital), color = primary_marketing[1], size = 2) +
  scale_y_continuous(labels = c('0 percent', '10', '20', '30', '40 percent'), breaks = c(seq(0,40,10)), limits = c(0,40)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none') +
  annotate('label', label = 'UnityPoint Health \nTrinity Muscatine', x = 2011.5 , y = 32, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'UnityPoint Health -\nTrinity Muscatine', x = 2011.5, y = 32, color = primary_marketing[3], fontface = 'bold') +
  annotate('label', label = 'Iowa Hospital Average', x = 2011.5, y = 19, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'Iowa Hospital Average', x = 2011.5, y = 19, color = primary_marketing[1], fontface = 'bold') +
  labs(title = 'UNITYPOINT HEALTH - TRINITY MUSCATINE HAS A LARGE AMOUNT OF\nMEDIUM RATINGS COMPARED TO OTHER IOWA HOSPITALS',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hopsital during your stay?"\nPercent of all patients who gave a rating of "7" or "8" (medium)',
       caption = 'Source: Medicare HCAHPS Survey, 2011-2016')

# High Ratings Analysis vs. All IA Hospitals
m.town.high <- ia_medicare_data %>% 
  filter(Hospital == 'TRINITY MUSCATINE') %>% 
  select(Hospital, Hospital_Rating_High_2016:Hospital_Rating_High_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "high") 

ia.high.avg <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_High_2016:Hospital_Rating_High_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         Hospital = "A",
         rating = "high") %>% 
  group_by(Hospital, year) %>% 
  summarize(value = mean(value, na.rm = TRUE))

ia.high.all <- ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_High_2016:Hospital_Rating_High_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         rating = "high")

ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_High_2016:Hospital_Rating_High_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1))) %>% 
  ggplot(aes(x = year, y = value, group = Hospital)) +
  geom_line(show_guide = FALSE, alpha = 0.05, size = 2) +
  geom_line(data = m.town.high, aes(x = year, y = value, color = Hospital, group = Hospital), size = 2) +
  geom_line(data = ia.high.avg, aes(x = year, y = value, group = Hospital), color = 'black', size = 2) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none')

ia_medicare_data %>% 
  select(Hospital, Hospital_Rating_High_2016:Hospital_Rating_High_2011) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1))) %>% 
  ggplot(aes(x = year, y = value, group = Hospital)) +
  geom_line(show_guide = FALSE, alpha = 0.05, size = 2) +
  geom_line(data = m.town.high, aes(x = year, y = value, color = Hospital, group = Hospital), size = 2, color = primary_marketing[3]) +
  geom_line(data = ia.high.avg, aes(x = year, y = value, group = Hospital), color = primary_marketing[1], size = 2) +
  scale_y_continuous(labels = c('50 percent', '60', '70', '80', '90', '100 percent'), breaks = c(seq(50,100,10)), limits = c(50,100)) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none') +
  annotate('label', label = 'UnityPoint Health -\nTrinity Muscatine', x = 2011.5, y = 58, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'UnityPoint Health -\nTrinity Muscatine', x = 2011.5, y = 58, color = primary_marketing[3], fontface = 'bold') +
  annotate('label', label = 'Iowa Hospital Average', x = 2011.5, y = 76, fill = '#F0F0F0', color = NA) +
  annotate('text', label = 'Iowa Hospital Average', x = 2011.5, y = 76, color = primary_marketing[1], fontface = 'bold') +
  labs(title = 'UNITYPOINT HEALTH - TRINITY MUSCATINE IS NOT OFTEN RATED HIGHLY\nCOMPARED TO OTHER IOWA HOSPITALS',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hopsital during your stay?"\nPercent of all patients who gave a rating of "9" or "10" (high)',
       caption = 'Source: Medicare HCAHPS Survey, 2011-2016')

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
  scale_x_continuous(breaks = c(seq(2009, 2016, 1))) + 
  labs(title = 'IN THE STATE OF IOWA, UNITYPOINT HEALTH - TRINITY MUSCATINE IS\nLEAST LIKELY TO BE DEFINITELY RECOMMENDED BY PATIENTS',
       subtitle = '"Would you recommend this hospital to your friends and family?"\nPercent of all patient responses who would definitely (Yes), probably, or not (No) recommend the facility',
       caption = 'Source: Medicare HCAHPS Survey') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -1)) +
  annotate('label', x = 2016, y = 75, label = 'YES', fill = '#A7A9AC', color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2016, y = 75, label = 'YES', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2016, y = 25, label = 'PROBABLY', fill = '#6d6E71', color = NA, size = 4, fontface = 'bold') +
  annotate('text', x = 2016, y = 25, label = 'PROBABLY', color = 'grey65', size = 4, fontface = 'bold') +
  annotate('label', x = 2016, y = 3.5, label = 'NO', fill = '#E84B37', color = NA, size = 6, fontface = 'bold') +
  annotate('text', x = 2016, y = 3.5, label = 'NO', color = 'darkred', size = 6, fontface = 'bold')

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
