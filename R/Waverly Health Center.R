# Load Libraries ----
library(tidyverse)
library(ggthemes)
library(qdap)

# List of Surrounding Facilities ----
surrounding.list.IHA <- c('Bremer', 'Floyd', 'Chickasaw', 'Fayette', 'Buchanan', 'Black Hawk', 'Grundy')
surrounding.list.medicare <- c('BUCHANAN COUNTY HEALTH CENTER', 'COMMUNITY MEMORIAL HOSPITAL MEDICAL CENTER', 'COVENANT MEDICAL CENTER', 'FLOYD COUNTY MEDICAL CENTER', 'GRUNDY COUNTY MEMORIAL HOSPITAL', 'GUNDERSEN PALMER LUTHERAN HOSPITAL AND CLINICS', 'MERCY HOSPITAL OF FRANCISCAN SISTERS-OELWEIN', 'MERCY MEDICAL CENTER-NEW HAMPTON', 'SARTORI MEMORIAL HOSPITAL', 'ALLEN HOSPITAL', 'WAVERLY HEALTH CENTER')

# List of Peer Facilities ----
peer.list.IHA <- c('Boone', 'Benton', 'Plymouth', 'Bremer', 'Washington', 'Mahaska', 'Buchanan', 'Winneshiek', 'Jones', 'Carroll', 'Buena Vista', 'Fayette')
peer.list.medicare <- c('BOONE COUNTY HOSPITAL', 'BUCHANAN COUNTY HEALTH CENTER', 'BUENA VISTA REGIONAL MEDICAL CENTER', 'COMMUNITY MEMORIAL HOSPITAL MEDICAL CENTER', 'FLOYD VALLEY HOSPITAL', 'GUNDERSEN PALMER LUTHERAN HOSPITAL AND CLINICS', 'MAHASKA  HEALTH PARTNERSHIP', 'MANNING REGIONAL HEALTHCARE CENTER', 'MERCY HOSPITAL OF FRANCISCAN SISTERS-OELWEIN', 'ST ANTHONY REGIONAL HOSPITAL & NURSING HOME', 'JONES REGIONAL MEDICAL CENTER', 'VIRGINIA GAY HOSPITAL', 'WASHINGTON COUNTY HOSPITAL AND CLINICS', 'WAVERLY HEALTH CENTER', 'WINNESHIEK MEDICAL CENTER')

# Load & Clean Data ----
medicare.data <- ia_medicare_data
iha.data <- Iowa_Healthcare_Data

# ED: Door-to-Diag ----
medicare.data %>% 
  select(Hospital:Door_To_Diag_Eval_2013) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_line(size = 2, alpha = 0.05) +
  #geom_jitter(data = medicare.data %>% 
  #             filter(Hospital %in% surrounding.list.medicare) %>% 
  #             select(Hospital:Door_To_Diag_Eval_2013) %>% 
  #             gather(variable, value, -Hospital), aes(x = variable, y = value), color = 'blue', size = 4, width = 0.05, height = 0) +
  #geom_jitter(data = medicare.data %>% 
  #              filter(Hospital %in% peer.list.medicare) %>% 
  #              select(Hospital:Door_To_Diag_Eval_2013) %>% 
  #              gather(variable, value, -Hospital), aes(x = variable, y = value), color = 'green', size = 4, width = 0.05, height = 0) +
  geom_line(data = medicare.data %>% 
             select(Hospital:Door_To_Diag_Eval_2013) %>% 
               gather(variable, value, -Hospital) %>% 
               group_by(variable) %>% 
               summarize(value = mean(value, na.rm = T)) %>% 
               mutate(Hospital = 1), aes(x = variable, y = value, group = Hospital), color = primary_marketing[4], size = 2) +
  #geom_line(data = medicare.data %>% 
  #            select(Hospital:Door_To_Diag_Eval_2013) %>% 
  #            filter(Hospital %in% surrounding.list.medicare) %>% 
  #            gather(variable, value, -Hospital) %>% 
  #            group_by(variable) %>% 
  #            summarize(value = mean(value, na.rm = T)) %>% 
  #            mutate(Hospital = 2), aes(x = variable, y = value, group = Hospital), color = primary_marketing[5], size = 2, width = 0.05,  height = 0) +
  #geom_line(data = medicare.data %>% 
  #            select(Hospital:Door_To_Diag_Eval_2013) %>% 
  #            filter(Hospital %in% peer.list.medicare) %>% 
  #            gather(variable, value, -Hospital) %>% 
  #            group_by(variable) %>% 
  #            summarize(value = mean(value, na.rm = T)) %>% 
  #            mutate(Hospital = 3), aes(x = variable, y = value, group = Hospital), color = primary_marketing[1], size = 2, width = 0.05,  height = 0) +
  geom_line(data = medicare.data %>% 
                filter(Hospital == 'WAVERLY HEALTH CENTER') %>% 
                select(Hospital:Door_To_Diag_Eval_2013) %>% 
                gather(variable, value, -Hospital), aes(x = variable, y = value), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'At Waverly Health Center, the door-to-diagnostic evaluation times\nin the emergency department are slower than the Iowa average',
       subtitle = 'Average (median) time patients spent in the emergency department before they were seen by a healthcare professional\nNote: No data available for Waverly Health Center in 2013-14', 
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_x_discrete(labels = c(seq(2013,2016,1))) +
  scale_y_continuous(labels = c('0 minutes', '10', '20', '30', '40', '50 minutes')) +
  theme(plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 3.5, y = 27, label = 'Waverly Health Center', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3.5, y = 27, label = 'Waverly Health Center', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 3.5, y = 18, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3.5, y = 18, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold')


# ED: Volumes ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
              filter(Hospital == 'Waverly Health Center') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Emergency department visits at Waverly Health Center continue to grow,\nwhile some peer hospitals begin to stagnate or decline',
       subtitle = 'Total emergency department visits, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Boone County Hospital,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nGundersen Palmer Lutheran Hospital and Clinics, Virginia Gay Hospital,\nUnityPoint Health-Jones Regional Medical Center, Floyd Valley Healthcare\nBuchanan County Health Center, Manning Regional Healthcare Center\nMercy Hospital of Franciscan Sisters, Community Memorial Hospital, Mahaska Health Partnership                 Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 12000), labels = c('0 visits', '2,500', '5,000', '7,500', '10,000', '12,500 visits')) +
  annotate('label', x = 1.5, y = 6250, label = 'Waverly Health Center', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 1.5, y = 6250, label = 'Waverly Health Center', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Surrounding Hospitals
iha.data %>% 
  select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% surrounding.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Emergency_Department_Visits_2011:Emergency_Department_Visits_2016) %>% 
              filter(Hospital == 'Waverly Health Center') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Emergency department visits are higher at Waverly Health Center\nthan the surrounding critical access hospitals',
       subtitle = 'Total emergency department visits, compared to local critical access hospitals',
       caption = 'Local Hospitals: Gundersen Palmer Lutheran Hospital and Clinics,\nBuchanan County Health Center, Floyd County Medical Center,\nMercy Medical Center-New Hampton, Grundy County Memorial Hospital,\nMercy Hospital of Franciscan Sisters, Community Memorial Hospital                                                             Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 8000), labels = c('0 visits', '2,000', '4,000', '6,000', '8,000 visits')) +
  annotate('label', x = 1.5, y = 7000, label = 'Waverly Health Center', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 1.5, y = 7000, label = 'Waverly Health Center', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(vjust = -2, hjust = 0))

# Hospital Ratings - High ----
medicare.data %>% 
  select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_line(alpha = 0.05, size = 2) +
  geom_line(data = medicare.data %>% 
              select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2016) %>% 
              gather(variable, value, -Hospital) %>% 
              group_by(variable) %>% 
              summarize(value = mean(value, na.rm = T)) %>% 
              mutate(Hospital = 1), color = primary_marketing[4], size = 2) +  
  geom_line(data = medicare.data %>% 
              filter(Hospital == 'WAVERLY HEALTH CENTER') %>% 
              select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2016) %>% 
              gather(variable, value, -Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Waverly Health Center is consistently rated higher than other Iowa hospitals',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patients who gave a rating of "9" or "10" (high)',
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_y_continuous(limits = c(40,100), breaks = c(seq(40, 100, 10)), labels = c('40 percent', '50', '60', '70', '80', '90', '100 percent')) +
  scale_x_discrete(labels = c(seq(2008, 2016, 1))) +
  theme(plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 3.5, y = 87, label = 'Waverly Health Center', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3.5, y = 87, label = 'Waverly Health Center', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 5, y = 72, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 5, y = 72, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold')

# Hospital Ratings - Low ----
medicare.data %>% 
  select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_line(alpha = 0.05, size = 2) +
  geom_line(data = medicare.data %>% 
              select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2016) %>% 
              gather(variable, value, -Hospital) %>% 
              group_by(variable) %>% 
              summarize(value = mean(value, na.rm = T)) %>% 
              mutate(Hospital = 1), color = primary_marketing[4], size = 2) +  
  geom_line(data = medicare.data %>% 
              filter(Hospital == 'WAVERLY HEALTH CENTER') %>% 
              select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2016) %>% 
              gather(variable, value, -Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Waverly Health Center has fewer poor ratings than the average Iowa hospital',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patients who gave a rating of "6" or lower (low)',
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_y_continuous(limits = c(0,25)) +
  scale_x_discrete(labels = c(seq(2008, 2016, 1))) +
  theme(plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 5.5, y = 2, label = 'Waverly Health Center', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 5.5, y = 2, label = 'Waverly Health Center', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 5.5, y = 7, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 5.5, y = 7, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold')

# Recommendation ----
m.town.recommend <- medicare.data %>% 
  filter(Hospital == 'WAVERLY HEALTH CENTER') %>% 
  select(Hospital, Recommend_Yes_2016:Recommend_No_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         cat = qdap::beg2char(variable, "_", 2),
         cat = factor(cat, levels = c( 'Recommend_No', 'Recommend_Probably', 'Recommend_Yes' )))

# Marketing Colors
ggplot(m.town.recommend, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c(NA, primary_marketing[4], primary_marketing[5])) +
  #scale_fill_manual(values = rev(condition_charts)) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0 percent', '20', '40', '60', '80', '100 percent')) +
  scale_x_continuous(breaks = c(seq(2008, 2016, 1))) + 
  labs(title = 'In Iowa, Waverly Health Center is one of the hospitals most likely to be\nrecommended by patients',
       subtitle = '"Would you recommend this hospital to your friends and family?"\nPercent of all patient responses who would definitely (Yes), probably, or not (No) recommend the facility',
       caption = 'Source: Center for Medicare & Medicaid Services') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 2016, y = 40, label = 'YES', fill = primary_marketing[5], color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2016, y = 40, label = 'YES', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2016, y = 90, label = 'PROBABLY', fill = primary_marketing[4], color = NA, size = 3.5, fontface = 'bold') +
  annotate('text', x = 2016, y = 90, label = 'PROBABLY', color = 'grey65', size = 3.5, fontface = 'bold')
