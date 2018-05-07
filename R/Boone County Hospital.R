# Load Libraries ----
library(tidyverse)
library(ggthemes)

# List of Surrounding Facilities ----
surrounding.list.IHA <- c('Bremer', 'Floyd', 'Chickasaw', 'Fayette', 'Buchanan', 'Black Hawk', 'Grundy')
surrounding.list.medicare <- c('BUCHANAN COUNTY HEALTH CENTER', 'COMMUNITY MEMORIAL HOSPITAL MEDICAL CENTER', 'COVENANT MEDICAL CENTER', 'FLOYD COUNTY MEDICAL CENTER', 'GRUNDY COUNTY MEMORIAL HOSPITAL', 'GUNDERSEN PALMER LUTHERAN HOSPITAL AND CLINICS', 'MERCY HOSPITAL OF FRANCISCAN SISTERS-OELWEIN', 'MERCY MEDICAL CENTER-NEW HAMPTON', 'SARTORI MEMORIAL HOSPITAL', 'ALLEN HOSPITAL', 'WAVERLY HEALTH CENTER')

# List of Peer Facilities ----
peer.list.IHA <- c('Boone', 'Benton', 'Plymouth', 'Bremer', 'Washington', 'Mahaska', 'Buchanan', 'Winneshiek', 'Jones', 'Carroll', 'Buena Vista')
peer.list.medicare <- c('BOONE COUNTY HOSPITAL', 'BUCHANAN COUNTY HEALTH CENTER', 'BUENA VISTA REGIONAL MEDICAL CENTER', 'COMMUNITY MEMORIAL HOSPITAL MEDICAL CENTER', 'FLOYD VALLEY HOSPITAL', 'MAHASKA  HEALTH PARTNERSHIP', 'MANNING REGIONAL HEALTHCARE CENTER', 'ST ANTHONY REGIONAL HOSPITAL & NURSING HOME', 'JONES REGIONAL MEDICAL CENTER', 'VIRGINIA GAY HOSPITAL', 'WASHINGTON COUNTY HOSPITAL AND CLINICS', 'WAVERLY HEALTH CENTER', 'WINNESHIEK MEDICAL CENTER')

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
            filter(Hospital == 'BOONE COUNTY HOSPITAL') %>% 
            select(Hospital:Door_To_Diag_Eval_2013) %>% 
            gather(variable, value, -Hospital), aes(x = variable, y = value), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'At Boone County Hospital, the door-to-diagnostic evaluation times\nin the emergency department are similar to the Iowa average',
       subtitle = 'Average (median) time patients spent in the emergency department before they were seen by a healthcare professional', 
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_x_discrete(labels = c(seq(2013,2017,1))) +
  scale_y_continuous(labels = c('0 minutes', '10', '20', '30', '40', '50 minutes')) +
  annotate('label', x = 3.5, y = 17, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3.5, y = 17, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 3.5, y = 24, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3.5, y = 24, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold') +
  theme(plot.caption = element_text(vjust = -2))


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
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Emergency department visits at Boone County Hospital are among\nthe highest compared to peer hospitals',
       subtitle = 'Total emergency department visits, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 12000), labels = c('0 visits', '2,500', '5,000', '7,500', '10,000', '12,500 visits')) +
  annotate('label', x = 1.5, y = 10000, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 1.5, y = 10000, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
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
  select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2017) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_line(alpha = 0.05, size = 2) +
  geom_line(data = medicare.data %>% 
              select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2017) %>% 
              gather(variable, value, -Hospital) %>% 
              group_by(variable) %>% 
              summarize(value = mean(value, na.rm = T)) %>% 
              mutate(Hospital = 1), color = primary_marketing[4], size = 2) +  
  geom_line(data = medicare.data %>% 
              filter(Hospital == 'BOONE COUNTY HOSPITAL') %>% 
              select(Hospital, Hospital_Rating_High_2008:Hospital_Rating_High_2017) %>% 
              gather(variable, value, -Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Boone County Hospital has fewer "high" ratings than other Iowa hospitals',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patients who gave a rating of "9" or "10" (high)',
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_y_continuous(limits = c(40,100), breaks = c(seq(40, 100, 10)), labels = c('40 percent', '50', '60', '70', '80', '90', '100 percent')) +
  scale_x_discrete(labels = c(seq(2008, 2017, 1))) +
  theme(plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 7, y = 68, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 7, y = 68, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 5, y = 77, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 5, y = 77, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold')

# Hospital Ratings - Low ----
medicare.data %>% 
  select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2017) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_line(alpha = 0.05, size = 2) +
  geom_line(data = medicare.data %>% 
              select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2017) %>% 
              gather(variable, value, -Hospital) %>% 
              group_by(variable) %>% 
              summarize(value = mean(value, na.rm = T)) %>% 
              mutate(Hospital = 1), color = primary_marketing[4], size = 2) +  
  geom_line(data = medicare.data %>% 
              filter(Hospital == 'BOONE COUNTY HOSPITAL') %>% 
              select(Hospital, Hospital_Rating_Low_2008:Hospital_Rating_Low_2017) %>% 
              gather(variable, value, -Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Boone County Hospital has more "poor" ratings than other Iowa hospitals',
       subtitle = '"Using any number from 0 to 10, where 0 is the worst hospital possible and 10 is the best hospital possible, what number would you use to\nrate this hospital during your stay?"\nPercent of all patients who gave a rating of "6" or lower (low)',
       caption = 'Source: Centers for Medicare & Medicaid Services') +
  scale_y_continuous(limits = c(0,25), labels = c('0 percent', '5', '10', '15', '20', '25 percent')) +
  scale_x_discrete(labels = c(seq(2008, 2017, 1))) +
  theme(plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 9, y = 7.5, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 9, y = 7.5, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  annotate('label', x = 5.5, y = 7, label = 'Iowa Hospital Average', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 5.5, y = 7, label = 'Iowa Hospital Average', color = primary_marketing[4], fontface = 'bold')

# Recommendation ----
m.town.recommend <- medicare.data %>% 
  filter(Hospital == 'BOONE COUNTY HOSPITAL') %>% 
  select(Hospital, Recommend_Yes_2017:Recommend_No_2008) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),
         cat = sub( "(^[^_]+[_][^_]+)(.+$)", "\\1", variable),
         cat = factor(cat, levels = c( 'Recommend_No', 'Recommend_Probably', 'Recommend_Yes' )))

# Marketing Colors
ggplot(m.town.recommend, aes(x = year, y = value, fill = cat)) +
  geom_col(position = 'stack') +
  theme_fivethirtyeight() +
  scale_fill_manual(values = c(primary_marketing[3], primary_marketing[4], primary_marketing[5])) +
  #scale_fill_manual(values = rev(condition_charts)) +
  scale_y_continuous(breaks = c(seq(0,100,20)), labels = c('0 percent', '20', '40', '60', '80', '100 percent')) +
  scale_x_continuous(breaks = c(seq(2008, 2017, 1))) + 
  labs(title = 'Overall, Boone County Hospital is likely to be recommended by patients',
       subtitle = '"Would you recommend this hospital to your friends and family?"\nPercent of all patient responses who would definitely (Yes), probably, or not (No) recommend the facility',
       caption = 'Source: Center for Medicare & Medicaid Services') +
  theme(legend.position = 'none',
        plot.caption = element_text(vjust = -2)) +
  annotate('label', x = 2017, y = 35, label = 'YES', fill = primary_marketing[5], color = NA, size = 7, fontface = 'bold') +
  annotate('text', x = 2017, y = 35, label = 'YES', color = 'grey80', size = 7, fontface = 'bold') +
  annotate('label', x = 2017, y = 85, label = 'PROBABLY', fill = primary_marketing[4], color = NA, size = 3.5, fontface = 'bold') +
  annotate('text', x = 2017, y = 85, label = 'PROBABLY', color = 'grey65', size = 3.5, fontface = 'bold') +
  annotate('label', x = 2017, y = 98.5, label = 'NO', fill = primary_marketing[3], color = NA, size = 3, fontface = 'bold') +
  annotate('text', x = 2017, y = 98.5, label = 'NO', color = primary_marketing[4], size = 3, fontface = 'bold')

# Outpatient: Volumes ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Total_Outpatient_Visits_2011:Total_Outpatient_Visits_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Total_Outpatient_Visits_2011:Total_Outpatient_Visits_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Total_Outpatient_Visits_2011:Total_Outpatient_Visits_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Outpatient visits at Boone County Hospital are about average\ncompared to peer hospitals',
       subtitle = 'Total outpatient visits, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 250000), labels = c('0 visits', '50,000', '100,000', '150,000', '200,000', '250,000 visits')) +
  annotate('label', x = 1.5, y = 80000, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 1.5, y = 80000, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Total Births: Volumes ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Total_Births_2011:Total_Births_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Overall, births at Boone County Hospital have been trending down',
       subtitle = 'Total births, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 400), labels = c('0 births', '100', '200', '300', '400 births')) +
  annotate('label', x = 3, y = 165, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3, y = 165, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Acute Length of Stay: Average ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Acute_Length_Stay_2011:Acute_Length_Stay_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Acute_Length_Stay_2011:Acute_Length_Stay_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Acute_Length_Stay_2011:Acute_Length_Stay_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'At Boone County Hospital, the average acute length of stay has remained\nconsistent since 2011',
       subtitle = 'Average acute length of stay in days, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 6), labels = c('0 days', '2', '4', '6 days')) +
  annotate('label', x = 3, y = 3.6, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3, y = 3.6, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Outpatient Surgeries: Total ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Total outpatient surgeries at Boone County Hospital remain consistent',
       subtitle = 'Total outpatient surgeries, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 4000), labels = c('0 surgeries', '1,000', '2,000', '3,000', '4,000 surgeries')) +
  annotate('label', x = 3, y = 1700, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 3, y = 1700, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Inpatient Surgeries: Total ----
# Peer Hospitals
iha.data %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  #geom_line(size = 2, alpha = 0.05) +
  geom_line(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_line(data = iha.data %>% 
              select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Total inpatient surgeries at Boone County Hospital are average\ncompared to peer hospitals',
       subtitle = 'Total inpatient surgeries, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 500), labels = c('0 surgeries', '100', '200', '300', '400', '500 surgeries')) +
  annotate('label', x = 2, y = 115, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 2, y = 115, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))

# Surgeries: Total ----
# Peer Hospitals
ip.surg <- iha.data %>% 
  filter(County %in% peer.list.IHA,
         Type_of_Facility == 'CAH') %>% 
  select(Hospital, Total_Inpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(level = 'Inpatient')

op.surg <- iha.data %>% 
  filter(County %in% peer.list.IHA,
         Type_of_Facility == 'CAH') %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Outpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  mutate(level = 'Outpatient')

all.surg <- rbind(ip.surg, op.surg) %>% 
  mutate(year = as.numeric(stringr::str_sub(variable, -4, -1)),) %>% 
  select(Hospital, value, level, year) %>% 
  spread(level, value) %>% 
  filter()

ann_line1 <- data.frame(y0=3100, y2=3500, y=3900, xmid=50, xmin=25, xmax=75, year=2011)
ann_line2 <- data.frame(y0=100, y2=100, y=100, xmid=450, xmin=375, xmax=475, year=2011)
ann_text <- data.frame(x=c(90,425), y=c(3500,500), year = 2011, label=c('MORE\nOUTPATIENT\nSURGERIES', 'MORE\nINPATIENT\nSURGERIES'))
ann_text_boone <- data.frame(x = 145, y = 1000, year = 2011, label = 'Boone\nCounty\nHospital')

ggplot(all.surg, aes(x = Inpatient, y = Outpatient)) +
  geom_point(pch = 21, size = 6, fill = primary_marketing[6], color = primary_marketing[4]) +
  geom_point(data = all.surg %>% 
               filter(Hospital == 'Boone County Hospital'), aes(x = Inpatient, y = Outpatient), 
             fill = primary_marketing[3], color = primary_marketing[4], size = 6, pch = 21) +
  facet_wrap(~year) +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, 4000), breaks = c(seq(0, 4000, 2000)), labels = c('0', '2,000', '4,000')) +
  scale_x_continuous(limits = c(0, 500), breaks = c(seq(0, 500, 250)), labels = c('0', '250', '500')) +
  labs(title = 'Compared to peer hospitals, Boone County Hospital sits in the lowest quadrant\nfor inpatient and outpatient surgeries',
       subtitle = 'Total inpatient and outpatient surgeries, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                                          Source: Iowa Hospital Association') +
  geom_segment(data=ann_line1,aes(xend=xmin,x=xmin,yend=y,y=y0),arrow=arrow(length=unit(0.2,"cm")),show_guide=F,
               color = primary_marketing[1]) +
  geom_segment(data=ann_line2,aes(xend=xmax,x=xmin,yend=y,y=y),arrow=arrow(length=unit(0.2,"cm")),show_guide=F,
               color = primary_marketing[1]) +
  geom_text(data = ann_text, aes(x = x, y = y, label = label), show_guide = F, size = 2.5, color = primary_marketing[1]) +
  geom_text(data = ann_text_boone, aes(x = x, y = y, label = label), show_guide = F, size = 2.5, color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))
  #geom_segment(data=ann_line,aes(x=xmid,xend=xmax,y=y,yend=y),arrow=arrow(length=unit(0.2,"cm")),show_guide=F)+
  #geom_segment(data=ann_line,aes(x=xmid,xend=xmid,y=y0,yend=y2),show_guide=F)

iha.data %>% 
  select(Hospital, Total_Outpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
  gather(variable, value, -Hospital) %>% 
  ggplot(aes(x = variable, y = value, group = Hospital)) +
  geom_point(data = iha.data %>% 
              filter(County %in% peer.list.IHA,
                     Type_of_Facility == 'CAH') %>% 
              select(Hospital, Total_Outpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[6], size = 2) +
  geom_point(data = iha.data %>% 
              select(Hospital, Total_Outpatient_Surgeries_2011:Total_Inpatient_Surgeries_2016) %>% 
              filter(Hospital == 'Boone County Hospital') %>% 
              gather(variable, value, -Hospital), aes(x = variable, y = value, group = Hospital), color = primary_marketing[3], size = 2) +
  theme_fivethirtyeight() +
  labs(title = 'Total inpatient surgeries at Boone County Hospital are average\ncompared to peer hospitals',
       subtitle = 'Total inpatient surgeries, compared to peer hospitals serving similarly sized communities',
       caption = 'Peer Hospitals: Winneshiek Medical Center, Waverly Health Center,\nBuena Vista Regional Medical Center, Washington County Hospital & Clinics,\nVirginia Gay Hospital, UnityPoint Health-Jones Regional Medical Center,\nFloyd Valley Healthcare, Buchanan County Health Center,\nManning Regional Healthcare Center, Community Memorial Hospital,\nMahaska Health Partnership                                                                                                                          Source: Iowa Hospital Association') +
  scale_x_discrete(labels = c(seq(2011, 2016, 1))) +
  scale_y_continuous(limits = c(0, 500), labels = c('0 surgeries', '100', '200', '300', '400', '500 surgeries')) +
  annotate('label', x = 2, y = 115, label = 'Boone County Hospital', fill = '#F0F0F0', color = NA, fontface = 'bold') +
  annotate('text', x = 2, y = 115, label = 'Boone County Hospital', color = primary_marketing[3], fontface = 'bold') +
  theme(plot.caption = element_text(hjust = 0, vjust = -2))