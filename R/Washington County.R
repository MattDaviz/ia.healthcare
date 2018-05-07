setwd("U:\\Tasks\\External\\Washington County Hospitals & Clinics")

library(ggplot2)
library(ggthemes)
library(extrafont)
library(plotrix)
library(reshape)
library(dplyr)
library(tidyr)

#Load and fliter data -----
data <- read.csv("iha data.csv")
local.hospitals <- data %>% 
  filter(hospital %in% c('Jefferson County Health Center',
                         'Henry County Health Center',
                         'Washington County Hospital & Clinics',
                         'Keokuk Area Hospital',
                         'Marengo Memorial Hospital',
                         'Keokuk County Health Center'))

peer.hospitals <- data %>% 
  filter(hospital %in% c('Washington County Hospital & Clinics',
                         'Virginia Gay Hospital',
                         'Boone County Hospital',
                         'Waverly Health Center',
                         'Community Memorial Hospital-Sumner',
                         'Buchanan County Health Center',
                         'Buena Vista Regional Medical Center',
                         'St. Anthony Regional Hospital',
                         'Manning Regional Healthcare Center',
                         'Guttenberg Municipal Hospital',
                         'Central Community Hospital',
                         'Regional Medical Center',
                         'Mercy Medical Center-Dyersville',
                         'Lakes Regional Healthcare',
                         'Gundersen Palmer Lutheran Hospital and Clinics',
                         'Mercy Hospital of Franciscan Sisters',
                         'Hansen Family Hospital',
                         'Henry County Health Center',
                         'Jefferson County Health Center',
                         'UnityPoint Health-Jones Regional Medical Center',
                         'Mahaska Health Partnership',
                         'Floyd Valley Hospital',
                         'Grinnell Regional Medical Center',
                         'Washington County Hospital & Clinics',
                         'Winneshiek Medical Center'))
  

#Total Facility Admissions----------
# Total Facility Admissions - Local Hospitals
totFacAdmLocal <- local.hospitals %>% 
  select(hospital, total_facility_admissions_2011, total_facility_admissions_2012, total_facility_admissions_2013, total_facility_admissions_2014, total_facility_admissions_2015)
totFacAdmLocalOther.tidy <- gather(totFacAdmLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totFacAdmLocalWash.tidy <- gather(totFacAdmLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totFacAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totFacAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totFacAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totFacAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totFacAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 5, y = 1250, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 5, y = 1250, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL FACILITY ADMISSIONS ARE DECREASING, BUT REMAIN HIGHEST\nLOCALLY',
       subtitle = 'Total facility admissions from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total facility admissions are decreasing, but remain highest locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total Facility Admissions - Peer Hospitals
totFacAdmPeer <- peer.hospitals %>% 
  select(hospital, total_facility_admissions_2011, total_facility_admissions_2012, total_facility_admissions_2013, total_facility_admissions_2014, total_facility_admissions_2015)
totFacAdmPeerOther.tidy <- gather(totFacAdmPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totFacAdmPeerWash.tidy <- gather(totFacAdmPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totFacAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totFacAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totFacAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totFacAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totFacAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 1350, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 1350, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL FACILITY ADMISSIONS ARE DECREASING',
       subtitle = 'Total facility admissions from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total facility admissions are decreasing compared to peers.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Total Swing Bed Admissions---------
# Total Swing Bed Admissions - Local Hospitals
totSBAdmLocal <- local.hospitals %>% 
  select(hospital, swing_bed_admissions_2011, swing_bed_admissions_2012, swing_bed_admissions_2013, swing_bed_admissions_2014, swing_bed_admissions_2015)
totSBAdmLocalOther.tidy <- gather(totSBAdmLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totSBAdmLocalWash.tidy <- gather(totSBAdmLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totSBAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totSBAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totSBAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totSBAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totSBAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 5, y = 205, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 5, y = 205, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL SWING BED ADMISSIONS ARE DECREASING, BUT REMAIN HIGHEST\nLOCALLY',
       subtitle = 'Total swing bed admissions from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total swing bed admissions are decreasing, but remain highest locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total Facility Admissions - Peer Hospitals
totSBAdmPeer <- peer.hospitals %>% 
  select(hospital, swing_bed_admissions_2011, swing_bed_admissions_2012, swing_bed_admissions_2013, swing_bed_admissions_2014, swing_bed_admissions_2015)
totSBAdmPeerOther.tidy <- gather(totSBAdmPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totSBAdmPeerWash.tidy <- gather(totSBAdmPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totSBAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totSBAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totSBAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totSBAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totSBAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 220, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 220, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL SWING BED ADMISSIONS ARE DECREASING',
       subtitle = 'Total swing bed admissions from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total swing bed admissions are decreasing.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Total Acute Admissions---------
# Total Acute Admissions - Local Hospitals
totAcuAdmLocal <- local.hospitals %>% 
  select(hospital, acute_admissions_2011, acute_admissions_2012, acute_admissions_2013, acute_admissions_2014, acute_admissions_2015)
totAcuAdmLocalOther.tidy <- gather(totAcuAdmLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totAcuAdmLocalWash.tidy <- gather(totAcuAdmLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totAcuAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totAcuAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totAcuAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totAcuAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totAcuAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 5, y = 1100, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 5, y = 1100, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL ACUTE ADMISSIONS ARE DECREASING, BUT REMAIN HIGH LOCALLY',
       subtitle = 'Total acute admissions from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total acute admissions are decreasing, but remain high locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total Acute Admissions - Peer Hospitals
totAcuAdmPeer <- peer.hospitals %>% 
  select(hospital, acute_admissions_2011, acute_admissions_2012, acute_admissions_2013, acute_admissions_2014, acute_admissions_2015)
totAcuAdmPeerOther.tidy <- gather(totAcuAdmPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totAcuAdmPeerWash.tidy <- gather(totAcuAdmPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totAcuAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totAcuAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totAcuAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totAcuAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totAcuAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 1200, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 1200, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL ACUTE ADMISSIONS ARE DECREASING',
       subtitle = 'Total acute admissions from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total acute admissions are decreasing.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Average Acute Length of Stay---------
# Total Acute Admissions - Local Hospitals
totAcuDurLocal <- local.hospitals %>% 
  select(hospital, acute_length_stay_2011, acute_length_stay_2012, acute_length_stay_2013, acute_length_stay_2014, acute_length_stay_2015)
totAcuDurLocalOther.tidy <- gather(totAcuDurLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totAcuDurLocalWash.tidy <- gather(totAcuDurLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totAcuDurLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totAcuDurLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totAcuDurLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totAcuDurLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totAcuDurLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = c('4 days', '3', '2', '1 days', '0'), breaks = c(4,3,2,1,0)) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 2.1, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 2.1, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'THE DURATION OF ACUTE VISITS IS LOW AND STABLE',
       subtitle = 'Average number of days for acute stays from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'The duration of acute visits is low and stable.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total Acute Admissions - Peer Hospitals
totAcuDurPeer <- peer.hospitals %>% 
  select(hospital, acute_length_stay_2011, acute_length_stay_2012, acute_length_stay_2013, acute_length_stay_2014, acute_length_stay_2015)
totAcuDurPeerOther.tidy <- gather(totAcuDurPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totAcuDurPeerWash.tidy <- gather(totAcuDurPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totAcuDurPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totAcuDurPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totAcuDurPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totAcuDurPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totAcuDurPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = c('6 days', '4', '2 days', '0'), breaks = c(6,4,2,0)) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 1.9, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 1.9, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'THE DURATION OF ACUTE VISITS IS LOW AND STABLE',
       subtitle = 'Average number of days for acute stays from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'The duration of acute visits is low and stable - peer group.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Total Emergency Department Visits---------
# Total ED Visits - Local Hospitals
totEDAdmLocal <- local.hospitals %>% 
  select(hospital, emergency_department_visits_2011, emergency_department_visits_2012, emergency_department_visits_2013, emergency_department_visits_2014, emergency_department_visits_2015)
totEDAdmLocalOther.tidy <- gather(totEDAdmLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totEDAdmLocalWash.tidy <- gather(totEDAdmLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totEDAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totEDAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totEDAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totEDAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totEDAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 5, y = 5000, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 5, y = 5000, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL EMERGENCY DEPARTMENT VISITS ARE STABLE, BUT ARE LOW\nLOCALLY',
       subtitle = 'Total emergency department visits from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total emergency department visits are stable, but are low locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total ED Visits - Peer Hospitals
totEDAdmPeer <- peer.hospitals %>% 
  select(hospital, emergency_department_visits_2011, emergency_department_visits_2012, emergency_department_visits_2013, emergency_department_visits_2014, emergency_department_visits_2015)
totEDAdmPeerOther.tidy <- gather(totEDAdmPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totEDAdmPeerWash.tidy <- gather(totEDAdmPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totEDAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totEDAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totEDAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totEDAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totEDAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 6500, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 6500, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL EMERGENCY DEPARTMENT VISITS ARE STABLE,\nWITH AVERAGE VOLUME WHEN COMPARED TO PEERS',
       subtitle = 'Total emergency department visits from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total emergency department visits are stable, with average volume when compared to peers.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Total Outpatient Visits---------
# Total Outpatient Visits - Local Hospitals
totOutAdmLocal <- local.hospitals %>% 
  select(hospital, total_outpatient_visits_2011, total_outpatient_visits_2012, total_outpatient_visits_2013, total_outpatient_visits_2014, total_outpatient_visits_2015)
totOutAdmLocalOther.tidy <- gather(totOutAdmLocal, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totOutAdmLocalWash.tidy <- gather(totOutAdmLocal, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totOutAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totOutAdmLocalOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totOutAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totOutAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totOutAdmLocalWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 5, y = 40000, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 5, y = 40000, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL OUTPATIENT VISITS ARE STABLE, AND ARE HIGH LOCALLY',
       subtitle = 'Total outpatient visits from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total outpatient visits are stable, and are high locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total ED Visits - Peer Hospitals
totOutAdmPeer <- peer.hospitals %>% 
  select(hospital, total_outpatient_visits_2011, total_outpatient_visits_2012, total_outpatient_visits_2013, total_outpatient_visits_2014, total_outpatient_visits_2015)
totOutAdmPeerOther.tidy <- gather(totOutAdmPeer, key, value, -hospital) %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totOutAdmPeerWash.tidy <- gather(totOutAdmPeer, key, value, -hospital) %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totOutAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), fill = '#6D6E71', color = '#A7A9AC', alpha = 1, size = 5, pch = 21) +
  #geom_line(data = totOutAdmPeerOther.tidy, aes(x = key, y = value, group = hospital), color = '#6D6E71', alpha = 1, size = 2) +
  geom_point(data = totOutAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), fill = '#E84B37', color = '#A7A9AC', size = 5, pch = 21) +
  #geom_line(data = totOutAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), color = '#E84B37', size = 2) +
  geom_smooth(data = totOutAdmPeerWash.tidy, aes(x = key, y = value, group = hospital), method = 'lm', se = FALSE, color = '#E84B37', size = 3) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  annotate('label', x = 4.5, y = 59000, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 4.5, y = 59000, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL OUTPATIENT VISITS ARE STABLE, BUT LOW COMPARED TO PEERS',
       subtitle = 'Total outpatient visits from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0))

# Create unique output filename
output_filename <- 'Total outpatient visits are stable, but low compared to peers.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

#Total Number of Surgeries---------
# Total Outpatient Surgeries - Local Hospitals
totOutSurLocal <- local.hospitals %>% 
  select(hospital, total_outpatient_surgeries_2011, total_outpatient_surgeries_2012, total_outpatient_surgeries_2013, total_outpatient_surgeries_2014, total_outpatient_surgeries_2015, total_inpatient_surgeries_2011, total_inpatient_surgeries_2012, total_inpatient_surgeries_2013, total_inpatient_surgeries_2014, total_inpatient_surgeries_2015)
totOutSurLocalOther <- totOutSurLocal %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
totOutSurLocalWash <- totOutSurLocal %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = totOutSurLocalOther, aes(x = total_inpatient_surgeries_2011, y = total_outpatient_surgeries_2011), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = totOutSurLocalOther, aes(x = total_inpatient_surgeries_2012, y = total_outpatient_surgeries_2012), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = totOutSurLocalOther, aes(x = total_inpatient_surgeries_2013, y = total_outpatient_surgeries_2013), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = totOutSurLocalOther, aes(x = total_inpatient_surgeries_2014, y = total_outpatient_surgeries_2014), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = totOutSurLocalOther, aes(x = total_inpatient_surgeries_2015, y = total_outpatient_surgeries_2015), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = totOutSurLocalWash, aes(x = total_inpatient_surgeries_2011, y = total_outpatient_surgeries_2011), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = totOutSurLocalWash, aes(x = total_inpatient_surgeries_2012, y = total_outpatient_surgeries_2012), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = totOutSurLocalWash, aes(x = total_inpatient_surgeries_2013, y = total_outpatient_surgeries_2013), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = totOutSurLocalWash, aes(x = total_inpatient_surgeries_2014, y = total_outpatient_surgeries_2014), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = totOutSurLocalWash, aes(x = total_inpatient_surgeries_2015, y = total_outpatient_surgeries_2015), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  annotate('text', x = 45, y = 400, label = "2014", fontface = 'bold') +
  annotate('text', x = 45, y = 700, label = "2011", fontface = 'bold') +
  annotate('text', x = 65, y = 725, label = "2012", fontface = 'bold') +
  annotate('text', x = 60, y = 580, label = "2013", fontface = 'bold') +
  annotate('text', x = 56, y = 530, label = "2015", fontface = 'bold') +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  xlab('Total Inpatient Surgeries') +
  ylab('Totat Outpatient Surgeries') +
  annotate('label', x = 56, y = 850, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 56, y = 850, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL INPATIENT AND OUTPATIENT SURGERIES REMAIN LOW LOCALLY',
       subtitle = 'Total outpatient and inpatient surgeries from 2011-2015, including local hospitals for comparison',
       caption = 'Local hospitals included: Jefferson County, Henry County, Marengo Memorial,\nKeokuk Area Hospital, Keokuk County Health Center                                                                                      Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0),
        axis.title = element_text())

# Create unique output filename
output_filename <- 'Total inpatient and outpatient surgeries remain low locally.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Total Outpatient Surgeries - Local Hospitals
TotOutSurPeer <- peer.hospitals %>% 
  select(hospital, total_outpatient_surgeries_2011, total_outpatient_surgeries_2012, total_outpatient_surgeries_2013, total_outpatient_surgeries_2014, total_outpatient_surgeries_2015, total_inpatient_surgeries_2011, total_inpatient_surgeries_2012, total_inpatient_surgeries_2013, total_inpatient_surgeries_2014, total_inpatient_surgeries_2015)
TotOutSurPeerOther <- TotOutSurPeer %>% 
  filter(hospital != 'Washington County Hospital & Clinics')
TotOutSurPeerWash <- TotOutSurPeer %>% 
  filter(hospital == 'Washington County Hospital & Clinics')

plot <- ggplot() +
  geom_point(data = TotOutSurPeerOther, aes(x = total_inpatient_surgeries_2011, y = total_outpatient_surgeries_2011), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = TotOutSurPeerOther, aes(x = total_inpatient_surgeries_2012, y = total_outpatient_surgeries_2012), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = TotOutSurPeerOther, aes(x = total_inpatient_surgeries_2013, y = total_outpatient_surgeries_2013), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = TotOutSurPeerOther, aes(x = total_inpatient_surgeries_2014, y = total_outpatient_surgeries_2014), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = TotOutSurPeerOther, aes(x = total_inpatient_surgeries_2015, y = total_outpatient_surgeries_2015), size = 6, color = '#A7A9AC', pch = 21, fill = '#6D6E71') +
  geom_point(data = TotOutSurPeerWash, aes(x = total_inpatient_surgeries_2011, y = total_outpatient_surgeries_2011), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = TotOutSurPeerWash, aes(x = total_inpatient_surgeries_2012, y = total_outpatient_surgeries_2012), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = TotOutSurPeerWash, aes(x = total_inpatient_surgeries_2013, y = total_outpatient_surgeries_2013), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = TotOutSurPeerWash, aes(x = total_inpatient_surgeries_2014, y = total_outpatient_surgeries_2014), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  geom_point(data = TotOutSurPeerWash, aes(x = total_inpatient_surgeries_2015, y = total_outpatient_surgeries_2015), size = 6, fill = '#E84B37', pch = 21, color = '#A7A9AC') +
  annotate('text', x = 30, y = 325, label = "2014", fontface = 'bold') +
  annotate('text', x = 30, y = 725, label = "2011", fontface = 'bold') +
  annotate('text', x = 85, y = 775, label = "2012", fontface = 'bold') +
  annotate('text', x = 85, y = 580, label = "2013", fontface = 'bold') +
  annotate('text', x = 75, y = 450, label = "2015", fontface = 'bold') +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  xlab('Total Inpatient Surgeries') +
  ylab('Totat Outpatient Surgeries') +
  annotate('label', x = 125, y = 0, fill = '#F0F0F0', color = NA, label = NA) +
  annotate('text', x = 125, y = 0, color = '#E84B37', label = 'Washington County\nHospitals & Clinics', fontface = 'bold') +
  labs(title = 'TOTAL INPATIENT AND OUTPATIENT SURGERIES REMAIN LOW COMPARED\nTO PEERS',
       subtitle = 'Total outpatient and inpatient surgeries from 2011-2015, including peer hospitals for comparison',
       caption = 'Peer hospitals included: Virginia Gay, Boone County, Waverly Health Center\nCommunity Memorial Hospital-Sumner, Buchanan County, Buena Vista Regional\nSt. Anthony Regional, Manning Regional, Guttenberg Municipal, Central Community\nRegional Medical Center, Mercy-Dyersville, Lakes Regional Healthcare,\nGundersen Palmer Lutheran, Mercy Hospital of Franciscan Sisters, Hansen Family Hospital,\nHenry County, Jefferson County, UnityPoint-Jones Regional, Mahaska Health Partnership,\nFloyd Valley, Grinnell Regional, Washington County, Winneshiek Medical Center                                     Source: Iowa Hospital Association') +
  theme(plot.caption = element_text(hjust = 0, vjust = 0),
        axis.title = element_text())

# Create unique output filename
output_filename <- 'Total inpatient and outpatient surgeries remain low compared to peers.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()
