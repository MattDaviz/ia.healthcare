install.packages(c('tidyr', 'dplyr', 'ggplot2', 'ggthemes', 'stringr', 'readr'))

# Load Libraries
library(tidyr); library(dplyr); library(ggplot2); library(ggthemes); library(stringr); library(readr)

# All Statistics -------------
# Load Data
setwd("U:\\Projects\\Van Buren County Hospital\\Master Plan")
data <- read_csv('all statistics.csv')
#stats <- data %>% 
#  select(variable, july.2012.actual, august.2012.actual, sept..2012.actual, october.2012.actual, november.2012.actual, december.2012.actual,
#         january.2013.actual, february.2013.actual, march.2013.actual, april.2013.actual, may.2013.actual, june.2013.actual, july.2013.actual,
#         august.2013.actual, sept..2013.actual, october.2013.actual, november.2013.actual, december.2013.actual, january.2014.actual,
#         february.2014.actual, march.2014.actual, april.2014.actual, may.2014.actual, june.2014.actual, july.2014.actual, august.2014.actual, 
#         september.2014.actual.1, october.2014.actual, november.2014.actual.1, december.2014.actual, january.2015.actual, february.2015.actual,
#         march.2015.actual, april.2015.actual, may.2015.actual, june.2015.actual, july.2015.actual, august.2015.actual, september.2015.actual,
#         october.2015.actual, november.2015.actual, december.2015.actual, january.2016.actual, february.2016.actual, march.2016.actual, 
#         april.2016.actual, may.2016.actual, june.2016.actual, july.2016.actual, augusut.2016.actual, september.2016.actual, 
#         october.2016.actual, november.2016.actual, december.2016.actual)

colnames(data)[1] <- 'statistic'

#stats.long <- stats %>% 
#  gather(variable, value, -statistic, july.2012.actual:december.2016.actual)

# Adult/Pediatrict All Admissions
#stats.long %>% 
#  filter(statistic == 'adult.pediatrics.all.admissions') %>% 
#  mutate(value = as.numeric(value)) %>% 
#  mutate(variable = factor(variable, levels = variable)) %>% 
#  ggplot() +
#  geom_line(aes(x = variable, y = value, group = statistic)) +
#  geom_smooth(aes(x = variable, y = value, group = statistic),method = 'lm', se =FALSE) +
#  theme_fivethirtyeight()

stats.long <- data %>% 
  gather(variable, value, -statistic, july.2012.actual:variance.actualytd17.toactualytd16)

stats.long.df <- left_join(stats.long, date, by = c('variable' = 'label'))
stats.long.df$date <- as.Date(stats.long.df$date, format = '%m/%d/%Y')
stats.long.df$month <- factor(stats.long.df$month, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August',
                                                              'September', 'October', 'November', 'December'))

# Acute graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('acute.admissions', 'acute.average.daily.census', 'acute.average.l.o.s.', 'acute.discharge.days', 
                          'acute.discharges.death', 'acute.patient.days.of.care', 'acute.percent.of.total')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  filter(statistic == 'acute.admissions') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = date, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = date, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
                   #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital acute statistics',
       subtitle = 'Acute statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')
plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital acute statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Adult/Pediatrics graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('adult.pediatrics.all.admissions', 'adult.pediatricts.discharges.deaths', 
                          'adult.pediatrics.patient.days.of.care', 'adult.pediatrics.average.daily.census', 
                          'adult.pediatrics.average.daily.census', 'adult.pediatrics.percent.of.occupancy',
                          'adult.pediatrics.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital adult and pediatric statistics',
       subtitle = 'Adult and pediatric statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')
plot
setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital adult and pediatric statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Medicare acute graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('medicare.acute.admissions', 'medicare.acute.discharges.deaths', 'medicare.acute.percent.of.total',
                          'medicare.acute.patient.days.of.care', 'medicare.acute.length.of.stay.hours', 'medicare.acute.percent.of.total',
                          'medicare.acute.discharge.days', 'medicare.acute.average.daily.census', 'medicare.acute.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital acute medicare statistics',
       subtitle = 'Acute medicare statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')
plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital acute medicare statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Non-medicare acute graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('non.medicare.acute.admissions', 'non.medicare.acute.discharges.deaths', 'non.medicare.acute.percent.of.total',
                          'non.medicare.acute.patient.days.of.care', 'non.medicare.acute.length.of.stay.hours', 'non.medicare.acute.percent.of.total',
                          'non.medicare.acute.discharge.days', 'non.medicare.acute.average.daily.census', 'non.medicare.acute.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital acute non-medicare statistics',
       subtitle = 'Acute non-medicare statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')
plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital acute non-medicare statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Skilled Nursing graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('skilled.nursing.admissions', 'skilled.nursing.discharges.deaths', 'skilled.nurisng.percent.of.total',
                          'skilled.nursing.patient.days.of.care', 'skilled.nursing.percent.of.total', 'skilled.nursing.discharge.days',
                          'skilled.nursing.average.daily.census', 'skilled.nursing.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital skilled nursing statistics',
       subtitle = 'Skilled nursing statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')

plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital skilled nursing statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Private Care graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('private.care.unit.admissions', 'private.care.unit.discharges.deaths', 'skilled.nurisng.percent.of.total',
                          'private.care.unit.patient.days.of.care', 'private.care.unit.percent.of.total', 'private.care.unit.discharge.days',
                          'private.care.unit.average.daily.census', 'private.care.unit.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 'november.2015.actual',
                                                'december.2015.actual', 'january.2016.actual', 'february.2016.actual', 'march.2016.actual',
                                                'april.2016.actual', 'may.2016.actual', 'june.2016.actual', 'july.2016.actual', 
                                                'augusut.2016.actual', 'september.2016.actual', 'october.2016.actual', 
                                                'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital private care statistics',
       subtitle = 'Private care statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')

plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital private care statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Newborns graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('newborns.admissions.births', 'newborns.admissions.births.discharges.deaths', 
                          'newborns.admissions.births.newborn.days.of.care', 'newborns.admissions.births.discharge.days',
                          'newborns.admissions.births.percent.of.occupancy', 'newborn.admissions.births.average.l.o.s.')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 
                                                'november.2015.actual', 'december.2015.actual', 'january.2016.actual', 
                                                'february.2016.actual', 'march.2016.actual', 'april.2016.actual', 'may.2016.actual',
                                                'june.2016.actual', 'july.2016.actual', 'augusut.2016.actual', 'september.2016.actual',
                                                'october.2016.actual', 'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital newborns statistics',
       subtitle = 'Newborns statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')

plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital newborns statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Remaining data graphs
plot <- stats.long.df %>% 
  filter(statistic %in% c('outpatient.visits', 'radiology', 'm.r.i.', 'ct.scan', 'other.imaging', 'lab.exams', 'bl.bank.procedures',
                          'physical.therapy.trtm', 'cardio.pulmonary', 'cardio.rehab', 'ambulance.runs', 'o.r.procedures-total',
                          'inpatient.o.r.units', 'same.day.surg.units', 'er.room.visits', 'observation.rm.days')) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 
                                                'november.2015.actual', 'december.2015.actual', 'january.2016.actual', 
                                                'february.2016.actual', 'march.2016.actual', 'april.2016.actual', 'may.2016.actual',
                                                'june.2016.actual', 'july.2016.actual', 'augusut.2016.actual', 'september.2016.actual',
                                                'october.2016.actual', 'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic, color = statistic), method = 'loess', se = FALSE, show.legend = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 3, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital other statistics',
       subtitle = 'Other statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')

plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital other statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()

# Physician visits graphs
plot <- stats.long.df %>% 
  filter(str_detect(statistic, c('keosauqua', 'bonaparte', 'farmington', 'douds', 'birmingham', 'fox.river'))) %>% 
  filter(str_detect(variable, '.actual'),
         !str_detect(variable, 'fye'),
         !str_detect(variable, 'actualytd'),
         !str_detect(variable, 'actual.1')) %>% 
  filter(variable != 'september.2013.actual') %>% 
  mutate(value = as.numeric(value),
         variable = factor(variable, levels = c('july.2012.actual', 'august.2012.actual', 'sept..2012.actual', 'october.2012.actual',
                                                'november.2012.actual', 'december.2012.actual', 'january.2013.actual', 
                                                'february.2013.actual', 'march.2013.actual', 'april.2013.actual', 'may.2013.actual',
                                                'june.2013.actual', 'july.2013.actual', 'august.2013.actual', 'sept..2013.actual', 
                                                'october.2013.actual', 'november.2013.actual', 'december.2013.actual',
                                                'january.2014.actual', 'february.2014.actual', 'march.2014.actual', 'april.2014.actual',
                                                'may.2014.actual', 'june.2014.actual', 'july.2014.actual', 'august.2014.actual', 
                                                'september.2014.actual', 'october.2014.actual', 'november.2014.actual', 
                                                'december.2014.actual', 'january.2015.actual', 'february.2015.actual', 'march.2015.actual',
                                                'april.2015.actual', 'may.2015.actual', 'june.2015.actual', 'july.2015.actual', 
                                                'august.2015.actual', 'september.2015.actual', 'october.2015.actual', 
                                                'november.2015.actual', 'december.2015.actual', 'january.2016.actual', 
                                                'february.2016.actual', 'march.2016.actual', 'april.2016.actual', 'may.2016.actual',
                                                'june.2016.actual', 'july.2016.actual', 'augusut.2016.actual', 'september.2016.actual',
                                                'october.2016.actual', 'november.2016.actual', 'december.2016.actual'))) %>% 
  ggplot() +
  #geom_line(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_point(aes(x = month, y = value, group = statistic), alpha = 0.25) +
  geom_smooth(aes(x = month, y = value,  group = statistic), method = 'loess', se = FALSE) +
  #scale_x_discrete(labels = c('July\n2012', 'October\n2014', 'December\n2016')) +#,
  #breaks = c('july.2012.actual','october.2014.actual', 'december.2016.actual')) +
  theme_fivethirtyeight() +
  theme(plot.caption = element_text(color = '#A1B69A', face = 'bold'))+
  facet_wrap(~statistic,  ncol = 1, scales = 'free_y') +
  labs(title = 'Van Buren County Hospital physician clinic visits statistics',
       subtitle = 'Physician clinic visits statistics for Van Buren County Hospital from July 2012 through December 2016',
       caption = 'INVISION Architecture')

plot

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital physician clinic visits statistics.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()
  

# All Market Shares -----------
# Load Data
setwd("U:\\Projects\\Van Buren County Hospital\\Master Plan")
data <- read.csv('all market shares.csv')
colnames(data)[1] <- 'statistic'

data.long <- data %>% 
  gather(variable, value, -statistic, -health.center)

palette <- c('grey75', 'grey75', 'grey75', 'grey75',  'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 
             'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 
             'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'grey75', 'green3')

plot <- data.long %>% 
  filter(variable != 'cy.2016.first.quarter') %>% 
  mutate(variable = factor(variable, levels = variable),
         health.center = factor(health.center, levels = c('Cedar Rapids - Mercy Medical Center',
                                                          'Other',
                                                          "Cedar Rapids - UnityPoint Health - St. Luke's Hospital", 
                                                          'Des Moines - Mercy Medical Center-Des Moines', 
                                                          'Des Moines - UnityPoint Health - Iowa Lutheran Hospital',
                                                          'Fairfield - Jefferson County Health Center',
                                                          'Grinnell - Grinnell Regional Medical Center', 
                                                          'Iowa City - Mercy Iowa City',
                                                          'Iowa City - University of Iowa Hospitals and Clinics',
                                                          'Mount Pleasant - Henry County Health Center', 
                                                          'Oskaloosa - Mahaska Health Partnership',
                                                          'Ottumwa - Ottumwa Regional Health Center',
                                                          'Pella - Pella Regional Health Center',
                                                          'Washington - Washington County Hospital & Clinics',
                                                          'Waterloo - Covenant Medical Center',
                                                          'West Burlington - Great River Medical Center',
                                                          'Bettendorf - UnityPoint Health - Quad Cities',
                                                          'Bloomfield - Davis County Hospital',
                                                          'Davenport - Genesis Medical Center',
                                                          'Des Moines - UnityPoint Health - Iowa Methodist Medical Center',
                                                          'Fort Madison - Fort Madison Community Hospital',
                                                          'Ames - Mary Greeley Medical Center',
                                                          'Keokuk - Keokuk Area Hospital',
                                                          'Red Oak - Montgomery County Memorial Hospital',
                                                          'West Des Moines - Mercy Medical Center-West Lakes',
                                                          'West Des Moines - UnityPoint Health - Methodist West Hospital',
                                                          'Centerville - Mercy Medical Center-Centerville',
                                                          'Guthrie Center - Guthrie County Hospital',
                                                          'Mason City - Mercy Medical Center-North Iowa',
                                                          'Clinton - Mercy Medical Center-Clinton',
                                                          'Council Bluffs - CHI Health Mercy Council Bluffs',
                                                          'Sioux City - Mercy Medical Center-Sioux City',
                                                          'Fort Madison Community Hospital',
                                                          'Iowa City - University Of Iowa Hospitals and Clinics',
                                                          'Keosauqua - Van Buren County Hospital'))) %>% 
  ggplot() +
  geom_col(aes(x = variable, y = value, fill = health.center), position = 'stack', color = 'grey70') +
  facet_wrap(~statistic) +
  scale_fill_manual(values = palette) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c('2011', '2012', '2013', '2014', '2015')) +
  theme_fivethirtyeight() +
  theme(legend.position = 'none',
        axis.title = element_text(face = 'bold'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold')) +
  ylab('Market Share') +
  xlab('') +
  labs(title = 'Van Buren County Hospital market shares among local counties',
       subtitle = 'Market share of all patient origins from 2011-2015 for nearby counties to Van Buren County Hospital',
       caption = 'INVISION Architecture')

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital market shares among local counties.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 3150, res = 300, quality = 400)

print(plot)
dev.off()


# ChoroplethR ----------

library(choroplethr)

unique(all_market_shares$variable)

library(choroplethrZip)
library(dplyr)

vbch.market.share <- all_market_shares %>% 
  filter(health.center == 'Keosauqua - Van Buren County Hospital',
         city != 'Van Buren') %>% 
  mutate(region = c('52535', '52542', '52551', '52565', '52570', '52573', '52588', '52620', '52626', '52630', '52651'))

county.zoom <- c(19111, 19177, 19051, 19179, 19101, 19087, 29199, 29197, 29045)

twenty.eleven <- vbch.market.share %>% 
  select(cy.2011, region) %>% 
  mutate(zoom = region) %>% 
  rename(value = cy.2011)

twenty.twelve <- vbch.market.share %>% 
  select(cy.2012, region) %>% 
  mutate(zoom = region) %>% 
  rename(value = cy.2012)

twenty.thirteen <- vbch.market.share %>% 
  select(cy.2013, region) %>% 
  mutate(zoom = region) %>% 
  rename(value = cy.2013)

twenty.fourteen <- vbch.market.share %>% 
  select(cy.2014, region) %>% 
  mutate(zoom = region) %>% 
  rename(value = cy.2014)

twenty.fifteen <- vbch.market.share %>% 
  select(cy.2015, region) %>% 
  mutate(zoom = region) %>% 
  rename(value = cy.2015)

library(choroplethr)
library(choroplethrZip)
library(R6)
library(RgoogleMaps)
library(ggmap)

ZipChoroplethSatellite = R6Class("ZipChoroplethSatellite", inherit = ZipChoropleth,
                                 public = list(
                                   
                                   get_reference_map = function()
                                   {
                                     # note: center is (long, lat) but MaxZoom is (lat, long)
                                     
                                     center = c(mean(self$choropleth.df$long),
                                                mean(self$choropleth.df$lat))
                                     
                                     max_zoom = MaxZoom(range(self$choropleth.df$lat),
                                                        range(self$choropleth.df$long))
                                     
                                     get_map(location = center,
                                             zoom    = max_zoom,
                                             maptype = "terrain",
                                             color   = "bw")
                                   }
                                 )
)

c = ZipChoroplethSatellite$new(twenty.eleven)
c$set_zoom_zip(state_zoom=NULL, county_zoom = county.zoom, zip_zoom=NULL, msa_zoom=NULL)
c$set_num_colors(1)
c$title  = "Van Buren County Hospital - 2011 Market Share"
c$legend = "Market Share"
c$render_with_reference_map()
str(c)

plot.11 <- zip_choropleth(twenty.eleven, county_zoom = county.zoom, num_colors = 1, reference_map = TRUE) +
  labs(title = 'Van Buren County Hospital - 2011 Market Share',
       caption = 'INVISION Architecture') +
  scale_fill_gradient2('Market Share  ', low = 'white', high = 'darkgreen', na.value = 'grey75', 
                      labels = scales::percent, limits = c(0,1)) +
  scale_color_discrete('black') +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key = element_rect(color = 'black', linetype = 'dashed'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold'))
plot.11

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital - 2011 market share.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2175, res = 300, quality = 400)

print(plot.11)
dev.off()

plot.12 <- zip_choropleth(twenty.twelve, county_zoom = county.zoom, num_colors = 1, reference_map = TRUE) +
  labs(title = 'Van Buren County Hospital - 2012 Market Share',
       caption = 'INVISION Architecture') +
  scale_fill_gradient2('Market Share  ', low = 'white', high = 'darkgreen', na.value = 'grey75', 
                       labels = scales::percent, limits = c(0,1)) +
  scale_color_discrete('black') +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key = element_rect(color = 'black', linetype = 'dashed'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold'))
plot.12

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital - 2012 market share.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2175, res = 300, quality = 400)

print(plot.12)
dev.off()

plot.13 <- zip_choropleth(twenty.thirteen, county_zoom = county.zoom, num_colors = 1, reference_map = TRUE) +
  labs(title = 'Van Buren County Hospital - 2013 Market Share',
       caption = 'INVISION Architecture') +
  scale_fill_gradient2('Market Share  ', low = 'white', high = 'darkgreen', na.value = 'grey75', 
                       labels = scales::percent, limits = c(0,1)) +
  scale_color_discrete('black') +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key = element_rect(color = 'black', linetype = 'dashed'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold'))

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital - 2013 market share.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2175, res = 300, quality = 400)

print(plot.13)
dev.off()

plot.14 <- zip_choropleth(twenty.fourteen, county_zoom = county.zoom, num_colors = 1, reference_map = TRUE) +
  labs(title = 'Van Buren County Hospital - 2014 Market Share',
       caption = 'INVISION Architecture') +
  scale_fill_gradient2('Market Share  ', low = 'white', high = 'darkgreen', na.value = 'grey75', 
                       labels = scales::percent, limits = c(0,1)) +
  scale_color_discrete('black') +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key = element_rect(color = 'black', linetype = 'dashed'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold'))

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital - 2014 market share.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2175, res = 300, quality = 400)

print(plot.14)
dev.off()

plot.15 <- zip_choropleth(twenty.fifteen, county_zoom = county.zoom, num_colors = 1, reference_map = TRUE) +
  labs(title = 'Van Buren County Hospital - 2015 Market Share',
       caption = 'INVISION Architecture') +
  scale_fill_gradient2('Market Share  ', low = 'white', high = 'darkgreen', na.value = 'grey75', 
                       labels = scales::percent, limits = c(0,1)) +
  scale_color_discrete('black') +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.key.width = unit(2, 'cm'),
        legend.key = element_rect(color = 'black', linetype = 'dashed'),
        plot.caption = element_text(color = '#A1B69A', face = 'bold'))

setwd('U:\\Projects\\Van Buren County Hospital\\Master Plan\\data viz')

# Create unique output filename
output_filename <- 'Van Buren County Hospital - 2015 market share.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2175, res = 300, quality = 400)

print(plot.15)
dev.off()


library(gridExtra)
library(grid)
grid.arrange(plot.11, plot.14,  plot.12,  plot.15, plot.13, ncol = 2)

#grid.arrange() for multiple plots together - not sure I can facet

data(zip.regions)

iowa.zip <- zip.regions %>% 
  filter(state.name == 'iowa')

library(ggmap)
geocode('Van Buren County Hospital')

test <- vbch.market.share %>% 
  select(city, health.center, cy.2011, cy.2012, cy.2013, cy.2014, cy.2015) %>% 
  gather(variable, value, -city, -health.center)
         
test$region <- vbch.market.share$region[match(vbch.market.share$city, test$city)]
         
zip_choropleth(test, county_zoom = county.zoom, num_colors = 1, title = '2011') +
  #labs(title = 'Van Buren County Hospital - 2011 Market Share',
  #     caption = 'INVISION Architecture',
  #     subtitle = 'Percent of market share by health care facility') +
  scale_fill_gradient2('Market Share', low = 'white', high = 'darkgreen', na.value = 'grey75', guide = 'colourbar', 
                       labels = scales::percent, limits = c(0,1)) +
  annotate('point', pch = '*', x = -91.9579, y = 40.73638, size = 10, color = 'darkorange') +
  theme_fivethirtyeight() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) +
  facet_wrap(~variable)


