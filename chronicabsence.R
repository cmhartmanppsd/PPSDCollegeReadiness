attendance <- filter(attendance, as.numeric(grade) %in% seq(1,13,1))

attendance$level <- ifelse(as.numeric(attendance$grade) < 6 |
                           as.numeric(attendance$grade) == 13,
                           "Elementary",
                           ifelse(as.numeric(attendance$grade) %in% c(6, 7, 8),
                                  'Middle', 'High'))

rosann_frame <- left_join(attendance, 
                          select(person, sasid, race, sex))

rosann_frame <- left_join(rosann_frame,
                          select(annual_demos, sasid, schoolyear, 
                                 lunch, lep, iep, lunch))

rosann_frame$chrabs <- ifelse(rosann_frame$attendance < .9,
                              1, 0)
rosann_frame$race <- as.character(rosann_frame$race)
rosann_frame$race <- ifelse(rosann_frame$race == 'Pacific Islander' |
                            rosann_frame$race == 'Asian',
                            'Asian/Pacific Islander', rosann_frame$race)
rosann_frame$race <- as.factor(rosann_frame$race)
rosann_frame$level <- factor(rosann_frame$level, ordered = TRUE, 
                             levels = c('Elementary', 'Middle', 'High'))

overall <- ggplot(data = rosann_frame %>%
                         filter(schoolyear %in% c('2009_2010', '2010_2011',
                                                  '2011_2012', '2012_2013')) %>%
                         group_by(level, schoolyear) %>%
                         summarize(chrabs = sum(chrabs, na.rm = TRUE),
                                   n = n()) %>%
                         mutate(pchrabs = chrabs/n), 
                  aes(schoolyear, pchrabs, group=level, color=level)) +
           geom_line() +
           geom_text(mapping = aes(schoolyear, pchrabs, 
                                   label = sprintf('%.1f', 100*pchrabs)),
                     size = 4, vjust=2, position = position_jitter()) +
           scale_y_continuous('Chronic Absence',
                              limits = c(0, 1), breaks = seq(0, 1, .1),
                              labels = percent_format()) +
           scale_x_discrete('School Year') +
           ggtitle('Chronic Absenteeism in Providence Public School District')

sex <- ggplot(data = rosann_frame %>%
                     filter(schoolyear %in% c('2009_2010', '2010_2011',
                                              '2011_2012', '2012_2013')) %>%
                     group_by(level, schoolyear, sex) %>%
                     summarize(chrabs = sum(chrabs, na.rm = TRUE),
                               n = n(),
                               pchrabs = sum(chrabs)/n()),
              aes(schoolyear, pchrabs, group=level, color = level)) +
       geom_line() +
       geom_text(mapping = aes(schoolyear, pchrabs, 
                               label = sprintf('%.1f', 100*pchrabs)),
                 size = 4, vjust=2, position = position_jitter()) +
       scale_y_continuous('Chronic Absence',
                          limits = c(0, 1), breaks = seq(0, 1, .1),
                          labels = percent_format()) +
       scale_x_discrete('School Year') +
       facet_grid(.~sex) +
       ggtitle('Chronic Absenteeism in Providence Public School District by Sex')

# write.csv(rosann_frame %>%
# filter(schoolyear %in% c('2009_2010', '2010_2011',
#                          '2011_2012', '2012_2013')) %>%
# group_by(level, schoolyear, sex) %>%
# summarize(chrabs = sum(chrabs, na.rm = TRUE),
#           n = n(),
#           pchrabs = sum(chrabs)/n()) %>%
# arrange(level, schoolyear), file = '', row.names = FALSE)
# 

race <- ggplot(data = rosann_frame %>%
                      filter(schoolyear %in% c('2009_2010', '2010_2011',
                                              '2011_2012', '2012_2013')) %>%
                      group_by(level, schoolyear, race) %>%
                      summarize(chrabs = sum(chrabs, na.rm = TRUE),
                                n = n(),
                                pchrabs = sum(chrabs)/n()),
               aes(schoolyear, pchrabs, group = level, color = level)) +
        geom_line() +
        geom_text(mapping = aes(schoolyear, pchrabs, 
                          label = sprintf('%.1f', 100*pchrabs)),
                  size = 4, vjust=2, position = position_jitter()) +
        scale_y_continuous('Chronic Absence',
                           limits = c(0, 1), breaks = seq(0, 1, .1),
                           labels = percent_format()) +
        scale_x_discrete('School Year') +
        facet_wrap(~ race, ncol=3) +
        ggtitle('Chronic Absenteeism in Providence Public School District by Race')


# write.csv(rosann_frame %>%
# filter(schoolyear %in% c('2009_2010', '2010_2011',
#                          '2011_2012', '2012_2013')) %>%
# group_by(level, schoolyear, race) %>%
# summarize(chrabs = sum(chrabs, na.rm = TRUE),
#           n = n(),
#           pchrabs = sum(chrabs)/n()) %>%
# arrange(level, schoolyear), file = '', row.names = FALSE)
# 

race_sex <- ggplot(data = rosann_frame %>%
                   filter(schoolyear %in% c('2009_2010', '2010_2011',
                                            '2011_2012', '2012_2013')) %>%
                   group_by(level, schoolyear, sex, race) %>%
                   summarize(chrabs = sum(chrabs, na.rm = TRUE),
                             n = n(),
                             pchrabs = sum(chrabs)/n()),
                aes(schoolyear, pchrabs, group = level, color = level)) +
            geom_line() +
            geom_text(mapping = aes(schoolyear, pchrabs, 
                                    label = sprintf('%.1f', 100*pchrabs)),
                      size = 4, vjust=2, position = position_jitter()) +
            scale_y_continuous('Chronic Absence',
                               limits = c(0, 1), breaks = seq(0, 1, .1),
                               labels = percent_format()) +
            scale_x_discrete('School Year') +
            facet_grid(race~sex) +
            ggtitle('Chronic Absenteeism in Providence Public School District by Race and Sex')


# write.csv(rosann_frame %>%
# filter(schoolyear %in% c('2009_2010', '2010_2011',
#                          '2011_2012', '2012_2013')) %>%
# group_by(level, schoolyear, sex, race) %>%
# summarize(chrabs = sum(chrabs, na.rm = TRUE),
#           n = n(),
#           pchrabs = sum(chrabs)/n()) %>%
# arrange(level, schoolyear, sex), file = '', row.names = FALSE)


# write.csv(rosann_frame %>%
# filter(schoolyear %in% c('2009_2010', '2010_2011',
#                          '2011_2012', '2012_2013')) %>%
# group_by(level, schoolyear, iep) %>%
# summarize(chrabs = sum(chrabs, na.rm = TRUE),
#           n = n(),
#           pchrabs = sum(chrabs)/n()) %>%
# arrange(level, schoolyear), file = '', row.names = FALSE)


lep <- ggplot(data = rosann_frame %>%
                filter(schoolyear %in% c('2009_2010', '2010_2011',
                                         '2011_2012', '2012_2013')) %>%
                group_by(level, schoolyear, lep) %>%
                summarize(chrabs = sum(chrabs, na.rm = TRUE),
                          n = n(),
                          pchrabs = sum(chrabs)/n()),
              aes(schoolyear, pchrabs, group=level, color = level)) +
       geom_line() +
       geom_text(mapping = aes(schoolyear, pchrabs, 
                               label = sprintf('%.1f', 100*pchrabs)),
                 size = 4, vjust=2, position = position_jitter()) +
       scale_y_continuous('Chronic Absence',
                          limits = c(0, 1), breaks = seq(0, 1, .1),
                          labels = percent_format()) +
       scale_x_discrete('School Year') +
       facet_grid(.~lep) +
       ggtitle('Chronic Absenteeism in Providence Public School District by LEP')

iep <- ggplot(data = rosann_frame %>%
                filter(schoolyear %in% c('2009_2010', '2010_2011',
                                         '2011_2012', '2012_2013')) %>%
                group_by(level, schoolyear, iep) %>%
                summarize(chrabs = sum(chrabs, na.rm = TRUE),
                          n = n(),
                          pchrabs = sum(chrabs)/n()),
              aes(schoolyear, pchrabs, group=level, color = level)) +
  geom_line() +
  geom_text(mapping = aes(schoolyear, pchrabs, 
                          label = sprintf('%.1f', 100*pchrabs)),
            size = 4, vjust=2, position = position_jitter()) +
  scale_y_continuous('Chronic Absence',
                     limits = c(0, 1), breaks = seq(0, 1, .1),
                     labels = percent_format()) +
  scale_x_discrete('School Year') +
  facet_grid(.~iep) +
  ggtitle('Chronic Absenteeism in Providence Public School District by IEP')


# write.csv(rosann_frame %>%
# filter(schoolyear %in% c('2009_2010', '2010_2011',
#                          '2011_2012', '2012_2013')) %>%
# group_by(level, schoolyear, lep) %>%
# summarize(chrabs = sum(chrabs, na.rm = TRUE),
#           n = n(),
#           pchrabs = sum(chrabs)/n()) %>%
# arrange(level, schoolyear), file = '', row.names = FALSE)


rosann_frame$lunch <- as.character(rosann_frame$lunch)
rosann_frame$lunch <- ifelse(rosann_frame$lunch=='F',
                             'Free Lunch',
                             ifelse(rosann_frame$lunch == 'P' | rosann_frame$lunch=='U',
                                    'Paid Lunch',
                                    ifelse(rosann_frame$lunch == 'R',
                                    'Reduced Lunch', rosann_frame$lunch)))
rosann_frame$lunch <- str_trim(rosann_frame$lunch, 'both')
rosann_frame$lunch <- ifelse(rosann_frame$lunch == '' | 
                             is.na(rosann_frame$lunch), 'Paid Lunch',
                             rosann_frame$lunch)
rosann_frame$lunch <- as.character(rosann_frame$lunch)
rosann_frame$lunch <- ifelse(rosann_frame$lunch == 'Free Lunch' |
                             rosann_frame$lunch == 'Reduced Lunch',
                             'Y', 'N')

frpl <- ggplot(data = rosann_frame %>%
                filter(schoolyear %in% c('2009_2010', '2010_2011',
                                         '2011_2012', '2012_2013')) %>%
                group_by(level, schoolyear, lunch) %>%
                summarize(chrabs = sum(chrabs, na.rm = TRUE),
                          n = n(),
                          pchrabs = sum(chrabs)/n()),
              aes(schoolyear, pchrabs, group=level, color = level)) +
  geom_line() +
  geom_text(mapping = aes(schoolyear, pchrabs, 
                          label = sprintf('%.1f', 100*pchrabs)),
            size = 4, vjust=2, position = position_jitter()) +
  scale_y_continuous('Chronic Absence',
                     limits = c(0, 1), breaks = seq(0, 1, .1),
                     labels = percent_format()) +
  scale_x_discrete('School Year') +
  facet_grid(.~lunch) +
  ggtitle('Chronic Absenteeism in Providence Public School District by FRPL')


ppi <- 300
png(filename='/Users/Jason/Desktop/overall.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(overall)
dev.off()

png(filename='/Users/Jason/Desktop/sex.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(sex)
dev.off()

png(filename='/Users/Jason/Desktop/race.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(race)
dev.off()

png(filename='/Users/Jason/Desktop/race_sex.png',
    width = 8.5*ppi, height = 10*ppi, units = 'px', res = ppi)
print(race_sex)
dev.off()

png(filename='/Users/Jason/Desktop/lep.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(lep)
dev.off()

png(filename='/Users/Jason/Desktop/iep.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(iep)
dev.off()

png(filename='/Users/Jason/Desktop/frpl.png',
    width = 8.5*ppi,height = 5*ppi, units = 'px', res = ppi)
print(frpl)
dev.off()

# write.csv(rosann_frame %>%
#           filter(schoolyear %in% c('2009_2010', '2010_2011',
#                                    '2011_2012', '2012_2013')) %>%
#           group_by(level, schoolyear, lunch) %>%
#           summarize(chrabs = sum(chrabs, na.rm = TRUE),
#                     n = n(),
#                     pchrabs = sum(chrabs)/n()) %>%
#           arrange(level, schoolyear), file = '', row.names = FALSE)
