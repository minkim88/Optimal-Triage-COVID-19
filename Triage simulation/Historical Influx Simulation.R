
# Create historical influx

hflow = read_excel('Historical Influx.xlsx')
names(hflow) = c('date','cum_patient','cum_death','patient','death')
hflow$patient[1] = 0
hflow$patient %<>% as.numeric

h1 = hflow %>% filter(date %>% between('2020-02-17', '2020-03-17')) %>% pull(patient)
h2 = hflow %>% filter(date %>% between('2020-06-01', '2020-07-30')) %>% pull(patient)
h3 = hflow %>% filter(date %>% between('2020-08-10', '2020-10-08')) %>% pull(patient)
h4 = hflow %>% filter(date %>% between('2020-11-13', '2021-02-20')) %>% pull(patient)

# Repeated simulation to find optimal triage threshold

result_H1 = find_threshold(h1, rep = 20)
result_H2 = find_threshold(h2, rep = 20)
result_H3 = find_threshold(h3, rep = 20)
result_H4 = find_threshold(h4, rep = 20)


H_aggr = bind_rows(result_H1 %>% mutate(Influx = 'H1'), result_H2 %>% mutate(Influx = 'H2'),
                   result_H3 %>% mutate(Influx = 'H3'), result_H4 %>% mutate(Influx = 'H4'))


# Historical Mortality by Threshold

H_aggr %>% mutate(mortality = death/tot, Influx = factor(Influx)) %>%
  group_by(threshold,Influx) %>% summarise(mortality = mean(mortality)) %>%
  ggplot(aes(x = threshold,y = mortality, group = Influx, color = Influx)) +
  geom_line(size = 1.5) +
  geom_point(data = H_aggr %>% mutate(mortality = death/tot, Influx = factor(Influx)) %>%
               group_by(threshold,Influx) %>% summarise(mortality = mean(mortality)) %>%
               group_by(Influx) %>% filter(mortality == min(mortality)),
             aes(x = threshold, y= mortality, group = Influx),color = 'black',inherit.aes = F, size = 2) +
  labs(x = 'Threshold', y = 'Mortality') +
  theme_bw() +
  theme(text = element_text(size = 20))


# Historical Mortality by Threshold and Type

H_aggr %>%
  dplyr::select(-death) %>%
  mutate(Type1 = resource_indep/tot, Type2 = resource_dep/tot, Type3 = threshold_dep/tot) %>%
  group_by(Influx,threshold) %>% summarise(Type1 = mean(Type1) , Type2 = mean(Type2), Type3 = mean(Type3)) %>%
  melt(id.vars = c('Influx', 'threshold')) %>%
  rename(Type = variable, Proportion = value) %>% 
  ggplot(aes(threshold, Proportion, group = Type, fill = Type, color = Type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(vars(Influx), labeller = label_both) +
  scale_fill_manual(values = c(Type1 = 'grey80', Type2 = 'grey50', Type3 = 'grey30')) +
  scale_color_manual(values = c(Type1 = 'grey80', Type2 = 'grey50', Type3 = 'grey30'))+
  theme_bw()+
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 20))+
  xlab('Threshold')



# Simulation using threshold = J-index

j_index = 0.01309538
j_index_h1 = simulation_J_index(h1,J_index = j_index, rep = 20)
j_index_h2 = simulation_J_index(h2,J_index = j_index, rep = 20)
j_index_h3 = simulation_J_index(h3,J_index = j_index, rep = 20)
j_index_h4 = simulation_J_index(h4,J_index = j_index, rep = 20)


H_aggr = bind_rows(result_H1 %>% mutate(H = 1), result_H2 %>% mutate(H = 2),
                   result_H3 %>% mutate(H = 3), result_H4 %>% mutate(H = 4))
J_H_aggr = bind_rows(j_index_h1 %>% mutate(H = 1), j_index_h2 %>% mutate(H = 2),
                     j_index_h3 %>% mutate(H = 3), j_index_h4 %>% mutate(H = 4))


## Historical Simulation Result

# Dummies (element_blank) for plot scale & size
dummy = data.frame(H = c(2, 2, 1), value = c(0.01, 0, 0.01), 
                   variable = c("Optimized mortality", 'Decreased mortality','Decreased mortality'), 
                   stringsAsFactors=FALSE)
dummy$H = factor(dummy$H, levels = c('1','2','3','4'))
dummy$variable = factor(dummy$variable, levels = c('Optimal threshold','Optimized mortality','Decreased mortality'))

# Result plot
H_aggr %>% mutate(mortality = death/tot, H = factor(H)) %>%
  group_by(threshold, H) %>% summarise(mortality = mean(mortality)) %>%
  group_by(H) %>% filter(mortality == min(mortality)) %>% rename(Opt_mortality = mortality) %>% 
  left_join(J_H_aggr %>% mutate(mortality = death/tot, H = factor(H)) %>%
              group_by(threshold, H) %>% summarise(mortality = mean(mortality)) %>%
              rename(J_Index = threshold, J_mortality = mortality)) %>%
  dplyr::select(H, threshold, J_Index, everything()) %>%
  mutate(threshold = threshold,
         `J Index Mortality - Optimized Mortality` = (J_mortality-Opt_mortality)/J_mortality,
         Opt_mortality = Opt_mortality) %>%
  dplyr::select(-J_Index, -J_mortality) %>%
  rename(`Optimal threshold` = threshold, 
         `Optimized mortality` = Opt_mortality,
         `Decreased mortality` = `J Index Mortality - Optimized Mortality`) %>% 
  melt(id.vars = 'H') %>% 
  ggplot(aes(x = H, y = value, group = variable)) +
  geom_blank(data=dummy) +
  facet_wrap(vars(variable), scales = 'free_y', ncol = 1) +
  scale_y_continuous(breaks= scales::pretty_breaks(3)) +
  geom_point(size = 2) + geom_line(size = 1) +
  labs(x = 'H (historical influx)', y = 'Value') +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 40))

