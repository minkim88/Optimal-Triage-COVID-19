# Data Setup

library(dplyr)
library(magrittr)

df_original_first = read.csv('data20201119_04.csv')

# Leave original data for EDA and remove observations with NAs in CSS, which is used as a target variable. 
df_original = df_original_first %>% filter(!is.na(CSS))


#### Split holdout cohort #############################
set.seed(2021)
holdout_idx = createDataPartition(df_original$CSS %>% as.factor,p = 0.1, list = F)
df_original[holdout_idx,]$CSS %>% table %>% prop.table
df_original[-holdout_idx,]$CSS %>% table %>% prop.table

holdout = df_original[holdout_idx,]
df = df_original[-holdout_idx,]

##### Modifying Variables 

df %<>% mutate_at(.funs = function(x){x = -x+2}, .vars = vars(PREG, FEVER:CURSIT))
df %<>% mutate(SEX = ifelse(SEX == 'M', 1, 0),
               OUTCOME = ifelse(OUTCOME == 1, 'released', 'dead'),
               PREG = ifelse(SEX == 1,0,PREG),
               PREGGW = ifelse(PREG == 0,0,PREGGW))

holdout %<>% mutate_at(.funs = function(x){x = -x+2}, .vars = vars(PREG, FEVER:CURSIT))
holdout %<>% mutate(SEX = ifelse(SEX == 'M', 1, 0),
                    OUTCOME = ifelse(OUTCOME == 1, 'released', 'dead'),
                    PREG = ifelse(SEX == 1,0,PREG),
                    PREGGW = ifelse(PREG == 0,0,PREGGW))

##### Missing Data
sort(colMeans(is.na(df)),decreasing = T)


### Create dataset with different endpoints
### Endpoints are defined by using different levels of CSS (Clinical Severity Score)
### Variables that are unkown at the time of disease confirmation -> CURSIT, OUTCOME, PERIOD are removed

df_5 = df %>% 
  mutate(target = ifelse(CSS >= 5, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>% 
  select(-c(CURSIT, OUTCOME, id, PERIOD,CSS)) %>% 
  filter(!is.na(target))

df_6 = df %>% 
  mutate(target = ifelse(CSS >= 6, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>% 
  select(-c(CURSIT, OUTCOME, id, PERIOD,CSS)) %>% 
  filter(!is.na(target))

df_7 = df %>% 
  mutate(target = ifelse(CSS >= 7, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>% 
  select(-c(CURSIT, OUTCOME, id, PERIOD,CSS)) %>% 
  filter(!is.na(target))

df_8 = df %>% 
  mutate(target = ifelse(CSS >= 8, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>% 
  select(-c(CURSIT, OUTCOME, id, PERIOD,CSS)) %>% 
  filter(!is.na(target))