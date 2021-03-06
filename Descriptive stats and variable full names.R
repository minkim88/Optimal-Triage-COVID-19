# Create descriptive stats table
# Change variables abbreviations into full names

library(dplyr)
library(magrittr)


df_eda = df_original %>% mutate_at(.funs = function(x){x = -x+2}, .vars = vars(PREG, FEVER:CURSIT))
df_eda %<>% mutate(SEX = ifelse(SEX == 'M', 1, 0),
                   OUTCOME = ifelse(OUTCOME == 1, 'released', 'dead'),
                   PREG = ifelse(SEX == 1,0,PREG),
                   PREGGW = ifelse(PREG == 0,0,PREGGW))

df_eda %<>% mutate(Severe = ifelse(CSS >= 6, 1, 0))

df_eda %<>% dplyr::select(-OUTCOME, -PERIOD, -id, -CURSIT, -CSS)

full_names = c('Age', 'Sex','Pregnancy','Pregnancy Weeks', 'Body mass index', 'Systolic blood pressure', 'Diastolic blood pressure',
               'Heart rate', 'Body temperature', 'Fever', 'Cough', 'Sputum', 'Soar throat', 'Rhinorrhea', 'Myalgia', 'Fatigue',
               'Shortness of breath', 'Headache', 'Altered conciousness', 'Vomiting', 'Diarrhea', 'Diabetes mellitus', 'Hypertension',
               'Heart failure', 'Cardiovascular disease', 'Asthma', 'COPD', 'Chronic kidney disease',
               'Malignancy', 'Chronic liver disease', 'Autoimmune disease', 'Dementia', 'Hemoglobin', 'Hematocrit', 'Lymphocyte',
               'Platelet', 'WBC', 'Severe Patient')
df_names = tibble(Variable = names(df_eda), Full_Name = full_names)

train = df_eda[-holdout_idx, ]
holdout = df_eda[holdout_idx, ]

create_descriptive = function(df){
  result = 
    do.call(data.frame,
            list(AVG = apply(df, 2, function(x) {
              ifelse(length(unique(x)) > 3, paste0(round(mean(x, na.rm = T),2), '(',round(sd(x, na.rm = T),2),')'),
                     paste0(sum(x,na.rm = T),' (',(prop.table(table(x))[2]*100) %>% round(1),'%)'))}),
              NAs = apply(df, 2, function(x) paste0(round(mean(is.na(x)),3)*100, '%'))))
  result$Variable = rownames(result)
  
  return(result)
}


fisher = (holdout %>% summarise_all(sum,na.rm = T))[,holdout %>% summarise_all(sum,na.rm = T) < 5] %>% names

variable_pvalue = function(train, holdout, var){
  
  if(var %in% fisher){
    
    tb_11 = sum(train %>% pull(var), na.rm = T)
    tb_12 = sum(holdout %>% pull(var), na.rm = T)
    
    pvalue = fisher.test(matrix(c(tb_11, nrow(train)- tb_11, tb_12, nrow(holdout) - tb_12), ncol = 2))$p.value
    return(tibble(Variable = var, pvalue = pvalue, test = 'Fisher'))
  }
  
  else if (bind_rows(train, holdout) %>% pull(var) %>% n_distinct(na.rm = T) > 2){
    
    pvalue = (t.test(train %>% pull(var), holdout %>% pull(var)))$p.value
    return(tibble(Variable = var, pvalue = pvalue, test = 'Welch'))
  } else {
    
    pvalue = prop.test(x = c(sum(train %>% pull(var), na.rm = T), sum(holdout %>% pull(var), na.rm = T)), n = c(nrow(train),nrow(holdout)))$p.value
    return(tibble(Variable = var, pvalue = pvalue, test = 'Chisq'))
  }
}

binned_descriptive = function(df){
  
  binned_vars = c('AGE', "BMI", "SBP", "DBP")
  binned_vars_df = df %>% select(binned_vars) %>% apply(MARGIN = 2, FUN = table) %>% unlist %>% as.data.frame 
  binned_vars_df$Variable = row.names(binned_vars_df)
  return(binned_vars_df %>% rename(value = '.') %>% select(Variable, value) %>% mutate(value = paste0(value,' (',round(value*100/nrow(df),2),'%)')))
  
}

EDA_Descriptive = 
  create_descriptive(df_eda) %>% 
  left_join(create_descriptive(train), by = 'Variable') %>% 
  left_join(create_descriptive(holdout), by = 'Variable') %>%
  left_join(df_names) %>% 
  select(Variable, Full_Name,everything())

names(EDA_Descriptive) = c('Variable', 'Full Name', 'Total', 'Total NA', 'Train Set', 'Train Set NA', 'Holdout Set', 'Holdout Set NA')
# write.csv(EDA_Descriptive,'Descriptive Stats.csv',row.names = T)

EDA_binned = 
  binned_descriptive(df_eda) %>% rename(Total = value) %>% 
  left_join(binned_descriptive(train) %>% rename(Train = value)) %>% 
  left_join(binned_descriptive(holdout) %>% rename(Holdout = value))

# write.csv(EDA_binned,'Descriptive Stats Binned Vars.csv',row.names = T)
pvalues_df = tibble()
for( var in names(df_eda)){
  temp = variable_pvalue(train,holdout, var)
  pvalues_df = bind_rows(pvalues_df, temp)
}

# write.csv(pvalues_df,'Descriptive Stats Pvalues.csv',row.names = F)


