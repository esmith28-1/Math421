df2 <- read_sas("hdd0318cy.sas7bdat")
df2 <- df2 %>% 
  filter(yod==18)
df2 <- df2 %>%
  select("yod", "payfix","pay_ub92","age",  
         "sex","raceethn","provider","moa", 
         "yoa","mod","admtype", "asource" , 
         "preopday" ,"los", "service" , "icu","ccu",    
         "dispub92", "payer"  ,"drg","trandb", 
         "randbg","randbs","orr", "anes","seq",   
         "lab","dtest", "ther","blood","phar", 
         "other","patcon","bwght","total","tot" ,  
         "ecodub92","b_wt","pt_state","diag_adm","ancilar" ,
         "campus","er_fee","er_chrg","er_mode","obs_chrg",
         "obs_hour","psycchrg","nicu_day")
missing_values <- df2 %>%
  summarise_all(~ sum(is.na(.)))
df2 <- df2 %>%
  select_if(~ !any(is.na(.)))
df2$total <- as.numeric(as.character(df2$total))
df2 <- df2 %>%
  filter(!is.na(raceethn) & !is.na(admtype))
df2 <- df2 %>%
  mutate(target = ifelse(total < median(total, na.rm = TRUE), "low", "high"))
table(df2$target)
df2 <- df2 %>%
  select(age, sex, raceethn, provider, moa, mod, admtype, campus, los, target)
library(caret)
df2 <- df2 %>% 
  mutate(target = as.factor(target),
         raceethn = as.factor(raceethn),
         sex = as.factor(sex), 
         provider = as.factor(provider), 
         campus = as.factor(campus),
         admtype = as.factor(admtype), 
         moa = as.factor(moa), 
         mod = as.factor(mod))
SplitIndex <- createDataPartition(df2$target, p = .10, 
                                  list = FALSE)
df2_train <- df2[ SplitIndex, ]
df2_test  <- df2[-SplitIndex, ]
df2_train <- df2_train[ complete.cases(df2_train), ]
df2_test  <- df2_test[ complete.cases(df2_test), ]
stopifnot(sum(is.na(df2_train)) == 0)
stopifnot(sum(is.na(df2_test))  == 0)
save(df2, df2_train, df2_test, file = "df2_loaded.RData")