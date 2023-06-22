rm(list = ls())

library(tidyverse)
library(haven)
library(Hmisc)
# install.packages("titanic")
# library(titanic)

titanic <- read_dta("titanic.dta")

# Step 1: Stratify the data by sex and age ####

titanic <- titanic %>% 
  mutate(a_m = ifelse(age == 1 & sex == 1, 1, 0),
         a_f = ifelse(age == 1 & sex == 0, 1, 0),
         c_m = ifelse(age == 0 & sex == 1, 1, 0),
         c_f = ifelse(age == 0 & sex == 0, 1, 0),
         first = ifelse(class == 1, 1, 0))

# Step 2: Calculate differences in mean survival rate for all four strata ====

s_am1 <- titanic %>% 
  filter(a_m == 1 & first == 1) %>% 
  summarise(mean(survived)) %>% 
  pull()

s_am0 <- titanic %>% 
  filter(a_m == 1 & first == 0) %>% 
  summarise(mean(survived)) %>% 
  pull()

d_s_am <- s_am1 - s_am0

s_af1 <- titanic %>% 
  filter(a_f == 1 & first == 1) %>% 
  summarise(mean(survived)) %>% 
  pull()

s_af0 <- titanic %>% 
  filter(a_f == 1 & first == 0) %>% 
  summarise(mean(survived)) %>% 
  pull()

d_s_af <- s_af1 - s_af0

s_cm1 <- titanic %>% 
  filter(c_m == 1 & first == 1) %>% 
  summarise(mean(survived)) %>% 
  pull()

s_cm0 <- titanic %>% 
  filter(c_m == 1 & first == 0) %>% 
  summarise(mean(survived)) %>% 
  pull()

d_s_cm <- s_cm1 - s_cm0

s_cf1 <- titanic %>% 
  filter(c_f == 1 & first == 1) %>% 
  summarise(mean(survived)) %>% 
  pull()

s_cf0 <- titanic %>% 
  filter(c_f == 1 & first == 0) %>% 
  summarise(mean(survived)) %>% 
  pull()

d_s_cf <- s_cf1 - s_cf0

# Step 3: Calculate weights for each causal parameter: ATE, ATT and ATU ====

## Step 3a: Display the number of observations. ====

n_am1 <- titanic %>% 
  filter(a_m == 1 & first == 1) %>% 
  summarise(n()) %>% 
  pull()

n_am0 <- titanic %>% 
  filter(a_m == 1 & first == 0) %>% 
  summarise(n()) %>% 
  pull()

n_af1 <- titanic %>% 
  filter(a_f == 1 & first == 1) %>% 
  summarise(n()) %>% 
  pull()

n_af0 <- titanic %>% 
  filter(a_f == 1 & first == 0) %>% 
  summarise(n()) %>% 
  pull()

n_cm1 <- titanic %>% 
  filter(c_m == 1 & first == 1) %>% 
  summarise(n()) %>% 
  pull()

n_cm0 <- titanic %>% 
  filter(c_m == 1 & first == 0) %>% 
  summarise(n()) %>% 
  pull()

n_cf1 <- titanic %>% 
  filter(c_f == 1 & first == 1) %>% 
  summarise(n()) %>% 
  pull()

n_cf0 <- titanic %>% 
  filter(c_f == 1 & first == 0) %>% 
  summarise(n()) %>% 
  pull()

## Step 3b: Now construct the weights based on the counts for each parameter ====

# Strata counts for ATE

# Counts of adult females
n_af <- n_af1 + n_af0
 
# Counts of adult males
n_am <- n_am1 + n_am0
 
# Counts of male children 
n_cm <- n_cm1 + n_cm0
 
# Counts of female children
n_cf <- n_cf1 + n_cf0

n_ate <- nrow(titanic)
 
### Construct weights for ATE by strata ====
wt_ate_af <- n_af/n_ate
wt_ate_am <- n_am/n_ate
wt_ate_cf <- n_cf/n_ate
wt_ate_cm <- n_cm/n_ate
 
# Strata counts for first class for ATT

n_att <- titanic %>% 
  filter(first == 1) %>% 
  summarise(n()) %>% 
  pull()

### Construct weights for ATT by strata ====
wt_att_af <- n_af1/n_att
wt_att_am <- n_am1/n_att
wt_att_cf <- n_cf1/n_att
wt_att_cm <- n_cm1/n_att

# Strata counts for non-first class for ATU

n_atu <- titanic %>% 
  filter(first == 0) %>% 
  summarise(n()) %>% 
  pull()

### Construct weights for ATU by strata ====
wt_atu_af <- n_af0/n_atu
wt_atu_am <- n_am0/n_atu
wt_atu_cf <- n_cf0/n_atu
wt_atu_cm <- n_cm0/n_atu

# Step 4: Estimate aggregate parameters using corresponding weights and differences within strata ====

strat_ate <- (wt_ate_af*d_s_af) + (wt_ate_am*d_s_am) + (wt_ate_cf*d_s_cf) + (wt_ate_cm*d_s_cm)
strat_att <- (wt_att_af*d_s_af) + (wt_att_am*d_s_am) + (wt_att_cf*d_s_cf) + (wt_att_cm*d_s_cm)
strat_atu <- (wt_atu_af*d_s_af) + (wt_atu_am*d_s_am) + (wt_atu_cf*d_s_cf) + (wt_atu_cm*d_s_cm)





