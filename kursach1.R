rm(list=ls()) 
setwd("D:/Group_125/Lyub/MathMod/MathMod/MathMod")
getwd()
library("dplyr")
library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
library(readr)
tbl = read_csv("eddypro.csv",skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
tbl = select(tbl, -(roll)) 
tbl<-tbl[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
names(tbl) 
tbl <- tbl[tbl$DOY>244 & tbl$DOY<334, c(1:ncol(tbl))]
tbl$daytime=as.logical(tbl$daytime)
filter(tbl,daytime == TRUE) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric) ] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(h2o_flux) 
vars = row.names(cor_td)[cor_td$h2o_flux^2 > .1] %>% na.exclude 
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep="")) 
formula 
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7)) 
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,] 
mod = lm(formula, data=tbl) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod)
anova(mod)
mod1 = lm(h2o_flux~(DOY + Tau + rand_err_Tau + H + rand_err_H + LE + rand_err_LE + 
                      + rand_err_h2o_flux + H_strg + h2o_time_lag + RH + VPD + u_star_ + TKE + T_star_ + un_Tau + un_H + un_LE + 
                      un_h2o_flux + mean_value + w_var + w_div_h2o_cov  + h2o_signal_strength_7200)^2,data=tbl)
mod1
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1)
anova(mod1)
mod2 = lm(h2o_flux~(DOY + Tau + rand_err_Tau + H + rand_err_H + LE + rand_err_LE + 
                      co2_flux + rand_err_co2_flux + rand_err_h2o_flux + 
                      H_strg + h2o_time_lag + RH + VPD + u_star_ + TKE + T_star_ + un_Tau + un_H + un_LE + 
                      un_h2o_flux + mean_value + w_var + w_div_h2o_cov  + 
                      h2o_signal_strength_7200)^2-Tau:h2o_time_lag-Tau:VPD-Tau:h2o_signal_strength_7200-rand_err_Tau:mean_value
          -H:TKE-H:un_H-H:w_var-rand_err_H:h2o_time_lag-rand_err_H:un_h2o_flux-rand_err_H:mean_value-LE:H_strg-LE:RH-LE:un_H-LE:un_LE
          -rand_err_LE:rand_err_h2o_flux-rand_err_LE:H_strg-rand_err_LE:h2o_signal_strength_7200-rand_err_h2o_flux:H_strg,data=tbl)
mod2
coef(mod2) 
resid(mod2) 
confint(mod2) 
summary(mod2)
anova(mod2)
mod3 = lm(h2o_flux~(DOY + Tau + rand_err_Tau + H + LE + rand_err_LE + 
                      co2_flux + rand_err_co2_flux + rand_err_h2o_flux + h2o_time_lag + RH + VPD + u_star_ + TKE + T_star_ + un_Tau + un_H + un_LE + 
                      un_h2o_flux + mean_value + w_var + w_div_h2o_cov  + 
                      h2o_signal_strength_7200)^2-Tau:h2o_time_lag-Tau:VPD-Tau:h2o_signal_strength_7200-rand_err_Tau:mean_value
          -H:TKE-H:un_H-H:w_var-rand_err_H:h2o_time_lag-rand_err_H:un_h2o_flux-rand_err_H:mean_value-LE:H_strg-LE:RH-LE:un_H-LE:un_LE
          -rand_err_LE:rand_err_h2o_flux-rand_err_LE:H_strg-rand_err_LE:h2o_signal_strength_7200-rand_err_h2o_flux:H_strg-Tau:RH
          -Tau:un_H-Tau:w_div_h2o_cov-rand_err_Tau:RH-rand_err_Tau:un_h2o_flux-H:LE-H:rand_err_co2_flux-H:RH-H:mean_value-H:w_div_h2o_cov
          -LE:u_star_-LE:TKE-LE:T_star_-LE:un_Tau-rand_err_LE:RH-rand_err_LE:VPD-rand_err_LE:un_Tau-rand_err_LE:un_H-rand_err_LE:un_LE
          -rand_err_LE:mean_value-rand_err_LE:w_div_h2o_cov-co2_flux:rand_err_co2_flux-co2_flux:u_star_-co2_flux:TKE-co2_flux:un_H-co2_flux:un_LE
          -co2_flux:un_h2o_flux-co2_flux:w_var-co2_flux:w_div_h2o_cov-co2_flux:h2o_signal_strength_7200-rand_err_co2_flux:RH
          -rand_err_co2_flux:VPD-rand_err_co2_flux:u_star_-rand_err_co2_flux:TKE-rand_err_co2_flux:T_star_-rand_err_co2_flux:un_Tau-rand_err_co2_flux:un_H
          -rand_err_co2_flux:un_LE-rand_err_co2_flux:mean_value-rand_err_co2_flux:w_var-rand_err_co2_flux:w_div_h2o_cov-rand_err_co2_flux:h2o_signal_strength_7200
          -rand_err_h2o_flux:u_star_-Tau:un_LE-rand_err_Tau:VPD-rand_err_Tau:u_star_-rand_err_Tau:w_var-H:VPD-LE:co2_flux-LE:VPD-LE:mean_value-LE:w_var
          -rand_err_LE:rand_err_co2_flux-rand_err_LE:TKE-co2_flux:h2o_time_lag-co2_flux:RH-co2_flux:VPD-co2_flux:mean_value-rand_err_co2_flux:rand_err_h2o_flux
          -rand_err_h2o_flux:h2o_time_lag-rand_err_h2o_flux:mean_value-rand_err_h2o_flux:h2o_signal_strength_7200-RH-DOY:TKE-Tau:un_h2o_flux
          -rand_err_Tau:w_div_h2o_cov-H:rand_err_LE-H:u_star_-rand_err_co2_flux:h2o_time_lag-rand_err_h2o_flux:RH-rand_err_h2o_flux:TKE-rand_err_h2o_flux:un_Tau
          -h2o_time_lag:RH-h2o_time_lag:u_star_-h2o_time_lag:TKE-h2o_time_lag:T_star_-h2o_time_lag:un_Tau-h2o_time_lag:un_LE-h2o_time_lag:w_div_h2o_cov-h2o_time_lag:h2o_signal_strength_7200
          -RH:un_Tau-RH:un_LE-RH:mean_value-RH:w_var-RH:w_div_h2o_cov-RH:h2o_signal_strength_7200-VPD:TKE-VPD:un_LE-VPD:mean_value-VPD:w_var-VPD:w_div_h2o_cov
          -Tau:mean_value-H:rand_err_h2o_flux-H:un_LE-H:un_h2o_flux-LE:rand_err_co2_flux-rand_err_LE:w_var-h2o_time_lag:VPD-h2o_time_lag:un_h2o_flux
          -h2o_time_lag:mean_value-h2o_time_lag:w_var-RH:u_star_-RH:un_H-VPD:un_Tau-VPD:un_H-VPD:un_h2o_flux-u_star_:TKE-u_star_:T_star_-u_star_:un_Tau
          -u_star_:un_H-u_star_:un_LE-u_star_:mean_value-u_star_:h2o_signal_strength_7200-TKE:T_star_-TKE:un_Tau-TKE:un_H-TKE:un_LE-TKE:un_h2o_flux
          -TKE:mean_value-TKE:w_var-TKE:w_div_h2o_cov-TKE:h2o_signal_strength_7200-T_star_:un_H-T_star_:mean_value-T_star_:w_var-T_star_:w_div_h2o_cov
          -un_Tau:un_h2o_flux-un_Tau:w_var-un_Tau:h2o_signal_strength_7200-un_H:un_h2o_flux-un_H:w_var-un_H:h2o_signal_strength_7200
          -un_LE:mean_value-un_LE:w_var-un_LE:w_div_h2o_cov-un_h2o_flux:mean_value-un_h2o_flux:w_var-un_h2o_flux:h2o_signal_strength_7200
          -DOY:un_Tau-rand_err_Tau:h2o_signal_strength_7200-H:co2_flux-rand_err_LE:u_star_-rand_err_co2_flux:un_h2o_flux-rand_err_h2o_flux:T_star_
          -rand_err_h2o_flux:un_H-rand_err_h2o_flux:w_var-h2o_time_lag:un_H-RH:TKE-RH:un_h2o_flux-u_star_:un_h2o_flux-T_star_:un_Tau-T_star_:un_LE
          -un_Tau:un_H-un_Tau:mean_value-un_Tau:w_div_h2o_cov-un_H:un_LE-un_H:mean_value-un_LE:un_h2o_flux-un_h2o_flux:w_div_h2o_cov-mean_value:w_var
          -mean_value:w_div_h2o_cov-w_var:h2o_signal_strength_7200-w_div_h2o_cov:h2o_signal_strength_7200-co2_flux:un_Tau-VPD:u_star_-VPD:T_star_
          -w_var:w_div_h2o_cov,data=tbl)
mod3
coef(mod3) 
resid(mod3) 
confint(mod3) 
summary(mod3)
anova(mod3)
plot(mod3)
