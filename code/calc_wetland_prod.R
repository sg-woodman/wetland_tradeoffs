############################################
#high frequency (mini dot) Data importing
############################################
#-----------------
#load libraries
#-----------------
library(lubridate)
library(lattice)
library(janitor)
library(dplyr)
library(ggplot2)

#------------------
#Load the data
#------------------



db <- read.csv("/Users/sam/Downloads/wet-wq.csv", na.strings = c("","#N/A"), stringsAsFactors=F)
summary(db) 

db <- db %>% 
  filter(!is.na(domgl)) %>% 
  filter(!is.na(pctdo)) %>% 
  filter(!is.na(d18o.do.airstd)) %>% 
  filter(!is.na(d18o.h2o.vsmow)) 

##########################
#oxygen isotope metabolism modelling
##########################

db$do.eq.mgl <- db$domgl * 100/db$pctdo #calculate dissolved oxygen concentration at equilibrium with atmosphere 
db$d18o.do.vsmow <- db$d18o.do.airstd + 23.8 + (db$d18o.do.airstd*23.8/1000)
db$o18.o16.o2 <- ((db$d18o.do.vsmow / 1000) * (0.00205)) + (0.00205)
db$o18.o16.h2o <- ((db$d18o.h2o.vsmow / 1000) * (0.00205)) + (0.00205)


#generic input terms - see table 2 of Bocaniov et al. 2015 JGLR for more detail
db$k.z <- (1/(db$depth/100))  #input the KO2 (assumed 1 m/d for now) divided by depth converted from cm to m
#depth for may sampling is value measured in july
db$o18.air <- 0.00209895#switched to d18O-air value = 23.88 permil, following Barkan & Luz 2005 
db$ap <- 1           # fractionation factor associated with photosynthesis
db$ag <- 0.9972      # fractionation factor associated with gas exchange
db$as <- 1.0007      # fractionation factor associated with DO solubility
db$o18o2 <- db$o18.o16.o2/(1+db$o18.o16.o2) #atomic fraction of O18 as AF=o18/(o18+o16)
db$o18h2o <- db$o18.o16.h2o/(1+db$o18.o16.h2o) #atomic fraction of O18 as AF=o18/(o18+o16)
db$o18air <- db$o18.air/(1+db$o18.air) #atomic fraction of O18 as AF=R/(R+1)
db$a <- db$ag*db$as*db$o18air
db$b <- db$ag*db$o18o2
db$c.985 <- 0.985*db$o18o2
db$d <- db$ap*db$o18h2o

# P:R (from Quay 1995 L&O) 
#Term G directly from Quay 1995 equation
db$g = (db$ag * ((db$as * db$o18air) - ((db$domgl / db$do.eq.mgl) * db$o18o2))) / (1 - (db$domgl / db$do.eq.mgl))

#PtoR ratios calculated using a fractionation value of 0.985
db$pr.985 <- ((db$o18o2*0.985)-db$g)/((db$o18h2o*1)-db$g)


db <- db %>% 
  filter(pr.985 > 0) 

summary(db$pr.985); hist(db$pr.985) 




#GPP
db$gpp.985 = db$k.z*(((db$domgl*(db$b-db$c.985))-
                          (db$do.eq.mgl*(db$a-db$c.985)))/(db$d-db$c.985))
hist(db$gpp.985)

plot(pr.985 ~ gpp.985, data = db)

#R
db$r.985 = db$k.z*(((db$domgl*(db$b - db$d)) -
                        (db$do.eq.mgl * (db$a - db$d))) / (db$d - db$c.985))

hist(db$r.985)


db <- db %>% 
  filter(gpp.985 > 0) 


plot(r.985 ~ gpp.985, data = db, pch = 19)
abline(a = 0, b = 1)


write_csv(db, here("data/processed/aqua_prod_2022.csv"))
