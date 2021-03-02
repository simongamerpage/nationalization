## Set WD, Libraries, and slice data
setwd("clea_fall20/nat_script_development/")

library(daff) # For comparison graphics

#-----------------#
# Data Conversion #
#-----------------#

## Testing using lower chamber CLEA
assign("df",get(load("test_data/CLEA_LC_R14.rdata")))


#--------------------#
# Load Scripts & Run #
#--------------------#

source("clea-nat-source-v1.R")
nationalization(dataSource = "test_data/CLEA_LC_R14.rdata",
                dataType = ".rdata",
                dataQuality = "party.based", # changing
                outputFolder = "test_output/",
                inequalityType = "Gini")

source("nationalization_v2_source.R")
buildPNMdatasets_v2(dataSource = "test_data/CLEA_LC_R14.rdata",
                    outputFolder = "base_output/",
                    ineq.ind = "Gini")


#---------------#
# Testing time! #
#---------------#

## Comparing how bad gini probably is dear god pls help me 
{oldGini <- read_xlsx("base_output/gini.values.xlsx")
oldGini[oldGini == -990] <- NA

newGini <- read_xlsx("test_output/gini.values.xlsx")

colnames(newGini) <- paste0(colnames(newGini),"_new")
newGini <- newGini %>% 
  rename(
    id=id_new,
    ctr_n=ctr_n_new,
    ctr=ctr_new,
    yr=yr_new,
    mn=mn_new,
    cst=cst_new,
    pty=pty_new
  ) %>% 
  print

gini.compare <- oldGini %>% 
  right_join(newGini,by = c("id","ctr_n","ctr","yr","mn","cst","pty")) %>%
  mutate(
    gini_diff=giniI_new-giniI,
    pv1_diff=pv1_new-pv1,
    vv1_diff=vv1_new-vv1
  ) %>% 
  print

summary(gini.compare)

prob_cases <- filter(gini.compare, gini_diff >= sd(newGini$giniI_new)*0.10 | abs(gini_diff) >= sd(newGini$giniI_new)*0.10)
}
## Testing Denmark 1915 (id==198) for Gini differences (FIXED!)
{assign("denmark",get(load("test_data/denmark1915.rdata")))

source("clea-nat-source-v1.R")
nationalization(dataSource = "test_data/denmark1915.rdata",
                dataType = ".rdata",
                dataQuality = "party.based",
                outputFolder = "test_output/",
                inequalityType = "Gini")

source("nationalization_v2_source.R")
buildPNMdatasets_v2(dataSource = "test_data/denmark1915.rdata",
                    outputFolder = "test_output/",
                    ineq.ind = "Gini")
}
## Testing Kosovo 2007 & 2010 for differences (FIXED!)
{assign("kosovo",get(load("test_data/kosovo2007_10.rdata")))

source("clea-nat-source-v1.R")
nationalization(dataSource = "test_data/kosovo2007_10.rdata",
                dataType = ".rdata",
                dataQuality = "party.based", 
                outputFolder = "test_output/",
                inequalityType = "Gini")

source("nationalization_v2_source.R")
buildPNMdatasets_v2(dataSource = "test_data/kosovo2007_10.rdata",
                    outputFolder = "test_output/",
                    ineq.ind = "Gini")
}
## Testing S. Africa, 1989 (FIXED!)
{assign("safrica",get(load("test_data/safrica1989.rdata")))

source("clea-nat-source-v1.R")
nationalization(dataSource = "test_data/safrica1989.rdata",
                dataType = ".rdata",
                dataQuality = "party.based", 
                outputFolder = "test_output/",
                inequalityType = "Gini")

source("nationalization_v2_source.R")
buildPNMdatasets_v2(dataSource = "test_data/safrica1989.rdata",
                    outputFolder = "test_output/",
                    ineq.ind = "Gini")
}

## Comparing party.level now to see how bad we are talking
oldPty <- read_xlsx("base_output/party.level.xlsx")
oldPty[oldPty == -990] <- NA

newPty <- read_xlsx("test_output/party.level.xlsx",guess_max = 10000)
colnames(newPty) <- paste0(colnames(newPty),"_new")
newPty <- newPty %>% 
  rename(
    id=id_new,
    ctr_n=ctr_n_new,
    ctr=ctr_new,
    yr=yr_new,
    mn=mn_new,
    pty=pty_new,
    pty_n=pty_n_new
  ) %>% 
  print

pty.compare <- oldPty %>% 
  right_join(newPty,by = c("id","ctr_n","ctr","yr","mn","pty","pty_n")) %>%
  mutate(
    pns_diff=PNS_new-PNS,
    pns_s_diff=PNS_s_new-PNS_s,
    pns_w_diff=PNS_w_new-PNS_w,
    pns_sw_diff=PNS_sw_new-PNS_sw,
    
    cst_tot_diff=cst_tot_new-cst_tot
  ) %>% 
  print

summary(pty.compare)

pns_sd10 <- sd(newPty$PNS_new, na.rm = T)*0.10
pns_s_sd10 <- sd(newPty$PNS_s_new, na.rm = T)*0.10
pns_w_sd10 <- sd(newPty$PNS_w_new, na.rm = T)*0.10
pns_sw_sd10 <- sd(newPty$PNS_sw_new, na.rm = T)*0.10

pty.compare[is.na(pty.compare)] <- 0
prob_pns <- pty.compare %>%
  mutate(
    is_pns_diff=ifelse(pns_diff >= pns_sd10 | abs(pns_diff) >= pns_sd10,1,0),
    is_pns_s_diff=ifelse(pns_s_diff >= pns_s_sd10 | abs(pns_s_diff) >= pns_s_sd10,1,0),
    is_pns_w_diff=ifelse(pns_w_diff >= pns_w_sd10 | abs(pns_w_diff) >= pns_w_sd10,1,0),
    is_pns_sw_diff=ifelse(pns_sw_diff >= pns_sw_sd10 | abs(pns_sw_diff) >= pns_sw_sd10,1,0)
    ) %>% 
  filter(
    #is_pns_diff == 1, # [STATUS: COMPLETE]
    #is_pns_s_diff == 1, # [STATUS: COMPLETE]
    #is_pns_w_diff == 1,
    #is_pns_sw_diff == 1,
  ) %>%
  print



















