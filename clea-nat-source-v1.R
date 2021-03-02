#*
#*
#*
#*
#*

nationalization <- function(dataSource, dataType, dataQuality,
                            outputFolder,inequalityType) {
  ## Initial message & starting efficiency timer
  initial.message <- function() {
    print("---------------------------------------------------", quote = FALSE)
    print("Initializing... Checking Packages & Data Existence", quote = FALSE)
    print("---------------------------------------------------", quote = FALSE)
  }
  initial.message()
  start.timer <- proc.time()
  
  #-------------------------------------------#
  # Loading/Installing Packages; Loading Data #
  #-------------------------------------------#
  
  ## Function to load packages/install if they do not exist on local machine
  usePackage <- function(required.package) {
    if (!is.element(required.package, installed.packages()[,1]))
      install.packages(required.package, dep = TRUE)
    require(required.package, character.only = TRUE)
  }
  
  usePackage("data.table") # For processing functions (more efficient than base R)
  usePackage("haven") # For reading Stata
  usePackage("readxl") # For reading Excel
  usePackage("ineq") # For Gini computation
  usePackage("openxlsx") # To write to .xlsx (desired format)
  
  ## Checking data file type, then loading respective data
  ifelse(dataType == ".csv", dat <- read.csv(dataSource), #; dat[,-c(X)]},
         ifelse(dataType == ".xlsx", dat <- read_xlsx(dataSource),
                ifelse(dataType == ".rdata", assign("dat",get(load(dataSource))),
                       ifelse(dataType == ".dta", dat <- read_dta(dataSource),{
                         print("No appropriate data type selected.", quote = FALSE); stop()
                       }))))
  
  ## Making sure inequality index is "Gini"; override anything else
  if (inequalityType != "Gini" & inequalityType %in% c("Gini", "RS", "Atkinson", "Theil", "Kolm",
                                                       "var", "square.var", "entropy") == FALSE) {
    inequalityType <- "Gini"
  }

  #----------------------#
  # Subset Data by Needs #
  #----------------------#

  ## Selecting crucial columns as a base 
  base <- subset(dat, select = c(id,ctr_n,ctr,yr,mn,cst,cst_n,pty,pty_n,vv1,pv1,pvs1,cv1,cvs1,seat))

  ## Creating the base for gini and party-level computations called data.a,
  ##... based on user preference of party, candidate, or shares based measures
  if (dataQuality == "party.based") {
    ## Removing -990s for NAs, indicating absent candidates, etc...
    data.a <- subset(base,select = -c(cv1,cvs1))
    data.a$can <- 0
    data.a$vv1 <- ifelse(data.a$vv1 < 0, NA, data.a$vv1)
    data.a$pv1 <- ifelse(data.a$pv1 < 0, NA, data.a$pv1)
    data.a$pvs1 <- ifelse(data.a$pvs1 < 0, NA, data.a$pvs1)
    data.a$na_candidate <- ifelse(is.na(data.a$vv1) & is.na(data.a$pv1),1,0)
    data.a <- unique(data.a)
    ## Computing shares on our own, once, to avoid broken shares in data
    data.a$pvs1 <- data.a$pv1 / data.a$vv1
    data.a <- data.a[order(data.a$cst), , drop = FALSE]
    data.a <- data.a[order(data.a$ctr_n,data.a$yr),]
    
    # Renaming to generic totals/shares, then informing user of success/moving along with party-based
    names(data.a)[names(data.a) == "pv1"] <- "vote.totals"
    names(data.a)[names(data.a) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for Gini & party-level measures...", quote = FALSE)
  } else (
    if (dataQuality == "candidate.based") {
      ## Removing -990s for NAs, indicating absent candidates, etc...
      data.a <- subset(base,select = -c(pv1,pvs1))
      data.a$vv1 <- ifelse(data.a$vv1 < 0, NA, data.a$vv1)
      data.a$cv1 <- ifelse(data.a$cv1 < 0, NA, data.a$cv1)
      data.a$cvs1 <- ifelse(data.a$cvs1 < 0, NA, data.a$cvs1)
      data.a$na_candidate <- ifelse(is.na(data.a$vv1) & is.na(data.a$cv1),1,0)
      data.a <- unique(data.a)

      # Renaming to generic totals/shares, then informing user of success/moving along with candidate-based
      names(data.a)[names(data.a) == "cv1"] <- "vote.totals"
      names(data.a)[names(data.a) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for Gini & party-level measures...", quote = FALSE)
    } else (
      if (dataQuality == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )
  
  ## Creating the base for national-level ENP computations, based on user preference
  ##... of party, candidate, or shares based measures
  if (dataQuality == "party.based") {
    data.b <- subset(base, select = -c(cv1,cvs1))
    data.b$can <- 0
    data.b$vv1 <- ifelse(data.b$vv1 < 0, NA, data.b$vv1)
    data.b$pv1 <- ifelse(data.b$pv1 <= 0, NA, data.b$pv1) # Note the <= difference here, vs. data in Gini section
    data.b$pvs1 <- ifelse(data.b$pvs1 < 0, NA, data.b$pvs1)
    data.b$na_candidate <- ifelse(is.na(data.b$vv1) | is.na(data.b$pv1),1,0)
    ## Computing unique constituency totals BEFORE filtering out NA candidates
    data.b <- data.table(data.b)[,cst_tot := length(unique(cst)), by=c("id","ctr_n","ctr","yr","mn")]
    ## Grabbing unique cases across entire frame, then filtering non-existent candidates
    data.b <- unique(data.b)
    data.b <- subset(data.b, na_candidate == 0)
    ## Renaming to generic totals/shares, then informing the user of success/moving along with party-based
    names(data.b)[names(data.b) == "pv1"] <- "vote.totals"
    names(data.b)[names(data.b) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for national-level ENP measures...", quote = FALSE)
  } else (
    if (dataQuality == "candidate.based") {
      data.b <- subset(base, select = -c(pv1,pvs1))
      data.b$can <- 0
      data.b$vv1 <- ifelse(data.b$vv1 < 0, NA, data.b$vv1)
      data.b$pv1 <- ifelse(data.b$cv1 <= 0, NA, data.b$pv1) # Note the <= difference here, vs. data in Gini section
      data.b$pvs1 <- ifelse(data.b$cvs1 < 0, NA, data.b$pvs1)
      data.b$na_candidate <- ifelse(is.na(data.b$vv1) & is.na(data.b$cv1),1,0)
      ## Comptuing unique constituency totals BEFORE filtering out NA candidates
      data.b <- data.table(data.b)[,cst_tot := length(unique(cst)), by=c("id","ctr_n","ctr","yr","mn")]
      ## Grabbing unique cases across entire frame, then filtering non-existent candidates
      data.b <- unique(data.b)
      data.b <- subset(data.b, na_candidate == 0)
      ## Renaming to generic totals/shares, then informing the user of success/moving along with party-based
      names(data.b)[names(data.b) == "cv1"] <- "vote.totals"
      names(data.b)[names(data.b) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for national-level ENP measures...", quote = FALSE)
    } else (
      if (dataQuality == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )
  
  ## Creating the base for national-level computations, based on user preference
  ##... of party, candidate, or shares based measures
  if (dataQuality == "party.based") {
    data.c <- subset(base, select = -c(cv1,cvs1))
    data.c$can <- 0
    data.c$vv1 <- ifelse(data.c$vv1 < 0, NA, data.c$vv1)
    data.c$pv1 <- ifelse(data.c$pv1 <= 0, NA, data.c$pv1)
    data.c$pvs1 <- ifelse(data.c$pvs1 < 0, NA, data.c$pvs1)
    data.c$na_candidate <- ifelse(is.na(data.c$vv1) & is.na(data.c$pv1),1,0)
    ## Computing unique constituency totals BEFORE filtering out NA candidates
    data.c <- data.table(data.c)[,cst_tot := length(unique(cst)), by=c("id","ctr_n","ctr","yr","mn")]
    ## Grabbing unique cases across entire frame, then filtering non-existent candidates
    data.c <- unique(data.c)
    data.c <- subset(data.c, na_candidate == 0)
    ## Renaming to generic totals/shares, then informing the user of success/moving along with party-based
    names(data.c)[names(data.c) == "pv1"] <- "vote.totals"
    names(data.c)[names(data.c) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for national-level inflation/PSNS measures...", quote = FALSE)
  } else (
    if (dataQuality == "candidate.based") {
      data.c <- subset(base, select = -c(pv1,pvs1))
      data.c$can <- 0
      data.c$vv1 <- ifelse(data.c$vv1 < 0, NA, data.c$vv1)
      data.c$pv1 <- ifelse(data.c$pv1 <= 0, NA, data.c$cv1) 
      data.c$pvs1 <- ifelse(data.c$pvs1 < 0, NA, data.c$cvs1)
      data.c$na_candidate <- ifelse(is.na(data.c$vv1) & is.na(data.c$cv1),1,0)
      ## Computing unique constituency totals BEFORE filtering out NA candidates
      data.c <- data.table(data.c)[,cst_tot := length(unique(cst)), by=c("id","ctr_n","ctr","yr","mn")]
      ## Grabbing unique cases across entire frame, then filtering non-existent candidates
      data.c <- unique(data.c)
      data.c <- subset(data.c, na_candidate == 0)
      ## Renaming to generic totals/shares, then informing the user of success/moving along with party-based
      names(data.c)[names(data.c) == "cv1"] <- "vote.totals"
      names(data.c)[names(data.c) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for national-level inflation/PSNS measures...", quote = FALSE)
    } else (
      if (dataQuality == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )
  
  #*
  #browser()
  #*
  
  #--------------------------#
  # Gini Inequality Measures #
  #--------------------------#
  ineq.message <- function() {
    print("--------------------------------", quote=FALSE)
    print("Computing inequality measures...", quote=FALSE)
    print("--------------------------------", quote=FALSE)
  }
  ineq.message()

  ## Grabbing distinct cases across the entire frame, maintaining all columns; quickly rename to "gini"
  unique_rows <- !duplicated(data.a[c("id","ctr_n","ctr","yr", "mn","cst","pty")])
  unique.df <- data.a[unique_rows,]
  gini.base <- unique.df

  ## Grabbing valid votes at the national-level to filter small parties (less than 5% of the national vote)
  gini.totals <- data.table(gini.base)[,vote.totals := sum(vote.totals, na.rm = TRUE), by=c("id","ctr_n","ctr","yr","mn","cst","pty")]
  gini.vv1 <- data.table(gini.totals)[,vv1 := sum(vv1, na.rm = TRUE),by=c("id","ctr_n","ctr","yr","mn","cst","pty")]
  gini.nat_vv1 <- data.table(gini.vv1)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn")] # Removed id
  gini.pty_nat_vv1 <- data.table(gini.nat_vv1)[,pty_nat_vv1 := sum(vote.totals),by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  gini.pty_nat_vv1$nat_pvs <- gini.pty_nat_vv1$pty_nat_vv1 / gini.pty_nat_vv1$nat_vv1
  gini.small_removed <- subset(gini.pty_nat_vv1,nat_pvs > 0.05)

  ## Recomputing vote shares ONCE to avoid possible broken shares; replacing NaN/NA/infinite with zero
  gini.small_removed$vote.shares <- gini.small_removed$vote.totals / gini.small_removed$vv1
  gini.small_removed$vote.shares <- ifelse(is.infinite(gini.small_removed$vote.shares), NA, gini.small_removed$vote.shares)
  gini.small_removed$vote.shares <- ifelse(is.na(gini.small_removed$vote.shares), 0, gini.small_removed$vote.shares)

  ## Computing Gini, then replacing negative/NaN values with zero
  out <- setDT(gini.small_removed)[,giniI := ineq(vote.shares, NULL,type = inequalityType, na.rm = TRUE),by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  out$giniI <- replace(out$giniI, out$giniI < 0, 0)
  out$giniI <- replace(out$giniI, out$giniI == "NaN", 0)
  
  ## Cleaning the measures up, ordering, then shipping!
  gini.measures <- subset(out, select = c(id,ctr_n,ctr,yr,mn,cst,pty,giniI,vote.totals,vv1))
  names(gini.measures)[names(gini.measures) == "vote.totals"] <- ifelse(dataQuality == "party.based","pv1",
                                                                        ifelse(dataQuality == "candidate.based","cv1",
                                                                               ifelse(dataQuality == "shares.based","shares",NA)))
  gini.measures <- gini.measures[order(gini.measures$cst), , drop = FALSE]
  gini.measures <- gini.measures[order(gini.measures$ctr_n,gini.measures$yr),]

  ## Writing Gini to .xlsx; informing of success
  # write.xlsx(gini.measures, paste0(outputFolder, "gini.values.xlsx"),
  #            keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("Gini inequality measures successfully computed! Moving to party-level...", quote = FALSE)
  
  #*
  #browser()
  #*
  
  #--------------------------------------#
  # Party-Level Nationalization Measures #
  #--------------------------------------#  
  party.message <- function() {
    print("---------------------------------", quote=FALSE)
    print("Computing Party Level Dataset...", quote=FALSE)
    print("---------------------------------", quote=FALSE)
  }
  party.message()

  ## Grabbing distinct cases, using same data.a (computed above in subset section)
  unique_rows <- !duplicated(data.a[c("id","ctr_n","ctr","yr", "mn","cst","pty")])
  unique.df <- data.a[unique_rows,]
  party.base <- unique.df
  
  ## Merging in gini, then using it to compute vanilla PNS
  party <- merge(party.base, gini.measures, by = c("id","ctr_n","ctr","yr","mn","cst","pty","vv1"),all.x = TRUE)
  party$PNS <- 1 - party$giniI
  
  rm(list = ls()[grep("^gini", ls())])
  
  ## Computing unique constituency length AND standardized PNS (PNS_s)
  party.cst_tot <- data.table(party)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")] # Removed id
  party.cst_tot$PNS_s <- party.cst_tot$PNS^(1/(log(party.cst_tot$cst_tot)))
  
  ## Replacing PNS_s with NA if it equals zero
  party.cst_tot$PNS_s <- ifelse(party.cst_tot$PNS_s == 0, NA, party.cst_tot$PNS_s)
  
  ## Computing prerequisites for weighted PNS (PNS_w)
  ## Grabbing unique constituency length, grabbing distinct values, then renaming back to "party"
  party.cst_tot <- as.data.frame(party.cst_tot) # Need to reassign as data.frame type for !duplicated() to work 
  unique_pty_rows <- !duplicated(party.cst_tot[c("id","ctr_n","ctr","yr", "mn","cst","pty")])
  unique.party <- party.cst_tot[unique_pty_rows,]
  party <- unique.party
  
  ## Grabbing nat_vv1, pty_vv1; reassigning to "party"
  party.nat_vv1 <- data.table(party)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn")] # Removed id
  party.pty_vv1 <- data.table(party.nat_vv1)[,pty_vv1 := sum(pv1, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party <- party.pty_vv1
  
  ## PNS_w denominator
  party$denominator <- party$nat_vv1 * party$pty_vv1
  
  ## Ordering by party and vote shares (descending) to compute p_j's cumulative sum; reassigning to "party"
  party$cst_vv1 <- party$vv1
  party.order <- party[order(party$id,party$ctr_n,party$ctr,party$yr,party$mn,party$pty,party$vote.shares),,drop = FALSE]
  party_pj <- data.table(party.order)[,p_j := cumsum(vote.totals), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party <- party_pj
  
  ## Computing the "inside" of the numerator's sum, then computing PNS_w numerator; reassigning to "party"
  party.inside <- data.table(party)[,inside := cst_vv1 * (p_j - (vote.totals / 2)), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id  #inside = cst_vv1 * (p_j - (pv1/2))
  party.numerator <- data.table(party.inside)[,numerator := sum(inside, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party <- party.numerator
  
  ## Computing PNS_w at long last! Replacing infinite/NaN with NA, then reassign to "party" 
  party.pns_w <- data.table(party)[,PNS_w := ((2 * numerator)/ denominator),by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party.pns_w$PNS_w[is.infinite(party.pns_w$PNS_w)] <- NA
  party.pns_w$PNS_w[is.nan(party.pns_w$PNS_w)] <- NA
  party <- party.pns_w

  ## Computing PNS_w AGAIN, but using vote.totals as a proxy for replacement
  party$cst_vv1_new <- party$vote.totals
  party_pj_new <- data.table(party)[,p_j_new := cumsum(vote.totals), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party <- party_pj_new
  party.inside_new <- data.table(party)[,inside_new := cst_vv1_new * (p_j_new - (vote.totals / 2)), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party.numerator_new <- data.table(party.inside_new)[,numerator_new := sum(inside_new, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party <- party.numerator_new
  
  party.pns_w_new <- data.table(party)[,PNS_w_new := ((2 * numerator_new) / denominator), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  party.pns_w_new$PNS_w[is.infinite(party.pns_w_new$PNS_w)] <- NA
  party.pns_w_new$PNS_w[is.nan(party.pns_w_new$PNS_w)] <- NA
  party <- party.pns_w_new
  
  ## Replacing PNS_w under given conditions (see below...)
  party$alt_vv1 <- 0
  party$alt_vv1 <- replace(party$alt_vv1, party$PNS_w >= 1, 1)
  party$PNS_w <- ifelse(party$PNS_w >= 1, party$PNS_w_new, party$PNS_w)
  party$PNS_w <- ifelse(party$PNS_w == 0, NA, party$PNS_w)
  party$PNS_w <- ifelse(party$PNS_w >= 1, party$PNS_w_new, party$PNS_w)
  party$alt_vv1 = replace(party$alt_vv1, is.na(party$PNS_w), 0)
  
  ## Select down to the relevant variables, and continue on to compute standardized/weighted PNS (PNS_sw)
  party.slim <- subset(party, select = c(id,ctr_n,ctr,yr,mn,cst,pty_n,pty,cst_vv1, 
                                         nat_vv1,PNS,PNS_s,PNS_w,alt_vv1,cst_tot))
  party <- party.slim
  
  ## Computing parts of PNS_sw computation
  party$top <- (party$nat_vv1)^2
  party$square <- (party$cst_vv1)^2
  
  party.pid <- data.table(party)[,pid := 1:length(nat_vv1),by=c("ctr_n","ctr","yr","mn","cst")] # Removed id
  party <- party.pid
  party$helper <- rep(NA,length(party$nat_vv1))
  #party$helper <- replace(party$helper, party$pid == 1, party$square)
  party$helper <- ifelse(party$pid == 1, party$square, party$helper)
  
  party.bottom <- data.table(party)[,bottom := sum(helper, na.rm=TRUE), by=c("ctr_n","ctr","yr")] # Removed id
  party <- party.bottom
  party$power_E <- party$top / party$bottom
  
  party.pns_sw <- data.table(party)[,PNS_sw := (PNS_w)^(1/(log10(power_E))), by=c("ctr_n","ctr","yr")] # Removed id
  party <- party.pns_sw
  party$PNS_sw <- replace(party$PNS_sw, is.na(party$PNS_sw), NA)
  party$PNS_sw <- replace(party$PNS_sw, party$PNS_sw > 1, NA)
  
  ## Final clean-up time!
  ## Replacing PNS if it equals 1 (meaning gini equals zero, thus NA); replacing PNS & friends with non-valuable pty codes
  party$PNS <- replace(party$PNS, party$PNS == 1, NA)
  party$PNS_s <- replace(party$PNS_s, party$pty >= 3996 & party$pty < 5000, NA)
  party$PNS_w <- replace(party$PNS_w, party$pty >= 3996 & party$pty < 5000, NA)
  party$PNS_sw <- replace(party$PNS_sw, party$pty >= 3996 & party$pty < 5000, NA)
  
  ## Selecting down to relevant output columns; arranging for viewing
  party.slimmer <- subset(party, select = c(id, ctr_n, ctr, yr, mn, pty_n, pty, PNS, PNS_s, PNS_w, PNS_sw, cst_tot))
  party.measures <- party.slimmer
  
  party.measures <- as.data.frame(party.measures[order(party.measures$ctr_n,party.measures$yr,party.measures$pty),])
  unique_rows_pty <- !duplicated(party.measures[c("id","ctr_n","ctr","yr", "mn","pty")])
  unique.df_pty <- party.measures[unique_rows_pty,]
  party.measures <- unique.df_pty
  
  ## Writing party-level to .xlsx; informing of success
  # write.xlsx(party.measures, paste0(outputFolder, "party.level.xlsx"),
  #            keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("Party-level measures successfully computed! Moving to national-level...", quote = FALSE)

  rm(list = ls()[grep("^party", ls())])
  #print(gc())
  #*
  #browser()
  #* 
  
  #-----------------------------------------#
  # National-Level Nationalization Measures #
  #-----------------------------------------#  
  national.message <- function() {
    print("------------------------------------", quote=FALSE)
    print("Computing National Level Dataset...", quote=FALSE)
    print("------------------------------------", quote=FALSE)
    
  }
  national.message()

  ## Computing ENP & Friends ##
  
  ## Grabbing distinct cases, using data.b computed in subset section
  data.b <- as.data.frame(data.b)
  nat.unique.rows <- !duplicated(data.b[c("id","ctr_n","ctr","yr", "mn","cst","pty")])
  unique.nat <- data.b[nat.unique.rows,]
  data.b <- unique.nat
  
  ## Computing vote totals at a higher level of aggregation (grouping by party)
  nat <- data.table(data.b)[,vote.totals := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","pty")] # Removed id
  
  ## Grabbing unique again to get the data to the national level; renaming to nat
  nat <- as.data.frame(nat)
  nat.unique.rows <- !duplicated(nat[c("id","ctr_n","ctr","yr", "mn","pty")])
  unique.nat <- nat[nat.unique.rows,]
  nat <- unique.nat
  
  ## Computing ENP_nat
  nat.nat_vv1 <- data.table(nat)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn")] # Removed id
  nat.shares_sq <- data.table(nat.nat_vv1)[,party_prop_nat2 := (vote.totals/nat_vv1)^2, by=c("ctr_n","ctr","yr","mn")] # Removed id
  nat.denom <- data.table(nat.shares_sq)[,denominator := sum(party_prop_nat2, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn")] # Removed id
  nat.enep <- data.table(nat.denom)[,ENEP_nat := 1/denominator, by=c("ctr_n","ctr","yr","mn")] # Removed id
  nat.enp <- subset(nat.enep, select = c(id,ctr_n,ctr,yr,mn,ENEP_nat))
  
  ## Computing ENP_cst (using same unique data above)
  nat.new_vv1 <- data.table(nat)[,new_vv1 := sum(vote.totals),by=c("ctr_n","ctr","yr","mn","cst")]
  nat.new_shares_sq <- data.table(nat.new_vv1)[,share_sq := (vote.totals/new_vv1)^2,by=c("ctr_n","ctr","yr","mn","cst")]
  nat.new_denom <- data.table(nat.new_shares_sq)[,denominator := sum(share_sq),by=c("ctr_n","ctr","yr","mn","cst")]
  nat.enep_cst <- data.table(nat.new_denom)[,ENEP_cst := 1/denominator,by=c("ctr_n","ctr","yr","mn","cst")]
  
  ## Adding an indicator if new vv1 (above) does not equal true vv1
  nat.enep_cst$indicator <- 0
  nat.enep_cst$indicator <- ifelse(nat.enep_cst$new_vv1 != nat.enep_cst$vv1,1,0)
  
  ## Grabbing unique elections & constituencies
  nat.enep_cst <- as.data.frame(nat.enep_cst)
  nat.unique.rows_enp <- !duplicated(nat.enep_cst[c("id","ctr_n","ctr","yr", "mn","cst","pty")])
  unique.nat_enp <- nat.enep_cst[nat.unique.rows_enp,]
  nat.enp_unique <- unique.nat_enp
  
  ## Summing the indicator (computed above)
  nat.new_indicator <- data.table(nat.enp_unique)[,indicator := ifelse(sum(indicator, na.rm=TRUE) > 0, 1, indicator),
                                                  by=c("ctr_n","ctr","yr","mn")]
  
  ## Computing weights, then weighted ENP (ENP_wght)
  nat.nat_vv1 <- data.table(nat.new_indicator)[,nat_vv1 := sum(new_vv1, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn")]
  nat.cst_wght <- data.table(nat.nat_vv1)[,cst_wght := (new_vv1/nat_vv1),by=c("ctr_n","ctr","yr","mn")]
  nat.weighted <- data.table(nat.cst_wght)[,weighted := (cst_wght * ENEP_cst),by=c("ctr_n","ctr","yr","mn")]
  nat.enep_wght <- data.table(nat.weighted)[,ENEP_wght := sum(weighted),by=c("ctr_n","ctr","yr","mn")]
  
  ## Now, we compute average ENP (ENP_avg) real quick!
  nat.enep_avg <- data.table(nat.enep_wght)[,ENEP_avg := mean(ENEP_cst),by=c("ctr_n","ctr","yr","mn")]
  nat.enp_avg_wght <- as.data.frame(nat.enep_avg)
  
  
  #test <- merge.data.frame(nat.enp_avg_wght,nat.enp,by = c("ctr_n","ctr","yr","mn"),all.y = TRUE)
  #
    
  
  
  
  
  #*
  browser()
  #* 
  
  ## Writing national-level to .xlsx; informing of success
  # write.xlsx(nat.measures, paste0(outputFolder, "national.level.xlsx"),
  #            keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("National-level measures successfully computed! Moving to constituency-level...", quote = FALSE)
  
  ## Printing timer to display efficiency
  cat("The entire computation took ", (proc.time()[1]-start.timer[1])/60, "mins \n")
  print("Done!", quote = FALSE)
}

