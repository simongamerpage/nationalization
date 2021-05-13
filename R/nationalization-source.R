#' @title Nationalization
#'
#' @author Simon Page
#'
#' @description This function creates and populates datasets of party nationalization measures,
#'              using CLEA data with output at the national-level, party-level, and the
#'              constituency-level.
#'
#' @param dataSource Character string of the location of the CLEA data to be used for nationalization measure generation.
#' @param dataFormat Format of the data file being read. Acceptable formats are the following: ".csv",".xlsx",".rdata", or ".dta"
#' @param outputFolder Character string of the location where the user desires the output files to be written on their local machine. Note that four files will be generated at the
#' end of this script (i.e., party-level, national-level, constituency-level, and Gini inequality measures).
#' @param inequalityType Optional argument, which defaults to "Gini" to compute Gini measures of inequality. Other options are available (i.e., "RS","Atkinson","Theil","Kolm",
#' "var","square.var","entropy"), but "Gini" is highly recommended.
#' @param CandidateOrPartyBased Optional argument, which defaults to "party.based"; this argument accepts only "party.based" or "candidate.based", which allows the user to proxy
#' candidate votes/candidate shares for party votes/party shares. This is indended for the advanced user, and "party.based" is highly recomended for accurate measures.
#' @param filterSmallParties Optional argument, which defaults to `TRUE`. Generally, this package filters out parties that did not achieve at least five percent (`5%`) of the
#' national vote before computing measures of nationalization. However, the user can opt to maintain all parties in the output.
#' @return
#' @export
#'
#' @import data.table
#' @import haven
#' @import readxl
#' @import ineq
#' @import openxlsx

nationalization <- function(dataSource,
                            dataFormat,
                            outputFolder,
                            inequalityType = "Gini",
                            CandidateOrPartyBased = "party.based",
                            filterSmallParties = FALSE) {

  # Initial message & starting efficiency timer
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

  # Function to load packages/install if they do not exist on local machine
  # ONLY relevant if running the script elsewhere than GitHub package; commented out
  # usePackage <- function(required.package) {
  #   if (!is.element(required.package, installed.packages()[,1]))
  #     install.packages(required.package, dep = TRUE)
  #   require(required.package, character.only = TRUE)
  # }
  #
  # usePackage("data.table") # For processing functions (more efficient than base R)
  # usePackage("haven") # For reading Stata
  # usePackage("readxl") # For reading Excel
  # usePackage("ineq") # For Gini computation
  # usePackage("openxlsx") # To write to .xlsx (desired output)

  # Checking data file format, then loading respective data
  ifelse(dataFormat == ".csv", dat <- read.csv(dataSource),
         ifelse(dataFormat == ".xlsx", dat <- read_xlsx(dataSource),
                ifelse(dataFormat == ".rdata", assign("dat",get(load(dataSource))),
                       ifelse(dataFormat == ".dta", dat <- read_dta(dataSource),{
                         print("No appropriate data format selected.", quote = FALSE); stop()
                       }))))

  # Making sure inequality index is "Gini" if not specified; enabling other inequality options as well
  if (inequalityType != "Gini" & inequalityType %in% c("Gini", "RS", "Atkinson", "Theil", "Kolm",
                                                       "var", "square.var", "entropy") == FALSE) {
      # Coerce to Gini if the argument is present, but not in the pre-approved list
      inequalityType <- "Gini"
    }

  #----------------------#
  # Subset Data by Needs #
  #----------------------#

  # Selecting crucial columns as a base
  base <- subset(dat, select = c(id,ctr_n,ctr,yr,mn,cst,cst_n,pty,pty_n,vv1,pv1,pvs1,cv1,cvs1,tier,seat))

  # Coercing argument to "party.based" if the selection is not within pre-approved possibilities
  if ((CandidateOrPartyBased != "party.based" |
       CandidateOrPartyBased != "candidate.based" |
       CandidateOrPartyBased != "shares.based")) {
      # Coerce to "party.based" if the argument is present, but not in the pre-approved options
      CandidateOrPartyBased <- "party.based"
    }

  # Creating the base for gini and party-level computations called data.a,
  #... based on user preference of party, candidate, or shares based measures
  if (CandidateOrPartyBased == "party.based") {

    # Removing -990s for NAs, indicating absent candidates, etc...
    data.a <- subset(base,select = -c(cv1,cvs1))
    data.a$can <- 0
    data.a$vv1 <- ifelse(data.a$vv1 == -990 | data.a$vv1 == -992 | data.a$vv1 == -994, NA, data.a$vv1)
    data.a$pv1 <- ifelse(data.a$pv1 == -990 | data.a$pv1 == -992 | data.a$pv1 == -994, NA, data.a$pv1)
    data.a$pvs1 <- ifelse(data.a$pvs1 == -990 | data.a$pvs1 == -992 | data.a$pvs1 == -994, NA, data.a$pvs1)
    data.a$na_candidate <- ifelse(is.na(data.a$vv1) & is.na(data.a$pv1),1,0)
    data.a <- unique(data.a)

    # Computing shares on our own, once, to avoid broken shares in data
    data.a$pvs1 <- data.a$pv1 / data.a$vv1
    data.a <- data.a[order(data.a$cst), , drop = FALSE]
    data.a <- data.a[order(data.a$ctr_n,data.a$yr),]

    # Renaming to generic totals/shares, then informing user of success/moving along with party-based
    names(data.a)[names(data.a) == "pv1"] <- "vote.totals"
    names(data.a)[names(data.a) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for Gini & party-level measures...", quote = FALSE)

  } else (
    if (CandidateOrPartyBased == "candidate.based") {

      # Removing -990s for NAs, indicating absent candidates, etc...
      data.a <- subset(base,select = -c(pv1,pvs1))
      data.a$vv1 <- ifelse(data.a$vv1 == -990 | data.a$vv1 == -992 | data.a$vv1 == -994, NA, data.a$vv1)
      data.a$cv1 <- ifelse(data.a$cv1 == -990 | data.a$cv1 == -992 | data.a$cv1 == -994, NA, data.a$cv1)
      data.a$cvs1 <- ifelse(data.a$cvs1 == -990 |data.a$cvs1 == -992 | data.a$cvs1 == -994, NA, data.a$cvs1)
      data.a$na_candidate <- ifelse(is.na(data.a$vv1) & is.na(data.a$cv1),1,0)
      data.a <- unique(data.a)

      # Renaming to generic totals/shares, then informing user of success/moving along with candidate-based
      names(data.a)[names(data.a) == "cv1"] <- "vote.totals"
      names(data.a)[names(data.a) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for Gini & party-level measures...", quote = FALSE)

    } else (
      if (CandidateOrPartyBased == "shares.based") {
        print("IN DEVELOPMENT"); stop()
      }
    )
  )

  # Creating the base for national-level ENP AND constituency-level computations called data.b,
  #... based on user preference of party, candidate, or shares based measures
  if (CandidateOrPartyBased == "party.based") {

    # Removing -990s for NAs, indicating absent candidates, etc...
    data.b <- subset(base, select = -c(cv1,cvs1))
    data.b$can <- 0
    data.b$vv1 <- ifelse(data.b$vv1 == -990 | data.b$vv1 == -992 | data.b$vv1 == -994, NA, data.b$vv1)
    data.b$pv1 <- ifelse(data.b$pv1 == -990 | data.b$pv1 == -992 | data.b$pv1 == -994 | data.b$pv1 == 0, NA, data.b$pv1) # Note removal of zero here
    data.b$pvs1 <- ifelse(data.b$pvs1 == -990 | data.b$pvs1 == -992 | data.b$pvs1 == -994, NA, data.b$pvs1)
    data.b$na_candidate <- ifelse(is.na(data.b$vv1) | is.na(data.b$pv1),1,0)

    # Computing unique constituency totals BEFORE filtering out NA candidates
    data.b <- as.data.frame(data.b)
    data.b <- data.table(data.b)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

    # Grabbing unique cases across entire frame, then filtering non-existent candidates
    data.b <- unique(data.b)
    data.b <- subset(data.b, na_candidate == 0)

    # Renaming to generic totals/shares, then informing the user of success/moving along with party-based
    names(data.b)[names(data.b) == "pv1"] <- "vote.totals"
    names(data.b)[names(data.b) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for national-level ENP measures...", quote = FALSE)
    print("Sucessfully created subset for constituency-level measures...", quote = FALSE)

  } else (
    if (CandidateOrPartyBased == "candidate.based") {

      # Removing -990s for NAs, indicating absent candidates, etc...
      data.b <- subset(base, select = -c(pv1,pvs1))
      data.b$can <- 0
      data.b$vv1 <- ifelse(data.b$vv1 == -990 | data.b$vv1 == -992 | data.b$vv1 == -994, NA, data.b$vv1)
      data.b$pv1 <- ifelse(data.b$cv1 == -990 | data.b$cv1 == -992 | data.b$cv1 == -994 | data.b$cv1 == 0, NA, data.b$pv1) # Note removal of zero here
      data.b$pvs1 <- ifelse(data.b$cvs1 == -990 | data.b$cvs1 == -992 | data.b$cvs1 == -994 | data.b$cvs1 == 0, NA, data.b$pvs1)
      data.b$na_candidate <- ifelse(is.na(data.b$vv1) | is.na(data.b$cv1),1,0) # Note the OR here

      # Comptuing unique constituency totals BEFORE filtering out NA candidates
      data.b <- as.data.frame(data.b)
      data.b <- data.table(data.b)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

      # Grabbing unique cases across entire frame, then filtering non-existent candidates
      data.b <- unique(data.b)
      data.b <- subset(data.b, na_candidate == 0)

      # Renaming to generic totals/shares, then informing the user of success/moving along with party-based
      names(data.b)[names(data.b) == "cv1"] <- "vote.totals"
      names(data.b)[names(data.b) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for national-level ENP measures...", quote = FALSE)
      print("Sucessfully created subset for constituency-level measures...", quote = FALSE)

    } else (
      if (CandidateOrPartyBased == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )

  # Creating the base for national-level PSNS computations called data.c,
  #... based on user preference of party, candidate, or shares based measures
  if (CandidateOrPartyBased == "party.based") {

    # Removing -990s for NAs, indicating absent candidates, etc...
    data.c <- subset(base, select = -c(cv1,cvs1))
    data.c$can <- 0
    data.c$vv1 <- ifelse(data.c$vv1 == -990 | data.c$vv1 == -992 | data.c$vv1 == -994, NA, data.c$vv1)
    data.c$pv1 <- ifelse(data.c$pv1 == -990 | data.c$pv1 == -992 | data.c$pv1 == -994 | data.c$pv1 == 0, NA, data.c$pv1)
    data.c$pvs1 <- ifelse(data.c$pvs1 == -990 | data.c$pvs1 == -992 | data.c$pvs1 == -994 | data.c$pvs1 == 0, NA, data.c$pvs1)
    data.c$na_candidate <- ifelse(is.na(data.c$vv1) & is.na(data.c$pv1),1,0)

    # Computing unique constituency totals BEFORE filtering out NA candidates
    data.c <- as.data.frame(data.c)
    data.c <- data.table(data.c)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

    # Grabbing unique cases across entire frame, then filtering non-existent candidates
    data.c <- unique(data.c)
    data.c <- subset(data.c, na_candidate == 0)

    # Renaming to generic totals/shares, then informing the user of success/moving along with party-based
    names(data.c)[names(data.c) == "pv1"] <- "vote.totals"
    names(data.c)[names(data.c) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for national-level inflation/PSNS measures...", quote = FALSE)

  } else (
    if (CandidateOrPartyBased == "candidate.based") {

      # Removing -990s for NAs, indicating absent candidates, etc...
      data.c <- subset(base, select = -c(pv1,pvs1))
      data.c$can <- 0
      data.c$vv1 <- ifelse(data.c$vv1 == -990 | data.c$vv1 == -992 | data.c$vv1 == -994, NA, data.c$vv1)
      data.c$pv1 <- ifelse(data.c$cv1 == -990 | data.c$cv1 == -992 | data.c$cv1 == -994 |data.c$cv1 == 0, NA, data.c$cv1)
      data.c$pvs1 <- ifelse(data.c$cvs1 == -990 | data.c$cvs1 == -992 | data.c$cvs1 == -994 |data.c$cvs1 == 0, NA, data.c$cvs1)
      data.c$na_candidate <- ifelse(is.na(data.c$vv1) & is.na(data.c$cv1),1,0)

      # Computing unique constituency totals BEFORE filtering out NA candidates
      data.c <- as.data.frame(data.c)
      data.c <- data.table(data.c)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

      # Grabbing unique cases across entire frame, then filtering non-existent candidates
      data.c <- unique(data.c)
      data.c <- subset(data.c, na_candidate == 0)

      # Renaming to generic totals/shares, then informing the user of success/moving along with party-based
      names(data.c)[names(data.c) == "cv1"] <- "vote.totals"
      names(data.c)[names(data.c) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for national-level inflation/PSNS measures...", quote = FALSE)

    } else (
      if (CandidateOrPartyBased == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )

  # Creating a base for national-level local_E computations called data.d,
  #... based on user preference of party, candidate, or shares based measures
  if (CandidateOrPartyBased == "party.based") {

    # Removing -990s for NAs, indicating absent candidates, etc...
    data.d <- subset(base, select = -c(cv1,cvs1))
    data.d$can <- 0
    data.d$vv1 <- ifelse(data.d$vv1 == -990 | data.d$vv1 == -992 | data.d$vv1 == -994, NA, data.d$vv1)
    data.d$pv1 <- ifelse(data.d$pv1 == -990 | data.d$pv1 == -992 | data.d$pv1 == -994 | data.d$pv1 == 0, NA, data.d$pv1)
    data.d$pvs1 <- ifelse(data.d$pvs1 == -990 | data.d$pvs1 == -992 | data.d$pvs1 == -994 | data.d$pvs1 == 0, NA, data.d$pvs1)
    data.d$seat <- ifelse(data.d$seat == -990 | data.d$seat == -992 | data.d$seat == -994 | data.d$seat == 0, NA, data.d$seat)
    data.d$na_candidate <- ifelse(is.na(data.d$vv1) & is.na(data.d$pv1) & is.na(data.d$seat),1,0)

    # Computing unique constituency totals BEFORE filtering out NA candidates
    data.d <- as.data.frame(data.d)
    data.d <- data.table(data.d)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

    # Grabbing unique cases across entire frame, then filtering non-existent candidates
    data.d <- unique(data.d)
    data.d <- subset(data.d, na_candidate == 0)

    # Renaming to generic totals/shares, then informing the user of success/moving along with party-based
    names(data.d)[names(data.d) == "pv1"] <- "vote.totals"
    names(data.d)[names(data.d) == "pvs1"] <- "vote.shares"
    print("Sucessfully created subset for national-level local_E measures...", quote = FALSE)

  } else (
    if (CandidateOrPartyBased == "candidate.based") {

      # Removing -990s for NAs, indicating absent candidates, etc...
      data.d <- subset(base, select = -c(pv1,pvs1))
      data.d$can <- 0
      data.d$vv1 <- ifelse(data.d$vv1 == -990 | data.d$vv1 == -992 | data.d$vv1 == -994, NA, data.d$vv1)
      data.d$pv1 <- ifelse(data.d$cv1 == -990 | data.d$cv1 == -992 | data.d$cv1 == -994 | data.d$cv1 == 0, NA, data.d$cv1)
      data.d$pvs1 <- ifelse(data.d$cvs1 == -990 | data.d$cvs1 == -992 | data.d$cvs1 == -994 | data.d$cvs1 == 0, NA, data.d$cvs1)
      data.d$seat <- ifelse(data.d$seat == -990 | data.d$seat == -992 | data.d$seat == -994 | data.d$seat == 0, NA, data.d$seat)
      data.d$na_candidate <- ifelse(is.na(data.d$vv1) & is.na(data.d$cv1) & is.na(data.d$seat),1,0)

      # Computing unique constituency totals BEFORE filtering out NA candidates
      data.d <- as.data.frame(data.d)
      data.d <- data.table(data.d)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]

      # Grabbing unique cases across entire frame, then filtering non-existent candidates
      data.d <- unique(data.d)
      data.d <- subset(data.d, na_candidate == 0)

      # Renaming generic totals/shares, then informing the user of success/moving along with party-based
      names(data.d)[names(data.d) == "cv1"] <- "vote.totals"
      names(data.d)[names(data.d) == "cvs1"] <- "vote.shares"
      print("Sucessfully created subset for national-level local_E measures...", quote = FALSE)

    } else (
      if (CandidateOrPartyBased == "shares.based") {
        print("IN DEVELOPMENT");stop()
      }
    )
  )

  #--------------------------#
  # Gini Inequality Measures #
  #--------------------------#
  ineq.message <- function() {
    print("--------------------------------", quote=FALSE)
    print("Computing inequality measures...", quote=FALSE)
    print("--------------------------------", quote=FALSE)
  }
  ineq.message()

  # Grabbing distinct cases, then renaming to "gini." plus the variable/data being created
  gini.unique_rows <- !duplicated(data.a[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  gini.base <- data.a[gini.unique_rows,]

  # Grabbing valid votes at the national-level to filter small parties (less than 5% of the national vote)
  gini.totals <- data.table(gini.base)[,vote.totals := sum(vote.totals, na.rm = TRUE), by=c("id","ctr_n","ctr","yr","mn","cst","pty","tier")]
  gini.vv1 <- data.table(gini.totals)[,vv1 := sum(vv1, na.rm = TRUE),by=c("id","ctr_n","ctr","yr","mn","cst","pty","tier")]
  gini.nat_vv1 <- data.table(gini.vv1)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","tier")]
  gini.pty_nat_vv1 <- data.table(gini.nat_vv1)[,pty_nat_vv1 := sum(vote.totals),by=c("ctr_n","ctr","yr","mn","pty","tier")]
  gini.pty_nat_vv1$nat_pvs <- gini.pty_nat_vv1$pty_nat_vv1 / gini.pty_nat_vv1$nat_vv1

  # Filtering small parties that do not meet the aforementioned threshold, based on user preference
  if (filterSmallParties == TRUE) {
    # Variant 1: Argument is specified and filter out small
    gini.small_removed <- subset(gini.pty_nat_vv1,nat_pvs > 0.05)
  } else (
    if (filterSmallParties == FALSE) {
      # Variant 2: Argument is specified and NO filtering small parties
      gini.small_removed <- gini.pty_nat_vv1
    }
  )

  # Recomputing vote shares ONCE to avoid possible broken shares; replacing NaN/NA/infinite with zero
  gini.small_removed$vote.shares <- gini.small_removed$vote.totals / gini.small_removed$vv1
  gini.small_removed$vote.shares <- ifelse(is.infinite(gini.small_removed$vote.shares), NA, gini.small_removed$vote.shares)
  gini.small_removed$vote.shares <- ifelse(is.na(gini.small_removed$vote.shares), 0, gini.small_removed$vote.shares)

  # Computing Gini inequality (note, computed by tier in multi-PR systems), then removing NA/NaN
  gini.ineq <- setDT(gini.small_removed)[,giniI := ineq(vote.shares, NULL,type = inequalityType, na.rm = TRUE),by=c("ctr_n","ctr","yr","mn","pty","tier")]
  gini.ineq$giniI <- replace(gini.ineq$giniI, gini.ineq$giniI < 0, 0)
  gini.ineq$giniI <- replace(gini.ineq$giniI, gini.ineq$giniI == "NaN", 0)

  # Cleaning the measures up, ordering, then shipping!
  # Note that "full." plus anything will be the name of final products being written to .xlsx
  full.gini <- subset(gini.ineq, select = c(id,ctr_n,ctr,yr,mn,tier,cst,pty,giniI,vote.totals,vv1))
  names(full.gini)[names(full.gini) == "vote.totals"] <- ifelse(CandidateOrPartyBased == "party.based","pv1",
                                                                ifelse(CandidateOrPartyBased == "candidate.based","cv1",
                                                                       ifelse(CandidateOrPartyBased == "shares.based","shares",NA)))
  full.gini <- full.gini[order(full.gini$cst), , drop = FALSE]
  full.gini <- full.gini[order(full.gini$ctr_n,full.gini$yr,full.gini$tier),]

  # Writing Gini to .xlsx; informing of success
  write.xlsx(full.gini, paste0(outputFolder, "gini.values.xlsx"),
             keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("Gini inequality measures successfully computed! Moving to party-level...", quote = FALSE)

  # Removing anything starting with "gini." from the local environment for memory efficiency
  rm(list = ls()[grep("^gini", ls())])

  #--------------------------------------#
  # Party-Level Nationalization Measures #
  #--------------------------------------#
  party.message <- function() {
    print("---------------------------------", quote=FALSE)
    print("Computing Party Level Dataset...", quote=FALSE)
    print("---------------------------------", quote=FALSE)
  }
  party.message()

  # Grabbing distinct cases, using same data.a (computed above in subset section); then,
  # renaming to "pty." plus the variable/data being created
  pty.unique_rows <- !duplicated(data.a[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  pty.base <- data.a[pty.unique_rows,]

  # Merging in gini, then using it to compute vanilla PNS
  pty.gini <- merge(pty.base, full.gini, by = c("id","ctr_n","ctr","yr","mn","cst","pty","vv1","tier"),all.x = TRUE)
  pty.gini$PNS <- 1 - pty.gini$giniI

  # Computing unique constituency length AND standardized PNS (PNS_s)
  pty.cst_tot <- data.table(pty.gini)[,cst_tot := length(unique(cst)), by=c("ctr_n","ctr","yr","mn")]
  pty.cst_tot$PNS_s <- pty.cst_tot$PNS^(1/(log(pty.cst_tot$cst_tot)))

  # Replacing PNS_s with NA if it equals zero
  pty.cst_tot$PNS_s <- ifelse(pty.cst_tot$PNS_s == 0, NA, pty.cst_tot$PNS_s)

  # Computing prerequisites for weighted PNS (PNS_w)
  # Grabbing distinct cases
  pty.cst_tot <- as.data.frame(pty.cst_tot) # Need to reassign as data.frame type for !duplicated() to work
  pty.cst_tot_unique_rows <- !duplicated(pty.cst_tot[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  pty.unique_cst_tot <- pty.cst_tot[pty.cst_tot_unique_rows,]

  # Grabbing nationl-level vv1 and party vv1 (nat_vv1, pty_vv1)
  pty.nat_vv1 <- data.table(pty.unique_cst_tot)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","tier")]
  pty.pty_vv1 <- data.table(pty.nat_vv1)[,pty_vv1 := sum(pv1, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.national <- pty.pty_vv1

  # PNS_w denominator
  pty.national$denominator <- pty.national$nat_vv1 * pty.national$pty_vv1

  # Ordering by party and vote shares (descending) to compute p_j's cumulative sum
  pty.national$cst_vv1 <- pty.national$vv1
  pty.ordered <- pty.national[order(pty.national$id,pty.national$ctr_n,pty.national$ctr,pty.national$yr,
                                    pty.national$mn,pty.national$pty,pty.national$vote.shares),,drop = FALSE]
  pty.p_j <- data.table(pty.ordered)[,p_j := cumsum(vote.totals), by=c("ctr_n","ctr","yr","mn","pty","tier")]

  # Computing the "inside" of the numerator's sum, then computing PNS_w numerator
  pty.inside <- data.table(pty.p_j)[,inside := cst_vv1 * (p_j - (vote.totals / 2)), by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.numerator <- data.table(pty.inside)[,numerator := sum(inside, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn","pty","tier")]

  # Computing PNS_w at long last! Replacing infinite/NaN with NA
  pty.pns_w <- data.table(pty.numerator)[,PNS_w := ((2 * numerator)/ denominator),by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.pns_w$PNS_w[is.infinite(pty.pns_w$PNS_w)] <- NA
  pty.pns_w$PNS_w[is.nan(pty.pns_w$PNS_w)] <- NA

  # Computing PNS_w AGAIN, but using vote.totals as a proxy for replacement
  pty.pns_w$cst_vv1_new <- pty.pns_w$vote.totals
  pty.pj_new <- data.table(pty.pns_w)[,p_j_new := cumsum(vote.totals), by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.inside_new <- data.table(pty.pj_new)[,inside_new := cst_vv1_new * (p_j_new - (vote.totals / 2)), by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.numerator_new <- data.table(pty.inside_new)[,numerator_new := sum(inside_new, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn","pty","tier")]

  pty.pns_w_new <- data.table(pty.numerator_new)[,PNS_w_new := ((2 * numerator_new) / denominator), by=c("ctr_n","ctr","yr","mn","pty","tier")]
  pty.pns_w_new$PNS_w[is.infinite(pty.pns_w_new$PNS_w)] <- NA
  pty.pns_w_new$PNS_w[is.nan(pty.pns_w_new$PNS_w)] <- NA

  # Replacing inappropriate PNS_w under given conditions (see below...)
  pty.pns_w_new$alt_vv1 <- 0
  pty.pns_w_new$alt_vv1 <- replace(pty.pns_w_new$alt_vv1, pty.pns_w_new$PNS_w >= 1, 1)
  pty.pns_w_new$PNS_w <- ifelse(pty.pns_w_new$PNS_w >= 1, pty.pns_w_new$PNS_w_new, pty.pns_w_new$PNS_w)
  pty.pns_w_new$PNS_w <- ifelse(pty.pns_w_new$PNS_w == 0, NA, pty.pns_w_new$PNS_w)
  pty.pns_w_new$PNS_w <- ifelse(pty.pns_w_new$PNS_w >= 1, pty.pns_w_new$PNS_w_new, pty.pns_w_new$PNS_w)
  pty.pns_w_new$alt_vv1 = replace(pty.pns_w_new$alt_vv1, is.na(pty.pns_w_new$PNS_w), 0)

  # Select down to the relevant variables, and continue on to compute standardized-weighted PNS (PNS_sw)
  pty.slim <- subset(pty.pns_w_new, select = c(id,ctr_n,ctr,yr,mn,cst,pty_n,pty,cst_vv1,
                                               nat_vv1,PNS,PNS_s,PNS_w,alt_vv1,cst_tot,tier))

  # Computing parts of PNS_sw computation
  pty.slim$top <- (pty.slim$nat_vv1)^2
  pty.slim$square <- (pty.slim$cst_vv1)^2

  pty.pid <- data.table(pty.slim)[,pid := 1:length(nat_vv1),by=c("ctr_n","ctr","yr","mn","cst","tier")]
  pty.pid$helper <- rep(NA,length(pty.pid$nat_vv1))
  pty.pid$helper <- ifelse(pty.pid$pid == 1, pty.pid$square, pty.pid$helper)

  pty.bottom <- data.table(pty.pid)[,bottom := sum(helper, na.rm=TRUE), by=c("ctr_n","ctr","yr","tier")]
  pty.bottom$power_E <- pty.bottom$top / pty.bottom$bottom

  # Computing PNS_sw
  pty.pns_sw <- data.table(pty.bottom)[,PNS_sw := (PNS_w)^(1/(log10(power_E))), by=c("ctr_n","ctr","yr","tier")]
  pty.pns_sw$PNS_sw <- replace(pty.pns_sw$PNS_sw, is.na(pty.pns_sw$PNS_sw), NA)
  pty.pns_sw$PNS_sw <- replace(pty.pns_sw$PNS_sw, pty.pns_sw$PNS_sw > 1, NA)

  # Final clean-up time! Creating "pty.to_clean" for this
  # Replacing PNS if it equals 1 (meaning gini equals zero, thus NA); replacing PNS & friends with non-valuable pty codes
  pty.to_clean <- pty.pns_sw
  pty.to_clean$PNS <- replace(pty.to_clean$PNS, pty.to_clean$PNS == 1, NA)
  pty.to_clean$PNS_s <- replace(pty.to_clean$PNS_s, pty.to_clean$pty >= 3996 & pty.to_clean$pty < 5000, NA)
  pty.to_clean$PNS_w <- replace(pty.to_clean$PNS_w, pty.to_clean$pty >= 3996 & pty.to_clean$pty < 5000, NA)
  pty.to_clean$PNS_sw <- replace(pty.to_clean$PNS_sw, pty.to_clean$pty >= 3996 & pty.to_clean$pty < 5000, NA)

  # Selecting down to relevant output columns; arranging for viewing
  pty.slimmer <- subset(pty.to_clean, select = c(id, ctr_n, ctr, yr, mn, pty_n, pty, tier, PNS, PNS_s, PNS_w, PNS_sw, cst_tot))
  full.pty <- pty.slimmer

  full.pty <- as.data.frame(full.pty[order(full.pty$ctr_n,full.pty$yr,full.pty$pty,full.pty$tier),])
  pty.full_rows <- !duplicated(full.pty[,c("id","ctr_n","ctr","yr", "mn","pty","tier")])
  full.pty <- full.pty[pty.full_rows,]

  # Writing party-level to .xlsx; informing of success
  write.xlsx(full.pty, paste0(outputFolder, "party.level.xlsx"),
             keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("Party-level measures successfully computed! Moving to national-level...", quote = FALSE)

  # Removing anything starting with "pty." from the local environment for memory efficiency
  rm(list = ls()[grep("^pty", ls())])

  #-----------------------------------------#
  # National-Level Nationalization Measures #
  #-----------------------------------------#
  national.message <- function() {
    print("------------------------------------", quote=FALSE)
    print("Computing National Level Dataset...", quote=FALSE)
    print("------------------------------------", quote=FALSE)

  }
  national.message()

  ##*************************##
  ## Computing ENP & Friends ##
  ##*************************##

  # Grabbing distinct cases, using data.b computed in subset section
  data.b <- as.data.frame(data.b)
  nat.unique.rows <- !duplicated(data.b[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  enp.distinct <- data.b[nat.unique.rows,]

  # Renaming to "enp." plus the variable/data being created for ENP section
  enp.totals <- data.table(enp.distinct)[,vote.totals := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","pty")]

  # Grabbing unique again to get the data to the national level
  enp.totals <- as.data.frame(enp.totals)
  enp.unique.rows <- !duplicated(enp.totals[c("id","ctr_n","ctr","yr", "mn","pty","tier")])
  enp.unique <- enp.totals[enp.unique.rows,]

  # Computing ENP_nat, then selecting down to relevant columns
  enp.nat_vv1 <- data.table(enp.unique)[,nat_vv1 := sum(vote.totals, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","tier")]
  enp.shares_sq <- data.table(enp.nat_vv1)[,party_prop_nat2 := (vote.totals/nat_vv1)^2, by=c("ctr_n","ctr","yr","mn","tier")]
  enp.denom <- data.table(enp.shares_sq)[,denominator := sum(party_prop_nat2, na.rm = TRUE), by=c("ctr_n","ctr","yr","mn","tier")]
  enp.enep_nat <- data.table(enp.denom)[,ENEP_nat := 1/denominator, by=c("ctr_n","ctr","yr","mn","tier")]
  enp.enep_nat <- subset(enp.enep_nat, select = c(id,ctr_n,ctr,yr,mn,tier,ENEP_nat,cst))

  # Computing ENP_cst (enp.distinct computed above)
  enp.new_vv1 <- data.table(enp.distinct)[,new_vv1 := sum(vote.totals),by=c("ctr_n","ctr","yr","mn","cst","tier")]
  enp.new_shares_sq <- data.table(enp.new_vv1)[,share_sq := (vote.totals/new_vv1)^2,by=c("ctr_n","ctr","yr","mn","cst","tier")]
  e.new_denom <- data.table(enp.new_shares_sq)[,denominator := sum(share_sq),by=c("ctr_n","ctr","yr","mn","cst","tier")]
  enp.enep_cst <- data.table(e.new_denom)[,ENEP_cst := 1/denominator,by=c("ctr_n","ctr","yr","mn","cst","tier")]

  # Adding an indicator if new vv1 (above) does not equal true vv1
  enp.enep_cst$indicator <- 0
  enp.enep_cst$indicator <- ifelse(enp.enep_cst$new_vv1 != enp.enep_cst$vv1,1,0)

  # Grabbing unique ENP_cst
  enp.enep_cst <- as.data.frame(enp.enep_cst)
  enp.unique.rows_enp <- !duplicated(enp.enep_cst[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  enp.unique_nat <- enp.enep_cst[enp.unique.rows_enp,]

  # Summing the indicator (computed above)
  enp.new_indicator <- data.table(enp.unique_nat)[,indicator := ifelse(sum(indicator, na.rm=TRUE) > 0, 1, indicator),
                                                  by=c("ctr_n","ctr","yr","mn","tier")]

  # Computing weights, then weighted ENP (ENP_wght)
  enp.new_nat_vv1 <- data.table(enp.new_indicator)[,nat_vv1 := sum(new_vv1, na.rm=TRUE),by=c("ctr_n","ctr","yr","mn","tier")]
  enp.cst_wght <- data.table(enp.new_nat_vv1)[,cst_wght := (new_vv1/nat_vv1),by=c("ctr_n","ctr","yr","mn","tier")]
  enp.weighted <- data.table(enp.cst_wght)[,weighted := (cst_wght * ENEP_cst),by=c("ctr_n","ctr","yr","mn","tier")]
  enp.enep_wght <- data.table(enp.weighted)[,ENEP_wght := sum(weighted),by=c("ctr_n","ctr","yr","mn","tier")]

  # Now, we compute average ENP (ENP_avg) using ENP_cst
  enp.enep_avg <- data.table(enp.enep_wght)[,ENEP_avg := mean(ENEP_cst),by=c("ctr_n","ctr","yr","mn","tier")]
  enp.enp_avg_wght <- as.data.frame(enp.enep_avg)

  # Merging both ENP-related sets, then cleaning them up
  enp.merge <- merge.data.frame(enp.enep_nat,enp.enp_avg_wght,by = c("id","ctr_n","ctr","yr","mn","cst","tier"))

  enp.slim <- subset(enp.merge, select = c(id,ctr_n,ctr,yr,mn,tier,ENEP_nat,ENEP_avg,ENEP_wght,indicator))
  enp.slim_rows <- !duplicated(enp.slim[c("id","ctr_n","ctr","yr", "mn","tier")])
  enp.full <- enp.slim[enp.slim_rows,]
  full.enp <- enp.full[order(enp.full$ctr_n,enp.full$yr,enp.full$mn,enp.full$tier),]

  # Creating Cox, MK_I, & MK_I_w
  full.enp$cox <- (full.enp$ENEP_nat - full.enp$ENEP_avg)/ full.enp$ENEP_nat
  full.enp$MK_I <- (full.enp$ENEP_nat -  full.enp$ENEP_avg)/ full.enp$ENEP_avg
  full.enp$MK_I_w <- (full.enp$ENEP_nat - full.enp$ENEP_wght)/ full.enp$ENEP_wght

  # Removing previous computations for memory efficiency (beginning with "enp.")
  rm(list = ls()[grep("^enp", ls())])

  ##**************************##
  ## Computing PSNS & friends ##
  ##**************************##

  # Grabbing distinct cases, using data.c computed in subset section
  data.c <- as.data.frame(data.c)
  nat.unique.rows <- !duplicated(data.c[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  psns.distinct <- data.c[nat.unique.rows,]

  # Computing ENP_cst (using unique data above)
  psns.new_vv1 <- data.table(psns.distinct)[,new_vv1 := sum(vote.totals),by=c("ctr_n","ctr","yr","mn","cst","tier")]
  psns.new_shares_sq <- data.table(psns.new_vv1)[,share_sq := (vote.totals/new_vv1)^2,by=c("ctr_n","ctr","yr","mn","cst","tier")]
  psns.new_denom <- data.table(psns.new_shares_sq)[,denominator := sum(share_sq, na.rm = TRUE),by=c("ctr_n","ctr","yr","mn","cst","tier")]
  psns.enep_cst <- data.table(psns.new_denom)[,ENEP_cst := 1/denominator,by=c("ctr_n","ctr","yr","mn","cst","tier")]

  # Merging this ENP_cst with the full ENP above at the national-level
  psns.full_enp <- merge(psns.enep_cst,full.enp, by = c("id","ctr_n","ctr","yr","mn","tier"))

  # Renaming to just "psns" for easier typing, then computing prerequisites for inflation values (there are many)
  psns <- psns.full_enp
  psns$I_i <- ((psns$ENEP_nat-psns$ENEP_cst)/ psns$ENEP_cst) * 100
  psns$alpha <- 0.5
  psns$beta <- 0.25
  psns$gamma <- 0.5

  psns.nat_vote <- data.table(psns)[, nat_vote := sum(new_vv1, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.cst_vote_prop <- data.table(psns.nat_vote)[,cst_vote_prop := new_vv1/nat_vote, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.product <- data.table(psns.cst_vote_prop)[,product := ENEP_cst *  cst_vote_prop, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.sum_cst <- data.table(psns.product)[,sum_cst := sum(product, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.denom <- data.table(psns.sum_cst)[,denominator := cst_tot * sum_cst, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.W_tilde <- data.table(psns.denom)[,W_tilde := ENEP_cst/ denominator, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.I_w <- data.table(psns.W_tilde)[,I_w := ((ENEP_nat - sum_cst)/sum_cst)*100, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.numerator <- data.table(psns.I_w)[,numerator := (I_i -  I_w)^2 * W_tilde, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.sum_numerator <- data.table(psns.numerator)[,sum_numerator := sum(numerator, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.coeff_var_I_i <- data.table(psns.sum_numerator)[,coeff_var_I_i := sqrt(sum_numerator)/I_w, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.numerator_r2 <- data.table(psns.coeff_var_I_i)[,numerator_r2 := ((I_i - I_w)^4) * W_tilde, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.sum_numerator_r2 <- data.table(psns.numerator_r2)[,sum_numerator_r2 := sum(numerator_r2, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.denominator_r2 <- data.table(psns.sum_numerator_r2)[,denominator2 := ((I_i - I_w)^2) * W_tilde, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.sum_denominator_r2 <- data.table(psns.denominator_r2)[,sum_denominator_r2 := sum(denominator2, na.rm=TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.sum_denominator_r2$sq_sum_denominator_r2 <- (psns.sum_denominator_r2$sum_denominator_r2)^2
  psns.kurtosis_I_i <- data.table(psns.sum_denominator_r2)[,kurtosis_I_i := sum_numerator_r2/sq_sum_denominator_r2, by = c("ctr_n","ctr","yr","mn","tier")]
  psns.D <- data.table(psns.kurtosis_I_i)[, D := abs(coeff_var_I_i)^gamma * abs(kurtosis_I_i)^(1-gamma) * sign(coeff_var_I_i*kurtosis_I_i),
                                    by = c("ctr_n","ctr","yr","mn","tier")]
  psns.MK_N <- data.table(psns.D)[, MK_N := abs(I_w)^alpha * abs(D)^(1-alpha) * sign(I_w*D), by = c("ctr_n","ctr","yr","mn","tier")]

  # NOT naming with "psns." to maintain the final object with all columns
  MK_N_two <- data.table(psns.MK_N)[, MK_N_two := abs(I_w)^alpha * abs(coeff_var_I_i)^beta * abs(kurtosis_I_i)^(1 - alpha - beta) * sign(I_w*coeff_var_I_i*kurtosis_I_i),
                                   by = c("ctr_n","ctr","yr","mn","tier")]

  # Removing previously-computed objects to maintain memory efficiency
  rm(list = ls()[grep("^psns", ls())])

  # A few more prerequisites, grabbing distinct cases again, then merging in party.level measures
  psns.nat_vv1 <- data.table(MK_N_two)[, nat_vv1 := sum(vote.totals, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.pty_vv1 <- data.table(psns.nat_vv1)[, pty_vv1 := sum(vote.totals, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","pty","tier")]
  psns.pty_vv1 <- as.data.frame(psns.pty_vv1)

  psns.unique.rows <- !duplicated(psns.pty_vv1[,c("id","ctr_n","ctr","yr", "mn","pty","tier")])
  psns.unique <- psns.pty_vv1[psns.unique.rows,]

  psns.pty_merge <- merge(psns.unique,full.pty,by = c("id", "ctr_n", "ctr", "yr", "mn", "pty", "pty_n", "cst_tot","tier"),all.x = TRUE)

  # Ordering by party, computing weights, then computing PSNS & friends
  psns.pty_merge <- psns.pty_merge[order(psns.pty_merge$ctr_n,psns.pty_merge$ctr,psns.pty_merge$yr,
                                      psns.pty_merge$mn,psns.pty_merge$pty,psns.pty_merge$pty_n),]
  psns.pty_merge$weight <- psns.pty_merge$pty_vv1/psns.pty_merge$nat_vv1

  psns.psns <- data.table(psns.pty_merge)[, PSNS := sum(PNS * weight, na.rm = TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.psns_s <- data.table(psns.psns)[, PSNS_s := sum(PNS_s * weight, na.rm=TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.psns_w <- data.table(psns.psns_s)[, PSNS_w := sum(PNS_w * weight, na.rm=TRUE), by = c("ctr_n","ctr","yr","mn","tier")]
  psns.psns_sw <- data.table(psns.psns_w)[, PSNS_sw := sum(PNS_sw *weight, na.rm=TRUE), by = c("ctr_n","ctr","yr","mn","tier")]

  # Replacing PSNS & friends with NA if they equal zero or are greater than 1
  psns.to_edit <- psns.psns_sw
  psns.to_edit$PSNS <- ifelse(psns.to_edit$PSNS == 0 | psns.to_edit$PSNS > 1, NA, psns.to_edit$PSNS)
  psns.to_edit$PSNS_s <- ifelse(psns.to_edit$PSNS_s == 0 | psns.to_edit$PSNS_s > 1, NA, psns.to_edit$PSNS_s)
  psns.to_edit$PSNS_w <- ifelse(psns.to_edit$PSNS_w == 0 | psns.to_edit$PSNS_w > 1, NA, psns.to_edit$PSNS_w)
  psns.to_edit$PSNS_sw <- ifelse(psns.to_edit$PSNS_sw == 0 | psns.to_edit$PSNS_sw > 1, NA, psns.to_edit$PSNS_sw)

  # Selecting relevant columns, then grabbing unique again
  psns.slim <- subset(psns.to_edit, select = c(id,ctr_n,ctr,yr,mn,tier,PSNS,PSNS_s,PSNS_w,PSNS_sw,MK_N,MK_N_two,cst_tot))
  psns.slim <- as.data.frame(psns.slim)
  psns.unique_rows <- !duplicated(psns.slim)
  full.psns <- psns.slim[psns.unique_rows,]

  # Removing previously-computed psns objects AGAIN to maintain memory efficiency
  rm(list = ls()[grep("^psns", ls())])

  ##*******************##
  ## Computing local_E ##
  ##*******************##

  # Grabbing distinct cases
  data.d <- as.data.frame(data.d)
  local.unique_rows <- !duplicated(data.d[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  local.unique <- data.d[local.unique_rows,]

  # Renaming to "local." plus the variable/data being created
  local.nat_vote <- data.table(local.unique)[, nat_vote := sum(vote.totals, na.rm=TRUE), by = c("ctr_n","ctr","yr", "mn","tier")]
  local.seat_cst <- data.table(local.nat_vote)[, seat_cst := sum(seat, na.rm=TRUE), by = c("ctr_n","ctr","yr", "mn","cst","tier")]
  local.party_vote_prop <- data.table(local.seat_cst)[, party_vote_prop := vote.totals/nat_vote, by = c("ctr_n","ctr","yr", "mn","cst","tier")]

  # Arranging, then computing seat proportion & party vote proportion
  local.party_vote_prop <- local.party_vote_prop[order(local.party_vote_prop$ctr_n,local.party_vote_prop$ctr,
                                                       local.party_vote_prop$yr,local.party_vote_prop$mn,
                                                       local.party_vote_prop$cst,local.party_vote_prop$pty)]
  local.seat_total <- data.table(local.party_vote_prop)[, seat_total := sum(seat, na.rm=TRUE), by = c("ctr_n","ctr","yr", "mn","tier")]
  local.seat_contest <- data.table(local.seat_total)[, seat_contest := sum(seat_cst, na.rm=TRUE), by = c("ctr_n","ctr","yr", "mn","pty","tier")]
  local.seat_prop <- data.table(local.seat_contest)[, seat_prop := seat_contest/seat_total, by = c("ctr_n","ctr","yr", "mn","pty","tier")]

  # Computing local_E by party & tier, then getting national sum
  local.local_E <- data.table(local.seat_prop)[, local_E := party_vote_prop * seat_prop, by = c("ctr_n","ctr","yr", "mn","pty","tier")]
  local.local_E_r2 <- data.table(local.local_E)[, local_E := sum(local_E, na.rm=TRUE), by = c("ctr_n","ctr","yr", "mn","tier")]

  # Selecting down to relevant columns, then grabbing distinct
  local.local_E_r2 <- subset(local.local_E_r2, select = c(id,ctr_n,ctr,yr,mn,tier,local_E))
  local.local_E_r2 <- as.data.frame(local.local_E_r2)
  local_E_rows <- !duplicated(local.local_E_r2[,c("id","ctr_n","ctr","yr", "mn","tier")])
  full.local_E <- local.local_E_r2[local_E_rows,]

  # Removing previous computations for more efficient memory usage
  rm(list = ls()[grep("^local", ls())])

  ##************************************##
  ## Merging/Cleaning up National-Level ##
  ##************************************##

  # Merging all of the national level ("full")!
  nat.enp_psns_merge <- merge(full.enp,full.psns,by = c("id","ctr_n","ctr","yr","mn","tier"),
                        all = TRUE, sort = TRUE)
  nat.full <- merge(nat.enp_psns_merge,full.local_E, by = c("id","ctr_n","ctr","yr","mn","tier"), all = TRUE)

  # Renaming columns to inflation[1-4] & indicator to nvvi, then cleaning up inflation/nvvi values (zero are NA)
  names(nat.full)[names(nat.full) == "cox"] <- "inflation1"
  names(nat.full)[names(nat.full) == "MK_I"] <- "inflation2"
  names(nat.full)[names(nat.full) == "MK_I_w"] <- "inflation3"
  names(nat.full)[names(nat.full) == "MK_N"] <- "inflation4"
  names(nat.full)[names(nat.full) == "indicator"] <- "nvvi"

  nat.full$inflation1 <- ifelse(nat.full$inflation1 == 0, NA, nat.full$inflation1)
  nat.full$inflation2 <- ifelse(nat.full$inflation2 == 0, NA, nat.full$inflation2)
  nat.full$inflation3 <- ifelse(nat.full$inflation3 == 0, NA, nat.full$inflation3)
  nat.full$inflation4 <- ifelse(nat.full$inflation4 == 0, NA, nat.full$inflation4)
  nat.full$nvvi <- ifelse(is.na(nat.full$nvvi), 0, nat.full$nvvi)

  # Replacing local_E to be within the appropriate range
  nat.full$local_E <- ifelse(nat.full$local_E ==0 | nat.full$local_E > 1, NA, nat.full$local_E)

  # Joining in data grid (with parties, etc.)
  grid <- as.data.frame(subset(base, select = c(id,ctr_n,ctr,yr,mn,tier,cst,cst_n,pty,pty_n)))
  full.nat <- merge(nat.full,grid, by = c("id","ctr_n","ctr","yr","mn","tier"), all = TRUE)

  # Grabbing unique, ordering, renaming ENP, then shipping!
  full.nat_rows <- !duplicated(full.nat[,c("id","ctr_n","ctr","yr", "mn","tier")])
  full.nat <- full.nat[full.nat_rows,]

  full.nat <- full.nat[order(full.nat$ctr_n,full.nat$yr,full.nat$tier),]

  names(full.nat)[names(full.nat) == "ENEP_nat"] <- "ENP_nat"
  names(full.nat)[names(full.nat) == "ENEP_avg"] <- "ENP_avg"
  names(full.nat)[names(full.nat) == "ENEP_wght"] <- "ENP_wght"

  full.nat <- subset(full.nat, select = c(id, ctr_n, ctr, yr, mn, tier, nvvi, ENP_nat, ENP_avg, ENP_wght, inflation1, inflation2,
                                          inflation3,inflation4, PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E, cst_tot))

  # Removing previous computations for more efficient memory usage
  rm(list = ls()[grep("^nat", ls())])

  # Writing national-level to .xlsx; informing of success
  write.xlsx(full.nat, paste0(outputFolder, "national.level.xlsx"),
             keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("National-level measures successfully computed! Moving to constituency-level...", quote = FALSE)

  #---------------------------------------------#
  # Constituency-Level Nationalization Measures #
  #---------------------------------------------#
  cst.message <- function() {
    print("---------------------------------------", quote=FALSE)
    print("Computing Constituency Level Dataset...", quote=FALSE)
    print("---------------------------------------", quote=FALSE)
  }
  cst.message()

  # Grabbing distinct cases, then renaming to "cst." plus the variable/data being created
  cst.unique_rows <- !duplicated(data.b[,c("id","ctr_n","ctr","yr", "mn","cst","pty","tier")])
  cst.base <- data.b[cst.unique_rows,]

  # Computing prerequisites for ENP_cst
  cst.cst_tot <- data.table(cst.base)[, cst_tot := length(unique(cst)), by = c("ctr_n","ctr","yr", "mn")]
  cst.new_vv1 <- data.table(cst.cst_tot)[, new_vv1 := sum(vote.totals, na.rm = TRUE), by = c("ctr_n","ctr","yr", "mn","cst","tier")]
  cst.share_sq <- data.table(cst.new_vv1)[, share_sq := (vote.totals/new_vv1)^2, by = c("ctr_n","ctr","yr", "mn","cst","tier")]
  cst.denom <- data.table(cst.share_sq)[, denom := sum(share_sq), by = c("ctr_n","ctr","yr", "mn","cst","tier")]

  # Computing ENP_cst
  cst.enp_cst <- data.table(cst.denom)[, ENP_cst := 1/denom, by = c("ctr_n","ctr","yr", "mn","cst","tier")]

  # Computing the vv1 indicator, cvvi, selecting down to relevant columns, then grabbing distinct cases again
  cst.enp_cst$cvvi <- 0
  cst.enp_cst$cvvi <- ifelse(cst.enp_cst$vv1 != cst.enp_cst$new_vv1,1,cst.enp_cst$cvvi)
  cst.enp_cst <- subset(cst.enp_cst, select = c(id,ctr_n,ctr,yr,mn,tier,cst,cst_n,ENP_cst,cvvi))

  cst.enp_cst <- as.data.frame(cst.enp_cst)
  cst.unique_rows_enp <- !duplicated(cst.enp_cst[,c("id","ctr_n","ctr","yr", "mn","cst","tier")])
  cst.enp <- cst.enp_cst[cst.unique_rows_enp,]

  # Merging in national-level to compute inflation5
  cst.nat <- merge(cst.enp,full.enp, by = c("id","ctr_n","ctr","yr", "mn","tier"), all = TRUE)
  cst.nat <- as.data.frame(cst.nat)
  cst.nat$inflation5 <- (cst.nat$ENEP_nat - cst.nat$ENP_cst)/cst.nat$ENP_cst

  # Merging in data grid to get party info
  cst.nat.pty <- merge(cst.nat,grid,by = c("id","ctr_n","ctr","yr", "mn","cst","cst_n","tier"), all = TRUE)

  # Selecting down to relevant columns, arranging, grabbing distinct, then shipping!
  cst.slim <- subset(cst.nat.pty, select = c(id,ctr_n,ctr,yr,mn,tier,cst,cst_n,cvvi,ENP_cst))
  cst.slim <- cst.slim[order(cst.slim$ctr_n,cst.slim$yr,cst.slim$mn,cst.slim$cst,cst.slim$tier),]

  cst.slim_unique_rows <- !duplicated(cst.slim[,c("id","ctr_n","ctr","yr", "mn","cst","tier")])
  full.cst <- cst.slim[cst.slim_unique_rows,]

  # Writing constituency-level to .xlsx; informing of success
  write.xlsx(full.cst, paste0(outputFolder, "constituency.level.xlsx"),
             keepNA = TRUE) # `keepNA` = TRUE maintains NAs in output file
  print("Constituency-level measures successfully computed!", quote = FALSE)

  # Removing anything starting with "cst." from the local environment for memory efficiency
  rm(list = ls()[grep("^cst", ls())])

  # Printing timer to display efficiency
  cat("The entire computation took ", (proc.time()[1]-start.timer[1])/60, "mins \n")
  print("Done!", quote = FALSE)
}
