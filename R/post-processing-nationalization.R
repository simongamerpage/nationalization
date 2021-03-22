#' Post-Processing Nationalization Measures
#'
#' This function serves to process completed nationalization measures to remove a number of
#' inappropriate values or values that are not valuable for analysis. This function mostly
#' cherry-picks specific cases, removing measures that do not make sense. In the future, this
#' script could/should evolve to include more cases/problematic measures to filter, making
#' this script an ever-evolving processing function.
#'
#' @param folderLocation Character string of the location in which nationalization data exists on the user's local machine. Note that this folder should be the location
#' where ALL the files should be (i.e., "C:/Users/Name/Downloads/" if the files all exist in the downloads folder).
#' @param removeSingleConstituencies Option for the user to remove countries that are a single, national constituency (i.e., countries/elections that are aggregated at the
#' national-level, where `cst_tot == 1`) because a number of measures hold no value for countries aggregated nationally. This argument accepts `TRUE` or `FALSE` exclusively.
#' @param rewriteLocation Character string of the folder location where the user would like the new, post-processed nationalization data to be rewritten. *NOTE* that if
#' this argument is left blank/not-specified, the rewrite will take place in the `folderLocation` location, OVERWRITING the existing data files.
#' @return
#' @export

post.processing <- function(folderLocation,
                            removeSingleConstituencies,
                            rewriteLocation) {
  # Initial message & starting the efficiency timer
  initial.message <- function() {
    print("----------------------------------------------------------", quote = FALSE)
    print("Initializing... Loading Pacakges & Nationalization Data...", quote = FALSE)
    print("----------------------------------------------------------", quote = FALSE)
  }
  initial.message()
  start.timer <- proc.time()

  # Function to load packages/install if they do not exist on local machine
  usePackage <- function(required.package) {
    if (!is.element(required.package, installed.packages()[,1]))
      install.packages(required.package, dep = TRUE)
    require(required.package, character.only = TRUE)
  }

  usePackage("data.table")
  usePackage("readxl")
  usePackage("openxlsx")

  #-----------------------------------------------------------#
  # Loading Files, Data, and Interpreting Function Parameters #
  #-----------------------------------------------------------#

  # Grabbing the list of files of nationalization measures; informing of files selected
  files <- sort(list.files(folderLocation),decreasing = TRUE)
  print("Loading the following files for processing:", quote = FALSE)
  print(files)

  # Loading nationalization data sets
  pty.level <- read_xlsx(paste0(folderLocation,files[1]),guess_max = 10000)
  nat.level <- read_xlsx(paste0(folderLocation,files[2]),guess_max = 10000)
  cst.level <- read_xlsx(paste0(folderLocation,files[4]),guess_max = 10000)

  # Filtering national-constituencies if removeSingleConstituencies == TRUE
  if (removeSingleConstituencies == FALSE |
      removeSingleConstituencies == "" |
      is.na(removeSingleConstituencies) |
      is.null(removeSingleConstituencies)) {
    print("Note that single-constituency elections are being included, which can be altered via the removeSingleConstituencies argument...", quote = FALSE)
  } else (
    if (removeSingleConstituencies == TRUE) {
      pty.level <- subset(pty.level, cst_tot != 1)
      nat.level <- subset(nat.level, cst_tot != 1)

      # Computing cst_tot quickly, then filtering
      cst.level.cst_tot <- data.table(cst.level)[, cst_tot := length(unique(cst)),
                                                 by = c("ctr_n","ctr","yr","mn")]
      cst.level <- subset(cst.level.cst_tot, cst_tot != 1)

      print("Note that single-constituency elections are being removed via the removeSingleConstituencies argument...", quote = FALSE)
    }
  )

  #---------------------------------#
  # Processing Party-Level Measures #
  #---------------------------------#
  pty.message <- function() {
    print("----------------------------------", quote = FALSE)
    print("Processing Party-Level Measures...", quote = FALSE)
    print("----------------------------------", quote = FALSE)
  }
  pty.message()

  pty.level$PNS_w <- ifelse(pty.level$pty == 3999 |
                              pty.level$pty >= 6000 |
                              pty.level$ctr == 292 |
                              pty.level$ctr == 376 |
                              pty.level$ctr == 674 |
                              (pty.level$ctr == 840 & pty.level$yr == 1959),
                            NA, pty.level$PNS_w)

  print("Party-level measures processed! Moving to the national-level processing...", quote = FALSE)

  #------------------------------------#
  # Processing National-Level Measures #
  #------------------------------------#
  nat.message <- function() {
    print("-------------------------------------", quote = FALSE)
    print("Processing National-Level Measures...", quote = FALSE)
    print("-------------------------------------", quote = FALSE)
  }
  nat.message()

  # Filtering ENP & friends for elections in the United States prior to 1834 and
  #... an election in Korea in 1950 (not a democratic election)
  nat.level$ENP_nat <- ifelse((nat.level$ctr == 840 & nat.level$yr < 1834),
                              NA, nat.level$ENP_nat)
  nat.level$ENP_nat <- ifelse((nat.level$ctr == 410 & nat.level$yr == 1950),
                              NA, nat.level$ENP_nat)
  nat.level$ENP_avg <- ifelse((nat.level$ctr == 840 & nat.level$yr < 1834),
                              NA, nat.level$ENP_avg)
  nat.level$ENP_avg <- ifelse((nat.level$ctr == 410 & nat.level$yr == 1950),
                              NA, nat.level$ENP_avg)
  nat.level$ENP_wght <- ifelse((nat.level$ctr == 840 & nat.level$yr < 1834),
                              NA, nat.level$ENP_wght)
  nat.level$ENP_wght <- ifelse((nat.level$ctr == 410 & nat.level$yr == 1950),
                              NA, nat.level$ENP_wght)

  # Filtering ENP & friends for a number of Sri Lankan elections
  nat.level$ENP_nat <- ifelse(
    (nat.level$ctr == 144 & nat.level$yr == 1947) |
      (nat.level$ctr == 144 & nat.level$yr == 1952) |
      (nat.level$ctr == 144 & nat.level$yr == 1956) |
      (nat.level$ctr == 144 & nat.level$yr == 1960) |
      (nat.level$ctr == 144 & nat.level$yr == 1965) |
      (nat.level$ctr == 144 & nat.level$yr == 1970) |
      (nat.level$ctr == 144 & nat.level$yr == 1977),
    NA, nat.level$ENP_nat
  )
  nat.level$ENP_avg <- ifelse(
    (nat.level$ctr == 144 & nat.level$yr == 1947) |
      (nat.level$ctr == 144 & nat.level$yr == 1952) |
      (nat.level$ctr == 144 & nat.level$yr == 1956) |
      (nat.level$ctr == 144 & nat.level$yr == 1960) |
      (nat.level$ctr == 144 & nat.level$yr == 1965) |
      (nat.level$ctr == 144 & nat.level$yr == 1970) |
      (nat.level$ctr == 144 & nat.level$yr == 1977),
    NA, nat.level$ENP_avg
  )
  nat.level$ENP_wght <- ifelse(
    (nat.level$ctr == 144 & nat.level$yr == 1947) |
      (nat.level$ctr == 144 & nat.level$yr == 1952) |
      (nat.level$ctr == 144 & nat.level$yr == 1956) |
      (nat.level$ctr == 144 & nat.level$yr == 1960) |
      (nat.level$ctr == 144 & nat.level$yr == 1965) |
      (nat.level$ctr == 144 & nat.level$yr == 1970) |
      (nat.level$ctr == 144 & nat.level$yr == 1977),
    NA, nat.level$ENP_wght
  )

  print("National-level measures processed! Moving to the constituency-level processing...", quote = FALSE)

  #----------------------------------------#
  # Processing Constituency-Level Measures #
  #----------------------------------------#
  cst.message <- function() {
    print("-----------------------------------------", quote = FALSE)
    print("Processing Constituency-Level Measures...", quote = FALSE)
    print("-----------------------------------------", quote = FALSE)
  }
  cst.message()

  cst.level$ENP_cst <- ifelse((cst.level$ctr == 840 & cst.level$yr < 1834),
                              NA, cst.level$ENP_cst)

  print("Constituency-level measures processed! Time to rewrite...", quote = FALSE)

  #---------------------------#
  # Rewriting post-processing #
  #---------------------------#
  writing.message <- function() {
    print("-----------------------------", quote = FALSE)
    print("Writing Processed Measures...", quote = FALSE)
    print("-----------------------------", quote = FALSE)
  }
  writing.message()

  # Rewriting, based on selection in rewriteLocation argument
  if (isFALSE(hasArg(rewriteLocation))) {
    print("Note that the rewriteLocation argument is either missing/NULL, thus data is being overwritten to the following folder location:", quote = FALSE)
    print(folderLocation)

    write.xlsx(pty.level,paste0(folderLocation,files[1]))
    write.xlsx(nat.level,paste0(folderLocation,files[2]))
    write.xlsx(cst.level,paste0(folderLocation,files[4]))

    print("Files sucessfully rewritten over existing national-level,constituency-level, and party-level data!", quote = FALSE)
  } else (
    {
      print("Writing post-proccessed files to the following location:", quote = FALSE)
      print(rewriteLocation)

      write.xlsx(pty.level,paste0(rewriteLocation,files[1]))
      write.xlsx(nat.level,paste0(rewriteLocation,files[2]))
      write.xlsx(cst.level,paste0(rewriteLocation,files[4]))

      print("Files sucessfully processed and written!", quote = FALSE)
    }
  )

  # Printing timer to display efficiency
  cat("This entire post-processing took ", (proc.time()[1]-start.timer[1])/60, "mins \n")
  print("Done!", quote = FALSE)
}
