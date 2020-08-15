### Data Dictionary Exploration
### kbmorales@protonmail.com
###


# Setup -------------------------------------------------------------------

# Data prep should be performed first
# source("bin/ppp_data_merge.R")
# based on source above, working directory should be: /DataKind-DC/CARES/

library(tidyverse)
library(lubridate)
library(knitr)
library(stringdist) # for fuzzy matching

# Summaries ---------------------------------------------------------------

glimpse(adbs)

# Data Check: LoanRange_Unified -------------------------------------------

# evaluate how many Loan Range values are sensible
table(adbs$LoanRange_Unified, useNA="always")


# Data Check: ZipCodes ----------------------------------------------------

# generate table to check lengths and also run a simple grep ZIP validator
table(grepl("\\d{5}([ \\-]\\d{4})?", adbs$Zip), nchar(adbs$Zip),useNA = "always", dnn = c("Passes Simple ZIP Validation","Number of Characters"))

# OUTPUT:
#                             Number of Characters
# Passes Simple ZIP Validation        5    <NA>
#                         FALSE       0     224
#                         TRUE  4885164       0
#                         <NA>        0       0
#                        
# result indicates that all present values are of valid length as simple 5 digit zips, however 224 are missing entirely and are recorded in original data as NAs
# let's code this into a new variable, so that we can later evaluate each row for validation along various checks

adbs <- adbs %>% 
  mutate(Zip_Valid_Format = case_when(grepl("\\d{5}", Zip)        ~ "Pass: 5 Digit Format",
                                      grepl("\\d{5}-\\d{4}", Zip) ~ "Pass: 5dash4 Digit Format ",
                                      TRUE ~ "Fail"))

table(adbs$Zip_Valid_Format, useNA = "always")

# let's validate against a large list of ZIPs from: https://simplemaps.com/data/us-cities
uszips <- read.csv("data/simplemaps_uszips_basicv1.72/uszips.csv")

length(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", uszips$zip))])            # 42,580 rows without a zip in the list
length(unique(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", uszips$zip))]))    # 5,036 unique unmatched zips
sample(unique(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", uszips$zip))]), 5) # this tends to give what are, according to Google Maps, valid ZIPs. Perhaps the simplemaps list is too old...
# we do not appear to be much closer, but at least the error rate isn't horrible (42,000 out of 4,800,000)

# instead let's use the file 'zip_to_zcta_2019.csv' and see if we get better coverage
ztz <- read.csv("data/Lookup Tables/zip_to_zcta_2019.csv")

length(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", ztz$ZIP_CODE))])            # 1,627 rows without a zip in the list
length(unique(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", ztz$ZIP_CODE))]))    # 331 unique unmatched zips
sample(unique(adbs$Zip[(!adbs$Zip %in% sprintf("%05d", ztz$ZIP_CODE))]), 5) # this sampling does not seem to be all valid zips!
# we appear to be much closer using this list

### Data Check: State Names -----------------------------------------------
# check against US Census data: American National Standards Institute (ANSI) Codes for States, the District of Columbia, Puerto Rico, and the Insular Areas of the United States
#via: https://www.census.gov/library/reference/code-lists/ansi.html
statecodes <- read.table("data/census/state.txt", sep = "|", header = TRUE)

table(adbs$State[(!adbs$State %in% statecodes$STUSAB)]) # list counts of unmatched results, showing 1 AE, 1 FI, 210 XX

adbs[adbs$State == "FI",] # based on the ZIP code, this should be FL, and can be 'fixed' easily enough as part of final data cleaning
adbs[adbs$State == "AE",] # when viewing ZIP, Lending data, this does indeed appear to be tied to a military address in Europe, Middle East, Africa or Canada

# check how the State field validates against the filename (source State)
adbs %>% 
  filter(State != str_remove_all(source_file, "^.*- \\s*|\\s*.csv.*$")) %>% 
    subset(select=c("State","source_file")) %>% 
      ftable()  
# we see that the only source files with unmatched values are 150kplus and Other, which is coherent
# interestingly, entries with state value XX appear in both the 150kplus and the Other source files


### Data Check: City Names -------------------------------------------------
# check City values against a large list of likely names, via: https://simplemaps.com/data/us-cities
uscities <- read.csv("data/simplemaps_uscities_basicv1.6/uscities.csv")

citydict <- sort(unique(tolower(gsub("[[:digit:][:space:][:punct:]]", "", uscities$city))))
adbscities <- sort(unique(tolower(gsub("[[:digit:][:space:][:punct:]]", "", adbs$City))))

citymatch_01 <- amatch(adbscities, citydict, method = "lv", maxDist = 0.1)
citymatch_05 <- amatch(adbscities, citydict, method = "lv", maxDist = 0.5)
citymatch_10 <- amatch(adbscities, citydict, method = "lv", maxDist = 1.0)

adbscities <- as.data.frame(adbscities)
adbscities$match_01 <- citydict[citymatch_01]
adbscities$match_05 <- citydict[citymatch_05]
adbscities$match_10 <- citydict[citymatch_10]

# this output is showing issues: for example, "schicago" matches to chicago, but really it is more likely to be "South Chicago"
# a hand built list, with the above as a baseline, may be most effective.

### Data Check: Jobs Retained ----------------------------------------------

#confirm all Jobs Retained values are integers (whole numbers) or NAs
summary(near(as.numeric(adbs$JobsRetained), as.integer(as.numeric(adbs$JobsRetained))))

# Coerce -------------------------------------------------------------------

### Coersions
adbs = adbs %>% 
  mutate(JobsRetained = as.numeric(JobsRetained),
         DateApproved = as.Date(DateApproved,
                                 "%m/%d/%Y"),
         LoanAmount = as.numeric(LoanAmount))


# Data Check: Date Approved  ------------------------------------------------
table(adbs$DateApproved, useNA="always")


# Duplicates --------------------------------------------------------------

### Duplicates

## Takes a long time! Be patient.

# sum(duplicated(adbs))
# 4353 exact duplicates

# adbs_dupes=adbs[duplicated(adbs),]
