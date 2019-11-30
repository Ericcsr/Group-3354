library(haven)

# 1. Read Data
## HealthStatus.2015 <- read_dta("Health_Status_and_Functioning.dta")
IndInc.2015 <- read_dta("Individual_Income.dta")
HouInc.2015 <- read_dta("Household_Income.dta")

# 2. Process NA and empty columns
## only maintain the columns with less than half are NA...:
HouInc.2015.v1 <- Filter(function(x){ length(which(!is.na(x)))/length(x) > 0.5}, HouInc.2015)
IndInc.2015.v1 <- Filter(function(x){ length(which(!is.na(x)))/length(x) > 0.5}, IndInc.2015)
## only maintain the columns with less than half are empty cells:
HouInc.2015.v2 <- Filter(function(x){ length(which(x!=""))/length(x) > 0.5 }, HouInc.2015.v1)
IndInc.2015.v2 <- Filter(function(x){ length(which(x!=""))/length(x) > 0.5 }, IndInc.2015.v1)

# 3. Select usable features
## For Household dataset: HouInc.2015.v2:
### detect if most of row in columns are "No":
for (i in c(4,5,36:39,46:48,67)) {
  print (length(which(HouInc.2015.v2[i]==2))/length(which(HouInc.2015.v2[i]==1)))
} #so column 5,38,39,46-48 should be deleted (almost all are "No")
for (i in c(6,19,22,24,27,29:35,50,64)) {
  print (length(which(HouInc.2015.v2[i]==0))/length(which(HouInc.2015.v2[i]!=0)))
  print (sum(is.na(HouInc.2015.v2[i])))
} #19,31,34,35 should be deleted, as almost all 0; through 29 is almost all 0, but it has statidtical indication, so remained.
### delete columns: 7-9,42-44,49,51-54,59-63,65:67(meaningless), 5,19,31,34,35,38,39,46-48(almost all "No")
HouInc.2015.v3 <- HouInc.2015.v2[ -c(5,7:9,19,31,34,35,38,39,42:44,46:49,51:54,59:63,65:67) ]

## For Individual dataset: IndInc.2015.v2:
### detect if most of row in columns are "No":
for (i in c(8:16, 20)) {
  print (length(which(IndInc.2015.v2[i]==2))/length(which(IndInc.2015.v2[i]==1)))
} #so columns 8-15 should be deleted (almost all values are 2), 16 and 20 can be remained
### combine column 18 and 19 of IndInc.2015.v2
IndInc.2015.v2$hd003_004_w3 <- IndInc.2015.v2$hd003 + IndInc.2015.v2$hd004_w3
### delete columns: 5, 21, 22(meaningless), 8-15(almost all "No"), 18-19(combined to new column)
IndInc.2015.v3 <- IndInc.2015.v2[ -c(5,8:15, 18, 19, 21, 22) ]

# 4. change from 1/2 to 1/0:
two2zero <- function(df, index) {
  for (i in index) {
    g <- df[i]
    g[g==2] <- 0
    df[i] <- g
  }
  return(df)
}
HouInc.2015.v3 <- two2zero(HouInc.2015.v3, c(4,9,29))

IndInc.2015.v3 <- two2zero(IndInc.2015.v3, c(4,7,9))

# 5. Special process for Household dataset, individual in the household should be combined:
## but they are almost all NA, so no need to append to household dataset.
for (j in c(1,3,5,6)) {
  value <- 0
  for (i in c(1,2,3,4,5,6,7,8,9,10,11,12)) {
    cname <- paste("ga007_", toString(i), "_s",toString(j), sep="")
    cvalue <- HouInc.2015[c(cname)]
    cvalue[is.na(cvalue)] <- 0
    value <- value + cvalue/j
  }
  print (lapply(value, function(x){ length(which(x==0))/length(x)}))
}

# 6. Write to csv
write_csv(HouInc.2015.v3,"Household_and_individual_data/household2.csv")
write_csv(IndInc.2015.v3,"Household_and_individual_data/individual2.csv")

# 7. Combine the csv according to the household id
total <- merge(IndInc.2015.v3,HouInc.2015.v3,by="ID")
write_csv(total,"Household_and_individual_data/household_and_individual_merged.csv")

## the process to "Everything.csv"
library(readr)
Everything_new <- read_csv("Everything_new.csv")
every <- Everything_new[-c(1,7,15,16,18:26,28:30,34,35,37:46)]
every_simple <- every[-c(19,46,58)]
e_nona <- na.omit(every_simple)
write_csv(e_nona,"Sorted data/Everything_noNA.csv")
