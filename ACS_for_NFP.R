#########################
###    Alex Jutca     ###
### NFP Market Sizing ###
###  Using ACS 2013   ###
###      3/18/15      ###
#########################
# clear all
rm(list=ls())

# where's working director?
getwd()

# disable scientific notation
options(scipen=999)

# import data
require(foreign)
acs2013 <- read.dta("C:/Users/ajutca/Downloads/usa_00023.dta")

# form a unique identifier
acs2013$serialnew<-acs2013$serial*100000000
acs2013$uniqueid<-acs2013$serialnew + acs2013$pernum

# check for duplicates
dups <- acs2013[duplicated(acs2013$uniqueid),]
dim(dups)

# drop some vars (irrelevant)
drops <- c("city", "bpl", "bpld")
acs2013 <- acs2013[, !(names(acs2013) %in% drops)]

# convert age to numeric and fix for added year
acs2013$age <- as.numeric(acs2013$age) - 1 

# convert fertyr to logical
acs2013$fertyr <- ifelse(acs2013$fertyr=="Yes", TRUE, FALSE)

# adjust citizen to be more informative
acs2013$citizen <- factor(as.character(acs2013$citizen))
levels(acs2013$citizen)[2] <- "US Born"

# import data on medicaid income eligibility thresholds by State
medicaid.data <- read.csv("medicaid_FPL_eligibility.csv")
drops <- c("Parents")
medicaid.data <- medicaid.data[, !(names(medicaid.data) %in% drops)]

# merge with the ACS data by state
acs <- merge(x=medicaid.data, y=acs2013, by.x="State", by.y="statefip", all=TRUE)

# create indicator for Medicaid eligibility for women who gave birth in past 12 months (income as % FPL <= eligibility threshold)
acs$med.elig <- ifelse(acs$fertyr == TRUE & acs$poverty <= acs$PregnantWomen, 1, 0)

# convert nchild to numeric and correct for the extra addition
acs$nchild <- as.numeric(acs$nchild) - 1

# convert eldch to numeric and fix for added year
acs$eldch <- as.numeric(acs$eldch) - 1
acs$yngch <- as.numeric(acs$yngch) - 1
#acs$eldch <- ifelse(acs$eldch==99, NA, acs$eldch)

####################################################
###       NFP Market Sizing Using 5 Methods      ###
####################################################
### Musts:                                       ###
### FertyrL == TRUE                              ###
### Sex == "Female" (should be implied by above) ###
### Nchild == 1                                  ###
### Age of child <=1                             ###
###                                              ###
### Scenarios:                                   ###
### 1. Medicaid enrolled                         ###
### 2. Pov <= Medicaid FPL during pregnancy      ###
### 3. !(Medicaid) & Pov <=  Medicaid threshold  ###
### 4. Poverty <= 100                            ###
### 5. Poverty <= 100 & (15 <= age <= 24)        ###
### NB: 5 is their actual client base.           ###
####################################################

# universe of moms who gave birth last year
moms <- acs[acs$fertyr == TRUE, c("perwt", "puma", "State", "eldch","yngch", "poverty", "med.elig")]
require(plyr)
moms.overall <- sum(moms$perwt)
moms.by.state <- ddply(moms, .(State), summarize, Moms = sum(perwt))
moms.by.puma <- ddply(moms, .(State, puma), summarize, Moms = sum(perwt))
moms.by.puma[order(moms.by.puma$Moms), ]

# for moms who gave birth last year, what are the oldest and youngest children
ddply(moms, .(eldch, yngch), summarize, sum(perwt))

# first-time moms (eldch==99 actually means NA, but it seems like lots of those people should be 0s; how can you have a child in past year but not have an eldest child's age?)
with(acs, sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99)]))

# medicaid eligible first-time moms
with(acs, sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & med.elig==1]))

# under 100% FPL
with(acs, sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & poverty < 100]))

# from NSDUH 2013, we know that 15% of pregnant women on medicaid had alc | drug dep | mhi
exclusion <- .15
with(acs, sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & poverty < 100])) * (1-exclusion)

# using NFP assumptions (#5 above)
with(acs, sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & poverty < 100 & (age <=24 & age >= 15)]))

# table of target pop by state
tpop.by.state <- ddply(acs, .(State), summarize, tpop=sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & poverty < 100]))
tpop.by.state <- tpop.by.state[order(tpop.by.state$tpop), ]

# single out those priority states (used for barplot below)
tpop.by.state$priority <- with(tpop.by.state, ifelse((State == "South Carolina" | State == "North Carolina" | State == "California" | State == "Texas" | State == "Michigan" | State == "New York"), 2, 1))

# plot the results
tpop.by.state$State[46:51] # 5 largest state markets

pdf("NFP_mkt_state.pdf", width=10, height=6)
barplot(tpop.by.state$tpop, ylim=c(0,70000), 
        names.arg = c(rep(" ", 35), "So. Carolina", rep(" ", 5), "No. Carolina", rep(" ", 3), "Michigan", "Pennsylvania", "Florida", "New York", "Texas", "California"),
        cex.names = .85,
        las = 2,
        space = 0.5,
        cex.axis = 0.75,
        col = tpop.by.state$priority,
        xlab = "State", 
        ylab = "First-Time Mothers Under 100% FPL", 
        main = "NFP Market Size by State")
dev.off()

# largest NFP market by PUMA
tpop.by.puma <- ddply(acs, .(State, puma), summarize, tpop=sum(perwt[fertyr==TRUE & (eldch==0 | eldch==99) & poverty < 100]))
tpop.by.puma <- tpop.by.puma[order(tpop.by.puma$tpop), ]

# get 15 largest PUMAs and associated states
N <- nrow(tpop.by.puma)
targetpumas <- tpop.by.puma[(N-15):N, ]


# display results
names <- c("Syracuse, NY", "NW Louisiana" ,"Fresno, CA","Newton Co., GA","Dubois, IN", "NE Columbus, OH","Knoxville, TN","Yuba City, CA","Bossier Prsh, LA","Memphis, TN" , "W Dane Co., SC",  "E Philadlephia, PA", "NW Chicago, IL", "S Phoenix, AZ","S Philadelphia, PA", "NE Detroit, MI")
pdf("NFP_mkt_puma.pdf", width=10, height=6)
barplot(targetpumas$tpop, ylim=c(0,3000), 
        names.arg = names,
        cex.names = .6,
        las = 2,
        space = 0.8,
        cex.axis = 0.75,
        xlab = " ", 
        ylab = "First-Time Mothers Under 100% FPL", 
        main = "15 Geographic Areas with Greatest Number\nof NFP-Eligible Mothers")
dev.off()

##############################################
### Market Sizing in Priority States
### follow above analysis, adding in geog.
### limitations of access to IAs
##############################################
# create list of 6 priority states
priority.states <- c("California", "New York", "South Carolina", "North Carolina", "Michigan", "Texas")

# subset for those prioritiy states
acs.priority.states <- subset(acs, State %in% priority.states)
acs.priority.states <- acs.priority.states[order(acs.priority.states[,"State"], acs.priority.states[,"puma"]),]

# import data set relating PUMAs and ZIP codes
pumas.zips <- read.csv("../../pumas_zips_priority_states.csv")
pumas.zips <- pumas.zips[-1,]

# keep 1st row of each unique combination of states and pumas
pumas.zips <- pumas.zips[!duplicated(pumas.zips[c("state","puma12")]), ]

# create state var for merging purposes
pumas.zips$State <- with(pumas.zips, ifelse(stab == "CA", "California",
                                           ifelse(stab == "TX", "Texas",
                                                  ifelse(stab == "NC", "North Carolina",
                                                         ifelse(stab == "SC", "South Carolina",
                                                                ifelse(stab == "NY","New York",
                                                                       ifelse(stab == "MI","Michigan", "")))))))
drops <- c("stab", "pop10", "state", "afact")
pumas.zips <- pumas.zips[, !(names(pumas.zips) %in% drops)]

# merge combination of pumas and zips to acs data
acs.priority.states <- merge(pumas.zips, acs.priority.states, by.x=c("State", "puma12"), by.y=c("State","puma"), all = FALSE)
