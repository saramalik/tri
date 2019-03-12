### Data Exploration
# Outcome variable is either pounds, hazard, score
# Method 1: Need to do two-way fixed effects: facility and year
# Method 2: like Hainmueller fair trade label. 
# need to improve balance between SP and non-SP
# need to look at change both at the facility level and the company level
# what about accounting for state/geographic stickiness?
# think about channel that pollution is emitted
###

rm(list=ls())
library("tidyverse")
library(readxl)
library(Matching)

setwd("/Users/smalik/DropBox/Grad School/Research/TRI")
load("~/Dropbox/Grad School/Research/TRI/TRI_RSEI.RData")
CPI_SuperPolluters <- read_excel("CPI_SuperPolluters.xlsx")
CPI <- read_excel("Facility air pollution dataset.xlsx", sheet = 'All facilities')

#Matching
CPI$SP <- ifelse(CPI$`Rank TRI '14` < 101 & is.na(CPI$`Rank TRI '14`) == FALSE & CPI$`Rank GHG '14` < 101 & is.na(CPI$`Rank GHG '14`) == FALSE , 1, 0)
CPI$TRI14 <- as.numeric(CPI$`TRI air emissions 14 (in pounds)`)
CPI$GHG14 <- as.numeric(CPI$`GHG direct emissions 14 (in metric tons)`)
m <- CPI[complete.cases(CPI[,c("TRI14", "GHG14")]),]
m$rowID<-as.numeric(rownames(m))

X <- cbind(m$TRI14, m$GHG14)
colnames(X) <- c("TRI14", "GHG14")
Tr <- m$SP

rr <- Match(Y = NULL, Tr = Tr, X = X, M = 1, ties = FALSE, exact = NULL, estimand = "ATT", CommonSupport = TRUE, replace = FALSE)
mb <- MatchBalance(SP ~ TRI14 + GHG14, data = m, match.out = rr, nboots = 500)

rr.mah = Match(Y = NULL, Tr = Tr, X = X, M = 1, estimand = "ATT", Weight = 3, CommonSupport = TRUE)
mb.mah <- MatchBalance(SP ~ TRI14 + GHG14, data = m, match.out = rr.mah)

genout <- GenMatch(Tr = Tr, X = X, M = 1, estimand = "ATT", pop.size = 1000)
rr.gen <- Match(Y = NULL, Tr = Tr, X = X, M = 1, estimand = "ATT", Weight.matrix = genout, CommonSupport = TRUE, replace = FALSE)
mb.gen <- MatchBalance(SP ~ TRI14 + GHG14, data = m, match.out = rr.gen)

rr<-data.frame(rr.gen$index.treated,rr.gen$index.control)
colnames(rr)<-c("cases","controls")

rr$casefrsid <- m$`FRS ID`[match(rr$cases, m$rowID)]
rr$controlfrsid <- m$`FRS ID`[match(rr$controls, m$rowID)]



################

byid <- TRI %>% 
  group_by(SubmissionYear,FRSID, FacilityName, StandardizedParentCompany, City, State, PrimaryNAICS) %>% 
  summarise(Pounds = sum(Pounds), Hazard = sum(Hazard), Score = sum(Score))
byid$FRSID <- as.character(byid$FRSID)
byid <- arrange(byid, FRSID, SubmissionYear)


byid_air <- TRI %>% filter(Media == 1 |Media == 2) %>%
  group_by(SubmissionYear,FRSID, FacilityName, StandardizedParentCompany, City, State, PrimaryNAICS) %>% 
  summarise(Pounds = sum(Pounds), Hazard = sum(Hazard), Score = sum(Score))
byid_air$FRSID <- as.character(byid_air$FRSID)
byid_air <- arrange(byid_air, FRSID, SubmissionYear)

################

#Matched Pair Difference
rr1 <- left_join(rr, byid_air[byid_air$SubmissionYear == 2017,c(2,8:10)], by=c("casefrsid" = "FRSID"))
rr2 <- left_join(rr1, byid_air[byid_air$SubmissionYear == 2017,c(2,8:10)], by=c("controlfrsid" = "FRSID"))
colnames(rr2) <- c("cases", "controls", "controlfrsid", "casefrsid", "case_lb17", "case_haz17", "case_score17", "control_lb17", 
                   "control_haz17", "control_score17")

rr2$lbdiff <- rr2$case_lb17 - rr2$control_lb17
rr2$hazdiff <- rr2$case_haz17 - rr2$control_haz17
rr2$scorediff <- rr2$case_score17 - rr2$control_score17

mean(rr2$lbdiff, na.rm = TRUE)
mean(rr2$hazdiff, na.rm = TRUE)
mean(rr2$scorediff, na.rm = TRUE)


##################
#Quantile regression








joined <- left_join(byid, SP, by=c("FRSID" = "Facility ID"))
joined$SP <- ifelse(is.na(joined$SP), 0, 1)


study <- joined[between(joined$SubmissionYear, 2010, 2017),]
for (var in unique(SP$`Facility ID`)) {
  dev.new()
  print(ggplot(data = study[study$FRSID ==var,], aes(x = SubmissionYear, y = Pounds, group = FRSID)) +
    geom_line(aes(color = factor(FRSID))) +
    geom_point())
}






#By SP and non-SP, check balance: 

media <- TRI %>% group_by(MediaText) %>% count(MediaText)
media <- TRI %>% group_by(Media) %>% count(Media)
# TRI$temp <- TRI$Pounds*TRI$Hazard
# 
# basin <- joined[joined$City == "BEULAH", ]
# basin <- joined[joined$FRSID == "110022360899", ]
# basin <- joined[joined$FacilityName == "GREAT PLAINS SYNFUELS PLANT", ]
# basin <- joined[grep("SYNFUELS", joined$FacilityName) ,]
# miss <- joined[joined$FRSID == "110000590851", ]

#SP <- CPI_SuperPolluters[,3]
#SP$SP <- 1

m$case <- ifelse(m$rowID %in% rr$cases, 1, 0)
case <- unique(m$`FRS ID`[m$case==1])
m$control <- ifelse(m$rowID %in% rr$controls, 1, 0)
control <- unique(m$`FRS ID`[m$control==1])
