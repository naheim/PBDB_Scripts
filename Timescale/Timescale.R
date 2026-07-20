### SET WORKING DIRECTORY
my.wd <- "~/Documents/PBDB_Scripts/Timescale/"
setwd(my.wd)

### Load Library
library(fossilbrush)
library(deeptime)

### CHOOSE TO INCLUDE OR NOT NEROTEROZOIC STAGES
include.npz <- TRUE # change to TRUE to include

### CHOOSE TO LUMP HOLOCENE AGES OR KEEP THEM SEPERATE
lump.holocene <- FALSE # change to TRUE to KEEP HOLOCENE AGES SEPERATE

# FIX TYPOS IN GTS_2020
GTS_2020$LAD[GTS_2020$Interval == "Changhsingian"] <- 251.902 # was 251.901
GTS_2020$LAD[GTS_2020$Interval == "Stage 10"] <- 486.85 # was 485.85
GTS_2020$Interval[GTS_2020$Interval == "Palaeozoic"] <- "Paleozoic" # using American spelling

###### ADD DATA FRAME FOR GEOLOGICAL TIMESCALE
# repeated stages with alternate names to drop.
repeat.ages <- c("Early Mississippian","Middle Mississippian","Late Mississippian","Early Pennsylvanian","Middle Pennsylvanian","Late Pennsylvanian") # prioritizing international names
# repeated epochs with alternate names to drop.
repeat.epochs <- c("Palaeocene") # prioritizing American spelling
# repeated periods with alternate names to drop.
repeat.periods <- c("Palaeogene","Tertiary") # prioritizing American spelling
repeat.ints <- c(repeat.ages, repeat.epochs, repeat.periods)
# remove repeat intervals
GTS_2020 <- subset(GTS_2020, !is.element(Interval, repeat.ints))


# Select Ages + Neoproterozoic Periods, if requested
if(include.npz == TRUE) {
     timescale <- subset(GTS_2020, Type == "ICS Age" | is.element(Interval, c('Tonian','Cryogenian','Ediacaran')))
} else {
     timescale <- subset(GTS_2020, Type == "ICS Age")
}

# lump Holocene if requested
if(lump.holocene == TRUE) {
     timescale <- subset(timescale, FAD >= 0.0117)
     timescale$LAD[timescale$FAD == 0.0117] <- 0.0000
     timescale$Interval[timescale$FAD == 0.0117] <- "Holocene"
}


timescale <- subset(timescale, !is.element(Interval, repeat.ints)) # remove repeat intervals

# Add and fill columns for Era, Period and Epoch
timescale$Era <- ""
timescale$Period <- ""
timescale$Epoch <- ""
for(i in 1:nrow(timescale)) {
     my.era <- subset(GTS_2020, FAD >= timescale$FAD[i] & LAD <= timescale$LAD[i] & Type == "ICS Era")$Interval
     my.period <- subset(GTS_2020, FAD >= timescale$FAD[i] & LAD <= timescale$LAD[i] & Type == "ICS Period")$Interval
     my.epoch <- subset(GTS_2020, FAD >= timescale$FAD[i] & LAD <= timescale$LAD[i] & Type == "ICS Epoch")$Interval
     
     if(length(my.era) == 1) {
          timescale$Era[i] <- my.era
     }
     if(length(my.period) == 1) {
          timescale$Period[i] <- my.period
     }
     if(length(my.epoch) == 1) {
          timescale$Epoch[i] <- my.epoch
     }
}

## select useful columns, change column names, and order timescale from youngest at top, 
colnames(timescale)[colnames(timescale) == "Interval"] <- "Age"
ts.names <- c('Era','Period','Epoch','Age',"FAD","LAD")
timescale <- timescale[, match(ts.names, colnames(timescale))]
timescale <- timescale[order(timescale$FAD),]
timescale$age_mid <- round((timescale$FAD + timescale$LAD)/2, 3)

# I don't like FAD and LAD as column names, since thse abbreviations typically apply to fossil occurrences not time intervals
colnames(timescale)[match("FAD", colnames(timescale))] <- "age_bottom"
colnames(timescale)[match("LAD", colnames(timescale))] <- "age_top"

write.csv(timescale, file="timescale_2020.csv", na="", row.names=FALSE)

##### PBDB Timescale ICS 2024/12
if(lump.holocene == TRUE) {
     ts.id <- 11
} else {
     ts.id <- 1
}

# Get Phanerozoic
url.ts <- paste0("https://paleobiodb.org/data1.2/intervals/list.csv?scale=",ts.id,"&type=Age&order=age")
timescale.pbdb <- read.csv(file=URLencode(url.ts))
dim(timescale.pbdb)

if(include.npz == TRUE) {
     url.pc <- "https://paleobiodb.org/data1.2/intervals/list.csv?name=Cryogenian,Ediacaran,Tonian&order=age"
     pc <- read.csv(file=URLencode(url.pc))
     timescale.pbdb <- rbind(timescale.pbdb, pc)
}

ts.eras <- read.csv(URLencode("https://paleobiodb.org/data1.2/intervals/list.csv?scale=1&type=Era&order=age"))
ts.periods <- read.csv(URLencode("https://paleobiodb.org/data1.2/intervals/list.csv?scale=1&type=Period&order=age"))
ts.epochs <- read.csv(URLencode("https://paleobiodb.org/data1.2/intervals/list.csv?scale=1&type=Epoch&order=age"))

ts.names <- c('interval_no','color','Era','Period','Epoch','interval_name',"b_age","t_age")

timescale.pbdb$Era <- ""
timescale.pbdb$Period <- ""
timescale.pbdb$Epoch <- ""
for(i in 1:nrow(timescale.pbdb)) {
     timescale.pbdb$Era[i] <- subset(ts.eras, b_age >= timescale.pbdb$b_age[i] & t_age <= timescale.pbdb$t_age[i])$interval_name
     timescale.pbdb$Period[i] <- subset(ts.periods, b_age >= timescale.pbdb$b_age[i] & t_age <= timescale.pbdb$t_age[i])$interval_name
     my.epoch <- subset(ts.epochs, b_age >= timescale.pbdb$b_age[i] & t_age <= timescale.pbdb$t_age[i])$interval_name
     if(length(my.epoch) == 1) {
          timescale.pbdb$Epoch[i] <- my.epoch
     }
}

timescale.pbdb <- timescale.pbdb[,match(ts.names, colnames(timescale.pbdb))]
timescale.pbdb$age_mid <- round((timescale.pbdb$b_age + timescale.pbdb$t_age)/2, 3)

# I don't like FAD and LAD as column names, since thse abbreviations typically apply to fossil occurrences not time intervals
colnames(timescale.pbdb)[match("b_age", colnames(timescale.pbdb))] <- "age_bottom"
colnames(timescale.pbdb)[match("t_age", colnames(timescale.pbdb))] <- "age_top"

write.csv(timescale.pbdb, file="timescale_2024.csv", na="", row.names=FALSE)


##### ICS 2022/10
timescale.2022 <- stages
if(include.npz == TRUE) {
    pc <- data.frame("name"=c("Ediacaran","Cryogenian","Tonian"),
                     "max_age"=c(635, 720, 1000),
                     "min_age"=c(538.8, 635, 720),
                     "abbr"=c("Ed","Cy","To"),
                     "color"=c("#FED96A","#FECC5C","#FEBF4E"),
                     "lab_color"=c("black","black","black"))
    timescale.2022 <- rbind(timescale.2022, pc)
}

if(include.npz == TRUE) {
     pc <- data.frame("name"=c("Ediacaran","Cryogenian","Tonian"),
                      "max_age"=c(635, 720, 1000),
                      "min_age"=c(538.8, 635, 720),
                      "abbr"=c("Ed","Cy","To"),
                      "color"=c("#FED96A","#FECC5C","#FEBF4E"),
                      "lab_color"=c("black","black","black"))
     timescale.2022 <- rbind(timescale.2022, pc)
}

if(lump.holocene == TRUE) {
     timescale.2022 <- subset(timescale.2022, !is.element(name, c("Meghalayan","Northgrippian","Greenlandian")))
     holo <- data.frame("name"="Holocene",
                        "max_age"=0.0117,
                        "min_age"=0.0000,
                        "abbr"="Ho",
                        "color"="#FEEBD2",
                        "lab_color"="black")
     timescale.2022 <- rbind(holo, timescale.2022)
}

write.csv(timescale.2022, file="Documents/PBDB_Scripts/Timescale/timescale_2022.csv", quote=FALSE, row.names=FALSE)

