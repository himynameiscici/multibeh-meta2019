##########################################
### Updated: 5.10.2019 (Liu, X.)
### packaging the data preperation and effect size calculation steps for the lifestyle outcome var Metananlysis
### prop2d() uses proportions to calculate d (within-subject designs)
### mean2d() uses means to calculate d (within-subject designs)
#########################################
#########################################
### read in data (with multiple tabs) and combine tabs
#########################################

# Set the working directory and read datafile
setwd("/Users/xiliu/Box Sync/Research/HIV social media/Meta Analysis")

# read in tabs seperately, and merge them
# Note: these files are not complete. for the sake of runtime they are a subset of the original datafile

library("gdata")
dat1 <- read.xls("MetaAnalysis5_8_2019_1.xls", sheet=1)
dat2 <- read.xls("MetaAnalysis5_8_2019_1.xls", sheet=2)
dat <- merge(x=dat1, y=dat2, by="authyear", all.x=TRUE)

#drop record.x and rename record.y into record for future merging
#rerord.x are all "h", record.y denotes the group
dat = dat[, !(colnames(dat) %in% c("record.x"))]
colnames(dat)[colnames(dat) == "record.y"] <- "record"

#this is the file sent by Ben. It seems to have more complete records
#keeping all in dat3, because this is the dynamic variables tab which we care about the most
#dat3 <- read.xls("MetaAnalysis5_8_2019_1.xls", sheet=3) this was the incomplete dat3
dat3 <- read.csv("MBX_Dynamic.csv")
#turn all variable names to upper case, to be consistent with the functions
if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}
for (i in 3:ncol(dat3)){
  x = colnames(dat3)[i]
  y <- str_split(x, fixed("_"))
  z <- toupper(y[[1]][1])
  colnames(dat3)[colnames(dat3)==x] <- gsub(y[[1]][1], z, colnames(dat3)[colnames(dat3)==x])
}
#a couple variables (e.g. sex_act) have "_" as part of the var name. Deal with this later.


dat <- merge(x=dat, y=dat3, by=c("authyear", "record"), all.y=TRUE)

#keeping all in dat (i.e. dat3), same reason as above
#skip this chunck for now because looks like we're not using the es outcomes yet
#dat4 <- read.xls("MetaAnalysis5_8_2019_1.xls", sheet=4)
#dat <- merge(x=dat, y=dat4, by=c("authyear", "record"), all.x=TRUE)

#save a copy
datFinal <- dat

var_names <- read.xls("var names and stud.xlsx", sheet=1)

library(foreign)

# Include functions in the current session
source("/Users/xiliu/Box Sync/Research/HIV social media/Meta Analysis/escal2d.R")

#########################################
### start with w = 1 and 2. proceed to more waves later
#########################################

### proportion
#loop through var_names
for (v in 1:nrow(var_names)){
  var_name = as.character(var_names$Stub[v])
  #check if the variable outcome (_out for mean, and _p for proportion) 
  #is in the data. If not, skip to the next variable
  if (sum(grepl(paste0(var_name,"_p_"), names(dat)))==0) {
    print(paste0("skip ", var_name))
    next
  }
  print(paste0("calculating ", var_name))
  n=sum(grepl(paste0("^",var_name,"_p_n.*w1$"),names(dat)))
  dat = prop2d(x=var_name,w=2,n,data=dat)
  print(paste0(var_name, " is done"))
}
  

#Note: MA works. AU (n=8) and Tobb (n=5) does not. Stuck at the step "Error in `[.data.frame`(dat, , c(paste0("m", x, "_n", u, "w", v))) : undefined columns selected
### mean
for (v in 1:nrow(var_names)){
  var_name = toupper(as.character(var_names$Stub[v]))
  #check if the variable outcome (_out for mean, and _p for proportion) 
  #is in the data. If not, skip to the next variable
  if (sum(grepl(paste0(var_name,"_out_"), names(dat)))==0) {
    print(paste0("skip ", var_name))
    next
  }
  print(paste0("calculating ", var_name))
  n=sum(grepl(paste0("^",var_name,"_out_n.*w1$"),names(dat)))
  dat = mean2d(x=var_name,w=2,n,data=dat)
  print(paste0(var_name, " is done"))
}




######################################################################################################################################
### break into c vs. b vs. cb
######################################################################################################################################

