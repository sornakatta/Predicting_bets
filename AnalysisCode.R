### Code analyzing Betfair dataset provided for Datathon 2015
### Goal is to identify bets that are likely to be lapsed or cancelled

## ------------------------------------------------------------------------

## First steps

# Setting directory 
setwd("~/Datathon/")

# Inputing data
Data1 <- read.table("DatathonWCDataGames1-10.csv", sep=",",header = T,stringsAsFactors = T)
Data2 <- read.table("DatathonWCDataGames11-20.csv", sep=",",header = T,stringsAsFactors = T)
Data3 <- read.table("DatathonWCDataGames21-30.csv", sep=",",header = T,stringsAsFactors = T)
Data4 <- read.table("DatathonWCDataGames31-40.csv", sep=",",header = T,stringsAsFactors = T)

# Combining into a single data frame
Data <- rbind(Data1,Data2,Data3,Data4)

## ------------------------------------------------------------------------

## Data cleaning and exploratory data analysis

# Removing all columns with only one unique parameter
#Data <- Data[,sapply(Data, function(x) length(unique(x)))>1]

# Fixing all the dates to computer readable format
library(lubridate)
Data[,cols <- grep("DATE|DT",names(Data))] <- lapply(Data[,cols <- grep("DATE|DT",names(Data))],dmy_hms)

# Setting 'COUNTRY_OF_RESIDENCE_NAME' and 'SELECTION_NAME' to be factors
Data[,cols <- grep("NAME",names(Data))] <- lapply(Data[,cols <- grep("NAME",names(Data))],as.factor)

# Looking at possible key variables 
# summary(Data$PROFIT_LOSS)
# summary(Data$BET_SIZE)

## ------------------------------------------------------------------------

## Making plots using entire data set (**time consuming**)

# Plotting location of bets placed with time, coloured by selection 
library(ggplot2)
g <- ggplot(Data)
g <- g + geom_point(aes(x=PLACED_DATE, y=COUNTRY_OF_RESIDENCE_NAME,col = STATUS_ID),size=4)
g <- g + labs(col= "Status of Bet",
              x = "Date of bet",
              y = "Location of bet",
              title = "Overview of bets")
g <- g +  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
g    
ggsave(file="Overview of bets.pdf", width=16,height=9)

# All the voided bets are from a single match that didn't take place (Australia vs Bangladesh). 
# Excluding that match
Data<-Data[!(Data$MATCH=="Australia v Bangladesh"),]

# Re-factoring to remove 'Void' level
Data$STATUS_ID <- factor(Data$STATUS_ID)

# Creating a factor variable for classifying profits/losses
Data$PL_FACTOR <- ordered(cut(Data$PROFIT_LOSS, c(-1000000,-50000,0,50000,1000000), 
                              labels=c("<-50000", "-50000-0", "0-50000","50000>")))

# Creating a factor variable for classifying bet size
Data$BS_FACTOR <- ordered(cut(Data$BET_SIZE, c(min(Data$BET_SIZE),100,1000,10000,max(Data$BET_SIZE)), 
                              labels=c("0-100", "100-1000", "1000-10000","10000+")))

# Plotting all the settled bets, with additional info of profit/loss and size of bet
library(ggplot2)
g <- ggplot(Data)
g <- g + geom_point(aes(x=PLACED_DATE, y=COUNTRY_OF_RESIDENCE_NAME,col = PL_FACTOR, shape = STATUS_ID,size=BS_FACTOR))
g <- g + labs(col= "Winnings", shape = 'Status of Bet', size="Size of bet",
              x = "Date of bet",
              y = "Location of bet",
              title = "Overview of settled bets with bet size and profit/loss")
g <- g +  theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
g  
ggsave(file="Overview of bets with profit loss and bet size.pdf", width=16,height=9)


# Building a decision tree to predict which bets are likely to cancel/lapse
# Load necessary packages
library(rpart)

# One version of the tree (Other variables can be included/excluded)
fit <- rpart(STATUS_ID ~ BET_SIZE + BET_PRICE + BID_TYP,
             data=Data, method="class")

# Visualizing the decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

## ------------------------------------------------------------------------

## Creating and working with a smaller dataset with summed profits/losses and bet size for each user

# Aggregate all transactions for unique ACCOUNT_ID and figure out total bet size and
# profits/losses, i.e. find the big players
Agg <- aggregate(cbind(BET_SIZE,PROFIT_LOSS) ~ ACCOUNT_ID, FUN=sum, data=Data)

# Sort Agg in descending order of bet size and profit/loss
AggOrd <- Agg[order(-Agg[,3], Agg[,2]),]
# Looking at the most profitable betters
#head(AggOrd)

# Add a column for (total profits or losses)/(total bets) (PLBS)
Agg$PLBS_RAT <- Agg[,3]/Agg[,2]

# Separate betters into three categories based on total amount betted
Agg$USER_TYPE <- ordered(cut(Agg$BET_SIZE, c(min(Agg$BET_SIZE),100000,1000000,max(Agg$BET_SIZE)), labels=c("Regular", "Power", "Super")))

# Plot density of Net earnings to bets ratio by user type
library(ggplot2)    
g <- ggplot(data = Agg, aes(x=PLBS_RAT, fill=USER_TYPE)) 
g <- g + geom_density(alpha=0.2)
g <- g + scale_fill_brewer(palette="Set1")
g <- g + facet_wrap( ~ USER_TYPE, scales = "free",ncol=1)
g <- g + xlab("Ratio of profits/losses to Total Bets") + ylab("Density") 
g
ggsave(file="Density plot of user types.pdf")

# Looking at Super users
summary(Agg$PLBS_RAT[Agg$USER_TYPE=="Super"])

## ------------------------------------------------------------------------

## Classify complete dataset using categories developed above

# Initially everybody is undefined!
Data$USER_TYPE <- "Undefined"

# Use ID's that have atleast 1 accepted bet
IDlist <- as.numeric(unique(Agg$ACCOUNT_ID))

# This werks!
Data <- Data[(Data$ACCOUNT_ID %in% IDlist),]


# Now use account ID to find User type from 'Agg' 
# Using a for loop in R is not ideal, but will have to do
for(i in 1:dim(Data)[1]){
  
  Data$USER_TYPE[i] <- as.character(Agg$USER_TYPE[grep(Data$ACCOUNT_ID[i],Agg$ACCOUNT_ID)])
  
}
Data$USER_TYPE <- as.factor(Data$USER_TYPE)

# Write the modified dataset to file
write.csv(Data, file = "Data_modified.csv", row.names = FALSE)

Data <- read.table("Data_modified.csv", sep=",",header = T,stringsAsFactors = T)
