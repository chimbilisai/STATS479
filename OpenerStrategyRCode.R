library(dplyr)
tbrays = read.csv('TBRays2018.pitch.gamelog.csv',header=TRUE)
#Pitching Data imported from
#https://www.baseball-reference.com/teams/tgl.cgi?team=TBR&t=p&year=2018
#Function to turn factors that are numbers into numerics (for later):
as.numeric.factor <- function(x) {
  as.numeric(levels(x))[x]
}

#quick view of data
head(tbrays)

#Filtering goal: need to get new columns with pitcher name and days rest for later:
filter.example = as.character(tbrays$Pitchers.Used..Rest.GameScore.Dec.[1:length(tbrays[,1])])
filter.example[1]
#Split the pitchers by comma.
split.pitchers = strsplit(x=filter.example, split = ",")
split.pitchers[1]
#Need to extract first pitcher of game (Starting pitcher)
starting.pitcher.filtered=c()
for (i in 1:length(filter.example)){
  starting.pitcher.filtered[i] = split.pitchers[[i]][1]
}
starting.pitcher.filtered[1]
#now, need to split by hyphen, to gather the starting pitcher's name and days of rest
pitcher.split.numbers = strsplit(x=starting.pitcher.filtered,split=" ")
pitcher.name.rest.split = strsplit(x=starting.pitcher.filtered,split="-")
pitcher.name.rest=c()
pitcher.name.rest.split[1]
#vector with "pitcher name (days rest" , "game score and decision)"
for (i in 1:length(starting.pitcher.filtered)){
  pitcher.name.rest[i] = pitcher.name.rest.split[[i]][1]
}
head(pitcher.name.rest)
#Now, each index looks like "Pitcher name (daysrest"
#Need it to be split by name, days rest. Then remove the '('. Then, the vector will be pitcher1, daysrest1, pitcher2, daysrest2 ....
split.name.rest = strsplit(x=pitcher.name.rest, split = " ")
pitcher.name=c()
pitcher.daysrest.notfinal=c()
#need to get rid of the '('....
for (i in 1:length(split.name.rest)){
  pitcher.name[i] = split.name.rest[[i]][1]
  pitcher.daysrest.notfinal[i] = split.name.rest[[i]][2]
}
pitcher.daysrest=c()
#Need to get rid of the '(' still.
for (i in 1:length(pitcher.daysrest.notfinal)){
  pitcher.daysrest[i] = substring(pitcher.daysrest.notfinal[i],2)
}
#substring takes a substring of the string, starting at the second character, in this case.
#Change names to characters, numbers to numeric
pitcher.names.final = (pitcher.name)
str(pitcher.names.final)
pitcher.daysrest.final=as.numeric(as.character(pitcher.daysrest))
df.name.rest = data.frame(pitcher.names.final,pitcher.daysrest.final)
names(df.name.rest) = c("PitcherName","DaysRest")
df.name.rest$PitcherName=(df.name.rest$PitcherName)
#we good.
head(df.name.rest)
#Knit resulting data frame with initial data frame.
tbrays1 = cbind(tbrays,df.name.rest)
#Now we have the initial data frame with new columns, pitcher name and days rest.
head(tbrays1)
boxplot(tbrays1$DaysRest, main = "Days of Rest Outliers")
#Questions: What to do with the days rest values of 99? 
#coerce them into values equal to the observed maximum days 
#of rest for the rest of the data?
#We have decided to coerce these days of rest into the maximum of the non-outliers, so that we have as much data as possible, with consistently comparable numbers.
yes.over7=which(tbrays1$DaysRest>7)
tbrays2=tbrays1
for (i in yes.over7){
  tbrays2$DaysRest[i]=7
}
max(tbrays2$DaysRest)
#Before, we had multiple days rest variable values which were outliers. For example,
#22,86, 99, etc. The closest possible days of rest value in the data outside of these outliers
#is 7. Therefore, we coerced the days rest variable into a range with a maximum of 7.

#Openers are Romo, Ryan Yarbrough, Ryne Stanek, Hunter Wood, Wilmer Font, Jonny Venters, Matt Andriese, Diego Castillo, and Yonny Chirinos.
#Need to put these pitchers names as the binary success factor for opener strategy.
openers = c("S.Romo","R.Stanek","R.Yarbrough","R.Stanek","H.Wood","W.Font","J.Venters","M.Andriese","D.Castillo","Y.Chirinos")

head(tbrays2)
#The first thing to consider with the opener strategy is when the Tampa Bay Rays actually began implementing the strategy.
#According to the article, the Rays implemented the strategy starting on May 19th. Therefore, we ought to only consider
#Opener Strategy data points from this date on as true Opener data points
str(tbrays2)
tbrays2$CharDate=as.character(tbrays2$Date)
tbrays2$CharDate[44]

tbrays2.44on=tbrays2[44:nrow(tbrays2),]

#Obtain indices of openers.
tbrays.openers = mutate(tbrays2.44on,Opener=if_else((tbrays2.44on$PitcherName%in%openers) ,1,0))
head(tbrays.openers)
opener.ind=(which(tbrays.openers$Opener==1)+43)
opener.ind
#These are the original dataframe's openers indices.
tbrays2$Opener=0
for (i in opener.ind){
  tbrays2$Opener[i]=1
}
Head(tbrays2)


#Now we need to input OPS. A team with a high ops is better than a team with low OPS.

teambattingdata = read.csv("TeamBatting2018.csv",header=TRUE)
#this is what the data looks like
head(teambattingdata)
Teambatting.teams = teambattingdata$Tm
Teambatting.OPS = teambattingdata$OPS
teambatting=cbind.data.frame(Teambatting.teams,Teambatting.OPS)

#work on new df incase of errors.
tbrays3=tbrays2

tbrays4 = merge(tbrays3,teambatting,by.x="Opp",by.y="Teambatting.teams")


#Home variable is not in the data yet.
#Build a new column to mark whether this is a home game or not (1 is home game)
tbrays5 <- mutate(tbrays4, Home = if_else(tbrays4$X == "@",0,1 ))


#We want to compare the Opener strategy to the Starter strategy.
#In order to do this, we must first calculate the mean ERA for the starting pitchers
#and compare the ERA for each "opener" game to this Starter mean ERA.
#If the ERA for one game for opener strategy is > mean(ERA|Starter) then 0. if <mean(ERA|Starter) then 1.
#i.e. ERA(Opener) > mean(ERA|Starter) -> 0
# ERA(Opener) < mean(ERA|Starter) -> 1.
#We will later use logistic regression to predict this probability.

#First, need to calculate mean(ERA|Starter)
head(tbrays5)
#ERA = (Earned Runs/Innings Pitched) * 9 [scaling factor for innings per typical game. not dependent on true length of a game.]
start.ER.sum = sum(tbrays5$ER[tbrays5$Opener==0])
start.IP.sum = sum(tbrays5$IP[tbrays5$Opener==0])
mean.ERA.starters = 9*sum(start.ER.sum/start.IP.sum)
mean.ERA.starters
#This ^ is our baseline value for the classification of our dependent variable.

opener.ER.sum = sum(tbrays5$ER[tbrays5$Opener==1])
opener.IP.sum = sum(tbrays5$IP[tbrays5$Opener==1])
mean.ERA.openers = 9*sum(opener.ER.sum/opener.IP.sum)
mean.ERA.openers
#Just for reference^

tbrays5$ERA.binary = ifelse(tbrays5$ERA<mean.ERA.openers,1,0)
head(tbrays5,10)


#lets get rid of the variables that we are not studying for now.
opener.df = select(tbrays5,Opener,Pit,DaysRest,Home,Teambatting.OPS,ERA.binary)

#Lets modify upon opener.df copy for our factored variables.

opener.df1=opener.df

opener.df1$Opener = as.factor(opener.df$Opener)
opener.df1$Home = as.factor(opener.df$Home)
opener.df1$ERA.binary = as.factor(opener.df$ERA.binary)

#Initial glm:
opener.glm = glm(ERA.binary ~ Opener + Pit + DaysRest + Home + Teambatting.OPS,family=binomial,data = opener.df1)
summary(opener.glm)
summary(opener.glm)


#Only variables are Opener, Pit, DaysRest, Home and Teambatting.OPS -> Y is ERA.binary

#Model Selection
library(bestglm)
AIC.opener=bestglm(Xy=opener.df,family=binomial,IC="AIC",method="exhaustive")
BIC.opener=bestglm(Xy=opener.df,family=binomial,IC="BIC",method="exhaustive")
AIC.opener$BestModels
BIC.opener$BestModels

#The outputs suggest that Opener is the most significant variable. They both agree that the other variables are
#Non-significant for the most part. Considering the low number of variables that we ought to include in our model,
#We suspect that using the AIC's results is potentially more beneficial, because we do not have to worry about
#The possibility of the AIC's results leading to overfitting, due to the low order of dimensionality of the
#number of variables to reasonably employ

#We are also going to use the model that includes both Opener and DaysRest, because they are close in Criterion values
#And including more variables helps account for the explained variability in our dependent variable as best as possible.

opener.daysrest.glm = glm(ERA.binary ~ Opener + DaysRest,family=binomial,data=opener.df1)
summary(opener.daysrest.glm)

#This is the model! :)

#get the average days of rest for probabilities and odds ratio
(daysrest.average = mean(opener.df1$DaysRest))
