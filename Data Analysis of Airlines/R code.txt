#Appendix: R code  
 
#Data Cleaning: 
str(raw_data) 
CleanSatisfaction<-raw_data[(raw_data$Satisfaction=="1" |              raw_data$Satisfaction=="1.5" |             raw_data$Satisfaction=="2" |             raw_data$Satisfaction=="2.5" |             raw_data$Satisfaction=="3" |             raw_data$Satisfaction=="3.5" |             raw_data$Satisfaction=="4" |             raw_data$Satisfaction=="4.5" |             raw_data$Satisfaction=="5" ),] 
#clean_data <- subset(raw_data,trimws(raw_data$Satisfaction)==c(1:5)) 
#clean_data df<-CleanSatisfaction newCol<-colnames(CleanSatisfaction) newCol<-gsub("\\.", "", newCol) newCol colnames(df)<-newCol 
 
a <- sub("No","0",df$Flightcancelled) b <- sub("Yes","1",a) df$Flightcancelled <- b 
df$Flightcancelled<-as.numeric(df$Flightcancelled) 
 
df$Satisfaction<- as.numeric(as.character(df$Satisfaction)) 
 
#Dataset Subsetting: 
fulldf<-df str(fulldf) 
#Custdf<-subset(df,AirlineName=="Southeast") 
Custdf<-df[df$AirlineName == 'Southeast Airlines Co. ',] 
#summary(df) 
#positive = posWords[which(posWords >=2)] 
#df$AirlineName df<-Custdf str(fulldf) 
 
 
#Association Rule Mining: 
library(methods) summary(df) createBuckets<- function(vec){   q <- quantile(vec, c(0.4, 0.6))   vBuckets <- replicate(length(vec), "Average")   vBuckets[vec <= q[1]] <- "Low"   vBuckets[vec > q[2]] <- "High"   return(vBuckets) 
} 
vBuckets<-replicate(length(df$Satisfaction),"Median") vBuckets[df$Satisfaction>3]<-"High" vBuckets[df$Satisfaction<3]<-"Low" Satisfaction<-as.factor(vBuckets) age<-createBuckets(df$Age) pricesensitive<-createBuckets(df$PriceSensitivity) yearoffirstflight<-createBuckets(df$YearofFirstFlight) noofflightspa<-createBuckets(df$NoofFlightspa) shoppingamount<-createBuckets(df$ShoppingAmountatAirport) scheduleddeparturehour<-createBuckets(df$ScheduledDepartureHour) library(arules) library(arulesViz) ruleDF<- data.frame(Satisfaction,df$AirlineStatus,age,df$Gender,pricesensitive,yearoffirstflight,noofflight spa,df$TypeofTravel,shoppingamount,df$Class,scheduleddeparturehour,df$ArrivalDelaygreater 5Mins) 
#ruleDF<- 
data.frame(Satisfaction,df$AirlineStatus,age,df$Gender,pricesensitive,yearoffirstflight,noofflight spa,df$TypeofTravel,shoppingamount,df$Class,scheduleddeparturehour,df$ArrivalDelaygreater
5Mins) 
hotelSurveyruleDFSE<-as(ruleDF,"transactions") 
rulesetsoutheastH<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.8),appearance = list(default="lhs", rhs=("Satisfaction=High"))) goodrulesH<-sort(rulesetsoutheastH,by="lift")[1:5] inspect(goodrulesH) 
plot1<-plot(goodrulesH, method = "graph", engine = "htmlwidget") rulesetsoutheastL<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.5),appearance = list(default="lhs", rhs=("Satisfaction=Low"))) goodrules2<-sort(rulesetsoutheastL,by="lift")[1:10] 
plot2<-plot(goodrules2, method = "graph", engine = "htmlwidget") 
 
#Linear Modeling: 
LM1<-
lm(Satisfaction~AirlineStatus+Age+Gender+PriceSensitivity+YearofFirstFlight+NoofFlightspa +XofFlightwithotherAirlines+TypeofTravel+NoofotherLoyaltyCards+ShoppingAmountatAirpor t+EatingandDrinkingatAirport+Class+DayofMonth+ScheduledDepartureHour++Flightcancelled +DepartureDelayinMinutes+ArrivalDelayinMinutes+Flighttimeinminutes+FlightDistance+Arriv alDelaygreater5Mins,data=df) summary(LM1) 
LM2<-
lm(Satisfaction~AirlineStatus+Age+Gender+ShoppingAmountatAirport+PriceSensitivity+Yearo fFirstFlight+NoofFlightspa+TypeofTravel+Class+ScheduledDepartureHour+ArrivalDelaygreate r5Mins,data=df) summary(LM2) 
 
dfp<-predict(LM2,interval="prediction") dfp<-merge(df$Satisfaction,dfp) draw(dfp) 
 
 
 
LM3<-
lm(Satisfaction~AirlineStatus+Age+Gender+Age*Gender+ShoppingAmountatAirport+PriceSen sitivity+YearofFirstFlight+NoofFlightspa+TypeofTravel+Class+ScheduledDepartureHour+Arriv alDelaygreater5Mins,data=df) summary(LM3) 
LM4<-
lm(Satisfaction~AirlineStatus+Age+Gender+Age*Gender+ShoppingAmountatAirport+PriceSen sitivity+Gender*PriceSensitivity+YearofFirstFlight+NoofFlightspa+TypeofTravel+Class+Sched uledDepartureHour+ArrivalDelaygreater5Mins,data=df) summary(LM4) 
LM6<-
lm(Satisfaction~AirlineStatus+Age+Gender+Age*Gender+ShoppingAmountatAirport+PriceSen sitivity+Gender*PriceSensitivity+YearofFirstFlight+Gender*NoofFlightspa+NoofFlightspa+Typ eofTravel+Class+ScheduledDepartureHour+ArrivalDelaygreater5Mins,data=df) summary(LM6) 
 
 
LM5<-lm(Satisfaction~NoofFlightspa,data=df) summary(LM5) 
plot(Satisfaction~NoofFlightspa,xlab="NoofFlightspa",ylab="Satisfaction",data=df) abline(LM5) 
#NoofFlightspa=0.05671105 
LM5<-lm(Satisfaction~TypeofTravel,data=df) summary(LM5) 
plot(Satisfaction~TypeofTravel,xlab="TypeofTravel",ylab="Satisfaction",data=df) 
#TypeofTravel=0.3350338 
LM5<-lm(Satisfaction~ShoppingAmountatAirport,data=df) summary(LM5) 
plot(Satisfaction~ShoppingAmountatAirport,xlab="ShoppingAmountatAirport",ylab="Satisfacti on",data=df) 
 
#ShoppingAmountatAirport=0.0002999279  LM5<-lm(Satisfaction~Class,data=df) summary(LM5) 
plot(Satisfaction~Class,xlab="Class",ylab="Satisfaction",data=df) 
 
#Class=0.002526544 
LM5<-lm(Satisfaction~ScheduledDepartureHour,data=df) summary(LM5) 
plot(Satisfaction~ScheduledDepartureHour,xlab="ScheduledDepartureHour",ylab="Satisfaction ",data=df) 
 
#ScheduledDepartureHour=-6.981177e-06 
LM5<-lm(Satisfaction~ArrivalDelaygreater5Mins,data=df) 
plot(Satisfaction~ArrivalDelaygreater5Mins,xlab="ArrivalDelaygreater5Mins",ylab="Satisfactio n",data=df) 
 
summary(LM5) 
#ArrivalDelaygreater5Mins=0.02528861 
#Linear Model with Airline Status as predictor 
LMAirlineStatus<-lm(Satisfaction~AirlineStatus,data=df) summary(LMAirlineStatus) 
plot(Satisfaction~AirlineStatus,xlab="AirlineStatus",ylab="Satisfaction",data=df) 
 
#AirlineStatus=0.1184333  
#Linear Model with Age as predictor LMAge<-lm(Satisfaction~Age,data=df) summary(LMAge) 
plot(Satisfaction~Age,xlab="Age",ylab="Satisfaction",data=df) abline(LMAge) 
#Age=0.0492023 
#Linear Model with Gender as predictor LMGender<-lm(Satisfaction~Gender,data=df) summary(LMGender) 
plot(Satisfaction~Gender,xlab="Gender",ylab="Satisfaction",data=df) abline(LMGender) 
#Gender=0.01760919  
#Linear Model with Price Sensitivity as predictor 
LMPriceSensitivity<-lm(Satisfaction~PriceSensitivity,data=df) summary(LMPriceSensitivity) 
plot(Satisfaction~PriceSensitivity,xlab="PriceSensitivity",ylab="Satisfaction",data=df) abline(LMPriceSensitivity) 
#PriceSensitivity=0.007641272  
#Linear Model with Airline Status Year of first flight as predictor LMFirstFlight<-lm(Satisfaction~YearofFirstFlight,data=df) summary(LMFirstFlight) 
plot(Satisfaction~YearofFirstFlight,xlab="YearofFirstFlight",ylab="Satisfaction",data=df) abline(LMFirstFlight) 
#YearofFirstFlight=5.270168e-05 
 
 
#SVM Model: #svm 
df$happy<-df$Satisfaction df$happy[df$happy>=4]<-"happy" df$happy[df$happy<4]<-"unhappy" df1<data.frame(df$happy,df$AirlineStatus,df$Age,df$Gender,df$PriceSensitivity,df$YearofFirstFlig ht,df$NoofFlightspa,df$TypeofTravel,df$ShoppingAmountatAirport,df$Class,df$ScheduledDep artureHour,df$ArrivalDelaygreater5Mins) cutPoint2_3 <- floor(2 * dim(df1)[1]/3) randIndex <- sample(1:dim(df1)[1]) trainData <- df1[randIndex[1:cutPoint2_3],] testData <- df1[randIndex[(cutPoint2_3+1):dim(df1)[1]],] happy<-testData$df.happy table(happy) 
# happy unhappy  
# 1675    1518 1675/(1518+1675) dim(testData) dim(trainData) library(kernlab) 
 
#try to lower error rate. 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel, data=trainData, kernel = 
"rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus, data=trainData, kernel = 
"rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus+df.Age, data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus+df.Age+df.Gender, data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) svmOutput <- ksvm(df.happy ~ 
df.TypeofTravel+df.AirlineStatus+df.Age+df.Gender+df.PriceSensitivity, data=trainData, kernel 
= "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) str(trainData) 
svmOutput <- ksvm(df.happy ~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=250,cross=3, prob.model=TRUE) 
 
svmPred <- predict(svmOutput, testData, type = "votes") compTable <- data.frame(testData$df.happy,svmPred[2,]) table(compTable) 
table(compTable,testData$df.happy) 
 
ctable <- as.table(matrix(c(1388,291,474,1040), nrow = 2, byrow = TRUE)) 
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),conf.level = 0, margin = 1, main = "Confusion Matrix") 
 
 
 
 
 
 
#Visualization: 
AgeSat<-aggregate(df2[, 1], list(df2$Age), mean) 
AgeSat<-data.frame(AgeSat) 
#CompOverallSat 
colnames(AgeSat) <- c("Age", "AverageCustRating") 
AgeSat<-merge(x = AgeSat, y = countvar, by = "Age", all = TRUE) 
#AgeSat 
plot2<-ggplot(AgeSat, aes(x=Age, y=AverageCustRating, label=CountOfFlights)) + geom_text(aes(label=CountOfFlights), vjust=-1.0) + 
geom_bar(stat="identity",colour="white",fill="blue") +theme(axis.text.x = element_text(angle = 
90, hjust = 1))+ ggtitle("Age wise average Customer Satisfaction") + theme(plot.title= element_text(hjust=0.5))  
#plot2<-ggplot(AgeSat, aes(x=Age, y=AverageCustRating)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
#plot2 # Gender plot 
countvar<-data.frame(table(df$Gender)) colnames(countvar) <- c("Gender", "NoOfTravelers") countvar1<-aggregate(df[, 1], list(df2$Gender), mean) colnames(countvar1) <- c("Gender", "AverageSatisfaction") 
countvar<-merge(x = countvar, y = countvar1, by = "Gender", all = TRUE) 
#countvar 
plot3<-ggplot(countvar, aes(x=Gender, y=AverageSatisfaction)) + geom_text(aes(label=NoOfTravelers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="lightseagreen") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Gender wise Customers") + theme(plot.title= element_text(hjust=0.5))  # Airline Status  
grouped_data <- aggregate(df, by=list(df$AirlineStatus, df$Satisfaction), FUN=length); grouped_data <-grouped_data[,c(1:3)] 
#grouped_data 
colnames(grouped_data) <- c("AirlineStatus", "Satisfaction","NoOfTravelers") grouped_data 
plot4<-ggplot(grouped_data, aes(factor(Satisfaction), NoOfTravelers, fill = AirlineStatus)) +        geom_bar(stat = "identity", width = 0.2, position = "dodge") +   labs(list(x = "Satisfaction", y = "Number of Travellers",fill = "group")) 
#Type of travel 
TypeTravel<-aggregate(df[, 1], list(df2$TypeofTravel), mean) colnames(TypeTravel) <- c("TypeOfTravel", "AverageSatisfaction") TypeTravel1<-data.frame(table(df$TypeofTravel)) colnames(TypeTravel1) <- c("TypeOfTravel", "NoOfCustomers") 
TypeTrav<-merge(x = TypeTravel, y = TypeTravel1, by = "TypeOfTravel", all = TRUE) TypeTrav 
plot5<-ggplot(TypeTrav, aes(x=TypeOfTravel, y=AverageSatisfaction)) + geom_text(aes(label=NoOfCustomers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="red") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Customer Satisfaction based on Type of Travel") + theme(plot.title= element_text(hjust=0.5))  
 
 
str(df) 
a <- sub("Blue","1",df$AirlineStatus) b <- sub("Silver","2",a) c<-sub("Gold","3",b) d<-sub("Platinum","4",c) df$AirlineStatus<-d 
df$AirlineStatus<-as.numeric(df$AirlineStatus) df$AirlineStatus<-jitter(df$AirlineStatus) df$Satisfaction<-jitter(df$Satisfaction) library(ggplot2) 
g<-ggplot(df,aes(x=AirlineStatus,y=Satisfaction))+geom_point() g 
a <- sub("Personal Travel","1",df$TypeofTravel) b <- sub("Mileage tickets","2",a) c<-sub("Business travel","3",b) df$TypeofTravel<-c df$TypeofTravel<-as.numeric(df$TypeofTravel) df$TypeofTravel<-jitter(df$TypeofTravel) 
g1<-ggplot(df,aes(x=TypeofTravel,y=Satisfaction))+geom_point() g1 
#1 Blue, 2 Gold,3 Platinum, 4 silver df$TypeofTravel1<-as.integer(df$TypeofTravel)  df$TypeofTravel1<-jitter(df$TypeofTravel1) 
g1<-ggplot(df,aes(x=TypeofTravel1,y=Satisfaction1))+geom_point() g1 
 
 
 
#graph about satsifaction based on state. 
df<-df[df$AirlineCode=="US",] df1<-df 
x1<-gsub('.*\\,', '', df1$OrginCity) df1$OrginCity<-x1 
#CustPerCity<-data.frame(table(df1$OrginCity)) SatState<-aggregate(df1[, 1], list(df1$OrginCity), mean) colnames(SatState)<-c("state","sat") 
g<-
ggplot(SatState,aes(x=row.names(SatState),y=sat))+geom_bar(stat="identity")+theme(plot.title= element_text(hjust=0.5)) g 
 
df<-df[df$AirlineStatus=="Platinum",] 
LM7<-
lm(Satisfaction~Age+Gender+PriceSensitivity+YearofFirstFlight+NoofFlightspa+XofFlightwith otherAirlines+TypeofTravel+NoofotherLoyaltyCards+ShoppingAmountatAirport+EatingandDri nkingatAirport+Class+DayofMonth+Flightdate+ScheduledDepartureHour++Flightcancelled+De partureDelayinMinutes+ArrivalDelayinMinutes+Flighttimeinminutes+FlightDistance+ArrivalDe laygreater5Mins,data=df) summary(LM7) 
LM7<-lm(Satisfaction~Age+TypeofTravel,data=df) summary(LM7) 
 
AgeGroups<-cut(df$Age, breaks=c(18, 24,30,36,42,48,54,60,66,72,78,84,90), right = FALSE) 
#AgeGroups 
AgeGroups<-gsub(',', ' to ', AgeGroups) 
AgeGroups<-gsub('\\[', '', AgeGroups) AgeGroups<-gsub('\\)', '', AgeGroups) 
 
df$Age<-AgeGroups 
 
 
table(df$Gender) 
#Female   Male  
#139    173 
173/(173+139) #0.5544871795 table(df$TypeofTravel) 
#Business travel Mileage tickets Personal Travel  
#246              19              47  
47/294 
#0.1598639456     table(df$Class) 
#Business      Eco Eco Plus  
#     26      259       27 
259/294 
#0.880952381 table(noofflightspa) 
#Average    High     Low  
#   55     123     134 
123/294 # 0.4183673469 library(dplyr) 
count<-aggregate(df[, 9], list(df$AirlineName),count) count<-data.frame(count) g<-ggplot(df) df<-as(df,"transaction") barplot(df$TypeofTravel) 
 
 
 
#PLATINUM ANALYSIS 
plat1<-df 
plat1<-plat1[plat1$AirlineStatus == "Platinum",] str(plat1) 
 
vBuckets<-replicate(length(plat1$Satisfaction),"Median") vBuckets[plat1$Satisfaction>3]<-"High" vBuckets[plat1$Satisfaction<3]<-"Low" 
 
plat1$Satisfaction<-as.factor(vBuckets) 
 
plat1Agg<-aggregate(plat1[, 11], list(plat1$Satisfaction), mean)#ShoppingAmountatAirport plat1Agg 
plat1Agg<-aggregate(plat1[, 12], list(plat1$Satisfaction), mean)#EatingandDrinkingatAirport plat1Agg 
plat1Agg<-aggregate(plat1[, 5], list(plat1$Satisfaction), mean) #PriceSensitivity plat1Agg 
 
 
#summary(fulldf) fdf<-fulldf 
#fulldf$AirlineName 
# plotting var charts 
CompOverallSat<-aggregate(fdf[, 1], list(fdf$AirlineName), mean) 
CompOverallSat<-data.frame(CompOverallSat) 
#CompOverallSat 
colnames(CompOverallSat) <- c("Airline", "AverageCustRating") library(ggplot2) CompOverallSat 
plot1<-ggplot(CompOverallSat, aes(x=Airline, y=AverageCustRating)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) 
#CompOverallSat <- CompOverallSat[order(CompOverallSat$AverageCustRating),]  
CompOverallSat 
CompOverallSat$Airline <- factor(CompOverallSat$Airline, levels = CompOverallSat$Airline[order(CompOverallSat$AverageCustRating)]) plot2<-ggplot(CompOverallSat, aes(x=Airline, y=AverageCustRating)) + 
geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))  GenderData <- data.frame(table(df$Gender)) colnames(GenderData) <- c("Gender", "NoOfTravelers") 
GenderDistribution <- ggplot(GenderData, aes(x=Gender, y=NoOfTravelers)) + geom_text(aes(label=NoOfTravelers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="blue") +theme(axis.text.x = element_text(angle = 
90, hjust = 1))+ ggtitle("Gender wise travelers - Southeast Airlines") newCol<-colnames(df) newCol<-gsub("\\.", "", newCol) colnames(df)<-newCol 
a <- sub("No","0",df$Flightcancelled) b <- sub("Yes","1",a) df$Flightcancelled <- b 
df$Flightcancelled<-as.numeric(df$Flightcancelled) df$Satisfaction<- as.numeric(as.character(df$Satisfaction)) us <- map_data("state") 
dfSoutheast<-df[df$AirlineCode=="US",] str(dfSoutheast) install.packages("mice") library(VIM) 
dfSoutheast<-dfSoutheast[complete.cases(dfSoutheast),] dffemale<-dfSoutheast[dfSoutheast$Gender=="Female",] 
dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport>0]<-"shopping" dffemale$ShoppingAmountatAirport[dffemale$ShoppingAmountatAirport==0]<-"not shopping" delay<-aggregate(dffemale[, 1], list(dffemale$ShoppingAmountatAirport), mean) delay<-aggregate(dfSoutheast[, 23], list(dfSoutheast$OriginState), mean) colnames(delay)<-c("X","Avaragesatisfaction") plot<-ggplot(delay, aes(x=X, y=Avaragesatisfaction)) + geom_bar(stat="identity",colour="white",fill="blue") + 
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Average Satisfaction In 
Man") 
plot 
 
delay<-aggregate(dfSoutheast[, "ArrivalDelayinMinutes"], list(dfSoutheast$OriginState), mean) mergedf<-merge(dfSoutheast$OriginState) 
GenderData <- data.frame(table(dfSoutheast$OriginState)) colnames(GenderData) <- c("stateName", "NoOfTravelers") str(GenderData) 
View(GenderData) 
GenderData$State<-tolower(GenderData$State) 
GenderData <- GenderData[-39,] 
GenderData <- GenderData[-45,] 
GenderData$state.abb<-statemap$state.abb 
GenderData$stateName<-tolower(GenderData$stateName) statemap <- data.frame(state.abb) statemap$Lon <- state.center$x statemap$Lat <- state.center$y 
GenderData$lon<-statemap$Lon GenderData$lat<-statemap$Lat 
map<-ggplot(GenderData,aes(map_id=stateName))+geom_map(map=us, 
aes(fill=NoOfTravelers))+expand_limits(x = us$long, y = us$lat)+coord_map() + ggtitle("No.Of Travelers per state")+geom_text(aes(x=GenderData$lon, y=GenderData$lat, 
label=GenderData$state.abb), size=2)+scale_fill_gradient(low = "white", high = "blue", guide = 
"colorbar") map 
statemap <- data.frame(state.abb) statemap$Lon <- state.center$x statemap$Lat <- state.center$y str(statemap) 
GenderData$StateAbb <- state.abb[match(GenderData$State, state.name)] 
View(GenderData) 
 
GenderData$Lon <- state.center$x 
GenderData$Lat <- state.center$y 
View(GenderData) 
 
 
 
str(raw_data) 
  
  
CleanSatisfaction<-raw_data[(raw_data$Satisfaction=="1" |              raw_data$Satisfaction=="1.5" |             raw_data$Satisfaction=="2" |             raw_data$Satisfaction=="2.5" |             raw_data$Satisfaction=="3" |             raw_data$Satisfaction=="3.5" |             raw_data$Satisfaction=="4" |             raw_data$Satisfaction=="4.5" |             raw_data$Satisfaction=="5" ),] 
#clean_data <- subset(raw_data,trimws(raw_data$Satisfaction)==c(1:5)) 
#clean_data df<-CleanSatisfaction newCol<-colnames(CleanSatisfaction) newCol<-gsub("\\.", "", newCol) newCol colnames(df)<-newCol 
  
  
a <- sub("No","0",df$Flightcancelled) b <- sub("Yes","1",a) df$Flightcancelled <- b 
df$Flightcancelled<-as.numeric(df$Flightcancelled) 
   
m <- mode(df$Age) m 
  
} 
  
vBuckets<-replicate(length(df$Satisfaction),"Median") vBuckets[df$Satisfaction>3]<-"High" vBuckets[df$Satisfaction<3]<-"Low" 
  
Satisfaction<-as.factor(vBuckets) 
  
age<-createBuckets(df$Age) 
pricesensitive<-createBuckets(df$PriceSensitivity) yearoffirstflight<-createBuckets(df$YearofFirstFlight) noofflightspa<-createBuckets(df$NoofFlightspa) shoppingamount<-createBuckets(df$ShoppingAmountatAirport) scheduleddeparturehour<-createBuckets(df$ScheduledDepartureHour) hotelSurveyruleDFSE<-as(ruleDF,"transactions") 
  
rulesetsoutheastH<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.8),appearance = list(default="lhs", rhs=("Satisfaction=High"))) 
  
goodrulesH<-sort(rulesetsoutheastH,by="lift")[1:5] inspect(goodrulesH) 
plot1<-plot(goodrulesH, method = "graph", engine = "htmlwidget")  rulesetsoutheastL<- apriori(hotelSurveyruleDFSE, parameter=list(support=0.05, confidence=0.5),appearance = list(default="lhs", rhs=("Satisfaction=Low"))) goodrules2<-sort(rulesetsoutheastL,by="lift")[1:10] 
plot2<-plot(goodrules2, method = "graph", engine = "htmlwidget") 
 
. 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel, data=trainData, kernel = 
"rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus, data=trainData, kernel = 
"rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus+df.Age, data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) 
svmOutput <- ksvm(df.happy ~ df.TypeofTravel+df.AirlineStatus+df.Age+df.Gender, data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) svmOutput <- ksvm(df.happy ~ 
df.TypeofTravel+df.AirlineStatus+df.Age+df.Gender+df.PriceSensitivity, data=trainData, kernel 
= "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) svmOutput <- ksvm(df.happy ~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE) svmOutput <- ksvm(df.happy ~ ., data=trainData, kernel = "rbfdot",kpar="automatic",C=1,cross=3, prob.model=TRUE)  svmPred <- predict(svmOutput, testData, type = "votes") compTable <- data.frame(testData$df.happy,svmPred[2,]) table(compTable) 
plot3<-ggplot(countvar, aes(x=Gender, y=AverageSatisfaction)) + geom_text(aes(label=NoOfTravelers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="lightseagreen") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Gender wise Customers") + theme(plot.title= element_text(hjust=0.5))  
  
# Airline Status   grouped_data <- aggregate(df, by=list(df$AirlineStatus, df$Satisfaction), FUN=length); grouped_data <-grouped_data[,c(1:3)] 
#grouped_data 
colnames(grouped_data) <- c("AirlineStatus", "Satisfaction","NoOfTravelers") grouped_data  
plot4<-ggplot(grouped_data, aes(factor(Satisfaction), NoOfTravelers, fill = AirlineStatus)) +        geom_bar(stat = "identity", width = 0.2, position = "dodge") +   labs(list(x = "Satisfaction", y = "Number of Travellers",fill = "group"))  
#Type of travel  
TypeTravel<-aggregate(df[, 1], list(df2$TypeofTravel), mean) colnames(TypeTravel) <- c("TypeOfTravel", "AverageSatisfaction") TypeTravel1<-data.frame(table(df$TypeofTravel)) colnames(TypeTravel1) <- c("TypeOfTravel", "NoOfCustomers") 
TypeTrav<-merge(x = TypeTravel, y = TypeTravel1, by = "TypeOfTravel", all = TRUE) TypeTrav  plot5<-ggplot(TypeTrav, aes(x=TypeOfTravel, y=AverageSatisfaction)) + geom_text(aes(label=NoOfCustomers), vjust=-1.0) + geom_bar(stat="identity",colour="white",fill="red") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Customer Satisfaction based on Type of Travel") + theme(plot.title= element_text(hjust=0.5))  
