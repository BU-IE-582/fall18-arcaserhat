require(data.table)
require(anytime)

matches_file_path='C:/Users/asus/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/asus/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)
odds=readRDS(odd_details_file_path)

data.table(matches)
data.table(odds)
matches=unique(matches)

#transform unix time to date
matches[,match_date:=anydate(date)]
#transform unix time to date time
matches[,match_time:=anytime(date)]
matches[,Year:=year(match_time)]

#filter na scores
matches=na.omit(matches,cols="score")

#order by home team and match date (decreasing)
matches=matches[order(home,-match_time)]
matches[,c("HomeGoals","AwayGoals"):=tstrsplit(score,':')]
#transform characters to numeric for scores
matches$HomeGoals=as.numeric(matches$HomeGoals)
matches[,AwayGoals:=as.numeric(AwayGoals)]
#calculate total goals
matches[,TotalGoals:=HomeGoals+AwayGoals]

# mark over under
matches[,IsOver:=0]
matches[TotalGoals>2.5,IsOver:=1]
# remove unnecessary columns
matches = matches[,c("leagueId","home", "away","score","date","type","match_date","match_time","Year","HomeGoals","AwayGoals","TotalGoals"):=NULL]


# 1-Pinnacle
pinnacle_odds=odds[bookmaker=='Pinnacle']

#order data in ascending date
odd_data_pinnacle=pinnacle_odds[order(matchId,oddtype,bookmaker,date)]
#filter over under 2.5
odds_ou_2.5=odd_data_pinnacle[betType=='ou' & totalhandicap=='2.5']

#take final odds and transform to wide format            

odds_final=odd_data_pinnacle[,list(final_odd=odd[.N]),
                             by=list(matchId,oddtype,bookmaker)]
wide_odds_final=dcast(odds_final,matchId~oddtype+bookmaker,
                      value.var='final_odd')
#remove over/under columns
wide_odds_final[,over_Pinnacle:=NULL]
wide_odds_final[,under_Pinnacle:=NULL]

#consider over/under 2.5 only
odds_ou_2.5_final=odds_ou_2.5[,list(final_odd=odd[.N]),
                              by=list(matchId,oddtype,bookmaker)]
wide_odds_ou_2.5_final=dcast(odds_ou_2.5_final,
                             matchId~oddtype+bookmaker,
                             value.var='final_odd')

#merge columns
merged_matches_pinnacle=merge(wide_odds_final,wide_odds_ou_2.5_final,by='matchId')
merged_matches_pinnacle=merged_matches_pinnacle[complete.cases(merged_matches_pinnacle)]
merged_pinnacle_final=merge(merged_matches_pinnacle,matches,by='matchId')

#Euclidean MDS 

euclideanMDS=dist(merged_pinnacle_final[,2:8])

distance.matrix_euclidean = dist(scale((merged_pinnacle_final[,2:8]), center=TRUE, scale=TRUE),
                        method="euclidean")

mds.stuff = cmdscale(distance.matrix_euclidean, eig=TRUE, x.ret=TRUE)

mds.var.per = round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

mds.values = mds.stuff$points
mds.data = data.frame(X=mds.values[,1],
                       Y=mds.values[,2])
require(ggplot2)
ggplot(data=mds.data, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("Pinnacle MDS plot using Euclidean distance")

#Manhattan MDS

ManhattanMDS=dist(merged_pinnacle_final[,2:8])

distance.matrix_manhattan = dist(scale((merged_pinnacle_final[,2:8]), center=TRUE, scale=TRUE),
                        method="manhattan")

mds.stuff_manhattan = cmdscale(distance.matrix_manhattan, eig=TRUE, x.ret=TRUE)

mds.var.per_manhattan = round(mds.stuff_manhattan$eig/sum(mds.stuff_manhattan$eig)*100, 1)

mds.values_manhattan = mds.stuff_manhattan$points
mds.data_manhattan = data.frame(X=mds.values_manhattan[,1],
                       Y=mds.values_manhattan[,2])

require(ggplot2)
ggplot(data=mds.data_manhattan, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_manhattan[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_manhattan[2], "%", sep="")) +
  ggtitle("Pinnacle MDS plot using Manhattan distance")

#2- SBOBET
SBOBET_odds=odds[bookmaker=='SBOBET']

#order data in ascending date
odd_data_SBOBET=SBOBET_odds[order(matchId,oddtype,bookmaker,date)]
#filter over under 2.5
odds_ou_2.5_2=odd_data_SBOBET[betType=='ou' & totalhandicap=='2.5']

#take final odds and transform to wide format            

odds_final_2=odd_data_SBOBET[,list(final_odd=odd[.N]),
                             by=list(matchId,oddtype,bookmaker)]
wide_odds_final_2=dcast(odds_final_2,matchId~oddtype+bookmaker,
                        value.var='final_odd')
#remove over/under columns
wide_odds_final_2[,over_SBOBET:=NULL]
wide_odds_final_2[,under_SBOBET:=NULL]

#consider over/under 2.5 only
odds_ou_2.5_final_2=odds_ou_2.5_2[,list(final_odd=odd[.N]),
                                  by=list(matchId,oddtype,bookmaker)]
wide_odds_ou_2.5_final_2=dcast(odds_ou_2.5_final_2,
                               matchId~oddtype+bookmaker,
                               value.var='final_odd')

#merge columns
merged_matches_SBOBET=merge(wide_odds_final_2,wide_odds_ou_2.5_final_2,by='matchId')
merged_matches_SBOBET=merged_matches_SBOBET[complete.cases(merged_matches_SBOBET)]
merged_SBOBET_final=merge(merged_matches_SBOBET,matches,by='matchId')

#Euclidean MDS 

euclideanMDS_2=dist(merged_SBOBET_final[,2:11])

distance.matrix_euclidean_2 = dist(scale((merged_SBOBET_final[,2:11]), center=TRUE, scale=TRUE),
                                  method="euclidean")

mds.stuff_2 = cmdscale(distance.matrix_euclidean_2, eig=TRUE, x.ret=TRUE)

mds.var.per_2 = round(mds.stuff_2$eig/sum(mds.stuff_2$eig)*100, 1)

mds.values_2 = mds.stuff_2$points
mds.data_2 = data.frame(X=mds.values_2[,1],
                       Y=mds.values_2[,2])
require(ggplot2)
ggplot(data=mds.data_2, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_2[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_2[2], "%", sep="")) +
  ggtitle("SSOBET MDS plot using Euclidean distance")

#Manhattan MDS

ManhattanMDS_2=dist(merged_SBOBET_final[,2:11])

distance.matrix_manhattan_2 = dist(scale((merged_SBOBET_final[,2:11]), center=TRUE, scale=TRUE),
                                  method="manhattan")

mds.stuff_manhattan_2 = cmdscale(distance.matrix_manhattan_2, eig=TRUE, x.ret=TRUE)

mds.var.per_manhattan_2 = round(mds.stuff_manhattan_2$eig/sum(mds.stuff_manhattan_2$eig)*100, 1)

mds.values_manhattan_2 = mds.stuff_manhattan_2$points
mds.data_manhattan_2 = data.frame(X=mds.values_manhattan_2[,1],
                                 Y=mds.values_manhattan_2[,2])

require(ggplot2)
ggplot(data=mds.data_manhattan_2, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_manhattan_2[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_manhattan_2[2], "%", sep="")) +
  ggtitle("SSOBET MDS plot using Manhattan distance")

#3- Unibet
Unibet_odds=odds[bookmaker=='Unibet']

#order data in ascending date
odd_data_Unibet=Unibet_odds[order(matchId,oddtype,bookmaker,date)]
#filter over under 2.5
odds_ou_2.5_3=odd_data_Unibet[betType=='ou' & totalhandicap=='2.5']

#take final odds and transform to wide format            

odds_final_3=odd_data_Unibet[,list(final_odd=odd[.N]),
                             by=list(matchId,oddtype,bookmaker)]
wide_odds_final_3=dcast(odds_final_3,matchId~oddtype+bookmaker,
                        value.var='final_odd')
#remove over/under columns
wide_odds_final_3[,over_Unibet:=NULL]
wide_odds_final_3[,under_Unibet:=NULL]

#consider over/under 2.5 only
odds_ou_2.5_final_3=odds_ou_2.5_3[,list(final_odd=odd[.N]),
                                  by=list(matchId,oddtype,bookmaker)]
wide_odds_ou_2.5_final_3=dcast(odds_ou_2.5_final_3,
                               matchId~oddtype+bookmaker,
                               value.var='final_odd')

#merge columns
merged_matches_Unibet=merge(wide_odds_final_3,wide_odds_ou_2.5_final_3,by='matchId')
merged_matches_Unibet=merged_matches_Unibet[complete.cases(merged_matches_Unibet)]
merged_Unibet_final=merge(merged_matches_Unibet,matches,by='matchId')

#Euclidean MDS 

euclideanMDS_3=dist(merged_Unibet_final[,2:13])

distance.matrix_euclidean_3 = dist(scale((merged_Unibet_final[,2:13]), center=TRUE, scale=TRUE),
                                  method="euclidean")

mds.stuff_3 = cmdscale(distance.matrix_euclidean_3, eig=TRUE, x.ret=TRUE)
.
mds.var.per_3 = round(mds.stuff_3$eig/sum(mds.stuff_3$eig)*100, 1)

mds.values_3 = mds.stuff_3$points
mds.data_3 = data.frame(X=mds.values_3[,1],
                       Y=mds.values_3[,2])
require(ggplot2)
ggplot(data=mds.data_3, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_3[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_3[2], "%", sep="")) +
  ggtitle("UniBet MDS plot using Euclidean distance")

#Manhattan MDS

ManhattanMDS_3=dist(merged_Unibet_final[,2:13])

distance.matrix_manhattan_3 = dist(scale((merged_Unibet_final[,2:13]), center=TRUE, scale=TRUE),
                                  method="manhattan")

mds.stuff_manhattan_3 = cmdscale(distance.matrix_manhattan_3, eig=TRUE, x.ret=TRUE)
.
mds.var.per_manhattan_3 = round(mds.stuff_manhattan_3$eig/sum(mds.stuff_manhattan_3$eig)*100, 1)

mds.values_manhattan_3 = mds.stuff_manhattan_3$points
mds.data_manhattan_3 = data.frame(X=mds.values_manhattan_3[,1],
                                 Y=mds.values_manhattan_3[,2])

require(ggplot2)
ggplot(data=mds.data_manhattan_3, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_manhattan_3[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_manhattan_3[2], "%", sep="")) +
  ggtitle("Unibet MDS plot using Manhattan distance")

#4- BetVictor
BetVictor_odds=odds[bookmaker=='BetVictor']

#order data in ascending date
odd_data_BetVictor=BetVictor_odds[order(matchId,oddtype,bookmaker,date)]
#filter over under 2.5
odds_ou_2.5_4=odd_data_BetVictor[betType=='ou' & totalhandicap=='2.5']

#take final odds and transform to wide format            

odds_final_4=odd_data_BetVictor[,list(final_odd=odd[.N]),
                                by=list(matchId,oddtype,bookmaker)]
wide_odds_final_4=dcast(odds_final_4,matchId~oddtype+bookmaker,
                        value.var='final_odd')
#remove over/under columns
wide_odds_final_4[,over_BetVictor:=NULL]
wide_odds_final_4[,under_BetVictor:=NULL]

#consider over/under 2.5 only
odds_ou_2.5_final_4=odds_ou_2.5_4[,list(final_odd=odd[.N]),
                                  by=list(matchId,oddtype,bookmaker)]
wide_odds_ou_2.5_final_4=dcast(odds_ou_2.5_final_4,
                               matchId~oddtype+bookmaker,
                               value.var='final_odd')

#merge columns
merged_matches_BetVictor=merge(wide_odds_final_4,wide_odds_ou_2.5_final_4,by='matchId')
merged_matches_BetVictor=merged_matches_BetVictor[complete.cases(merged_matches_BetVictor)]
merged_BetVictor_final=merge(merged_matches_BetVictor,matches,by='matchId')

#Euclidean MDS 

euclideanMDS_4=dist(merged_BetVictor_final[,2:13])

distance.matrix_euclidean_4 = dist(scale((merged_BetVictor_final[,2:13]), center=TRUE, scale=TRUE),
                                    method="euclidean")

mds.stuff_4 = cmdscale(distance.matrix_euclidean_4, eig=TRUE, x.ret=TRUE)

mds.var.per_4 = round(mds.stuff_4$eig/sum(mds.stuff_4$eig)*100, 1)

mds.values_4 = mds.stuff_4$points
mds.data_4 = data.frame(X=mds.values_4[,1],
                         Y=mds.values_4[,2])
require(ggplot2)
ggplot(data=mds.data_4, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_4[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_4[2], "%", sep="")) +
  ggtitle("BetVictor MDS plot using Euclidean distance")

#Manhattan MDS

ManhattanMDS_4=dist(merged_BetVictor_final[,2:13])

distance.matrix_manhattan_4 = dist(scale((merged_BetVictor_final[,2:13]), center=TRUE, scale=TRUE),
                                    method="manhattan")

mds.stuff_manhattan_4 = cmdscale(distance.matrix_manhattan_4, eig=TRUE, x.ret=TRUE)

mds.var.per_manhattan_4 = round(mds.stuff_manhattan_4$eig/sum(mds.stuff_manhattan_4$eig)*100, 1)
mds.values_manhattan_4 = mds.stuff_manhattan_4$points
mds.data_manhattan_4 = data.frame(X=mds.values_manhattan_4[,1],
                                   Y=mds.values_manhattan_4[,2])

require(ggplot2)
ggplot(data=mds.data_manhattan_4, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_manhattan_4[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_manhattan_4[2], "%", sep="")) +
  ggtitle("BetVictor MDS plot using Manhattan distance")

#5- bet365
bet365_odds=odds[bookmaker=='bet365']

#order data in ascending date
odd_data_bet365=bet365_odds[order(matchId,oddtype,bookmaker,date)]
#filter over under 2.5
odds_ou_2.5_5=odd_data_bet365[betType=='ou' & totalhandicap=='2.5']

#take final odds and transform to wide format            

odds_final_5=odd_data_bet365[,list(final_odd=odd[.N]),
                             by=list(matchId,oddtype,bookmaker)]
wide_odds_final_5=dcast(odds_final_5,matchId~oddtype+bookmaker,
                        value.var='final_odd')
#remove over/under columns
wide_odds_final_5[,over_bet365:=NULL]
wide_odds_final_5[,under_bet365:=NULL]

#consider over/under 2.5 only
odds_ou_2.5_final_5=odds_ou_2.5_5[,list(final_odd=odd[.N]),
                                  by=list(matchId,oddtype,bookmaker)]
wide_odds_ou_2.5_final_5=dcast(odds_ou_2.5_final_5,
                               matchId~oddtype+bookmaker,
                               value.var='final_odd')

#merge columns
merged_matches_bet365=merge(wide_odds_final_5,wide_odds_ou_2.5_final_5,by='matchId')
merged_matches_bet365=merged_matches_bet365[complete.cases(merged_matches_bet365)]
merged_bet365_final=merge(merged_matches_bet365,matches,by='matchId')

#Euclidean MDS 

euclideanMDS_5=dist(merged_bet365_final[,2:13])

distance.matrix_euclidean_5 = dist(scale((merged_bet365_final[,2:13]), center=TRUE, scale=TRUE),
                                   method="euclidean")

mds.stuff_5 = cmdscale(distance.matrix_euclidean_5, eig=TRUE, x.ret=TRUE)

mds.var.per_5 = round(mds.stuff_5$eig/sum(mds.stuff_5$eig)*100, 1)

mds.values_5 = mds.stuff_5$points
mds.data_5 = data.frame(X=mds.values_5[,1],
                         Y=mds.values_5[,2])
require(ggplot2)
ggplot(data=mds.data_5, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_5[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_5[2], "%", sep="")) +
  ggtitle("bet365 MDS plot using Euclidean distance")

#Manhattan MDS

ManhattanMDS_5=dist(merged_bet365_final[,2:13])

distance.matrix_manhattan_5 = dist(scale((merged_bet365_final[,2:13]), center=TRUE, scale=TRUE),
                                   method="manhattan")

mds.stuff_manhattan_5 = cmdscale(distance.matrix_manhattan_5, eig=TRUE, x.ret=TRUE)

mds.var.per_manhattan_5 = round(mds.stuff_manhattan_5$eig/sum(mds.stuff_manhattan_5$eig)*100, 1)

mds.values_manhattan_5 = mds.stuff_manhattan_5$points
mds.data_manhattan_5 = data.frame(X=mds.values_manhattan_5[,1],
                                  Y=mds.values_manhattan_5[,2])

require(ggplot2)
ggplot(data=mds.data_manhattan_5, aes(x=X, y=Y)) +geom_point()+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per_manhattan_5[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per_manhattan_5[2], "%", sep="")) +
  ggtitle("bet365 MDS plot using Manhattan distance")
