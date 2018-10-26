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

# mark match outcome
matches[,Result:=0]
matches[AwayGoals==HomeGoals, Result:=1] #TIE
matches[AwayGoals<HomeGoals, Result:=2] #HOME
matches[AwayGoals>HomeGoals,Result:=3] #AWAY
# remove unnecessary columns
matches = matches[,c("leagueId","home", "away","score","date","type","match_date","match_time","Year","HomeGoals","AwayGoals"):=NULL]


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

#PCA 
pca=princomp(merged_pinnacle_final[,2:8],cor = TRUE)
plot(pca, main = 'Pinnacle odds - PCA')
str(pca)
summary(pca)

plot(pca$scores[,1],pca$scores[,2],col=merged_pinnacle_final$Result,pch=".",cex=5,xlab='Principal Component 1', ylab='Principal Component 2', main = 'Pinnacle PCA for Match Outcomes')
legend(-7, 8, legend=c("TIE", "HOME", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

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

#PCA 
pca_2=princomp(merged_SBOBET_final[,2:11],cor = TRUE)
str(pca_2)
summary(pca_2)

plot(pca_2$scores[,1],pca_2$scores[,2],col=merged_SBOBET_final$Result,pch=".",cex=5,xlab='Principal Component 1', ylab='Principal Component 2', main = 'SBOBET PCA for Match Outcomes')
legend(-7, 8, legend=c("TIE", "HOME", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

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

#PCA 
pca_3=princomp(merged_Unibet_final[,2:13],cor = TRUE)
str(pca_3)
summary(pca_3)

plot(pca_3$scores[,1],pca_3$scores[,2],col=merged_Unibet_final$Result,pch=".",cex=5,xlab='Principal Component 1', ylab='Principal Component 2', main = 'Unibet PCA for Match Outcomes')
legend(-7, 8, legend=c("TIE", "HOME", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

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

#PCA 
pca_4=princomp(merged_BetVictor_final[,2:13],cor = TRUE)
str(pca_4)
summary(pca_4)

plot(pca_4$scores[,1],pca_4$scores[,2],col=merged_BetVictor_final$Result,pch=".",cex=5,xlab='Principal Component 1', ylab='Principal Component 2', main = 'BetVictor PCA for Match Outcomes')
legend(-7, 8, legend=c("TIE", "HOME", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)

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

#PCA 
pca_5=princomp(merged_bet365_final[,2:13],cor = TRUE)
str(pca_5)
summary(pca_5)

plot(pca_5$scores[,1],pca_5$scores[,2],col=merged_bet365_final$Result,pch=".",cex=5,xlab='Principal Component 1', ylab='Principal Component 2', main = 'bet365 PCA for Match Outcomes')
legend(-7, 8, legend=c("TIE", "HOME", "AWAY"),
       col=c(1, 2,3), pch=".", cex=0.8, pt.cex=5)