require(data.table)
require(anytime)

matches_file_path='C:/Users/asus/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_file_path='C:/Users/asus/Desktop/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

matches=readRDS(matches_file_path)

odds=readRDS(odd_details_file_path)
matches=unique(matches)

#transform unix time to date
matches[,match_date:=anydate(date)]
#transform unix time to date time
matches[,match_time:=anytime(date)]
matches[,Year:=year(match_time)]

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

#filter na scores
matches=na.omit(matches,cols="score")

#filter over under 2.5
odds_ov_un=odds[betType=='ou' & totalhandicap=='2.5']

#remove total handicap
odds_ov_un[,totalhandicap:=NULL]

#order data in ascending date
odds_ov_un=odds_ov_un[order(matchId, oddtype,bookmaker,date)]

odds_ov_un_initial=odds_ov_un[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_ov_un_final=odds_ov_un[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

#transform to wide format            
wide_odds_initial=dcast(odds_ov_un_initial,
                        matchId~oddtype+bookmaker,
                        value.var='start_odd')

#transform to long
long_ov_un_initial=melt(wide_odds_initial,id.vars=1,
                        measure.vars=2:ncol(wide_odds_initial))

#1- Pinnacle
#get pinnacle initial over under odds
pinnacle_over_under=odds_ov_un_initial[bookmaker=='Pinnacle']

pinnacle_wide=dcast(pinnacle_over_under,
                    matchId~oddtype,
                    value.var='start_odd')

# join odds with matches
merged_matches=merge(matches,pinnacle_wide,by='matchId')

#setkey(matches,matchId)
#setkey(pinnacle_wide,matchId)
#joined_matches=pinnacle_wide[matches]

merged_matches[,probOver:=1/over]
merged_matches[,probUnder:=1/under]

merged_matches[,totalProb:=probOver+probUnder]

merged_matches[,probOver:=probOver/totalProb]
merged_matches[,probUnder:=probUnder/totalProb]

merged_matches=merged_matches[complete.cases(merged_matches)]
merged_matches[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table=merged_matches[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(Year,odd_cut_over)]
plot(summary_table[,list(empirical_over,probabilistic_over)],main='Pinnacle over probabilities',xlim = c(0,1), ylim = c(0,1),cex=4)
abline(0,1,col='red')
summary_table=summary_table[order(Year)]

#2- bet-at-home
#get bet-at-home initial over under odds
bet_at_home_over_under=odds_ov_un_initial[bookmaker=='bet-at-home']

bet_at_home_wide=dcast(bet_at_home_over_under,
                    matchId~oddtype,
                    value.var='start_odd')

# join odds with matches
merged_matches2=merge(matches,bet_at_home_wide,by='matchId')

#setkey(matches,matchId)
#setkey(bet_at_home_wide,matchId)
#joined_matches=bet_at_home_wide[matches]

merged_matches2[,probOver:=1/over]
merged_matches2[,probUnder:=1/under]

merged_matches2[,totalProb:=probOver+probUnder]

merged_matches2[,probOver:=probOver/totalProb]
merged_matches2[,probUnder:=probUnder/totalProb]

merged_matches2=merged_matches2[complete.cases(merged_matches2)]
merged_matches2[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches2[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table2=merged_matches2[,list(empirical_over=mean(IsOver),
                                   probabilistic_over=mean(probOver),.N),
                             by=list(Year,odd_cut_over)]
plot(summary_table2[,list(empirical_over,probabilistic_over)],main='bet-at-home over probabilities',xlim = c(0,1), ylim = c(0,1),cex=4)
abline(0,1,col='red')
summary_table2=summary_table2[order(Year)]

#3- Unibet
#Unibet initial over under odds
Unibet_over_under=odds_ov_un_initial[bookmaker=='Unibet']

Unibet_wide=dcast(Unibet_over_under,
                       matchId~oddtype,
                       value.var='start_odd')

# join odds with matches
merged_matches3=merge(matches,Unibet_wide,by='matchId')

#setkey(matches,matchId)
#setkey(Unibet_wide,matchId)
#joined_matches=Unibet_wide[matches]

merged_matches3[,probOver:=1/over]
merged_matches3[,probUnder:=1/under]

merged_matches3[,totalProb:=probOver+probUnder]

merged_matches3[,probOver:=probOver/totalProb]
merged_matches3[,probUnder:=probUnder/totalProb]

merged_matches3=merged_matches3[complete.cases(merged_matches3)]
merged_matches3[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches3[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table3=merged_matches3[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]
plot(summary_table3[,list(empirical_over,probabilistic_over)], main='Unibet over probabilities',xlim = c(0,1), ylim = c(0,1),cex=4)
abline(0,1,col='red')
summary_table3=summary_table3[order(Year)]

#4- BetVictor
#BetVictor initial over under odds
BetVictor_over_under=odds_ov_un_initial[bookmaker=='BetVictor']

BetVictor_wide=dcast(BetVictor_over_under,
                  matchId~oddtype,
                  value.var='start_odd')

# join odds with matches
merged_matches4=merge(matches,BetVictor_wide,by='matchId')

#setkey(matches,matchId)
#setkey(BetVictor_wide,matchId)
#joined_matches=BetVictor_wide[matches]

merged_matches4[,probOver:=1/over]
merged_matches4[,probUnder:=1/under]

merged_matches4[,totalProb:=probOver+probUnder]

merged_matches4[,probOver:=probOver/totalProb]
merged_matches4[,probUnder:=probUnder/totalProb]

merged_matches4=merged_matches4[complete.cases(merged_matches4)]
merged_matches4[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches4[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table4=merged_matches4[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]
plot(summary_table4[,list(empirical_over,probabilistic_over)],main='BetVictor over probabilities',xlim = c(0,1), ylim = c(0,1),cex=4)
abline(0,1,col='red')
summary_table4=summary_table4[order(Year)]

#5- bet365
#bet365 initial over under odds
bet365_over_under=odds_ov_un_initial[bookmaker=='bet365']

bet365_wide=dcast(bet365_over_under,
                     matchId~oddtype,
                     value.var='start_odd')

# join odds with matches
merged_matches5=merge(matches,bet365_wide,by='matchId')

#setkey(matches,matchId)
#setkey(bet365_wide,matchId)
#joined_matches=bet365_wide[matches]

merged_matches5[,probOver:=1/over]
merged_matches5[,probUnder:=1/under]

merged_matches5[,totalProb:=probOver+probUnder]

merged_matches5[,probOver:=probOver/totalProb]
merged_matches5[,probUnder:=probUnder/totalProb]

merged_matches5=merged_matches5[complete.cases(merged_matches5)]
merged_matches5[,totalProb:=NULL]

cutpoints=seq(0,1,0.05)
merged_matches5[,odd_cut_over:=cut(probOver,cutpoints)]

summary_table5=merged_matches5[,list(empirical_over=mean(IsOver),
                                     probabilistic_over=mean(probOver),.N),
                               by=list(Year,odd_cut_over)]
plot(summary_table5[,list(empirical_over,probabilistic_over)],main='bet365 over probabilities',xlim = c(0,1), ylim = c(0,1),cex=4)
abline(0,1,col='red')
summary_table5=summary_table5[order(Year)]

## PART 1.b
#Yearly change in bet365 odds (ou bettype & over odds)

cutpoints_b=seq(0,1,0.1)
merged_matches_b=merged_matches[,odd_cut_over:=cut(probOver,cutpoints_b)]

summary_table_b=merged_matches_b[,list(empirical_over=mean(IsOver),
                                       probabilistic_over=mean(probOver),.N),
                                 by=list(Year,odd_cut_over)]


summary_table_b=summary_table_b[order(Year)]
bin1 =summary_table_b[odd_cut_over=="(0.3,0.4]"]
plot(bin1[,list(Year,empirical_over)], pch=3, ylim = c(0,1), main="(0.3,0.4] bin", ylab='Probability', col="red")
par(new=TRUE)
plot(bin1[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2015,1, c("Empirical Over","Probabilistic Over"),pch=c(1,3), col =c("black","red"))

bin2 =summary_table_b[odd_cut_over=="(0.4,0.5]"]
plot(bin2[,list(Year,empirical_over)],pch=3, ylim = c(0,1), main="(0.4,0.5] bin", ylab='Probabilities', col="red")
par(new=TRUE)
plot(bin2[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2015,1, c("Empirical Over","Probabilistic Over"), pch=c(1,3), col =c("black","red"))

bin3 =summary_table_b[odd_cut_over=="(0.5,0.6]"]
plot(bin3[,list(Year,empirical_over)],pch=3, ylim = c(0,1), main="(0.5,0.6] bin", ylab='Probabilities', col="red")
par(new=TRUE)
plot(bin3[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2015,1, c("Empirical Over","Probabilistic Over"), pch=c(1,3), col =c("black","red"))

bin4 =summary_table_b[odd_cut_over=="(0.6,0.7]"]
plot(bin4[,list(Year,empirical_over)],pch=3, ylim = c(0,1), main="(0.6,0.7] bin", ylab='Probabilities', col="red")
par(new=TRUE)
plot(bin4[,list(Year, probabilistic_over)],ylab='', ylim = c(0,1))
legend(2015,1, c("Empirical Over","Probabilistic Over"), pch=c(1,3), col =c("black","red"))
