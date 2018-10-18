require(data.table)
require(anytime)
require(ggplot2)

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

#filter na scores
matches=na.omit(matches,cols="score")

#filter over 1x2
odds_1x2=odds[betType=='1x2' & bookmaker=='bet365' & oddtype == 'odd1']

#transform unix time to date
odds_1x2[,match_date:=anydate(date)]
#transform unix time to date time
odds_1x2[,match_time:=anytime(date)]
odds_1x2[,Year:=year(match_time)]

#filter over 1x2
odds_1x2=odds_1x2[Year==2018]

#remove total handicap
odds_1x2[,totalhandicap:=NULL]

#order data in ascending date
odds_1x2=odds_1x2[order(matchId, oddtype,bookmaker,date)]

odds_1x2_initial=odds_1x2[,list(start_odd=odd[1]),
                              by=list(matchId,oddtype,bookmaker)]

odds_1x2_final=odds_1x2[,list(final_odd=odd[.N]),
                            by=list(matchId,oddtype,bookmaker)]

summary_table=odds_1x2[,list(date, odd,.N), by=list(matchId)]
ggplot(summary_table,aes(x=N, y=odd, color=date))+geom_line(aes(group=matchId))+scale_color_gradient(low="black", high="light blue") + ggtitle("Change in odds for bet365")
