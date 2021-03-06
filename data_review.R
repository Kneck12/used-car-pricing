rm(list = ls())
library(tidyverse)

#read cleaned datasets
data_cleaned_2015=read.csv('data/data_cleaned_2015.csv')
data_cleaned_2021=read.csv('data/data_cleaned_2021.csv')

#group data by all relevant metrics, add mean, sd, n, filter low n's (grouping by input vector for later selectability in Shiny)

group_data=function(grouping){
  grouped_prices_2015=data_cleaned_2015 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
  grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt))
  
  merged_detail=inner_join(grouped_prices_2021,grouped_prices_2015,by=grouping,suffix = c(".2021", ".2015"))
  merged_detail=merged_detail %>% mutate(newcol=avg_price.2021-avg_price.2015)
  return (merged_detail)
}


#sum data by defined grouping metric
summing_data=function(merged_detail,summing){
  return(merged_detail %>% group_by_at(summing) %>%  summarize(weighted_total=weighted.mean(newcol, cnt.2021+cnt.2015), count2021=sum(cnt.2021),count2015=sum(cnt.2015),avg2021=weighted.mean(avg_price.2021,cnt.2021),avg2015=weighted.mean(avg_price.2015,cnt.2015),sd2021=sum(sd_price.2021^2,na.rm=TRUE)^0.5,sd2015=sum(sd_price.2015^2,na.rm=TRUE)^0.5) %>% arrange(desc(weighted_total)))
}

grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')

merged_detail=group_data(grouping)
x=merged_detail %>% select(cnt.2021)
sum(x[ncol(x)])

#-------------------
#test for total counts depending on level of matching detail
grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
summing=c('manufacturer')

count_table = data.frame(Detail=character(8), Count_total_matched_cars=integer(8), stringsAsFactors=FALSE) 

i=1
while(i<9){
  merged_detail=group_data(grouping[1:i])
  x=merged_detail %>% select(cnt.2021)
  y=sum(x[ncol(x)])
  count_table$Detail[i]= toString(grouping[1:i])
  count_table$Count_total_matched_cars[i]=y
  i=i+1
}
View(count_table)

#-------------------
#test for total results depending on matching and output detail

grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
summing=c('manufacturer','odometer')


merged_detail=group_data(grouping)
sum=summing_data(merged_detail,summing)
#sum[,2]=as.character(sum[,2])
sum[, 2] <- sapply(sum[, 2], as.character)
sum

#-------------------
#2021 view

grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
summing=c('manufacturer', 'age')

grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 

tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg))

manufacturers=c('porsche','mercedes','cadillac')
#view(tmp %>% select(manufacturer) %>% unique())

tmp %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=age_bucket, y=avg , color=manufacturer)) + geom_line()+geom_point()

tmp %>% select(!count) %>% select(!sd) %>% spread(age_bucket, avg) %>% mutate(percentagedrop=`6`/`1`) %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip()


#generalized based on input
grouping=c('manufacturer','model','odometer','age','condition', 'transmission','state','color')
inputsel='age'
summing=c('manufacturer', inputsel)
summing
grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 

tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg))

manufacturers=c('porsche','mercedes','cadillac')

inputsel='age_bucket'
tmp %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes_string(x=inputsel, y='avg' , color='manufacturer')) + geom_line()+geom_point()

tmp %>% select(!count) %>% select(!sd) %>% spread(inputsel, avg) %>% mutate(percentagedrop=`6`/`1`) %>% filter(manufacturer %in% manufacturers) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip()






