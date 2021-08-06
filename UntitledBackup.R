grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
summing=c('manufacturer', 'age_bucket')

manufacturers=c('porsche','mercedes','cadillac')

grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 

tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg)) %>% filter(manufacturer %in% manufacturers)
tmp

function(input,output){
  #beginning of content
  
  
  
  manufacturer_2021_dep <- reactive({
    
    grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
    summing=c('manufacturer', 'age_bucket')
    
    manufacturers=input$checkgroup_manufacturers2021
    
    grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 
    
    tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg)) %>% filter(manufacturer %in% manufacturers)
    return (tmp)
  })
  
  manufacturer_2021_analysis <- reactive({
    
    grouping=c('manufacturer','modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')
    inputsel=input$selector   
    summing=c('manufacturer', inputsel)
    
    manufacturers=input$checkgroup_manufacturers2021_2
    
    grouped_prices_2021=data_cleaned_2021 %>% group_by_at(grouping) %>% summarize(avg_price=mean(price), sd_price=sd(price), cnt=n()) %>% arrange(desc(cnt)) 
    
    tmp = grouped_prices_2021 %>% group_by_at(summing) %>%  summarize(count=sum(cnt),avg=weighted.mean(avg_price,cnt),sd=sum(sd_price^2,na.rm=TRUE)^0.5) %>% arrange(desc(avg)) %>% filter(manufacturer %in% manufacturers)
    return (tmp2)
  })
  
  
  #beginning of outputs
  
  output$depreciation <- renderPlot(
    manufacturer_2021_dep() %>% ggplot(aes(x=age_bucket, y=avg , color=manufacturer)) + geom_line()+geom_point() 
    + ggtitle('Average vehicle price depending on vehicle age per manufacturer')
    + ylab('Vehicle price in USD')
    + xlab('Vehicle Age (1: 1-2 years, 2: 3-5 years, 3: 6-8 years, 4:9-12 years, 5: 13-20 years, 6: 21 years+')
    + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12, face="bold")
    )
  )
  
  
  output$dep_percent <- renderPlot(
    manufacturer_2021_dep() %>% select(!count) %>% select(!sd) %>% spread(age_bucket, avg) %>% mutate(percentagedrop=(1-`5`/`1`)*100) %>% ggplot(aes(x=reorder(manufacturer, percentagedrop), y=percentagedrop)) + geom_bar(stat='identity') + coord_flip() 
    + ggtitle('Average depreciation 12-20 year old cars vs. 1-2 year old cars per manufacturer')
    + ylab('Manufacturer')
    + xlab('Vehicle price change in percent 1-2 yrs. vs 12-20 yrs age')
    + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12, face="bold")
    )
  )
  
  
  output$free_2021 <- renderPlot(
    manufacturer_2021_analysis() %>% ggplot(aes_string(x=inputsel, y='avg' , color='manufacturer')) + geom_line()+geom_point() 
    + ggtitle('Price discrepancy by select x axis variable per manufacturer')
    + ylab('Vehicle price in USD')
    + xlab('See selector')
    + theme(
      plot.title = element_text(color="black", size=14, face="bold.italic"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12, face="bold")
    )
  ) 
  
  #end of content
}













#Tab2 start
tabItem(tabName = '2021free',
        #start
        
        fluidRow(
          HTML('<H1>2021 average car value depending on input variable per manufacturer</H1><br><br>')
        ),              
        
        fluidRow(
          column(3,
                 
                 selectizeInput(inputId='selector',label='Variable for comparison',
                                choices=c('modelnew','odo_bucket','age_bucket','condition_bucket', 'transmission','state','color')),
                 
                 
                 prettyCheckboxGroup('checkgroup_manufacturers2021_2', 'Choose manufacturers to display',
                                     selected = c('audi','mercedes','bmw'),
                                     #inline = FALSE,
                                     #width = NULL,
                                     choiceNames = unique(data_cleaned_2021$manufacturer),
                                     choiceValues = unique(data_cleaned_2021$manufacturer)
                 )                         
          ),
          column(9,
                 fluidRow(
                   plotOutput("free_2021")
                 )
          )
        ),
        
        fluidRow(
          HTML('<br><br>'),
          box(
            title = "Conclusion", background = "maroon", solidHeader = TRUE, width=12,
            HTML('<li>The depreciation of cars over the course of up to 20 years varies significantly between manufacturers.</li>
                         <li>There is no clear trend visible for either premium vs. mass-produced cars, or by geography of the manufacturer</li>
                         <li>Manufacturers shoudl carefully track their performance vs. competitors, (e.g. see Mercedes performing worse than BMW and Audi) and take this feedback into future lineup product development.</li>
                         <br>')
          )
        )
        
        #end
)
#Tab2 end  