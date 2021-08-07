library(shinydashboard)
dashboardPage(
  dashboardHeader(title='Car price analysis'),
  dashboardSidebar(
    sidebarUserPanel("Car price analysis - Selection"),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("car")),
      menuItem("2021 View", tabName = "2021", icon = icon("car-side"),
               menuSubItem("Manufacturer view", tabName = "2021manufacturer", icon = icon("industry")),
               menuSubItem("Mixed view", tabName = "2021free", icon = icon("expand-arrows-alt"))               
      ),
      menuItem("2015 - 2021 Comparison", tabName = "comp", icon = icon("history"),
               menuSubItem("Manufacturer view", tabName = "20212015manufacturer", icon = icon("industry")),
               menuSubItem("Mixed view", tabName = "20212015mix", icon = icon("expand-arrows-alt")),
               menuSubItem("Data view", tabName = "20212015data", icon = icon("table"))
               #menuSubItem("State view", tabName = "20212015state", icon = icon("map"))
      ),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      
      #start of body tab content
      
      
      
      
      
      
      #Tab1 start
      tabItem(tabName = '2021manufacturer',
              #start
              
              fluidRow(
                HTML('<H1><b>2021 average car value depending on age per manufacturer</b></H1><br>')
              ),              
              
              fluidRow(
                column(3, 
                       prettyCheckboxGroup('checkgroup_manufacturers2021', 'Choose manufacturers to display',
                                           selected = c('audi','mercedes','bmw'),
                                           #inline = FALSE,
                                           #width = NULL,
                                           choiceNames = unique(data_cleaned_2021$manufacturer),
                                           choiceValues = unique(data_cleaned_2021$manufacturer)
                       )                         
                       ,style = "background-color:#FFFFFF;"),
                column(1,),
                column(8,
                       fluidRow(
                         plotOutput("depreciation",height = "300px")
                       ),
                       fluidRow(
                         HTML('<br><br>')
                       ),
                       fluidRow(
                         plotOutput("dep_percent",height = "240px")
                       )
                )
              ),
              
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Conclusion", background = "maroon", solidHeader = TRUE, width=12,
                  HTML('<li>The depreciation of cars over the course of up to 20 years varies significantly between manufacturers.</li>
                         <li>There is no clear trend visible for either premium vs. mass-produced cars, or by geography of the manufacturer</li>
                         <li>Manufacturers shoudl carefully track their performance vs. competitors, (e.g. see Mercedes performing worse than BMW and Audi) and take this feedback into future lineup product development.</li>
                         <br>')
                )
              )
              
              #end
      ),
      #Tab1 end
      
      #Tab2 start
      tabItem(tabName = '2021free',
              #start
              
              fluidRow(
                HTML('<H1><b>2021 average car value depending on input variable per manufacturer</b></H1><br>')
              ),              
              
              fluidRow(
                column(3,
                       prettyCheckboxGroup('checkgroup_manufacturers2021_2', 'Choose manufacturers to display',
                                           selected = c('audi','mercedes','bmw'),
                                           #inline = FALSE,
                                           #width = NULL,
                                           choiceNames = unique(data_cleaned_2021$manufacturer),
                                           choiceValues = unique(data_cleaned_2021$manufacturer)
                       )
                       
                       
                       
                       
                       ,style = "background-color:#FFFFFF;"),
                column(1,),
                column(8,
                       fluidRow(style = "background-color:#FFFFFF;",
                                selectizeInput(inputId='selector',label='Variable for comparison',
                                               choices=c('model','odometer','age','condition', 'transmission','state','color'),
                                               selected = 'age'
                                ),
                                HTML("<br>"),
                                plotOutput("free_2021",height = "490px")
                       )
                )
              ),
              
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Conclusion", background = "maroon", solidHeader = TRUE, width=12,
                  HTML('<li>Price eductions observed for all manufacturers fairly similarly over miles driven</li>
                         <li>Condition has less of a clear impact on pricing, likely due to inconsistent classification of cars by sellers</li>
                         <li>A model comparison enables more detailed comparisons, e.g. between Audi A3 and BMW 1series vehicles</li>
                         <br>')
                )
              )
              
              #end
      ),
      #Tab2 end
      
      
      #Tab3 start
      tabItem(tabName = '20212015manufacturer',
              #start
              
              fluidRow(
                HTML('<H1><b>Average car value development 2015-2021 (for same type/age/... of car)</b></H1><br>')
              ),              
              
              fluidRow(
                column(3, 
                       prettyCheckboxGroup('checkgroup_manufacturers2021_3', 'Choose manufacturers to display',
                                           selected = unique(data_cleaned_2021$manufacturer),
                                           #inline = FALSE,
                                           #width = NULL,
                                           choiceNames = unique(data_cleaned_2021$manufacturer),
                                           choiceValues = unique(data_cleaned_2021$manufacturer)
                       )                         
                       ,style = "background-color:#FFFFFF;"),
                column(1,),
                column(8,
                       fluidRow(
                         plotOutput("value_dev_1",height = "590px")
                       )
                )
              ),
              
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Conclusion", background = "maroon", solidHeader = TRUE, width=12,
                  HTML('<li>Prices across manufacturers have risen signifiantly, well above expected inflation levels.</li>
                         <li>There is no clear trend visible for either premium vs. mass-produced cars, or by geography of the manufacturer</li>
                         <li>Manufacturers of larger and Pick-up truck vehicles are mostly positioned at the upper end of the scale</li>
                         <br>')
                )
              )
              
              #end
      ),
      #Tab3 end
      
      
      
      
      #Tab4 start
      tabItem(tabName = '20212015mix',
              #start
              
              fluidRow(
                HTML('<H1><b>Average car value development 2015-2021 in detail</b></H1><br>')
              ),              
              
              fluidRow(
                column(3, 
                       
                       prettyCheckboxGroup('checkgroup_manufacturers2021_4', 'Choose manufacturers to display',
                                           selected = c('audi','mercedes','bmw'),
                                           #inline = FALSE,
                                           #width = NULL,
                                           choiceNames = unique(data_cleaned_2021$manufacturer),
                                           choiceValues = unique(data_cleaned_2021$manufacturer)
                       )                         
                       ,style = "background-color:#FFFFFF;"),
                column(1,),
                column(8,
                       fluidRow(style = "background-color:#FFFFFF;",
                                selectizeInput(inputId='selector2',label='Variable for comparison',
                                               choices=c('model','odometer','age','condition', 'transmission','state','color'),
                                               selected = 'age'
                                ),
                                HTML("<br>"),
                                plotOutput("value_dev_2",height = "490px")
                       )
                )
              ),
              
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Conclusion", background = "maroon", solidHeader = TRUE, width=12,
                  HTML('<li>No immediate trends across manufacturers visible for age, miles driven, condition, or other variables.</li>
                         <li>Data needs to be reviewed individually by manufacturers as basis for product strategy review.</li>
                         <br>')
                )
              )
              
              #end
      ),
      #Tab4 end
      
      
      #Tab5 start
      tabItem(tabName = '20212015data',
              #start
              
              fluidRow(
                HTML('<H1><b>Average car value development 2015-2021</b></H1><br>')
              ),              
              
              fluidRow(
                column(2, 
                       prettyCheckboxGroup('checkgroup_manufacturers2021_5', 'Choose manufacturers to display',
                                           selected = c('audi','mercedes','bmw'),
                                           #inline = FALSE,
                                           #width = NULL,
                                           choiceNames = unique(data_cleaned_2021$manufacturer),
                                           choiceValues = unique(data_cleaned_2021$manufacturer)
                       )                         
                       ,style = "background-color:#FFFFFF;"),
                column(10,
                       fluidRow(
                         div(dataTableOutput("value_dev_table"), style = "font-size:60%")
                       )
                )
              )
              
              #end
      ),
      #Tab5 end
      
      
      #Tab6 start
      tabItem(tabName = 'overview',
              #start
              
              fluidRow(
                HTML('<H1><b>How did used car pricing develop over the past 6 years?</b></H1><br>
                     ')
              ),              
              
              fluidRow(
                HTML('<style>
img {
  display: block;
  margin-left: auto;
  margin-right: auto;
}
</style>
                     <br><img src="carsale.jpg", alt="Carsale picture", width="1100px", align="middle"><br>'),
                box(
                  title = "Situation", background = "olive", solidHeader = TRUE, width=12,
                  HTML('<li>People view the expected resale value of their car as a key purchasing decision</li>
                         <li>Manufacturers realize this, and design vehicles and options for longevity with the resale value as a key influencing factor</li>
                         <li>In order to do so, manufacturers need to understand the effects of vehicle changes on the resale value</li>
                         <br>')
                )
              ),
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Objective", background = "green", solidHeader = TRUE, width=12,
                  HTML('Compare resale value for equal cars between 2021 and 2015
                         <br>')
                )
              ),
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Methodology", background = "light-blue", solidHeader = TRUE, width=12,
                  HTML('
                  <img src="methodology.png" alt="Methodology"><br><br>
                  <li>Two different datasets, with used car sales across the US</li>
                         <li>In order to enable unbiased comparison, we need to compare very similar cars to one another</li>
                         <li>Cars from each database are filtered, and matched based on the above listed criteria. Data is then aggregated.</li>
                         <li>Further details impacting value, such as engine size, trim, interior, and technology options are not considered due to data limitations</li>  
                       <br>')
                )
              )             
              
              #attribute: senivpetro
              
              
              #end
      ),
      #Tab6 end
      
      
      #Tab7 start
      tabItem(tabName = 'conclusion',
              #start
              
              fluidRow(
                HTML('<H1><b>Conclusion</b></H1><br>
                     ')
              ),              
              
              fluidRow(
                box(
                  title = "Conclusion", background = "olive", solidHeader = TRUE, width=12,
                  HTML('<li>Used car prices have increased a lot across all brands over the past 6 years.</li>
                         <li>Increases differ significantly by manufacturer, indicating that some manufacturers poduct strategies have been superior to others</li>
                         <li>While no clear trend is visible across detailed metrics, manufacturers can use the provided dashboard to benchmark their pricing with direct competitors as a basis for strategic product roadmap reviews.</li>
                         <br>')
                )
              ),
              fluidRow(
                HTML('<br>'),
                box(
                  title = "Next steps", background = "green", solidHeader = TRUE, width=12,
                  HTML('<li>For further optimization, data should be broken down by additional metrics such as car trim, engine size, and additional selected options.</li>
                        <li>Results hsould be qualitatively compared to new vehicle release strategies and car quality reviews to identify additional reasons for price discrepancies.</li>
                         <br>')
                )
              )
              
              
              #end
      )
      #Tab7 end      
      
    )
  )
)
