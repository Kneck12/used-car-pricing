library(shinydashboard)
dashboardPage(
  dashboardHeader(title='Car price analysis'),
  dashboardSidebar(
    sidebarUserPanel("Car price analysis - Selection"),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("car")),
      menuItem("2021 View", tabName = "2021", icon = icon("database"),
               menuSubItem("Manufacturer view", tabName = "2021manufacturer", icon = icon("map")),
               menuSubItem("General view", tabName = "2021free", icon = icon("map"))               
               ),
      menuItem("2015 - 2021 Comparison", tabName = "comp", icon = icon("database"),
               menuSubItem("Manufacturer view", tabName = "20212015manufacturer", icon = icon("map")),
               menuSubItem("Mixed view", tabName = "20212015mix", icon = icon("map")),
               menuSubItem("State view", tabName = "20212015state", icon = icon("map"))
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
                HTML('<H1>2021 average car value depending on age per manufacturer</H1><br><br>')
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
                         ),
                  column(9,
                         fluidRow(
                           plotOutput("depreciation")
                         ),
                         fluidRow(
                           HTML('<br><br>')
                         ),
                         fluidRow(
                           plotOutput("dep_percent")
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
              ),
      #Tab1 end

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
                  HTML('<li>Price eductions observed for all manufacturers fairly similarly over miles driven</li>
                         <li>Condition has less of a clear impact on pricing, likely due to inconsistent classification of cars by sellers</li>
                         <li></li>
                         <br>')
                )
              )
              
              #end
      )
      #Tab2 end
    
      
      

    )
  )
)


# selectizeInput(inputId='origin',label='Departure Airport',
#                choices=unique(flights$origin)),
#selectizeInput("dest", "Arrival Airport",
#              choices=unique(flights$dest))