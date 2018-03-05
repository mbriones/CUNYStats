library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(maps)
library(scales)
library(sp)
library(rgdal)
library(maptools)
library(httr)
library(broom)
library(ggmap)


cuny <- read.csv("Data/cunylinkedin2.csv")
cuny2 <- read.csv("Data/cunylinkedin2.csv")
cunyscraped <- read.csv("Data/cunyscraped.csv")
plot_data <- read.csv("Data/mapplotdata.csv")

ui <- navbarPage("CUNY School and Job Placement",
        tabsetPanel(id = "tabset",
                 tabPanel("Job",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('School', 'CUNY School', choices = c("All", as.character(unique(cuny$School))), selected = "Queens College"),
                              selectInput('Degree', 'Degree Received', ''),
                              div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year1', 'From:', choices = unique(cuny$YearGrad), selected = "2012")),
                              div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year2', 'To:', choices = unique(cuny$YearGrad), selected = "2015")),
                              sliderInput('range', 'Salary Range:', min = 0, max = 200000, value = c(0,200000)),
                              selectInput('Question', 'What would you like to know?', choices = c("Select",
                                                                                                   "What job sector are graduates currently in?",
                                                                                                   "What job titles do graduates currently hold?"), selected = "Select")),
                     
                     mainPanel(
                       plotOutput("jobplots")
                      )
                    )
                  ),
                 
                 tabPanel("Salary",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('Schooldos', 'CUNY School', choices = c("All", as.character(unique(cuny$School))), selected = "Queens College"),
                              selectInput('Degreedos', 'Degree Received', ''),
                              div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year1dos', 'From:', choices = unique(cuny$YearGrad), selected = "2012")),
                              div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year2dos', 'To:', choices = unique(cuny$YearGrad), selected = "2015")),
                              selectInput('Questiondos', 'What would you like to know?', choices = c("Select",
                                                                                                   "What salary are graduates making based on job sector?",
                                                                                                   "What salary are graduates making based on job title?"), selected = "Select")),
                            
                            
                            mainPanel(
                              plotOutput("salaryplots")
                      )
                    )
                  ),
   
                        
                 tabPanel("Location",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput('School', 'CUNY School', choices = unique(cuny$School), selected = "Queens College"),
                             selectInput('Degree', 'Degree Received', choices = unique(cuny$Degree), selected = "Computer Sci"),
                             div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year1', 'From:', choices = unique(cuny$YearGrad), selected = "2012")),
                             div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year2', 'To:', choices = unique(cuny$YearGrad), selected = "2015")),
                             sliderInput('range', 'Salary Range:', min = 10000, max = 200000, value = c(10000,200000)),
                             selectInput('Question', 'What would you like to know?', choices = c("Select",
                                                                                                  "Where are graduates working?"
                                                                                                  ), selected = "Select")),
           
           
                            mainPanel(
                              plotOutput("locationmap")
                    )
                  )
                ),

                 tabPanel("Rankings",
                         sidebarLayout(
                          sidebarPanel(
                            selectInput('Schooltres', 'CUNY School', choices = unique(cuny$School), selected = "Queens College"),
                            selectInput('Degreetres', 'Degree Received', choices = unique(cuny$Degree), selected = "Computer Sci"),
                            div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year1tres', 'From:', choices = unique(cuny$YearGrad), selected = "2012")),
                            div(style="display: inline-block;vertical-align:center; width: 110px;",selectInput('Year2tres', 'To:', choices = unique(cuny$YearGrad), selected = "2015"))),
           
                          mainPanel(
                            fluidRow(align = "center", 
                                     column(5, strong("Which School Leads to the Highest Average Salary?"), textOutput("SchoolSalaryHigh")
                                     ),
                                     column(5, strong("Which School Leads to the Lowest Average Salary?"), textOutput("SchoolSalaryLow")),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     
                            fluidRow(align = "center", 
                                     column(5, strong("Which Degree Leads to the Highest Average Salary?"), textOutput("DegreeSalaryHigh")
                                     ),
                                     column(5, strong("Which Degree Leads to the Lowest Average Salary?"), textOutput("DegreeSalaryLow")),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     
                            fluidRow(align = "center", 
                                     column(5, strong("Most Popular Degree?"), textOutput("DegreePopularMost")
                                     ),
                                     column(5, strong("Least Popular Degree?"), textOutput("DegreePopularLeast")),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     
                            fluidRow(align = "center", 
                                     column(10, strong("Average Salary for Degree Chosen"), textOutput("DegreeSalaryAverage")
                                     )
                                     
                    )
                  )
                )
              )
            )
          )
        )
      )
    )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   outVarDegree <- reactive({
    newdegreechoice <- cuny %>% filter(School == input$School)
    unique(newdegreechoice$Degree)
   })
   
   outVarDegreedos <- reactive({
     newdegreechoice <- cuny %>% filter(School == input$Schooldos)
     unique(newdegreechoice$Degree)
   }) 
   
   observe({
     updateSelectInput(session, "Degree", choices = c("All", as.character(outVarDegree())))
     updateSelectInput(session, "Degreedos", choices = c("All", as.character(outVarDegreedos())))
   })
   
#-------------------
   #JOB
#-------------------
   output$jobplots <- renderPlot({
     
          salarymax <- as.numeric(max(input$range))
          salarymin <- as.numeric(min(input$range))
     
          cunyplot <- subset(cuny, (CurrentSalary < salarymax & CurrentSalary > salarymin) & 
                                   (YearGrad >= input$Year1 & YearGrad <= input$Year2), 
                                   select = c(School, Degree, Job, CurrentSalary, DegreeSector, JobSector))
     
          #-------------
          companyp <- subset(cunyplot, School == input$School & Degree == input$Degree, 
                            select = c(JobSector))
          
          companyplot <-ggplot(companyp, aes(JobSector)) + geom_bar(color="black") + xlab("Job Sector") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
                 
          #-------------
          degreecompanyp <- subset(cunyplot, School == input$School, 
                              select = c(JobSector, DegreeSector))
          
          degreecompanyplot <- ggplot(degreecompanyp, aes(JobSector)) + geom_bar(aes(fill=DegreeSector), position = "dodge", color = "black") + xlab("Job Sector") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
          
          #-------------
          allcompanyp <- subset(cunyplot, select = c(JobSector, DegreeSector))
          
          allcompanyplot <-ggplot(allcompanyp, aes(JobSector)) + geom_bar(aes(fill=DegreeSector), position = "dodge", color = "black") + xlab("Job Sector") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
          
          #-------------
          jobp <- subset(cunyplot, School == input$School & Degree == input$Degree, 
                         select = c(Job))
          
          jobplot <-ggplot(jobp, aes(Job)) + geom_bar(color="black") + coord_flip() + xlab("Occupation") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
          
          #-------------
          degreejobp <- subset(cunyplot, School == input$School, 
                               select = c(Job, DegreeSector))
          
          degreejobplot <- ggplot(degreejobp, aes(Job)) + geom_bar(aes(fill=DegreeSector), position = "dodge", color = "black") + coord_flip() + xlab("Occupation") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
          
          #-------------
          alljobp <- subset(cunyplot, select = c(Job, DegreeSector))
          
          alljobplot <-ggplot(alljobp, aes(Job)) + geom_bar(aes(fill=DegreeSector), position = "dodge", color = "black") + coord_flip() + xlab("Occupation") +
            ylab("Number of Students") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(color = "black"))
          
          #--------------
          if (input$School == "All" & input$Degree == "All" & input$Question == "What job sector are graduates currently in?") {
              print(allcompanyplot)
          } else if (input$School != "All" & input$Degree != "All" & input$Question == "What job sector are graduates currently in?") {
              print(companyplot)
          } else if (input$School != "All" & input$Degree == "All" & input$Question == "What job sector are graduates currently in?") {
              print(degreecompanyplot)
          } else if (input$School == "All" & input$Degree == "All" & input$Question == "What job titles do graduates currently hold?") {
              print(alljobplot)
          } else if (input$School != "All" & input$Degree != "All" & input$Question == "What job titles do graduates currently hold?") {
              print(jobplot)
          } else if (input$School != "All" & input$Degree == "All" & input$Question == "What job titles do graduates currently hold?") {
              print(degreejobplot)
          } else 
              print("Try Again")
    })
   
#----------------------------
   #SALARY
#----------------------------   
   output$salaryplots <- renderPlot({
     
     cunysplot <- subset(cuny2, YearGrad >= input$Year1dos & YearGrad <= input$Year2dos, 
                        select = c(School, Degree, Job, CurrentSalary, DegreeSector, JobSector))
     
     scrapedsplot <- subset(cunyscraped, YearGrad >= input$Year1dos & YearGrad <= input$Year2dos, 
                            select = c(School, Degree, Job, averageSalary, DegreeSector, JobSector))
    
     #-------------------------    
     
     degreesalarycompanyp <- subset(cunysplot, School == input$Schooldos, 
                              select = c(JobSector, Degree, CurrentSalary))
     
     degreescrapedcompanyp <- subset(scrapedsplot, School == input$Schooldos, 
                                    select = c(JobSector, Degree, averageSalary))
     
     degreesalarycompanyp2 <- degreesalarycompanyp %>% group_by(JobSector, Degree) %>% summarize(Average = mean(CurrentSalary))
     
     degreescrapedcompanyp2 <- degreescrapedcompanyp %>% group_by(JobSector, Degree) %>% summarize(Average = mean(averageSalary))
     
     degreesalarycompanyplot <- ggplot(degreesalarycompanyp2, aes(x = JobSector,  y= Average)) + geom_bar(stat="identity", aes(fill=Degree), position = "dodge", color = "black") + scale_y_continuous(labels = comma) + xlab("Job Sector") +
       ylab("Average Salary") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=degreescrapedcompanyp2, aes(fill = Degree), position = "dodge", stat="identity", alpha = 0.4)
     
     #-------------------------
     allsalarycompanyp <- subset(cunysplot, select = c(JobSector, DegreeSector, CurrentSalary))
     
     allscrapedcompanyp <- subset(scrapedsplot, select = c(JobSector, DegreeSector, averageSalary))
     
     allsalarycompanyp2 <- allsalarycompanyp %>% group_by(JobSector, DegreeSector) %>% summarize(Average = mean(CurrentSalary))
    
     allscrapedcompanyp2 <- allscrapedcompanyp %>% group_by(JobSector, DegreeSector) %>% summarize(Average = mean(averageSalary))
     
     allsalarycompanyplot <-ggplot(allsalarycompanyp2, aes(x = JobSector,  y= Average)) + geom_bar(stat="identity", aes(fill=DegreeSector), position = "dodge", color = "black") + scale_y_continuous(labels = comma) + xlab("Job Sector") +
       ylab("Average Salary") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=allscrapedcompanyp2, aes(fill = DegreeSector), position = "dodge", stat="identity", alpha = 0.4)
     
     #-------------------------
     companysalaryp <- subset(cunysplot, School == input$Schooldos & Degree == input$Degreedos, 
                              select = c(JobSector, CurrentSalary))
     
     companyscrapedp <- subset(scrapedsplot, School == input$Schooldos & Degree == input$Degreedos, 
                              select = c(JobSector, averageSalary))
     
     companysalaryp2 <- companysalaryp %>% group_by(JobSector) %>% summarize(Average = mean(CurrentSalary))
     
     companyscrapedp2 <- companyscrapedp %>% group_by(JobSector) %>% summarize(Average = mean(averageSalary))
     
     companysalaryplot <-ggplot(companysalaryp2, aes(x=JobSector, y= Average)) + geom_bar(stat="identity", color="black") + scale_y_continuous(labels = comma) + xlab("Job Sector") + ylab("Average Salary") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=companyscrapedp2, fill = "red", stat="identity", alpha = 0.4)
     
     #------------------------
     degreesalaryjobp <- subset(cunysplot, School == input$Schooldos, 
                          select = c(Job, Degree, CurrentSalary))
     
     scrapedsalaryjobp <- subset(scrapedsplot, School == input$Schooldos, 
                                select = c(Job, Degree, averageSalary))
     
     degreesalaryjobp2 <- degreesalaryjobp %>% group_by(Job, Degree) %>% summarize(Average = mean(CurrentSalary))
     
     scrapedsalaryjobp2 <- scrapedsalaryjobp %>% group_by(Job, Degree) %>% summarize(Average = mean(averageSalary))
     
     degreesalaryjobplot <- ggplot(degreesalaryjobp2, aes(x = Job,  y= Average)) + geom_bar(stat="identity", aes(fill=Degree), position = "dodge", color = "black") + coord_flip() + scale_y_continuous(labels = comma) + xlab("Occupation") +
       ylab("Average Salary") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=scrapedsalaryjobp2, aes(fill = Degree), position = "dodge", stat="identity", alpha = 0.4)
     
     #-------------------------
     allsalaryjobp <- subset(cunysplot, select = c(Job, DegreeSector, CurrentSalary))
     
     allscrapedjobp <- subset(scrapedsplot, select = c(Job, DegreeSector, averageSalary))
     
     allsalaryjobp2 <- allsalaryjobp %>% group_by(Job, DegreeSector) %>% summarize(Average = mean(CurrentSalary))
     
     allscrapedjobp2 <- allscrapedjobp %>% group_by(Job, DegreeSector) %>% summarize(Average = mean(averageSalary))
     
     allsalaryjobplot <-ggplot(allsalaryjobp2, aes(x = Job,  y= Average)) + geom_bar(stat="identity", aes(fill=DegreeSector), position = "dodge", color = "black") + coord_flip() + scale_y_continuous(labels = comma) + xlab("Occupation") + ylab("Average Salary") +
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=allscrapedjobp2, aes(fill = DegreeSector), position = "dodge", stat="identity", alpha = 0.4)
     
     #-------------------------
     jobsalaryp <- subset(cunysplot, School == input$Schooldos & Degree == input$Degreedos, 
                          select = c(Job, CurrentSalary))
     
     jobscrapedp <- subset(scrapedsplot, School == input$Schooldos & Degree == input$Degreedos, 
                          select = c(Job, averageSalary))
     
     jobsalaryp2 <- jobsalaryp %>% group_by(Job) %>% summarize(Average = mean(CurrentSalary))
     
     jobscrapedp2 <- jobscrapedp %>% group_by(Job) %>% summarize(Average = mean(averageSalary))
     
     jobsalaryplot <-ggplot(jobsalaryp2, aes(x = Job,  y= Average)) + geom_bar(stat="identity", color="black") + coord_flip() + scale_y_continuous(labels = comma) + xlab("Occupation") + ylab("Average Salary") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_blank(), axis.line = element_line(color = "black")) + geom_bar(data=jobscrapedp2, fill = "red", stat="identity", alpha = 0.4)
     
     #-------------------------
     if (input$Schooldos == "All" & input$Degreedos == "All" & input$Questiondos == "What salary are graduates making based on job sector?") {
       print(allsalarycompanyplot)
     } else if (input$Schooldos != "All" & input$Degreedos != "All" & input$Questiondos == "What salary are graduates making based on job sector?") {
       print(companysalaryplot)
     } else if (input$Schooldos != "All" & input$Degreedos == "All" & input$Questiondos == "What salary are graduates making based on job sector?") {
       print(degreesalarycompanyplot)
     } else if (input$Schooldos == "All" & input$Degreedos == "All" & input$Questiondos == "What salary are graduates making based on job title?") {
       print(allsalaryjobplot)
     } else if (input$Schooldos != "All" & input$Degreedos != "All" & input$Questiondos == "What salary are graduates making based on job title?") {
       print(jobsalaryplot)
     } else if (input$Schooldos != "All" & input$Degreedos == "All" & input$Questiondos == "What salary are graduates making based on job title?") {
       print(degreesalaryjobplot)
     } else 
       print("Try Again")
   })
   
#-------------------------------
   #LOCATION
#-------------------------------   
   
   output$locationmap <- renderPlot({
     r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
     nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
     nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
     nyc_map <- get_map(location = c(lon = -73.90, lat = 40.71), maptype = "terrain", zoom = 11)
     ggmap(nyc_map) + 
       geom_polygon(data=plot_data, aes(x=long, y=lat, group=group, fill=num_points), alpha=0.75)
   })

#-------------------------------
   #RANKINGS
#-------------------------------
   
   SchoolSalaryHigh <- reactive({
     schoolsal <- subset(cuny, YearGrad >= input$Year1tres & YearGrad <= input$Year2tres, 
            select = c(School, CurrentSalary))
     schoolsaldos <- aggregate(.~School, data=schoolsal, mean)
     schoolsaldos$School[schoolsaldos$CurrentSalary == max(schoolsaldos$CurrentSalary)]
   })
   
   SchoolSalaryLow <- reactive({
     schoolsal <- subset(cuny, YearGrad >= input$Year1tres & YearGrad <= input$Year2tres, 
                         select = c(School, CurrentSalary))
     schoolsaldos <- aggregate(.~School, data=schoolsal, mean)
     schoolsaldos$School[schoolsaldos$CurrentSalary == min(schoolsaldos$CurrentSalary)]
   })
   
   DegreeSalaryHigh <- reactive({
     degreesal <- subset(cuny, (YearGrad >= input$Year1tres & YearGrad <= input$Year2tres) & School == input$Schooltres, 
                         select = c(Degree, CurrentSalary))
     degreesaldos <- aggregate(.~Degree, data=degreesal, mean)
     degreesaldos$Degree[degreesaldos$CurrentSalary == max(degreesaldos$CurrentSalary)]
   })
   
   DegreeSalaryLow <- reactive({
     degreesal <- subset(cuny, (YearGrad >= input$Year1tres & YearGrad <= input$Year2tres) & School == input$Schooltres, 
                         select = c(Degree, CurrentSalary))
     degreesaldos <- aggregate(.~Degree, data=degreesal, mean)
     degreesaldos$Degree[degreesaldos$CurrentSalary == min(degreesaldos$CurrentSalary)]
   })
   
   DegreePopularMost <- reactive({
     degreepopmost <- subset(cuny, (YearGrad >= input$Year1tres & YearGrad <= input$Year2tres) & School == input$Schooltres, 
                         select = c(Degree))
     degreepopmost$count <- rep(1,nrow(degreepopmost))
     degreepopmostdos <- aggregate(.~Degree, data=degreepopmost, sum)
     degreepopmostdos$Degree[degreepopmostdos$count == max(degreepopmostdos$count)]
   })
   
   DegreePopularLeast <- reactive({
     degreepopleast <- subset(cuny, (YearGrad >= input$Year1tres & YearGrad <= input$Year2tres) & School == input$Schooltres, 
                             select = c(Degree))
     degreepopleast$count <- rep(1,nrow(degreepopleast))
     degreepopleastdos <- aggregate(.~Degree, data=degreepopleast, sum)
     degreepopleastdos$Degree[degreepopleastdos$count == min(degreepopleastdos$count)]
   })
   
   AvgSalaryDegree <- reactive({
     avgsalarydeg <- subset(cuny, (YearGrad >= input$Year1tres & YearGrad <= input$Year2tres) & School == input$Schooltres
                              & Degree == input$Degreetres, 
                              select = c(CurrentSalary))
     mean(avgsalarydeg$CurrentSalary)
   })
   
   output$SchoolSalaryHigh <- renderText({as.character(SchoolSalaryHigh())
   })
   
   output$SchoolSalaryLow <- renderText({as.character(SchoolSalaryLow())
   })
   
   output$DegreeSalaryHigh <- renderText({as.character(DegreeSalaryHigh()) 
   })
   
   output$DegreeSalaryLow <- renderText({as.character(DegreeSalaryLow()) 
   })
   
   output$DegreePopularMost <- renderText({as.character(DegreePopularMost())
   })
   
   output$DegreePopularLeast <- renderText({as.character(DegreePopularLeast()) 
   })
   
   output$DegreeSalaryAverage <- renderText({as.numeric(AvgSalaryDegree())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

