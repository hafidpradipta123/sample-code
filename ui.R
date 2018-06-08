library(shiny)
library(tidyverse)
library(highcharter)
library(ggrepel)
library(scales)
library(plotly)
library(cowplot)
library(knitr)
kun = "#000066"
library(gridExtra)
schema <- read_csv('survey_results_schema.csv')
survey <- read_csv("survey_results_public.csv")
survey %>%
  mutate(DevType= str_split(DevType, pattern = ';')) %>% 
  unnest(DevType) %>%
  select(DevType) %>% 
  filter(DevType != "") %>% 
  group_by(DevType) %>% 
  summarise(n = n())->dtype
jobtitle <- dtype[1]
survey2 <- survey[c(1:17,51:62,66:82,94:99,115:128)]

shinyUI(fluidPage(
  

  titlePanel("Stack Overflow 2018 Developer Software"),
fluidRow(column(10,
  tabsetPanel(
    tabPanel("Description of the App", 
             h3("Exploratory Data Analysis of Stack Overflow 2018 Developer software "),
             h4("It may took a while to load the plot. The average time to load the plot is 70 seconds(in each tabs)."),
              h5("Therefore, take your time and prepare several minutes to load it. I am working on how to make it load faster but in the meanwhile, I want to show my work. For best view, please use computer and use full screen because the plot is dynamic. "),
             h5("The code is taken from  https://www.kaggle.com/jackcook/how-to-become-a-data-scientist . 
                I tried to play with it because the code is very efficient and tidy. I am curious to figure out each layer of the code. "),
             h5("The dataset is taken from :https://www.kaggle.com/stackoverflow/stack-overflow-2018-developer-survey"),
             h5("This is my personal project and my aim to make this app is to hone my skill in R and Shiny."),
             h5(" The app consist of 6 other tabs. The next 5 tabs, you can choose which occupation do you want and it will show the information based on the occupation "),
             h5("The second tab is about general description such as : code as hoby, open source contribution, education, employment status, major in undergrad, student status and country origin.  "),
             h5("The third tab is about non technical work information such as years of coding, job satisfaction, size of the company,  and self taught type"),
             h5("The fourth tab is about the information of technical work such as language used, databaswe worked, platform worked "),
             h5("The fifth tab is about lifestyle such as time to get up, hours in front of computers, frequency of exercise "),
             h5("The sixth tab is about polling on several statements of job assessment, how to contact via email, assessment benefit"),
             h4("The Occupation dropdown does not work in 'Polling' tab!! However the results are quite similar. I will try to figure it out"),
             h5("The last tab is about all that is mentioned above but I summarize it with all of the occupation.")
             ),
      tabPanel("General Description",
               selectInput(inputId= "input1", label="Select Your Occupation",jobtitle),
             mainPanel( plotOutput("plot2",width = "150%")),
             mainPanel( plotOutput("plot3",width = "150%")),
             mainPanel( plotOutput("plot4",width = "150%")),
             mainPanel( plotOutput("plot5",width = "150%")),
             mainPanel( plotOutput("plot8",width = "150%")),
             mainPanel( plotOutput("plot9",width = "150%"))
    ),
    tabPanel("Work Non Technical",
               selectInput(inputId= "input1", label="Select Your Occupation",jobtitle),
             mainPanel( plotOutput("plot6",width = "150%")),
             mainPanel( plotOutput("plot7",width = "150%")),
             mainPanel( plotOutput("plot10",width = "150%")),
             mainPanel( plotOutput("plot11",width = "150%")),
             mainPanel( plotOutput("plot12",width = "150%")),
             mainPanel( plotOutput("plot14",width = "150%")),
             mainPanel( plotOutput("plot17",width = "150%"))
    ),#tabpanel
    
    
    tabPanel("Work Technical",
               selectInput(inputId= "input1", label="Select Your Occupation",jobtitle),
             mainPanel( plotOutput("plot15",width = "150%")),
             mainPanel( plotOutput("plot28",width = "150%")),
             mainPanel( plotOutput("plot23",width = "150%")),
             mainPanel( plotOutput("plot24",width = "150%")),
             mainPanel( plotOutput("plot25",width = "150%")),
             mainPanel( plotOutput("plot26",width = "150%")),
             mainPanel( plotOutput("plot27",width = "150%"))
    ),
    #tabpanel
    tabPanel("Lifestyle",
             selectInput(inputId= "input1", label="Select Your Occupation",jobtitle),
             mainPanel( plotOutput("plot18",width = "150%")),
             mainPanel( plotOutput("plot19",width = "150%")),
             mainPanel( plotOutput("plot20",width = "150%")),
             mainPanel( plotOutput("plot21",width = "150%")),
             mainPanel( plotOutput("plot22",width = "150%"))
    ),
    #tabpanel
    
    tabPanel("Polling",
             selectInput(inputId= "input1", label="Select Your Occupation",jobtitle),
             mainPanel(tableOutput("table1")),
             mainPanel(tableOutput("table2")),
             mainPanel(tableOutput("table3")),
             mainPanel(tableOutput("table4")),
             mainPanel(tableOutput("table5")),
             mainPanel( plotOutput("plot29",width = "150%"))
             
    ),#tabpanel
      tabPanel("Visualization across Title",
                               sidebarPanel(
                                 radioButtons(inputId="rbutt","Type of the plot:",
                                              choices = c("Title vs Hobby"="tho",
                                                          "Title vs Formal Education"="tfe",
                                                          "Title vs Open Source"="toso",
                                                          "Title vs Employment"="tem",
                                                          "Title vs Undergrad Major"="tum",
                                                          "Title vs Coding Time"="tct",
                                                          "Title vs Coding Profesionnaly"="tctp",
                                                          "Title vs Salary"="tsal",
                                                          "Title vs Job Satisfaction"="tjs",
                                                          "Title vs Student Status"="tsts",
                                                          "Title vs Country"="tc",
                                                          "Title vs Career Satisfaction"="tcst",
                                                          "Title vs Company Size"="tcsz",
                                                          "Title vs Hope Five Years"="thf",
                                                          "Title vs Search Status"="tscss",
                                                          "Title vs Last or New Job"="tls",
                                                          "Title vs UpdateCV"="tucv",
                                                          "Title vs Wake up Time"="twt",
                                                          "Title vs Skip Meals"="tsm",
                                                          "Title vs Hours Computer"="thc",
                                                          "Title vs Hours Outside"="thou",
                                                          "Title vs Exercise"="tex",
                                                          "Title vs Parents Education"="tpe",
                                                          "Title vs Age"="tage",
                                                          "Title vs Dependents"="tdep",
                                                          "Title vs Language Working With"="tlw",
                                                          "Title vs Language Desire "="tld",
                                                          "Title vs Databse Working With"="tdw",
                                                          "Title vs Database Desire "="tdd",
                                                          "Title vs Platform Working With"="tpw",
                                                          "Title vs Platform Desire "="tpd",
                                                          "Title vs Framework Working With"="tfw",
                                                          "Title vs Framework Desire "="tfd",
                                                          "Title vs IDE used"="tide",
                                                          "Title vs Operating System used"="tosy"
                                                          
                                              ))
                                 ),mainPanel( plotOutput("plot30", width = "120%", height = '600px')
                                              )
               )
    )
  ))
))

