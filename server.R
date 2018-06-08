options(shiny.maxRequestSize = 200*1024^2)
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

survey$EducationParents[survey$EducationParents=="Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"] <- "Secondary School"
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

 
  
  #Hobby open source
  output$plot2 <- renderPlot({
    pOpenSource <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(OpenSource)) %>% 
      group_by(DevType,OpenSource) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = OpenSource, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Do you code as a hobby?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pHobby <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Hobby)) %>% 
      group_by(DevType,Hobby) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Hobby, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Do you contribute to open source projects?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pOpenSource,pHobby, ncol = 2)
    print(result)
  })
  
  #Formal Education
  output$plot3 <- renderPlot({
    survey$FormalEducation[survey$FormalEducation=="Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"] <- "Secondary School"
    pFormalEducation <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(FormalEducation)) %>% 
      group_by(DevType,FormalEducation) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = FormalEducation, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your highest level of education? ") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    
    print(pFormalEducation)
  }) 
  
  #employment
  output$plot4 <- renderPlot({
    pEmployment <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Employment)) %>% 
      group_by(DevType,Employment) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Employment, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your employment status?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pEmployment)
  })
  
  #undergrad major
  output$plot5 <- renderPlot({
    
    pUndergradMajor <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(UndergradMajor)) %>% 
      group_by(DevType,UndergradMajor) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = UndergradMajor, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your Undergrad Major?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pUndergradMajor)
  })
  
  
  #coding and coding prof
  output$plot6 <- renderPlot({
    pYearsCoding <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(YearsCoding = case_when(str_detect(YearsCoding, "0-2 years") ~ "a. 0-2 years",
                                     str_detect(YearsCoding, "3-5 years") ~ "b. 3-5 years",
                                     str_detect(YearsCoding, "6-8 years") ~ "c. 6-8 years",
                                     str_detect(YearsCoding, "9-11 years") ~ "d. 9-11 years",
                                     str_detect(YearsCoding, "12-14 years") ~ "e. 12-14 years",
                                     str_detect(YearsCoding, "15-17 years") ~ "f. 15-17 years",
                                     str_detect(YearsCoding, "18-20 years") ~ "g. 18-20 years",
                                     str_detect(YearsCoding, "21-23 years") ~ "h.21-23 years",
                                     str_detect(YearsCoding, "After 12:01 PM") ~ "i. After 12:01 PM",
                                     str_detect(YearsCoding, "24-26 years") ~ "j. 24-26 years",
                                     str_detect(YearsCoding, "27-29 years") ~ "k. 27-29 years",
                                     str_detect(YearsCoding, "30 or more years") ~ "l. 30 or more years",
                                     TRUE ~ YearsCoding)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(YearsCoding)) %>% 
      group_by(DevType,YearsCoding) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = YearsCoding, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How long have you been coding? ") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    
    pYearsCodingProf <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(YearsCodingProf = case_when(str_detect(YearsCodingProf, "0-2 years") ~ "a. 0-2 years",
                                         str_detect(YearsCodingProf, "3-5 years") ~ "b. 3-5 years",
                                         str_detect(YearsCodingProf, "6-8 years") ~ "c. 6-8 years",
                                         str_detect(YearsCodingProf, "9-11 years") ~ "d. 9-11 years",
                                         str_detect(YearsCodingProf, "12-14 years") ~ "e. 12-14 years",
                                         str_detect(YearsCodingProf, "15-17 years") ~ "f. 15-17 years",
                                         str_detect(YearsCodingProf, "18-20 years") ~ "g. 18-20 years",
                                         str_detect(YearsCodingProf, "21-23 years") ~ "h.21-23 years",
                                         str_detect(YearsCodingProf, "After 12:01 PM") ~ "i. After 12:01 PM",
                                         str_detect(YearsCodingProf, "24-26 years") ~ "j. 24-26 years",
                                         str_detect(YearsCodingProf, "27-29 years") ~ "k. 27-29 years",
                                         str_detect(YearsCodingProf, "30 or more years") ~ "l. 30 or more years",
                                         TRUE ~ YearsCodingProf)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(YearsCodingProf)) %>% 
      group_by(DevType,YearsCodingProf) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = YearsCodingProf, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How long have you coded professionally?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pYearsCoding,pYearsCodingProf, ncol = 2)
    print(result)
  })
  
  
  #6Jobsatistfaction 
  output$plot7 <- renderPlot({
    pJobSatisfaction <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(JobSatisfaction = case_when(str_detect(JobSatisfaction, "Extremely dissatisfied") ~ "a. Extremely dissatisfied",
                                         str_detect(JobSatisfaction, "Moderately dissatisfied") ~ "b. Moderately dissatisfied",
                                         str_detect(JobSatisfaction, "Slightly dissatisfied") ~ "c. Slightly dissatisfied",
                                         str_detect(JobSatisfaction, "Neither satisfied nor dissatisfied") ~ "d. Neither satisfied nor dissatisfied",
                                         str_detect(JobSatisfaction, "Slightly satisfied") ~ "e. Slightly satisfied",
                                         str_detect(JobSatisfaction, "Moderately satisfied") ~ "f. Moderately satisfied",
                                         str_detect(JobSatisfaction, "Extremely satisfied") ~ "g. Extremely satisfied",
                                         TRUE ~ JobSatisfaction)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(JobSatisfaction)) %>% 
      group_by(DevType,JobSatisfaction) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = JobSatisfaction, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How satistfied are you with your current job?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pJobSatisfaction)
    
  })
  #student full time
  output$plot8 <- renderPlot({
    
    pStudent <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Student)) %>% 
      group_by(DevType,Student) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Student, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Are you currently enrolled in formal education?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pStudent)
    
  })
  
  #country
  output$plot9 <- renderPlot({ 
    pCountry <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Country)) %>%
      mutate(Country = str_split(Country, pattern = ";")) %>%
      unnest(Country) %>% 
      group_by(DevType,Country) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(Country = reorder(Country, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Country, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Top 10 Countries of Respondents") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pCountry)
  })
  
  
  #Career
  output$plot10 <- renderPlot({
    pCareerSatisfaction <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(CareerSatisfaction = case_when(str_detect(CareerSatisfaction, "Extremely dissatisfied") ~ "a. Extremely dissatisfied",
                                            str_detect(CareerSatisfaction, "Moderately dissatisfied") ~ "b. Moderately dissatisfied",
                                            str_detect(CareerSatisfaction, "Slightly dissatisfied") ~ "c. Slightly dissatisfied",
                                            str_detect(CareerSatisfaction, "Neither satisfied nor dissatisfied") ~ "d. Neither satisfied nor dissatisfied",
                                            str_detect(CareerSatisfaction, "Slightly satisfied") ~ "e. Slightly satisfied",
                                            str_detect(CareerSatisfaction, "Moderately satisfied") ~ "f. Moderately satisfied",
                                            str_detect(CareerSatisfaction, "Extremely satisfied") ~ "g. Extremely satisfied",
                                            TRUE ~ CareerSatisfaction)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(CareerSatisfaction)) %>% 
      group_by(DevType,CareerSatisfaction) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = CareerSatisfaction, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How satistfied are you with your career?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pCareerSatisfaction)
    
    
  })
  #companysize
  output$plot11 <- renderPlot({
    
    pCompanySize <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(CompanySize = case_when(str_detect(CompanySize, "Fewer than 10 employees") ~ "c1 (fewer than 10)",
                                     str_detect(CompanySize, "10 to 19 employees") ~ "c2 (10-19)",
                                     str_detect(CompanySize, "20 to 99 employees") ~ "c3 (20-99)",
                                     str_detect(CompanySize, "100 to 499 employees") ~ "c4 (100-499)",
                                     str_detect(CompanySize, "500 to 999 employees") ~ "c5 (500-999)",
                                     str_detect(CompanySize, "1,000 to 4,999 employees") ~ "c6(1,000-4,999)",
                                     str_detect(CompanySize, "5,000 to 9,999 employees") ~ "c7(5,000-9,999)",
                                     str_detect(CompanySize, "10,000 or more employees") ~ "c8(more than 10,000)",
                                     TRUE ~ CompanySize)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(CompanySize)) %>% 
      group_by(DevType,CompanySize) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = CompanySize, label = ifelse(freq > 8, round(freq), "")))+
      ggtitle("How large is your company?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pCompanySize)
    
    
  })    
  #8HopeFiveYears & Search status
  output$plot12 <- renderPlot({
    pHopeFiveYears <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(HopeFiveYears = case_when(str_detect(HopeFiveYears, "Working as a founder or co-founder of my own company") ~ "Create my own company",
                                       str_detect(HopeFiveYears, "Working in a different or more specialized technical role than the one I'm in now") ~ "Working in a different or more specialized technical",
                                       str_detect(HopeFiveYears, "Working in a career completely unrelated to software development") ~ "Change my career",
                                       TRUE ~ HopeFiveYears)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(HopeFiveYears)) %>% 
      group_by(DevType,HopeFiveYears) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = HopeFiveYears, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your hope for the next five years?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pHopeFiveYears)
    
    
  })
  
  #search Status
  output$plot13 <- renderPlot({
    pJobSearchStatus <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(JobSearchStatus)) %>% 
      group_by(DevType,JobSearchStatus) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = JobSearchStatus, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your job looking status??") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    print(pJobSearchStatus)
    
    
  })
  
  # last new job 
  output$plot14 <- renderPlot({
    pLastNewJob <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(LastNewJob = case_when(str_detect(LastNewJob, "Less than a year ago") ~ "a. Less than a year ago",
                                    str_detect(LastNewJob, "Between 1 and 2 years ago") ~ "b. Between 1 and 2 years ago",
                                    str_detect(LastNewJob, "Between 2 and 4 years ago") ~ "c. Between 2 and 4 years ago",
                                    str_detect(LastNewJob, "More than 4 years ago") ~ "d. More than 4 years ago",
                                    str_detect(LastNewJob, "I've never had a job") ~ "e. I've never had a job",
                                    TRUE ~ LastNewJob)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(LastNewJob)) %>% 
      group_by(DevType,LastNewJob) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = LastNewJob, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("When was the last time that you took a job with a new employer?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    print(pLastNewJob)
    
  })
  
  #update CV
  output$plot15 <- renderPlot({
    pUpdateCV <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(UpdateCV)) %>% 
      group_by(DevType,UpdateCV) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = UpdateCV, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Why do you update your CV?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pUpdateCV)
    
  })
  
  #10 Education Type not included
  output$plot16 <- renderPlot({
    pEducationTypes <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(EducationTypes)) %>%
      mutate(EducationTypes = str_split(EducationTypes, pattern = ";")) %>%
      unnest(EducationTypes) %>% 
      group_by(DevType,EducationTypes) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(EducationTypes = reorder(EducationTypes, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = EducationTypes, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Top 10 language do you use?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pEducationTypes)
    
  })
  
  #selftaughttype
  output$plot17 <- renderPlot({
    
    pSelfTaughtTypes <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(SelfTaughtTypes)) %>%
      mutate(SelfTaughtTypes = str_split(SelfTaughtTypes, pattern = ";")) %>%
      unnest(SelfTaughtTypes) %>% 
      group_by(DevType,SelfTaughtTypes) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(SelfTaughtTypes = reorder(SelfTaughtTypes, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = SelfTaughtTypes, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is your self taught type? ") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    print(pSelfTaughtTypes)
    
  })
  
  
  
  ##############more personal############
  #wake time & skipmeals
  output$plot18 <- renderPlot({
    pWakeTime <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      mutate(WakeTime = case_when(str_detect(WakeTime, "Before 5:00 AM") ~ "a. Before 5:00 AM",
                                  str_detect(WakeTime, "Between 5:00 - 6:00 AM") ~ "b. 5:00 - 6:00 AM",
                                  str_detect(WakeTime, "Between 6:01 - 7:00 AM") ~ "c. 6:01 - 7:00 AM",
                                  str_detect(WakeTime, "Between 7:01 - 8:00 AM") ~ "d. 7:01 - 8:00 AM",
                                  str_detect(WakeTime, "Between 8:01 - 9:00 AM") ~ "e. 7:01 - 8:00 AM",
                                  str_detect(WakeTime, "Between 9:01 - 10:00 AM") ~ "f. 9:01 - 10:00 AM",
                                  str_detect(WakeTime, "Between 10:01 - 11:00 AM") ~ "g. 10:01 - 11:00 AM",
                                  str_detect(WakeTime, "Between 11:01 AM - 12:00 PM") ~ "h.11:01 AM - 12:00 PM",
                                  str_detect(WakeTime, "After 12:01 PM") ~ "i. 12:01 PM",
                                  str_detect(WakeTime, "I do not have a set schedule") ~ "j. I do not have a set schedule",
                                  str_detect(WakeTime, "I work night shifts") ~ "k. I work night shifts",
                                  TRUE ~ WakeTime)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(WakeTime)) %>% 
      group_by(DevType,WakeTime) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = WakeTime, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What time do you get up?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pSkipMeals <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(SkipMeals)) %>% 
      group_by(DevType,SkipMeals) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = SkipMeals, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("SkipMeals?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    result <- grid.arrange(pWakeTime,pSkipMeals, ncol = 2)
    print(result)
  })
  
  #HoursComputer & hours outside
  output$plot19 <- renderPlot({
    pHoursComputer <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(HoursComputer = case_when(str_detect(HoursComputer, "Less than 1 hour") ~ "0-1 Hour",
                                       TRUE ~ HoursComputer)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(HoursComputer)) %>% 
      group_by(DevType,HoursComputer) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = HoursComputer, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("HoursComputer?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pHoursOutside <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      mutate(HoursOutside = case_when(str_detect(HoursOutside, "Less than 30 minutes") ~ "a. Less than 30 minutes",
                                      str_detect(HoursOutside, "30 - 59 minutes") ~ "b. 30 - 59 minutes",
                                      str_detect(HoursOutside, "1 - 2 hours") ~ "c. 1 - 2 hours",
                                      str_detect(HoursOutside, "3 - 4 hours") ~ "d. 3 - 4 hours",
                                      str_detect(HoursOutside, "Over 4 hours") ~ "e. Over 4 hours",
                                      TRUE ~ HoursOutside)) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      group_by(DevType,HoursOutside) %>% 
      filter(!is.na(HoursOutside)) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = HoursOutside, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("HoursOutside?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    result <- grid.arrange(pHoursOutside,pHoursComputer, ncol = 2)
    print(result)
    
    
    
    
  })
  
  #Exercise
  
  output$plot20 <- renderPlot({
    pExercise <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Exercise)) %>% 
      group_by(DevType,Exercise) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Exercise, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Do you Exercise?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    print(pExercise)
    
    
    
    
  })
  
  #Edu parents
  output$plot21 <- renderPlot({
    
    pEducationParents <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(EducationParents)) %>% 
      group_by(DevType,EducationParents) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = EducationParents, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What is the Education of your parents?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    print(pEducationParents)
    
    
    
    
  })
  
  
  #Age & Dependents
  output$plot22 <- renderPlot({
    
    pAge <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Age)) %>% 
      group_by(DevType,Age) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Age, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How old are you?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pDependents <-  survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>% 
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(Dependents)) %>% 
      group_by(DevType,Dependents) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>% 
      ggplot(aes(x = DevType, y = freq, fill = Dependents, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Do you have any dependent?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    result <- grid.arrange(pAge,pDependents, ncol = 2)
    print(result)
    
  })
  
  #language working with
  output$plot23 <- renderPlot({
    
    pLanguageWorkedWith <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(LanguageWorkedWith)) %>%
      mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
      unnest(LanguageWorkedWith) %>% 
      group_by(DevType,LanguageWorkedWith) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(LanguageWorkedWith = reorder(LanguageWorkedWith, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = LanguageWorkedWith, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Top 10 language do you use?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pLanguageDesireNextYear <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(LanguageDesireNextYear)) %>%
      mutate(LanguageDesireNextYear = str_split(LanguageDesireNextYear, pattern = ";")) %>%
      unnest(LanguageDesireNextYear) %>% 
      group_by(DevType,LanguageDesireNextYear) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(LanguageDesireNextYear = reorder(LanguageDesireNextYear, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = LanguageDesireNextYear, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("What kind of language do you want to use next year?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pLanguageWorkedWith,pLanguageDesireNextYear, ncol = 2)
    print(result)
  })
  
  #database
  output$plot24 <- renderPlot({
    
    pDatabaseWorkedWith <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(DatabaseWorkedWith)) %>%
      mutate(DatabaseWorkedWith = str_split(DatabaseWorkedWith, pattern = ";")) %>%
      unnest(DatabaseWorkedWith) %>% 
      group_by(DevType,DatabaseWorkedWith) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(DatabaseWorkedWith = reorder(DatabaseWorkedWith, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = DatabaseWorkedWith, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Database that you are working on") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pDatabaseDesireNextYear <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(DatabaseDesireNextYear)) %>%
      mutate(DatabaseDesireNextYear = str_split(DatabaseDesireNextYear, pattern = ";")) %>%
      unnest(DatabaseDesireNextYear) %>% 
      group_by(DevType,DatabaseDesireNextYear) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(DatabaseDesireNextYear = reorder(DatabaseDesireNextYear, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = DatabaseDesireNextYear, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Top 10 Database Next year?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pDatabaseWorkedWith,pDatabaseDesireNextYear, ncol = 2)
    print(result)
  })
  
  #platform work and desire
  output$plot25 <- renderPlot({
    
    pPlatformWorkedWith <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(PlatformWorkedWith)) %>%
      mutate(PlatformWorkedWith = str_split(PlatformWorkedWith, pattern = ";")) %>%
      unnest(PlatformWorkedWith) %>% 
      group_by(DevType,PlatformWorkedWith) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(PlatformWorkedWith = reorder(PlatformWorkedWith, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = PlatformWorkedWith, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Platform that you are working with") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pPlatformDesireNextYear <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(PlatformDesireNextYear)) %>%
      mutate(PlatformDesireNextYear = str_split(PlatformDesireNextYear, pattern = ";")) %>%
      unnest(PlatformDesireNextYear) %>% 
      group_by(DevType,PlatformDesireNextYear) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(PlatformDesireNextYear = reorder(PlatformDesireNextYear, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = PlatformDesireNextYear, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Platform that you desire next year") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pPlatformWorkedWith,pPlatformDesireNextYear, ncol = 2)
    print(result)
  })
  
  #framework
  output$plot26 <- renderPlot({
    
    pFrameworkWorkedWith <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(FrameworkWorkedWith)) %>%
      mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
      unnest(FrameworkWorkedWith) %>% 
      group_by(DevType,FrameworkWorkedWith) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = FrameworkWorkedWith, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Framework that you are working with") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pFrameworkDesireNextYear <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(FrameworkDesireNextYear)) %>%
      mutate(FrameworkDesireNextYear = str_split(FrameworkDesireNextYear, pattern = ";")) %>%
      unnest(FrameworkDesireNextYear) %>% 
      group_by(DevType,FrameworkDesireNextYear) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = FrameworkDesireNextYear, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Framework that you desire next year") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pFrameworkWorkedWith,pFrameworkDesireNextYear, ncol = 2)
    print(result)
  }) 
  
  
  #IDE OS
  output$plot27 <- renderPlot({
    
    pIDE <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(IDE)) %>%
      mutate(IDE = str_split(IDE, pattern = ";")) %>%
      unnest(IDE) %>% 
      group_by(DevType,IDE) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(IDE = reorder(IDE, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = IDE, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("IDE that you are working with") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    pOperatingSystem <- survey %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>% 
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
      filter(!is.na(OperatingSystem)) %>%
      mutate(OperatingSystem = str_split(OperatingSystem, pattern = ";")) %>%
      unnest(OperatingSystem) %>% 
      group_by(DevType,OperatingSystem) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      mutate(OperatingSystem = reorder(OperatingSystem, -n)) %>%
      top_n(10) %>% 
      ggplot(aes(x = DevType, y = freq, fill = OperatingSystem, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("Operating System that you are working with") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    result <- grid.arrange(pIDE,pOperatingSystem, ncol = 2)
    print(result)
  })
  
  #Converted salary
  output$plot28 <- renderPlot({
    
    pConvertedsalary <- survey %>%
      unnest(DevType) %>%
      filter(!is.na(DevType),DevType %in% c(input$input1)) %>% 
      filter(!is.na(ConvertedSalary)) %>%
      mutate(DevType= str_split(DevType, pattern = ';')) %>%
      unnest(DevType) %>% 
      mutate(SalaryCategories = cut(ConvertedSalary,c(0,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,Inf), right = FALSE, labels = c("0-40k","41-50k","51-60k","61-70k","71-80k","81-90k","91-100k","101-110k","111-120k","121-130k","131-140k","140k above"))) %>% 
      group_by(DevType,SalaryCategories) %>% 
      summarise(n = n()) %>% 
      mutate(freq = n/sum(n)*100) %>%
      ggplot(aes(x = DevType, y = freq, fill = SalaryCategories, label = ifelse(freq > 5, paste0(round(freq),"%"), "")))+
      ggtitle("How much do you earn yearly?") +
      labs(x = NULL, y = "Frequency (%)") +
      geom_bar(stat = "identity", position = position_stack())+
      geom_text(position = position_stack(vjust = 0.5))+
      theme(
        axis.title.x = element_text(margin = margin(t = 8))
      )
    
    
    print(pConvertedsalary)
  })
  
  
  #AJ rank
  output$table1 <- renderTable({
    
    char <- nchar("Imagine that you are assessing a potential job opportunity. Please rank the following aspects of the job opportunity in order of importance (by dragging the choices up and down), where 1 is the most important and 10 is the least important.")+2
    AssesJobRank <-  substring(schema$QuestionText[18:27],char,350)
    
    assessjobnames <- grep("^AssessJob", names(survey), value=T)
    
    names <- c()
    scores <- c()
    
    for (assessjobname in assessjobnames) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(assessjobname) %>% 
        count()
      
      # Usefulness = a weighted average determining the usefulness of this platform
      score <- (usefulness[[2]][1] * 1 + usefulness[[2]][2] * 0.9 + usefulness[[2]][3] * 0.8+
                  usefulness[[2]][4] * 0.7 + usefulness[[2]][5] * 0.6 + usefulness[[2]][6] * 0.5+
                  usefulness[[2]][7] * 0.4 + usefulness[[2]][8] * 0.3 + usefulness[[2]][9] * 0.2+
                  usefulness[[2]][10])*0.1
      
      names <- c(names, gsub("Job", "", assessjobname))
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Score = scores,
      Name = names
    )
    scores_df <- cbind(scores_df, AssesJobRank)
    scores_df %>% 
      arrange(desc(Score)) %>% 
      select(AssesJobRank) %>% 
      mutate(Rank = c(1:length(AssesJobRank))) -> result
    colnames(result)[1] <- ("Job Assesment")
    result
    
    
    
  }, caption = "Imagine that you are assessing a potential job opportunity. Please rank the following aspects of the job opportunity in order of importance (by dragging the choices up and down), where 1 is the most important and 10 is the least important.")
  
  #Assessment Benefit Rank
  output$table2 <- renderTable({
    ABRanks <- substring(schema$QuestionText[28:38],248,350)
    
    
    ABenefits <- grep("^AssessBenefits", names(survey), value=T)
    
    names <- c()
    scores <- c()
    
    for (ABenefit in ABenefits) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(ABenefit) %>% 
        count()
      
      score <- (usefulness[[2]][1] * 11 + usefulness[[2]][2] * 10 + usefulness[[2]][3] * 9+
                  usefulness[[2]][4] * 8 + usefulness[[2]][5] * 7+usefulness[[2]][6] * 6 + usefulness[[2]][7] * 5 + usefulness[[2]][8] * 4+ usefulness[[2]][9] * 3 + usefulness[[2]][10] * 2+usefulness[[2]][11] * 1 )
      names <- c(names, gsub("AB", "", ABenefit))
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Score = scores,
      Name = names
    )
    
    scores_df <- cbind(scores_df, ABRanks)
    scores_df %>% 
      arrange(desc(Score)) %>% 
      select(ABRanks) %>% 
      mutate(Rank = c(1:length(ABRanks))) -> result
    colnames(result)[1] <- ("Assessment Benefit")
    result
    
    
  }, caption = "Imagine that you are assessing a potential job opportunity. Please rank the following aspects of the job opportunity in order of importance (by dragging the choices up and down), where 1 is the most important and 10 is the least important.")
  
  #JC Priorities
  output$table3 <- renderTable({
    
    char <- nchar("Imagine that a company wanted to contact you about a job that is a good fit for you. Please rank your preference in how you are contacted (by dragging the choices up and down), where 1 is the most preferred and 5 is the least preferred.")+2
    JCrank <-  substring(schema$QuestionText[39:43],char,350)
    
    #colnames(survey)[18:27] <- assessjobnames
    
    
    jcpriorities <- grep("^JobContactPriorities", names(survey), value=T)
    
    names <- c()
    scores <- c()
    
    for (jcpriority in jcpriorities) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(jcpriority) %>% 
        count()
      
      score <- (usefulness[[2]][1] * 5 + usefulness[[2]][2] * 4 + usefulness[[2]][3] * 3+
                  usefulness[[2]][4] * 2 + usefulness[[2]][5] * 1 )
      names <- c(names, gsub("JC", "", jcpriority))
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Score = scores,
      Name = names
    )
    
    scores_df <- cbind(scores_df,JCrank)
    
    scores_df %>% 
      arrange(desc(Score)) %>% 
      select(JCrank) %>% 
      mutate(Rank = c(1:length(JCrank))) -> result
    colnames(result)[1] <- ("Job Contact Priorities")
    result
    
  },caption = "Imagine that a company wanted to contact you about a job that is a good fit for you. Please rank your preference in how you are contacted (by dragging the choices up and down), where 1 is the most preferred and 5 is the least preferred.")
  
  
  #Ad Priorities
  output$table4 <- renderTable({
    
    char <- nchar("Please rank the following advertising qualities in order of their importance to you (by dragging the choices up and down), where 1 is the most important, and 7 is the least important.")+2
    AdPriorRank <-  substring(schema$QuestionText[87:93],char,350)
    
    
    adprioranks <- grep("^AdsPriorities", names(survey), value=T)
    
    names <- c()
    scores <- c()
    
    for (adpriorank in adprioranks) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(adpriorank) %>% 
        count()
      
      score <- (usefulness[[2]][1] * 7 + usefulness[[2]][2] * 6 + usefulness[[2]][3] * 5+
                  usefulness[[2]][4] * 4 + usefulness[[2]][5] * 3 ++ usefulness[[2]][6] * 2+ usefulness[[2]][7] * 1)
      names <- c(names, gsub("ADP", "", adpriorank))
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Score = scores,
      Name = names
    )
    
    scores_df <- cbind(scores_df,AdPriorRank)
    scores_df %>% 
      arrange(desc(Score)) %>% 
      select(AdPriorRank) %>% 
      mutate(Rank = c(1:length(AdPriorRank))) -> result
    colnames(result)[1] <- ("Ads Priorities")
    
    print(result)
    
  }, caption= "Please rank the following advertising qualities in order of their importance to you (by dragging the choices up and down), where 1 is the most important, and 7 is the least important")
  
  
  #JE Priorities
  output$table5 <- renderTable({
    
    char <- nchar("Imagine that same company decided to contact you through email. Please rank the following items by how important it is to include them in the message (by dragging the choices up and down), where 1 is the most important and 7 is the least important.")+2
    JEPriorRank <-  substring(schema$QuestionText[44:50],char,350)
    
    
    jepriors <- grep("^JobEmailPriorities", names(survey), value=T)
    
    names <- c()
    scores <- c()
    
    for (jeprior in jepriors) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(jeprior) %>% 
        count()
      
      score <- (usefulness[[2]][1] * 7 + usefulness[[2]][2] * 6 + usefulness[[2]][3] * 5+
                  usefulness[[2]][4] * 4 + usefulness[[2]][5] * 3 ++ usefulness[[2]][6] * 2+ usefulness[[2]][7] * 1)
      names <- c(names, gsub("JE", "", jeprior))
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Score = scores,
      Name = names
    )
    
    scores_df <- cbind(scores_df,JEPriorRank)
    scores_df %>% 
      arrange(desc(Score)) %>% 
      select(JEPriorRank) %>% 
      mutate(Rank = c(1:length(JEPriorRank))) -> result
    colnames(result)[1] <- ("Job Email Priorities")
    result
  },caption = "Imagine that same company decided to contact you through email. Please rank the following items by how important it is to include them in the message (by dragging the choices up and down), where 1 is the most important and 7 is the least important.")
  
  #Htools Plot it the rest does not need to plot
  output$plot29 <- renderPlot({
    
    char <- nchar("Please rate your interest in participating in each of the following hypothetical tools on Stack Overflow, where 1 is not at all interested and 5 is extremely interested.")+2
    HToolsRank <-  substring(schema$QuestionText[110:114],char,350)
    
    
    
    HTools <- grep("^HypotheticalTools", names(survey), value=T)
    HTools
    names <- c()
    popularities <- c()
    scores <- c()
    
    for (HTool  in HTools ) {
      usefulness <- survey %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        filter(!is.na(DevType),DevType %in% c(input$input1)) %>%
        group_by_(HTool) %>%
        count()
      
      # Popularity = the number of people who responded to this question
      popularity <- usefulness[[2]][1] + usefulness[[2]][2] + usefulness[[2]][3]+ usefulness[[2]][4]+ usefulness[[2]][5]
      
      # Usefulness = a weighted average determining the usefulness of this platform
      score <- (usefulness[[2]][3] * 0.2 + usefulness[[2]][1] * 0.4 + usefulness[[2]][4] * 0.6+ usefulness[[2]][5] * 0.8+ usefulness[[2]][2] * 1) / popularity
      
      names <- c(names, gsub("Hypotheticaltools", "", HTools ))
      popularities <- c(popularities, popularity)
      scores <- c(scores, score)
    }
    
    scores_df <- data.frame(
      Popularity = popularities,
      Score = scores,
      Name = names
    )
    scores_df <- scores_df[1:5,]
    scores_df <- cbind(scores_df, HToolsRank)
    
    ggplot(scores_df, aes(x = Score, y = Popularity)) +
      ggtitle("Popularity and Scores of Hypothetical Tools") +
      geom_point() +
      geom_text(aes(label = HToolsRank), nudge_y = 3)+
      xlim(0.4,0.7)->result
    print(result)
    
  })
  
  #output for tab 4
  output$plot30 <- renderPlot({
    
    if (input$rbutt=="tho"){
      
      survey %>%
        select(Respondent, DevType, Hobby) %>%
        filter(DevType != "", Hobby !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,Hobby) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Hobby, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs coding as a Hoby") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
      
    }
    
    if (input$rbutt=="toso"){
      survey %>%
        select(Respondent, DevType, OpenSource) %>%
        filter(DevType != "", OpenSource !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,OpenSource) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = OpenSource, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Contribution to open source") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tfe"){
      survey$FormalEducation[survey$FormalEducation=="Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"] <- "Secondary School"
      survey %>%
        select(Respondent, DevType, FormalEducation) %>%
        filter(DevType != "", FormalEducation !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,FormalEducation) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = FormalEducation, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Formal Education") +
        labs(x = NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
      
    }
    
    if (input$rbutt=="tem"){
      survey %>%
        select(Respondent, DevType, Employment) %>%
        filter(DevType != "", Employment !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,Employment) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Employment, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Employment Type") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tum"){
      survey %>%
        select(Respondent, DevType, UndergradMajor) %>%
        filter(DevType != "", UndergradMajor !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,UndergradMajor) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = UndergradMajor, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Undergraduate Major") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tct"){
      survey %>%
        select(Respondent, DevType, YearsCoding ) %>%
        mutate(YearsCoding = case_when(str_detect(YearsCoding, "0-2 years") ~ "a. 0-2 years",
                                       str_detect(YearsCoding, "3-5 years") ~ "b. 3-5 years",
                                       str_detect(YearsCoding, "6-8 years") ~ "c. 6-8 years",
                                       str_detect(YearsCoding, "9-11 years") ~ "d. 9-11 years",
                                       str_detect(YearsCoding, "12-14 years") ~ "e. 12-14 years",
                                       str_detect(YearsCoding, "15-17 years") ~ "f. 15-17 years",
                                       str_detect(YearsCoding, "18-20 years") ~ "g. 18-20 years",
                                       str_detect(YearsCoding, "21-23 years") ~ "h.21-23 years",
                                       str_detect(YearsCoding, "After 12:01 PM") ~ "i. After 12:01 PM",
                                       str_detect(YearsCoding, "24-26 years") ~ "j. 24-26 years",
                                       str_detect(YearsCoding, "27-29 years") ~ "k. 27-29 years",
                                       str_detect(YearsCoding, "30 or more years") ~ "l. 30 or more years",
                                       TRUE ~ YearsCoding)) %>%
        filter(DevType != "", YearsCoding !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,YearsCoding) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = YearsCoding, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Coding Time") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tctp"){
      survey %>%
        select(Respondent, DevType, YearsCodingProf) %>%
        mutate(YearsCodingProf = case_when(str_detect(YearsCodingProf, "0-2 years") ~ "a. 0-2 years",
                                           str_detect(YearsCodingProf, "3-5 years") ~ "b. 3-5 years",
                                           str_detect(YearsCodingProf, "6-8 years") ~ "c. 6-8 years",
                                           str_detect(YearsCodingProf, "9-11 years") ~ "d. 9-11 years",
                                           str_detect(YearsCodingProf, "12-14 years") ~ "e. 12-14 years",
                                           str_detect(YearsCodingProf, "15-17 years") ~ "f. 15-17 years",
                                           str_detect(YearsCodingProf, "18-20 years") ~ "g. 18-20 years",
                                           str_detect(YearsCodingProf, "21-23 years") ~ "h.21-23 years",
                                           str_detect(YearsCodingProf, "After 12:01 PM") ~ "i. After 12:01 PM",
                                           str_detect(YearsCodingProf, "24-26 years") ~ "j. 24-26 years",
                                           str_detect(YearsCodingProf, "27-29 years") ~ "k. 27-29 years",
                                           str_detect(YearsCodingProf, "30 or more years") ~ "l. 30 or more years",
                                           TRUE ~ YearsCodingProf)) %>%
        filter(DevType != "", YearsCodingProf !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,YearsCodingProf) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = YearsCodingProf, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Coding Time Professionally") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tjs"){
      survey %>%
        select(Respondent, DevType, JobSatisfaction) %>%
        mutate(JobSatisfaction = case_when(str_detect(JobSatisfaction, "Extremely dissatisfied") ~ "a. Extremely dissatisfied",
                                           str_detect(JobSatisfaction, "Moderately dissatisfied") ~ "b. Moderately dissatisfied",
                                           str_detect(JobSatisfaction, "Slightly dissatisfied") ~ "c. Slightly dissatisfied",
                                           str_detect(JobSatisfaction, "Neither satisfied nor dissatisfied") ~ "d. Neither satisfied nor dissatisfied",
                                           str_detect(JobSatisfaction, "Slightly satisfied") ~ "e. Slightly satisfied",
                                           str_detect(JobSatisfaction, "Moderately satisfied") ~ "f. Moderately satisfied",
                                           str_detect(JobSatisfaction, "Extremely satisfied") ~ "g. Extremely satisfied",
                                           TRUE ~ JobSatisfaction)) %>%
        filter(DevType != "", JobSatisfaction !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,JobSatisfaction) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = JobSatisfaction, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Job Satisfaction") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tsts"){
      survey %>%
        select(Respondent, DevType, Student) %>%
        filter(DevType != "", Student !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,Student) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Student, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Student Status") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tc"){
      survey %>%
        select(Respondent, DevType, Country) %>%
        filter(DevType != "", Country !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,Country) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(Country = reorder(Country, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Country, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Respondents' Country") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
      
    }
    
    if (input$rbutt=="tcst"){
      survey %>%
        select(Respondent, DevType, CareerSatisfaction) %>%
        filter(DevType != "", CareerSatisfaction !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>%
        mutate(CareerSatisfaction = case_when(str_detect(CareerSatisfaction, "Extremely dissatisfied") ~ "a. Extremely dissatisfied",
                                              str_detect(CareerSatisfaction, "Moderately dissatisfied") ~ "b. Moderately dissatisfied",
                                              str_detect(CareerSatisfaction, "Slightly dissatisfied") ~ "c. Slightly dissatisfied",
                                              str_detect(CareerSatisfaction, "Neither satisfied nor dissatisfied") ~ "d. Neither satisfied nor dissatisfied",
                                              str_detect(CareerSatisfaction, "Slightly satisfied") ~ "e. Slightly satisfied",
                                              str_detect(CareerSatisfaction, "Moderately satisfied") ~ "f. Moderately satisfied",
                                              str_detect(CareerSatisfaction, "Extremely satisfied") ~ "g. Extremely satisfied",
                                              TRUE ~ CareerSatisfaction)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,CareerSatisfaction) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = CareerSatisfaction, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Career Satisfaction") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tcsz"){
      survey %>%
        select(Respondent, DevType, CompanySize) %>%
        filter(DevType != "", CompanySize !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>%
        mutate(CompanySize = case_when(str_detect(CompanySize, "Fewer than 10 employees") ~ "c1 (fewer than 10)",
                                       str_detect(CompanySize, "10 to 19 employees") ~ "c2 (10-19)",
                                       str_detect(CompanySize, "20 to 99 employees") ~ "c3 (20-99)",
                                       str_detect(CompanySize, "100 to 499 employees") ~ "c4 (100-499)",
                                       str_detect(CompanySize, "500 to 999 employees") ~ "c5 (500-999)",
                                       str_detect(CompanySize, "1,000 to 4,999 employees") ~ "c6(1,000-4,999)",
                                       str_detect(CompanySize, "5,000 to 9,999 employees") ~ "c7(5,000-9,999)",
                                       str_detect(CompanySize, "10,000 or more employees") ~ "c8(more than 10,000)",
                                       TRUE ~ CompanySize)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,CompanySize) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = CompanySize, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Company Size ") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="thf"){
      survey %>%
        select(Respondent, DevType, HopeFiveYears) %>%
        filter(DevType != "", HopeFiveYears !="") %>%
        mutate(HopeFiveYears = case_when(str_detect(HopeFiveYears, "Working as a founder or co-founder of my own company") ~ "Create my own company",
                                         str_detect(HopeFiveYears, "Working in a different or more specialized technical role than the one I'm in now") ~ "Working in a different or more specialized technical",
                                         str_detect(HopeFiveYears, "Working in a career completely unrelated to software development") ~ "Change my career",
                                         TRUE ~ HopeFiveYears)) %>%
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,HopeFiveYears) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = HopeFiveYears, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Hope in Five Years") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tscss"){
      survey %>%
        select(Respondent, DevType, JobSearchStatus) %>%
        filter(DevType != "", JobSearchStatus !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,JobSearchStatus) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = JobSearchStatus, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Job Search Status") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tls"){
      survey %>%
        select(Respondent, DevType, LastNewJob) %>%
        filter(DevType != "", LastNewJob !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>%
        mutate(LastNewJob = case_when(str_detect(LastNewJob, "Less than a year ago") ~ "a. Less than a year ago",
                                      str_detect(LastNewJob, "Between 1 and 2 years ago") ~ "b. Between 1 and 2 years ago",
                                      str_detect(LastNewJob, "Between 2 and 4 years ago") ~ "c. Between 2 and 4 years ago",
                                      str_detect(LastNewJob, "More than 4 years ago") ~ "d. More than 4 years ago",
                                      str_detect(LastNewJob, "I've never had a job") ~ "e. I've never had a job",
                                      TRUE ~ LastNewJob)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,LastNewJob) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = LastNewJob, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Last or New Job") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tucv"){
      survey %>%
        select(Respondent, DevType, UpdateCV) %>%
        filter(DevType != "", UpdateCV !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,UpdateCV) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = UpdateCV, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Update CV") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="twt"){
      survey %>%
        select(Respondent, DevType, WakeTime) %>%
        filter(DevType != "", WakeTime !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>%
        mutate(WakeTime = case_when(str_detect(WakeTime, "Before 5:00 AM") ~ "a. Before 5:00 AM",
                                    str_detect(WakeTime, "Between 5:00 - 6:00 AM") ~ "b. 5:00 - 6:00 AM",
                                    str_detect(WakeTime, "Between 6:01 - 7:00 AM") ~ "c. 6:01 - 7:00 AM",
                                    str_detect(WakeTime, "Between 7:01 - 8:00 AM") ~ "d. 7:01 - 8:00 AM",
                                    str_detect(WakeTime, "Between 8:01 - 9:00 AM") ~ "e. 7:01 - 8:00 AM",
                                    str_detect(WakeTime, "Between 9:01 - 10:00 AM") ~ "f. 9:01 - 10:00 AM",
                                    str_detect(WakeTime, "Between 10:01 - 11:00 AM") ~ "g. 10:01 - 11:00 AM",
                                    str_detect(WakeTime, "Between 11:01 AM - 12:00 PM") ~ "h.11:01 AM - 12:00 PM",
                                    str_detect(WakeTime, "After 12:01 PM") ~ "i. 12:01 PM",
                                    str_detect(WakeTime, "I do not have a set schedule") ~ "j. I do not have a set schedule",
                                    str_detect(WakeTime, "I work night shifts") ~ "k. I work night shifts",
                                    TRUE ~ WakeTime)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,WakeTime) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = WakeTime, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Wake up Time") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tsm"){
      survey %>%
        select(Respondent, DevType, SkipMeals) %>%
        filter(DevType != "", SkipMeals !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,SkipMeals) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = SkipMeals, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Skip Meals") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="thc"){
      survey %>%
        select(Respondent, DevType, HoursComputer) %>%
        filter(DevType != "", HoursComputer !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        mutate(HoursComputer = case_when(str_detect(HoursComputer, "Less than 1 hour") ~ "0-1 Hour",
                                         TRUE ~ HoursComputer)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,HoursComputer) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = HoursComputer, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Hours in front of Computers") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="thou"){
      survey %>%
        select(Respondent, DevType, HoursOutside) %>%
        filter(DevType != "", HoursOutside !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        mutate(HoursOutside = case_when(str_detect(HoursOutside, "Less than 30 minutes") ~ "a. Less than 30 minutes",
                                        str_detect(HoursOutside, "30 - 59 minutes") ~ "b. 30 - 59 minutes",
                                        str_detect(HoursOutside, "1 - 2 hours") ~ "c. 1 - 2 hours",
                                        str_detect(HoursOutside, "3 - 4 hours") ~ "d. 3 - 4 hours",
                                        str_detect(HoursOutside, "Over 4 hours") ~ "e. Over 4 hours",
                                        TRUE ~ HoursOutside)) %>%
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,HoursOutside) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = HoursOutside, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Hours Outside") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tex"){
      survey %>%
        select(Respondent, DevType, Exercise) %>%
        filter(DevType != "", Exercise !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,Exercise) %>% 
        summarise(n = n()) %>%
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Exercise, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Frequency of Exercise") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tpe"){
      survey$EducationParents[survey$EducationParents=="Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)"] <- "Secondary School"
      survey %>%
        select(Respondent, DevType, EducationParents) %>%
        filter(DevType != "", EducationParents !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,EducationParents) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = EducationParents, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Parents Education") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tage"){
      survey %>%
        select(Respondent, DevType, Age) %>%
        filter(DevType != "", Age !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,Age) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Age, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Age") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tdep"){
      survey %>%
        select(Respondent, DevType, Dependents) %>%
        filter(DevType != "", Dependents !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>% 
        group_by(DevType,Dependents) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = Dependents, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Number of Dependents") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tlw"){
      survey %>%
        select(Respondent, DevType, LanguageWorkedWith) %>%
        filter(DevType != "", LanguageWorkedWith !="") %>% 
        mutate(LanguageWorkedWith = str_split(LanguageWorkedWith, pattern = ";")) %>%
        unnest(LanguageWorkedWith) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,LanguageWorkedWith) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(LanguageWorkedWith = reorder(LanguageWorkedWith, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = LanguageWorkedWith, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Language Worked With") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tld"){
      survey %>%
        select(Respondent, DevType, LanguageDesireNextYear) %>%
        filter(DevType != "", LanguageDesireNextYear !="") %>% 
        mutate(LanguageDesireNextYear = str_split(LanguageDesireNextYear, pattern = ";")) %>%
        unnest(LanguageDesireNextYear) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,LanguageDesireNextYear) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(LanguageDesireNextYear = reorder(LanguageDesireNextYear, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = LanguageDesireNextYear, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Language Desire Next Year") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tdw"){
      survey %>%
        select(Respondent, DevType, DatabaseWorkedWith) %>%
        filter(DevType != "", DatabaseWorkedWith !="") %>% 
        mutate(DatabaseWorkedWith = str_split(DatabaseWorkedWith, pattern = ";")) %>%
        unnest(DatabaseWorkedWith) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,DatabaseWorkedWith) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(DatabaseWorkedWith = reorder(DatabaseWorkedWith, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = DatabaseWorkedWith, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Database Worked With") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tdd"){
      survey %>%
        select(Respondent, DevType, DatabaseDesireNextYear) %>%
        filter(DevType != "", DatabaseDesireNextYear !="") %>% 
        mutate(DatabaseDesireNextYear = str_split(DatabaseDesireNextYear, pattern = ";")) %>%
        unnest(DatabaseDesireNextYear) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,DatabaseDesireNextYear) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(DatabaseDesireNextYear = reorder(DatabaseDesireNextYear, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = DatabaseDesireNextYear, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Database Desire Next Year") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tpw"){
      survey %>%
        select(Respondent, DevType, PlatformWorkedWith) %>%
        filter(DevType != "", PlatformWorkedWith !="") %>% 
        mutate(PlatformWorkedWith = str_split(PlatformWorkedWith, pattern = ";")) %>%
        unnest(PlatformWorkedWith) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,PlatformWorkedWith) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(PlatformWorkedWith = reorder(PlatformWorkedWith, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = PlatformWorkedWith, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Platform Working With") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tpd"){
      survey %>%
        select(Respondent, DevType, PlatformDesireNextYear) %>%
        filter(DevType != "", PlatformDesireNextYear !="") %>% 
        mutate(PlatformDesireNextYear = str_split(PlatformDesireNextYear, pattern = ";")) %>%
        unnest(PlatformDesireNextYear) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,PlatformDesireNextYear) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(PlatformDesireNextYear = reorder(PlatformDesireNextYear, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = PlatformDesireNextYear, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Platform Desire Next Year") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="vertical")->result
      print(result)
    }
    
    if (input$rbutt=="tfw"){
      survey %>%
        select(Respondent, DevType, FrameworkWorkedWith) %>%
        filter(DevType != "", FrameworkWorkedWith !="") %>% 
        mutate(FrameworkWorkedWith = str_split(FrameworkWorkedWith, pattern = ";")) %>%
        unnest(FrameworkWorkedWith) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,FrameworkWorkedWith) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(FrameworkWorkedWith = reorder(FrameworkWorkedWith, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = FrameworkWorkedWith, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Framework Desire") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tfd"){
      survey %>%
        select(Respondent, DevType, FrameworkDesireNextYear) %>%
        filter(DevType != "", FrameworkDesireNextYear !="") %>% 
        mutate(FrameworkDesireNextYear = str_split(FrameworkDesireNextYear, pattern = ";")) %>%
        unnest(FrameworkDesireNextYear) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,FrameworkDesireNextYear) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(FrameworkDesireNextYear = reorder(FrameworkDesireNextYear, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = FrameworkDesireNextYear, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Framework Desire Next Year") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tide"){
      survey %>%
        select(Respondent, DevType, IDE) %>%
        filter(DevType != "", IDE !="") %>% 
        mutate(IDE = str_split(IDE, pattern = ";")) %>%
        unnest(IDE) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,IDE) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        mutate(IDE = reorder(IDE, -n)) %>%
        top_n(10) %>% 
        ggplot(aes(x = DevType, y = freq, fill = IDE, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs IDE used") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tosy"){
      survey %>%
        select(Respondent, DevType, OperatingSystem) %>%
        filter(DevType != "", OperatingSystem !="") %>% 
        mutate(OperatingSystem = str_split(OperatingSystem, pattern = ";")) %>%
        unnest(OperatingSystem) %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>%
        mutate(DevType = case_when(str_detect(DevType, "C-suite executive (CEO, CTO, etc.)") ~ "C-Suite",
                                   str_detect(DevType, "Data scientist or machine learning specialist") ~ "Data Scientist",
                                   str_detect(DevType, "Desktop or enterprise applications developer") ~ "App Developer",
                                   str_detect(DevType, "Embedded applications or devices developer") ~ "Devices Developer",
                                   TRUE ~ DevType)) %>%
        group_by(DevType,OperatingSystem) %>%
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>%
        ggplot(aes(x = DevType, y = freq, fill = OperatingSystem, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Operating System Used") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
    if (input$rbutt=="tsal"){
      survey %>%
        mutate(SalaryCategories = cut(ConvertedSalary,c(0,40000,50000,60000,70000,80000,90000,100000,110000,120000,130000,140000,Inf), right = FALSE, labels = c("0-40k","41-50k","51-60k","61-70k","71-80k","81-90k","91-100k","101-110k","111-120k","121-130k","131-140k","140k above"))) %>% 
        filter(DevType != "", SalaryCategories !="") %>% 
        mutate(DevType= str_split(DevType, pattern = ';')) %>% 
        unnest(DevType) %>% 
        group_by(DevType,SalaryCategories) %>% 
        summarise(n = n()) %>% 
        mutate(freq = n/sum(n)*100) %>% 
        ggplot(aes(x = DevType, y = freq, fill = SalaryCategories, label = ifelse(freq > 8, round(freq), "")))+
        ggtitle("Job Title vs Salary") +
        labs(x=NULL, y = "Frequency (%)") +
        geom_bar(stat = "identity", position = position_stack())+
        geom_text(position = position_stack(vjust = 0.5))+
        theme(
          axis.title.x = element_text(margin = margin(t = 8)),
          axis.text.x = element_text(angle = 270, hjust = 0)
        )+scale_fill_discrete(guide = guide_legend()) +
        theme(legend.position="bottom",legend.direction="horizontal")->result
      print(result)
    }
    
  })
  
})

rsconnect::setAccountInfo(name='hafidpradipta',
                          token='BBE53952BC403B7F474AC06B0C47FB89',
                          secret='sPB7yGK8xJmh2Xytpa1+9Nisfsj2hvwcrTTx2gbM')
