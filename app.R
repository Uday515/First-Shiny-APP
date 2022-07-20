#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(DT)
library(ggplot2)
library(fmsb)
library(fontawesome)

df = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/all_doc.csv')
df1 = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/dead.csv')
df2 = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/catt.csv')
df3 = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/avgSofa.csv')
df4 = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/fed_catt.csv')
df5 = read.csv('/Users/udaysingh/Downloads/r_learn/doc_per/Dataset/doctor_characteristic_data.csv')
  
ui <- dashboardPage(
  
  
  skin="purple",
  dashboardHeader(title = "Performances of Physicians", titleWidth = 300,
                  
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Source of data",
                                 message =tags$div(  
                                                    tags$br(),"The data has been collected from (click ",tags$br(),"here) which includes information about the",tags$br()," performances of 25 Physicians and their", 
                                                    tags$br()," patient conditions, plus some", 
                                                    tags$br()," related information."),
                                 
                                 href = "https://ssc.ca/en/case-study/developing-a-physician-performance-model-critical-care-assessing-quality-and-value",
                                 icon = icon("list")
                                 ),
                               messageItem(
                                 from = "Purpose for dashboard ",
                                 message = tags$div(tags$br(),
                                                    "The purpose of the dashboard was",tags$br(),"  to help hospital administrators evaluate",tags$br(),"  the doctors' performance based on key ",tags$br()," features highlighted in this dashboard. "),
                                 icon = icon("question"),
                                
                               )
                  )
                  
                  ),

  dashboardSidebar( 
    
    selectInput("Doctor_ID","Please select a doctor ", choices = c("Doc_ID 1" =1, "Doc_ID 2" = 2, "Doc_ID 3" = 3, "Doc_ID 4" = 4, "Doc_ID 5" = 5, "Doc_ID 6" = 6, "Doc_ID 7" = 7, "Doc_ID 8" = 8, 
                                                                   "Doc_ID 9" = 9, "Doc_ID 10" = 10, "Doc_ID 11" = 11, "Doc_ID 12" = 12, "Doc_ID 13" = 13,
                                                                   "Doc_ID 14" = 14, "Doc_ID 15" = 15, "Doc_ID 16" = 16, "Doc_ID 17" = 17, "Doc_ID 18" = 18, "Doc_ID 19" = 19,
                                                                   "Doc_ID 20" = 20, "Doc_ID 21" = 21, "Doc_ID 22" = 22, "Doc_ID 23" = 23, "Doc_ID 24" = 24, "Doc_ID 25" = 25),selectize = T)
  ),
  
  

  dashboardBody(


    fluidRow(
      
     
      column(6,valueBoxOutput("vbox",width = 12),
             valueBoxOutput("vbox2",width = 6),
             valueBoxOutput("vbox3",width = 6),
             valueBoxOutput("vbox4",width = 6),
             valueBoxOutput("vbox5",width = 6)
             ),
    
      
      
      
      
      column(6,
      h1("Physician's basic Information"),
      column(6,
            
               h4("No. of ICU's woked in"),textOutput("site"),tags$head(tags$style("#site{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br(),
               h4("Leadership role(Leader)/if not(Leader_x)"),textOutput("rank"),tags$head(tags$style("#rank{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br(),
               h4("Age"),textOutput("age"),tags$head(tags$style("#age{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br()
               
             
      ),
      column(6,
             
               h4("Gender"),textOutput("sex"),tags$head(tags$style("#sex{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br(),
               h4("Domain"),textOutput("domain"),tags$head(tags$style("#domain{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br(),
               h4("Education"),textOutput("edu"),tags$head(tags$style("#edu{color: red;font-size: 22px; font-style: italic; font-weight: bold;}")),
               br()
             )
      
    
    
      ,style = "background-color:white; padding : 20px"),  
    ),
    
    br(),
    fluidRow(

      column(5, h1("Mortality rate for the patients' treated"),plotOutput("distPie")),
      column(7, h1("Number of Patient's disease type"),plotOutput("graph2"))
      
      
    ),
    
   

    
    fluidRow(
      
      column(5,h1("Score for different performance measure "),plotOutput("spider")),
      column(7,h1("Data table for important feature based on Patient's disease type "),dataTableOutput("data_table"))
      
      
    ),
    textOutput("author")
    
  )
)



server <- function(input, output) { 
  
  output$distPie <- renderPlot({
    
    x = c(df1[input$Doctor_ID,"D"],df1[input$Doctor_ID,"A"])
    pie_percent = round(100*x/sum(x), 1)
    
    cols = c('#9cd43c', '#e47c84')
    
    pie(x, labels = pie_percent, 
        main = "Mortality rate in %"
        ,col = cols)  
    legend("topright", 
           c("Dead", "Alive"), 
           cex = 1.5,  
           fill = cols) 
    
    
  })
  
  
  
  
  output$data_table= renderDataTable(df3)
  
  output$graph2 <- renderPlot({

    data <- data.frame(
      Types_of_patients = c('Gastrointestinal', 'Cardiovascular', 'Neuro', 'Respiratory', 'Trauma'),
      Patient_count = c(df2[input$Doctor_ID,2],df2[input$Doctor_ID,3],df2[input$Doctor_ID,4],df2[input$Doctor_ID,5],df2[input$Doctor_ID,6])
    )
    
    p <- ggplot(data, aes(x=Types_of_patients, y=Patient_count,fill=Types_of_patients)) + 
      geom_bar(stat = "identity")+guides(fill = guide_legend(override.aes = list(size = 15)))+
      geom_text(aes(label = Patient_count), vjust = -0.2) + theme(text = element_text(size = 20))
      #+theme(legend.text=element_text(size=15))+theme(axis.text = element_text(size = 15))  
    p
    
    
  })
  
  
  output$spider <- renderPlot({
    
    x1 <-c (5,3,df4[input$Doctor_ID,"Advocacy"])
    x2 <-c(5,3,df4[input$Doctor_ID,"Scientific_knowledge"])
    x3 <-c (5,3,df4[input$Doctor_ID,"Med_expert"])
    x4 <- c (5,3,df4[input$Doctor_ID,"Professionalism"])
    x5 <- c (5,3,df4[input$Doctor_ID,"Communication"])
    x6 <- c (5,3,df4[input$Doctor_ID,"Collaboration"])
    x7 <- c (5,3,df4[input$Doctor_ID,"Managemnet"])
    
    
    dff <- data.frame(Advocacy=x1, Knowledge=x2, Expertise=x3,Professionalism=x4
                      
                      ,Communication=x5, Collaboration = x6,Managemnet= x7 )
    radarchart(dff,axistype=1 , 
               
               #custom polygon
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
               
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(3,5,.50), cglwd=0.8,
               
               #custom labels
               vlcex=1 
    )
    
    
  })
  
  
  output$vbox <- renderValueBox({
    valueBox(
      value<-renderText(round(df[input$Doctor_ID,"Q23"],2)),
      "Score out of 5", width = 6,icon =  icon("star"),color = "purple")
      #renderText(input$df$Q23["input$Doctor_ID"])
      
    
    
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      value <- renderText(df[input$Doctor_ID,"patients_count"]),
      "Patient count",width = 6,icon =  icon("user", lib = 'glyphicon'),color = "yellow"
      #renderText(input$df$Q23["input$Doctor_ID"])
      
      
    )
    
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(
      value<-renderText(df[input$Doctor_ID,"bedside_count"]),
      "Critical Patient treated",width = 6,icon =  icon("heart-empty", lib = 'glyphicon'),color = "green"
      
      #renderText(input$df$Q23["input$Doctor_ID"])
      
    )
    
  })
  
  output$vbox4 <- renderValueBox({
    valueBox(
      value<-renderText(round(df[input$Doctor_ID,"M4"],2)),
      "2016 Survay Score out of 8",width = 6,icon =  icon("fast-backward", lib = 'glyphicon')
      
      #renderText(input$df$Q23["input$Doctor_ID"])
      
    )
    
  })
  #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  output$vbox5 <- renderValueBox({
    valueBox(
      value<-renderText(round(df[input$Doctor_ID,"M5"],2)),
      "2016 Resedency Score 1- Poor, 2- moderate, 3- good",width = 6,icon =  icon("thumbs-up", lib = 'glyphicon'),color = "fuchsia"
      
      #renderText(input$df$Q23["input$Doctor_ID"])
      
    )
    
  })
  
  output$author <- renderText({ 
    paste("Dashboard By Uday Singh — 14 July 2022")
  })
  
  output$age <- renderText({ 
    df5[input$Doctor_ID,"M8"]
  })
  
  output$rank <- renderText({ 
    
    df5[input$Doctor_ID,"M2"]
  })
  
  output$sex <- renderText({ 
    
    df5[input$Doctor_ID,"M6"]
  })
  
  output$domain <- renderText({ 
    
    df5[input$Doctor_ID,"M7"]
  })
  
  output$edu <- renderText({ 
    
    df5[input$Doctor_ID,"M9"]
  })
  
    
  output$site <- renderText({ 
    
    df5[input$Doctor_ID,"M1"]
  })
  
  
 # colm <- as.numeric(input$Doctor_ID)
  #output$Doctor_score <- renderText({input$df[Doctor,"Q23"]})
  output$Doctor_ID <- renderText({input$Doctor_ID})
  }

shinyApp(ui, server)