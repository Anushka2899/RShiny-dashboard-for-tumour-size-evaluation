install.packages("shiny")
install.packages("tidyverse")
install.packages("shinythemes")
install.packages("DT")
install.packages("dplyr")
install.packages("knitr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("RColorBrewer")
install.packages("tumgr")

library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(dplyr)
library(knitr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(tumgr)

#USER INTERFACE PART

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel( list("Plots for Tumor Size Evaluation",icon("fa-shower")), windowTitle = list("Plot",icon("stopwatch-20"))),
                tabsetPanel(
                  tabPanel(
                    fluidRow(style = "height:10px", style = "background: white"),
                    fluidRow(style =  "background: white",
                             column(width = 3,style = "background: white",
                                    br(),
                                    wellPanel(style = "background: white",
                                              selectInput("plot_type", "Select Plot Type", choices = c("none","Waterfall", "Spider"), selected = "none"),
                                              fileInput("file_input", NULL,multiple = FALSE,accept = c("csv")),
                                              checkboxInput(inputId = "default_data", label = "Use Example Data", value = FALSE),
                                              radioButtons(inputId = "file_choice",label = list(icon("file-code"),"Select file type"), choices = c("csv","xlsx","txt","sas7bdat")),
                                              textInput(inputId = "filter_Condition",label = list(icon("filter"),"Apply Filter")),
                                              actionButton("click",label = list("Load Data"))
                                    ),
                                    
                                    
                                    wellPanel(
                                      h3("Explanatory variables:"),
                                      selectInput(inputId = "x_vars", label = "Select Variable for X axis", choices = NULL),
                                      selectInput(inputId = "y_vars", label = "Select Variable for Y axis", choices = NULL),
                                      selectInput(inputId = "comp_vars", label = list(icon("list-alt"),"Select Variable for Comparison"), choices = NULL),
                                      
                                      wellPanel(
                                        h3(list(icon("chart-line"),"Plotting options:")),
                                        textInput("tlab","",placeholder="Title for plot"),
                                        textInput("xlab","",placeholder="label for x-axis"),
                                        textInput("ylab","",placeholder="label for y-axis"),
                                        sliderInput("pr_var", label = "Reference line for PR", min = -50, max = -1, value = -30),
                                        sliderInput("pd_var", label = "Reference line for PD", min = 1, max = 50, value = 30),
                                        actionButton("click2",label = list("Generate Plot")),
                                      ))),
                             column(width = 9, style =  "background: white",   tabsetPanel(type = "tabs",
                                                                                           tabPanel("Data", dataTableOutput(outputId = "data_content")),
                                                                                           tabPanel("Plot",
                                                                                                    plotlyOutput(outputId = "km_curve", height = 650))))
                             
                             
                    )
                  ))
)


#Server code

server<- function(input, output, session) {
  
  filedata <- reactive({
    if(input$default_data==F){
      infile <- input$file_input
      if (is.null(infile)){
        return(NULL)
      }
      read.csv(infile$datapath)
    } else if(input$default_data==T && input$plot_type=="Waterfall"){
      set.seed(1234)
      tumorgrowth <- sampleData
      tumorgrowth <- do.call(rbind,
                             by(tumorgrowth, tumorgrowth$name,
                                function(subset) within(subset,
                                                        { treatment <- ifelse(rbinom(1,1,0.5), "Drug","Control")   ## subjects are randomly placed in control or drug treatment arms
                                                        o <- order(date)
                                                        date <- date[o]
                                                        size <- size[o]
                                                        baseline <- size[1]
                                                        percentChange <- 100*(size-baseline)/baseline
                                                        time <- ifelse(date > 240, 240, date) ## data censored at 240 days
                                                        })))
      rownames(tumorgrowth) <- NULL
      
      tumorgrowth<- mutate(tumorgrowth, sex=((rep(c("M", "F"), length.out=453)))) %>%
        mutate(tumorgrowth, agegrp=((rep(c("<18",">=18"), length.out=453)))) %>% 
        mutate(tumorgrowth, response=if_else(between(percentChange,-100,-30), 'CR',
                                             if_else(between(percentChange,-29,-1), 'PR',
                                                     if_else(between(percentChange, 0,30), 'SD',
                                                             if_else(percentChange >=31, 'PD','')))))
      
      tumorgrowth1<-head(tumorgrowth,201)
      tumorgrowth1<-tumorgrowth1 %>% mutate(num1=1:nrow(tumorgrowth1))
      
      tumorgrowth2<-tumorgrowth1[order(tumorgrowth1$name, tumorgrowth1$time, -tumorgrowth1$percentChange),] %>%
        mutate(new_value=abs(tumorgrowth1$percentChange)) %>%
        mutate(num=1:nrow(tumorgrowth1))
      
      tg<-tumorgrowth2 %>%
        select(num, name, new_value) %>%
        group_by(name) %>%
        slice(which.max(new_value))
      
      merge1<-merge(tumorgrowth1, tg, all=TRUE, sort = FALSE ) %>%
        mutate(merge1, BOR=if_else(num1==num,"Y",""))
      
      tumorgrowth3<-merge1 %>%
        select(name, sex, agegrp, date, size, time, percentChange, baseline, o,  treatment, response, BOR)%>%
        filter(BOR=='Y') %>%
        arrange(percentChange) %>%
        mutate(name=as.character(name))
      
    } else if(input$default_data==T && input$plot_type=="Spider"){
      set.seed(1234)
      tumorgrowth <- sampleData
      tumorgrowth <- do.call(rbind,
                             by(tumorgrowth, tumorgrowth$name,
                                function(subset) within(subset,
                                                        { treatment <- ifelse(rbinom(1,1,0.5), "Drug","Control")   ## subjects are randomly placed in control or drug treatment arms
                                                        o <- order(date)
                                                        date <- date[o]
                                                        size <- size[o]
                                                        baseline <- size[1]
                                                        percentChange <- 100*(size-baseline)/baseline
                                                        time <- ifelse(date > 240, 240, date) ## data censored at 240 days
                                                        })))
      rownames(tumorgrowth) <- NULL
      
      tumorgrowth<- mutate(tumorgrowth, sex=((rep(c("M", "F"), length.out=453)))) %>%
        mutate(tumorgrowth, agegrp=((rep(c("<18",">=18"), length.out=453)))) %>% 
        mutate(tumorgrowth, response=if_else(between(percentChange,-100,-30), 'CR',
                                             if_else(between(percentChange,-29,-1), 'PR',
                                                     if_else(between(percentChange, 0,30), 'SD',
                                                             if_else(percentChange >=31, 'PD','')))))
      
      tumorgrowth1<-head(tumorgrowth,201)
      tumorgrowth1<-tumorgrowth1 %>% mutate(num1=1:nrow(tumorgrowth1))
      
      tumorgrowth2<-tumorgrowth1[order(tumorgrowth1$name, tumorgrowth1$time, -tumorgrowth1$percentChange),] %>%
        mutate(new_value=abs(tumorgrowth1$percentChange)) %>%
        mutate(num=1:nrow(tumorgrowth1))
      
      tg<-tumorgrowth2 %>%
        select(num, name, new_value) %>%
        group_by(name) %>%
        slice(which.max(new_value))
      
      merge1<-merge(tumorgrowth1, tg, all=TRUE, sort = FALSE ) %>%
        mutate(merge1, BOR=if_else(num1==num,"Y",""))
      
      tumorgrowth3<-merge1 %>%
        select(name, sex, agegrp, date, size, time, percentChange, baseline, o,  treatment, response, BOR)
    }
  })
  
  filter_data <- eventReactive(input$click, {
    filt <- input$filter_Condition
    if (filt==""){
      return(filedata())}
    
    filter_data <- subset(filedata(),eval(parse(text=input$filter_Condition)))
    
    
    
  })
  
  observeEvent(input$click,{
    unique_list<- names(filter_data())
    updateSelectInput(session = session, "y_vars", choices = unique_list)
    updateSelectInput(session = session, "comp_vars", choices = unique_list)
    updateSelectInput(session = session, "x_vars", choices = unique_list)
  })
  
  
  fall_plot <- eventReactive(input$click2,{
    #4
    #Waterfall plot using ggplot
    
    simple_plot <- ggplot(filter_data(),aes(x=reorder(.data[[input$x_vars]], -.data[[input$y_vars]]),y=.data[[input$y_vars]], group=.data[[input$x_vars]], fill=.data[[input$comp_vars]]))+
      labs(x=input$xlab, y=input$ylab)+
      geom_hline(yintercept = 0)+
      geom_hline(yintercept = c(input$pd_var,input$pr_var), lwd=0.1, linetype=2,col='blue')+
      ggtitle(input$tlab)+
      theme_classic() %+replace%
      theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.title.y = element_text(face="bold",angle=90), plot.title = element_text(hjust=0.5)) +
      coord_cartesian(ylim = c(-100,100)) +
      geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))+
      geom_text(aes(label = response), vjust=-0.2, size= 2.5,nudge_y =  T)+
      guides(fill=guide_legend(title = input$comp_vars))+
      scale_fill_brewer(palette = "Set2" )
    
    fall_plot <- ggplotly(simple_plot,tooltip="all")
  })
  
  spider_plot <- eventReactive(input$click2,{
    ## Plot settings
    simple_plot1 <- ggplot(filter_data(),aes(x=.data[[input$x_vars]],y=.data[[input$y_vars]], group=.data[[input$comp_vars]])) +
      theme_bw(base_size=14) +
      theme(axis.title.x = element_text(face="bold"), axis.text.x = element_text(face="bold")) +
      theme(axis.title.y = element_text(face="bold"), axis.text.y = element_text(face="bold")) +
      theme(plot.title = element_text(size=18, hjust=0.5)) +
      labs(x = input$xlab, y = input$ylab)+
      ggtitle(input$tlab)+
      geom_line(aes(color=treatment)) +
      geom_point(aes( color=treatment), show.legend=FALSE) +
      scale_colour_discrete(name="Treatment", labels=c("Control", "Drug")) +
      coord_cartesian(xlim=c(0, 240))
    
    spider_plot <- ggplotly(simple_plot1,tooltip = "all")
  }) 
  
  output$km_curve <- renderPlotly({
    if(input$plot_type == "Waterfall"){
      fall_plot()
    } else if(input$plot_type == "Spider"){
      spider_plot()
    }
  }
  )
  
  
  output$data_content <- renderDataTable(filter_data(),filter="top")
  
  
  
}

shinyApp(ui, server)
