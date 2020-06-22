#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)

library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(treemap)

library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)

# Set working directory
if(rstudioapi::isAvailable()){
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# Run the script to generate the data
source("Load_prepare_data.R")


# --------------------------------------------------  USER INTERFACE  ----------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "Madrid during COVID-19",
    theme = shinytheme("united"),
    # theme = "bootstrap.css",

    # # Application title
    # h1(id="big-heading", "Shiny App Test"),
    # tags$style(HTML("#big-heading{text-align:center}")),
    
    ## Título
    titlePanel(div(strong("Doesn't money ", em("really"), "buy happiness... even during COVID-19?"), 
                   style = "margin: auto; width: 90%; text-align:justify; color:black; 
                   padding:15px; font-size:150%")
               ),
    
    hr(style = "width: 97%"),
    
    ## Introducción
    fluidRow(
        style="margin: auto; width: 90%; text-align:justify; color:black; background-color:papayawhip; padding:15px;
            border-radius:10px; font-size:130%",
        
        p("Spain before COVID-19 was one of the EU members with higher risk of poverty and social exclusion.
              Half of its population could not afford contingencies in their invoices. 
              According to the", em("Centro de Investigaciones Sociológicas (CIS)"), ", economy, politics and corruption were 
              the main concerns of the Spanish people. However, the serious impact of the pandemic and the lockdown 
              period ordered by the central government has provoked this fears to be modified and, in some cases, deepened."),
            
        p("Thanks to a survey conducted by the Community of Madrid and by means of a series of graphs, interactive 
        models and the interrelationship of all of them, it is intended to visualize and conclude how the COVID-19
        has affected the population in terms of psychological health and personal concerns based on some previous 
          circumstances, like mean wages or house characteristics.")
        ),
    
    ## Explicación de variables
    fluidRow(
        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
        
        h1("What has been taken into account?"),
        
        br(),
        
        p("The parameters studied, analyzed and subsequently shown are divided in four main blocks:"),
        
        p(strong("Economical."), "Average income per district as reference of the prior
          distributions per district.",
          style = "margin-left:60px"),
        
        p(strong("House features."), "We created a habitability feature, that was computed 
          taking into account different characteristics such as if the house has outdoors spaces, if it has access to 
          exterior light or just interior backyards, whether it has different services such as internet connection, 
          computer, heating and AC, etc.",
          style = "margin-left:60px"),
        
        p(strong("Employment impact."), "The data collected by means of the question: 
          ”Have you experience any change in your work? and if you have what is it?” has been group in five main groups 
          from minimal or none impact to maximal impact, in this case, to be included in a Downsizing Plan 
          ('Expediente de Regulación de Empleo, ERE') or fired.",
          style = "margin-left:60px"),
        
            p(em("GROUP 0: "), "Unchanged",
              style = "margin-left:120px"),
            p(em("GROUP 1: "), "Small changes, but same salary (e.g. teleworking)",
              style = "margin-left:120px"),
            p(em("GROUP 2: "), "Small changes and salary reduction",
              style = "margin-left:120px"),
            p(em("GROUP 3: "), "ERTE",
              style = "margin-left:120px"),
            p(em("GROUP 4: "), "ERE",
              style = "margin-left:120px"),
        
        p(strong("Mental health impact."), "We created a score based on the respondents’ answers to questions related 
          to feeling lonely, useless, having troubles to sleep or focus in daily tasks, etc. during the lockdown.",
          style = "margin-left:60px"),
        
        br(),
        
        h1("What do we expect to see?"),
        
        br(),
        
        p("As preliminary result of this study and also based on the news and info communicated by the Social Security,
          we expected a clear peak of ERTEs and teleworking related measures along the working population joined with 
          a general disaffection of the population.")
        
    ),
    
    br(),
    
    # LEAFLET MAP
    fluidRow(
        style = "margin: auto; width: 85%",
        
        absolutePanel(
            draggable = T, 
            style="z-index:500; margin-left:30px; margin-top:200px; font-size:130%",
            
            selectInput("variable", 
                        label = "Choose a variable to display",
                        choices = list("Vital space" = "cohab.density",
                                       "Habitability" = "habitability.score",
                                       "Mental health" = "mental.health.score",
                                       "Concern" = "concern",
                                       "Occupation impact" = "occ.change.freq",
                                       "Occupation impact: Teleworking" = "occ.impact.group1",
                                       "Occupation impact: Minor changes" = "occ.impact.group2",
                                       "Occupation impact: ERTE" = "occ.impact.group3",
                                       "Occupation impact: ERE" = "occ.impact.group4",
                                       "Average income" = "avg.income"),
                        selected = "avg.income"),
            
            helpText(verbatimTextOutput("variable.explanation")),
            
            checkboxInput("legend", "Show legend", TRUE),
            
            actionButton("reset", "Reset zoom")
        ),
        
        leafletOutput("map", height=700),
        
    ),
    
    br(),
    
    # Intermidiate text
    fluidRow(
        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
        
        h1("What does the map show us?"),
        
        br(),
        
        p("Attending to the results, it is possible to observe that our initial hypothesis regarding the destruction 
          of jobs has been refused not only considering the global results of the city of Madrid, but, by its 
          different districts. This does not mean at all that the impact of the pandemic on the economic and labor 
          markets has not been considerable, but taking into account this level of aggregation (by districts)", strong("it cannot
          be stated whether its most negative aspect (in the form of ERTEs and EREs) has been greater in one zone or 
          another. "), "However, it would be of great interest to carry out this same study at a lower level of aggregation,
          drilling down to the neighborhood level for example, as we would probably observe a more noticeable impact 
          by zones."),
        
        p("Moreover, ", strong("it cannot be assumed a clear correlation between average income and the impact on employment"), 
        ", although it is possible that there has been a starker change in the most impoverished areas."),
        
        p("Besides, since it has been compulsory to stay at home for almost three months, the time and space share with 
          the same people could have an important effect on psychological condition. This potential worsening of mental 
          health and emotional state we thought a priori could be strongly influenced by housing related conditions, 
          translated into the space each person could enjoy during confinement as well as the quality of the space itself.
          As it is shown in the different maps, the conditions of both", em("habitability"), " and", em("vital space"), 
          " variables are better in the northern area (as it was expected, richer areas with more spacious dwellings than in the center), but 
          this is not translated into a clear correlation with the mental health, showing the best score in Vicálvaro, 
          a district in the southeast which at the same time it is one of the worst performers in the other variables.")
        
    ),
    
    # Panels with Graphs and Data    
    div(
        tabsetPanel(
            
            tabPanel(div("Graphs", style="color: black"),
                     br(),
                     fluidRow(
                         
                         column( width = 2, offset = 1,
                                 
                                 fixedRow(
                                     sliderInput("slider1", label = h5("Age range selection:"), min = 0, max = 100, value = c(20, 60)),
                                     br()
                                 ),
                                 
                                 fixedRow(
                                     selectInput('graph1', 'Type of graph 1:', 
                                                 c("Barplot" = "bar", "Treemap" = "tree", "Pie" = "pie"),
                                                 selected = "bar")
                                 ),
                                 
                                 fixedRow(
                                     selectInput('graph2', 'Type of graph 2:',
                                                 c("Barplot" = "bar", "Treemap" = "tree", "Pie" = "pie"),
                                                 selected = "tree")
                                 ),
                                 
                                 fixedRow(
                                     selectInput('graph3', 'Type of graph 3:',
                                                 c("Barplot" = "bar", "Treemap" = "tree", "Pie" = "pie"),
                                                 selected = "pie")
                                 ),
                                 
                                 
                                 fixedRow(
                                     
                                     br(),br(),br(), br(),br(),
                                     
                                     radioButtons("var1", "Choose one variable:",choiceNames = list("Employment impact", "Health impact","Concern impact"),
                                                  choiceValues = list("imp1", "imp2", "imp3"))
                                 )),
                         
                         column(8,
                                
                                fluidRow(
                                    column(4, plotOutput('grafico1')),
                                    column(4, plotOutput('grafico2')),
                                    column(4, plotOutput('grafico3'))
                                ),
                                
                                br(), br(),
                                
                                fluidRow(
                                    plotOutput('grafico4')
                                )
                         ))
            ),
            
            tabPanel(div("Data", style="color: black"),
                     br(),
                     fluidRow(style = "margin: auto; width: 80%",
                              # Create a new row for the table.
                              DT::dataTableOutput("table")
                     ))), 
        
        style = "margin: auto; width: 90%; font-size: 120%"
    ),
    
    
    # Final conclusions
    fluidRow(
        style = "margin: auto; width: 90%; text-align:justify; color:black; padding:15px; font-size:130%",
        
        h1("And then what?"),
        
        br(),
        
        p("Merging all the results gathered in the previous sections, ", strong("we might be inclined to  conclude that the COVID-19
          crisis and the lockdown seem to have affected in a lesser way than it was expected "), "(and continuously conveyed
          through social media and the media outlets), not only in the economical field, but also regarding mental health
          and emotional stability. Allowing us to infer that the economy of city of Madrid has not been strongly affected,
          resisting in a positive manner the consequences of the coronavirus economic crisis,", strong("and also that the lockdown 
          measures have not take such a remarkable toll on the mental health of the citizens of Madrid "), "no matter the diverse
          characteristics of the place where they have stayed during the pandemic."),
        
        p("Anyway, this a priori positive upside should not mislead us, as we should emphasize that, again, ", strong("we are 
          taking aggregated values in a very coarse-grain fashion "), "(just take a moment to consider", em("what would happen 
          if the impact in the majority of Madrid main metropolitan area were negative… and in such a short period of time… we would 
          be really facing a huge crisis indeed) "), strong("and a deeper study at neighborhood or even street level could add more 
          relevant information to the topic"), "), as currently data by districts could be biasing and smoothing the negative impact.")
        
    ),
    
    
    # Panel de abajo informacion y nombres   
    
    hr(),
    
    fluidRow(   
        
        column(
            
            br(),
            
            p("For more information about the survey please check Portal de datos abiertos del Ayuntamiento de Madrid page clicking    ",
              a(href="https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=79c6ec6846c22710VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default", "Here",target="_blank"),
              style="vertical-align:middle;text-align:justify;color:black"),
            
            width=6, offset = 1, align="center"),
    
        column(
            
            img(src="imagen.png",width="100px",height="100px"),
            
            width=2, offset = 2)),
    

    p(em("Developed by Gonzalo Rodriguez and Laura Gutierrez"),style="text-align:center; font-family: times")
)



# --------------------------------------------------  SERVER  ----------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # val_max<-input$slider1[2]
    # val_min<-input$slider1[1]
    
    #output$range <- renderPrint({ input$slider1[2] })
    
    # 
    # impact <- dat_sel[dat_sel$occ.change!=0,]
    # impact<- count(dat_sel, occ.impact, name = 'Count')
    # impact$occ.impact <- paste('GROUP', impact$occ.impact, sep = ' - ')
    # 
    # 
    output$grafico1 <- renderPlot({
        
        dat_sel <- dat[which(dat$age >= input$slider1[1] & dat$age <= input$slider1[2]), c(12,11,4,7,8)]
        impact <- dat_sel[dat_sel$occ.change!=0,]
        impact<- count(dat_sel, occ.impact, name = 'Count')
        impact$occ.impact <- paste('GROUP', impact$occ.impact, sep = ' - ')
        
        if(input$graph1=="bar"){
            
            ggplot(impact, aes(fill=as.factor(occ.impact), y=Count, x=as.factor(occ.impact))) + 
                geom_bar(position="dodge", stat="identity")+
                ggtitle("Employment impact")+xlab("") + ylab("")+
                theme(plot.title = element_text(hjust = 0.5,size = 18), legend.position='bottom', legend.title = element_blank(), axis.text.x = element_blank())
                
            
        }
        
        else if(input$graph1=="tree"){
            treemap(impact, index=c("occ.impact"), vSize="Count", type="index",
                    align.labels=list(c("center", "center")), title="Employment impact", overlap.labels=1)
        }
        
        else if(input$graph1=="pie"){
            impact$fraction = impact$Count / sum(impact$Count)
            impact$ymax = cumsum(impact$fraction)
            impact$ymin = c(0, head(impact$ymax, n=-1))
            ggplot(impact, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(occ.impact))) +
                geom_rect() +
                coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
                xlim(c(2, 4)) +  # Try to remove that to see how to make a pie chart
                theme_void()+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+ggtitle("Employment impact")
        }


    })
    
    output$grafico2 <- renderPlot({
        
        dat_sel <- dat[which(dat$age >= input$slider1[1] & dat$age <= input$slider1[2]), c(12,11,4,7,8)]
        health<- count(dat_sel, mental.health, name = 'Count')
        
        if(input$graph2=="bar"){
            
            ggplot(health, aes(fill=as.factor(mental.health), y=Count, x=as.factor(mental.health))) + 
                geom_bar(position="dodge", stat="identity")+
                ggtitle("Health impact")+xlab("") + ylab("")+
                theme(plot.title = element_text(hjust = 0.5,size = 18), legend.position='bottom', legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
            
        }
        
        else if(input$graph2=="tree"){
            treemap(health, index=c("mental.health"), vSize="Count", type="index",
                    align.labels=list(c("center", "center")), title="Health impact", overlap.labels=1)
        }
        
        else if(input$graph2=="pie"){
            health$fraction = health$Count / sum(health$Count)
            health$ymax = cumsum(health$fraction)
            health$ymin = c(0, head(health$ymax, n=-1))
            ggplot(health, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(mental.health))) +
                geom_rect() +
                coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
                xlim(c(2, 4)) +  # Try to remove that to see how to make a pie chart
                theme_void()+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+
                ggtitle("Health impact")
        }
        
        
    })
    
    
    output$grafico3 <- renderPlot({
        
        dat_sel <- dat[which(dat$age >= input$slider1[1] & dat$age <= input$slider1[2]), c(12,11,4,7,8)]
        preocupa <- count(dat_sel, concern, name = 'Count')
        
        if(input$graph3=="bar"){
            
            ggplot(preocupa, aes(fill=as.factor(concern), y=Count, x=as.factor(concern))) + 
                geom_bar(position="dodge", stat="identity")+
                ggtitle("Concern impact")+xlab("") + ylab("")+
                theme(plot.title = element_text(hjust = 0.5,size = 18), legend.position='bottom', legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
            
        }
        
        else if(input$graph3=="tree"){
            treemap(preocupa, index=c("concern"), vSize="Count", type="index",
                    align.labels=list(c("center", "center")), title="Concern impact", overlap.labels=1)
        }
        
        else if(input$graph3=="pie"){
            preocupa$fraction = preocupa$Count / sum(preocupa$Count)
            preocupa$ymax = cumsum(preocupa$fraction)
            preocupa$ymin = c(0, head(preocupa$ymax, n=-1))
            ggplot(preocupa, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=as.factor(concern))) +
                geom_rect() +
                coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
                xlim(c(2, 4)) +  # Try to remove that to see how to make a pie chart
                theme_void()+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+
                ggtitle("Concern impact")
        }
        
        
    })
    
    
    
    output$grafico4 <- renderPlot({
        
        dat_sel <- dat[which(dat$age >= input$slider1[1] & dat$age <= input$slider1[2]), c(12,11,4,7,8,9)]
        
        if(input$var1=="imp1"){
            
            histo_sel<- count(dat_sel, occ.impact, age, name = 'Count')
            ggplot(histo_sel, aes(fill=as.factor(occ.impact), y=Count, x=age)) + 
                geom_bar(position="stack", stat="identity")+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+
                ggtitle("Employment impact")
        }
        
        else if(input$var1=="imp2"){
            histo_sel<- count(dat_sel, mental.health, age, name = 'Count')
            ggplot(histo_sel, aes(fill=as.factor(mental.health), y=Count, x=age)) + 
                geom_bar(position="stack", stat="identity")+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+
                ggtitle("Health impact")
        }
        
        else if(input$var1=="imp3"){
            histo_sel<- count(dat_sel, concern, age, name = 'Count')
            ggplot(histo_sel, aes(fill=as.factor(concern), y=Count, x=age)) + 
                geom_bar(position="stack", stat="identity")+theme(plot.title = element_text(hjust = 0.5,size = 18),legend.title = element_blank(), legend.position = "bottom")+
                ggtitle("Concern impact")
        }
        
        
    })
    
    
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- dat[which(dat$age >= input$slider1[1] & dat$age <= input$slider1[2]), ]
        data
    }))
    
    
    ##################### CODE LEAFLET MAP ############################
    
    # Renders help text in variable selection
    output$variable.explanation <- renderText({
        ifelse(input$variable == "cohab.density",
               paste("The average space a person enjoys within", 
                     "his home. Calculated as number of cohabitants",
                     "divided by square meters.",
                     sep="\n"),
               ifelse(input$variable == "habitability.score", 
                      paste("The habitability score is computed taking",
                            "into account different factors, such as if",
                            "the house is exterior, has outdoors spaces,",
                            "dedicated rooms, etc.",
                            sep="\n"),
                      ifelse(input$variable == "mental.health.score", 
                             paste("The mental health score is computed taking",
                                   "into account different animic factors, such",
                                   "as if the person has felt lonely, depressed,",
                                   "useless, etc.",
                                   sep="\n"),
                             ifelse(input$variable == "concern",
                                    paste("The main concern expressed by the people",
                                          "within the district. They are grouped in",
                                          "three big groups.",
                                          sep="\n"),
                                    ifelse(input$variable == "occ.change.freq", 
                                           paste("The pandemic impact upon occupation",
                                                 "by districts. Percentage of employed",
                                                 "people affected.",
                                                 sep="\n"),
                                           ifelse(input$variable == "occ.impact.group1", 
                                                  paste("The percentage of employed people",
                                                        "affected by teleworking",
                                                        sep="\n"),
                                                  ifelse(input$variable == "occ.impact.group2", 
                                                         paste("The percentage of employed people",
                                                               "affected by minor changes",
                                                               "(e.g. schedules, shifts...)",
                                                               sep="\n"),
                                                         ifelse(input$variable == "occ.impact.group3", 
                                                                paste("The percentage of employed people",
                                                                      "affected by ERTE",
                                                                      sep="\n"),
                                                                ifelse(input$variable == "occ.impact.group4", 
                                                                       paste("The percentage of employed people",
                                                                             "affected by ERE",
                                                                             sep="\n"),
                                                                       paste("The median houseld income for the district.",
                                                                             sep="\n")
                                                                       )))))))))
                                    })
    
    # Reactive variable for color palette depending on variable 
    palette <- reactive({
        if(input$variable == "cohab.density")
            colorNumeric(palette = "Greens", domain = my_spdf@data[["cohab.density"]])
        else{
            if(input$variable == "habitability.score")
                colorNumeric(palette = "Greens", domain = my_spdf@data[["habitability.score"]])
            else{
                if(input$variable == "mental.health.score")
                    colorNumeric(palette = "Greens", domain = my_spdf@data[["mental.health.score"]])
                else{
                    if(input$variable == "concern")
                        colorFactor(palette = "Set2", domain = 1:4)
                    else{
                        if(input$variable == "occ.change.freq")
                            colorNumeric(palette = "Blues", domain = seq(0.5, 1, 0.01))
                        else{
                            if(input$variable == "occ.impact.group1")
                                colorNumeric(palette = "Purples", domain = seq(0, 1, 0.01))
                            else{
                                if(input$variable == "occ.impact.group2")
                                    colorNumeric(palette = "YlOrRd", domain = seq(0, 0.4, 0.01))
                                else{
                                    if(input$variable == "occ.impact.group3")
                                        colorNumeric(palette = "YlOrRd", domain = seq(0, 0.4, 0.01))
                                    else{
                                        if(input$variable == "occ.impact.group4")
                                            colorNumeric(palette = "YlOrRd", domain = seq(0, 0.4, 0.01))
                                        else
                                            colorNumeric(palette = "Greens", domain =  my_spdf@data[["avg.income"]])
                                    }}}}}}}}
    })
    
    # Reactive variable for labelling variable selected
    var.label <- reactive({
        ifelse(input$variable == "cohab.density", "Vital space (m2/p)",
               ifelse(input$variable == "habitability.score", "Habitability",
                      ifelse(input$variable == "mental.health.score", "Mental health",
                             ifelse(input$variable == "concern", "Concern",
                                    ifelse(input$variable == "occ.change.freq", "Occupation impact (%)",
                                           ifelse(input$variable == "occ.impact.group1", "Occupation impact: Teleworking (%)",
                                                  ifelse(input$variable == "occ.impact.group2", "Occupation impact: Minor changes (%)",
                                                         ifelse(input$variable == "occ.impact.group3", "Occupation impact: ERTE (%)",
                                                                ifelse(input$variable == "occ.impact.group4", "Occupation impact: ERE (%)",
                                                                       "Average income (€/yr)")))))))))
    })
    
    
    # legend.values <- reactive({
    #     
    # })
    
    # Reactive variable for displaying district info while hovering cursor
    text <- reactive({
        if( is.numeric(my_spdf@data[[input$variable]]) ){
            paste(
                "District: ", my_spdf@data$NOMBRE, "<br/>",
                "Average age: ", my_spdf@data$age, "<br/>",
                var.label(), ": ", round(my_spdf@data[[input$variable]], 2),
                sep="") %>%
                lapply(htmltools::HTML)
        }
        
        else{
            paste(
                "District: ", my_spdf@data$NOMBRE, "<br/>",
                "Avg. age: ", my_spdf@data$age,
                sep="") %>%
                lapply(htmltools::HTML)
        }
    })
    
    # Rendering fixed map
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        
        leaflet(my_spdf) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lat = 40.47, lng = -3.703790, zoom = 11)
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        
        # palette <- colorNumeric(palette="Greens", domain=my_spdf@data[[input$variable]])
        # 
        # var.label <- ifelse(input$variable == "cohab.density", "Vital space (m2/p)",
        #                     ifelse(input$variable == "house.type", "House type",
        #                            ifelse(input$variable == "mental.health", "Mental health",
        #                                   ifelse(input$variable == "concern", "Concern",
        #                                          "Average income (â¬/yr)"))))
        # 
        # text <-
        #     paste(
        #         "District: ", my_spdf@data$NOMBRE,"<br/>",
        #         var.label, ": ", round(my_spdf@data[[input$variable]], 2),
        #         sep="") %>%
        #     lapply(htmltools::HTML)
        
        leafletProxy("map", data = my_spdf) %>%
            clearShapes() %>%
            clearControls() %>%
            # setView(lat = 40.45, lng = -3.703790, zoom = 10) %>%
            addPolygons(
                fillColor = ~palette()(my_spdf@data[[input$variable]]),
                stroke = T,
                fillOpacity = 0.6,
                color="black",
                weight=1.,
                label = text(),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"))
        # addLegend(pal=palette, values=~my_spdf@data[[input$variable]], opacity=0.9,
        #               title = var.label, position = "topright" )
    })
    
    # Use a separate observer to recreate the legend as needed.
    observe({
        proxy <- leafletProxy("map", data = my_spdf)
        
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        proxy %>% clearControls()
        # if (input$legend) {
        #     proxy %>% 
        #         addLegend(pal=palette(), values=~my_spdf@data[[input$variable]], 
        #                   opacity=0.9, title = var.label(), position = "topright" )
        # }
        
        if (input$legend) {
            if(input$variable == "concern"){
                proxy %>%
                    addLegend(colors=palette()(1:4), labels=c("Economic","Health","Politics","Others"),
                              opacity=0.9, title = var.label(), position = "topright" )
            }
            else{
                if(input$variable == "occ.impact"){
                    proxy %>%
                        addLegend(colors=palette()(0:4), labels=c("Unchanged","Teleworking","Minor changes","ERTE","ERE"),
                                  opacity=0.9, title = var.label(), position = "topright" )
                }
                else{
                    proxy %>% 
                        addLegend(pal=palette(), values=~my_spdf@data[[input$variable]],
                                  opacity=0.9, title = var.label(), position = "topright" )
                }}}
    })
    
    # Observer to RESET ZOOM using ActionButton
    observeEvent(
        input$reset,
        {
            leafletProxy("map", data = my_spdf) %>%
                setView(lat = 40.47, lng = -3.703790, zoom = 11)
        })
    
}


# --------------------------------------------------  APPLICATION  ----------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
