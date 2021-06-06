library(shiny)
library(plotly)
library(dplyr)
library(data.table)  


# datMain <- read.csv("C:/Users/Sam/Desktop/CFEA/Rough/CFEA_in_R/ClinicalAttribute.csv", header = TRUE)
# colnames(datMain)[1] <- 'SampleID'
# #colnames(datMain)[]  <- gsub("(?!^)(?=[[:upper:]])", " ", colnames(datMain), perl=T)
# datMain[,"CDE_DxAge"] <- (cut_interval(datMain[,"CDE_DxAge"], length = 8))
# 
# #class(datMain[,'CDE_DxAge'])
# FactorCols <- NULL
# NumericCols <- NULL
# for(x in colnames(datMain)){
#   if(class(datMain[,x])  == 'factor') {FactorCols <- append(FactorCols,as.character(x))}
#   else if(class(datMain[,x])  == 'integer') {NumericCols <- append(NumericCols,as.character(x))}
# }
#  

ui <- fluidPage(
  class = " main", style = "background-color:  #F2F2F2; padding-bottom:3vh;",
  includeCSS("www/css/style.css"),
  fluidRow(class = "introduction jumbotron",
           style = "display:flex;height:500px;  text-align:center; background-color:  #FDFFFC;
                flex-direction: column; justify-content: center;",
           column(12, 
                  HTML("<h1 style='font-size:45px'>GBM data Analytics</h1>"),
                   div( style = "display: block; float:left;",
                       fileInput("mainData","Upload Dataset",
                                 multiple = FALSE, accept = c('.csv')),
                       checkboxInput("fheader1", label = "File contains header", value = TRUE),
                       actionButton("loadfile", label = "Load Data"),
                       selectInput("primeFact", label = "Primary Factor",
                                   choices = c('Upload Data'),
                                   #choices = FactorCols,
                                   #selected = 'GeneExp_Subtype',
                                   multiple = FALSE))
           )),
  
  fluidRow(class = "mainSections",
           column(12, 
                  titlePanel("GeneExp_Subtype Status with Features"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                                 selectInput("type1", label = "Type",
                                             choices = c('Upload Data'),
                                             #c('Upload Data'),
                                             #selected = 'GeneExp_Subtype',
                                             multiple = FALSE),
                                 selectInput("factor1", label = "Factor",
                                             choices = c('Upload Data'),  
                                               #colnames(datMain),
                                             #selected = 'x_TIME_TO_EVENT' ,
                                             multiple = FALSE)
                    ),
                    mainPanel(width = 9,
                              fluidRow(column(12,plotlyOutput("plot1", height = "65vh")))
                    ))
           )),
  
  
  fluidRow(class = "mainSections",
           HTML("<h2>Compare Data Features By GeneExp_Subtype Status</h2>"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("numType", label = "Numeric Type",
                                      #choices = NumericCols,
                                     # selected = NumericCols[11],
                                     choices = c('Upload Data'),
                                      multiple = FALSE),
                          selectInput("factType", label = "Factor Type",
                                      choices = c('Upload Data'),
                                      #choices = FactorCols,
                                      #selected = 'CDE_DxAge',
                                      multiple = FALSE)
             ),
             mainPanel(width = 9,
                       fluidRow(column(12,plotlyOutput("plot2", height = "65vh")))
             )
           )
  ),
  
  fluidRow(class = "mainSections",
           titlePanel("Correlation using Linear Model Prediction Between Features"),
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("lmcol1", label = "Type",
                                      choices = c('Upload Data'),
                                      #choices = NumericCols,
                                      #selected = NumericCols[11],
                                      multiple = FALSE),
                          selectInput("lmcol2", label = "Factor",
                                      choices = c('Upload Data'),
                                      #choices = NumericCols,
                                     # selected = NumericCols[19],
                                      multiple = FALSE)
             ),
             mainPanel(width = 9,
                       fluidRow(column(12,plotlyOutput("lmplot", height = "85vh"))))
           ))
)


server <- function(input,output,session){
  
  t <- list(family = 'Arial',
            size = 20, color = '#424B54')
  
  NumericCols <- NULL
  FactorCols <- NULL
  
  
  dfmain <- eventReactive(input$loadfile,{
    req(input$mainData)
    datMain <- read.csv(input$mainData$datapath, header = input$fheader1)
    colnames(datMain)[1] <- 'SampleID'
    #colnames(datMain)[]  <- gsub("(?!^)(?=[[:upper:]])", " ", colnames(datMain), perl=T)
    datMain[,"CDE_DxAge"] <- (cut_interval(datMain[,"CDE_DxAge"], length = 8))
    rownames(datMain) <- datMain[,1]
#    dat <- datMain
    return(datMain)
  })

  colChoiceAttrName <- eventReactive(input$loadfile, {
    dat = dfmain()
    colChoices <- as.vector(colnames(dat))
  })
  
  colChoiceFactorName <- eventReactive(input$loadfile, {
    datMain = dfmain()
    for(x in colnames(datMain)){
      if(class(datMain[,x])  == 'factor') {FactorCols <- append(FactorCols,as.character(x))}
    }
    return(FactorCols)
  })
  colChoiceNumericName <- eventReactive(input$loadfile, {
    datMain = dfmain()
    for(x in colnames(datMain)){
      if(class(datMain[,x])  == 'integer') {NumericCols <- append(NumericCols,as.character(x))}
    }
    return(NumericCols)
  })
  
  observe({
    updateSelectInput(
      session,
      "primeFact",
      choices = colChoiceFactorName()
    )
   })  
  observe({
    updateSelectInput(
      session,
      "type1",
      choices = colChoiceFactorName()
    )})
  observe({
    updateSelectInput(
      session,
      "factor1",
      choices = colChoiceNumericName()
    )
  })
  observe({
    updateSelectInput(
      session,
      "numType",
      choices = colChoiceNumericName()
    )
})
  observe({
    updateSelectInput(
      session,
      "factType",
      choices = colChoiceFactorName()
    )
    })
  observe({
    updateSelectInput(
      session,
      "lmcol1",
      choices = colChoiceNumericName()
    )
    })
  observe({
    
    updateSelectInput(
      session,
      "lmcol2",
      choices = colChoiceNumericName()
    )
  })
  
  output$plot1 <- renderPlotly({
    req(input$loadfile)
    datMain =  dfmain()
    FactorCols  <- colChoiceFactorName()
    NumericCols <- colChoiceNumericName()
    a <- input$type1
    b <- input$factor1
    dat <- datMain
    if(b %in% NumericCols){
      print("Entered plot1 opt1")
      fig1 <- plot_ly() %>% 
        add_trace(data = dat,
                  line  = list(color = 'rgb(8,48,107)',outliercolor = 'rgb(8,48,107)', 
                               outlierwidth = 2, width = 2),
                  y = dat[,b],
                  color = dat[,a],
                  colors =  c('#AA9ABA','#E3B9BC','#008DD5','#685F74'),
                  type = "box") %>% 
        layout(
          xaxis = list(title = paste(a," status.", sep = "")),
          yaxis = list(title = b),
          paper_bgcolor = '#F2F2F2',
          plot_bgcolor = '#F2F2F2',
          font = t)
      
      return(fig1)
    }
    else if(b %in% FactorCols)
    {

      data.table::melt(dat, id.vars=a) %>%
        plot_ly(x = a, y = ~value, type = 'bar', 
                name = ~b, color = ~b) %>%
        layout(yaxis = list(title = 'Count'), barmode = 'stack')
      
      # print("Entered in bar plot")
      # freqT <- dat %>% count(a, b = dat[,b])
      # y1 <- freqT$n[freqT[,a] == "Classical"]
      # y2 <- freqT$n[freqT[,a] == "Mesenchymal"]
      # y3 <- freqT$n[freqT[,a] == "Neural"]
      # y4 <- freqT$n[freqT[,a] == "Proneural"]
      # print("Showing freq")
      # print(freqT)
      # fig2 <- plot_ly(x = levels(freqT$b), y = y1, type = 'bar', name = 'Classical',
      #                 #text = y1, textposition = 'auto',
      #                 marker = list(
      #                   color = '#AA9ABA',
      #                   line = list(color = 'rgb(8,48,107)', width = 2)))
      # fig2 <- fig2 %>% add_trace(y = y2, name = 'Mesenchymal',
      #                            #text = y2, textposition = 'auto',
      #                            marker = list(
      #                              color = '#E3B9BC',
      #                              line = list(color = 'rgb(8,48,107)', width = 2)))
      # fig2 <- fig2 %>% add_trace(y = y3, name = 'Neural',
      #                            #text = y3, textposition = 'auto',
      #                            marker = list(
      #                              color = '#008DD5',
      #                              line = list(color = 'rgb(8,48,107)', width = 2)))
      # fig2 <- fig2 %>% add_trace(y = y4, name = 'Proneural',
      #                            #text = y4, textposition = 'auto',
      #                            marker = list(
      #                              color = '#685F74',
      #                              line = list(color = 'rgb(8,48,107)', width = 2)))
      # fig2 <- fig2 %>% layout(
      #   xaxis = list(title = b),
      #   yaxis = list(title = 'Number'), 
      #   paper_bgcolor = '#F2F2F2',
      #   plot_bgcolor = '#F2F2F2',
      #   font = t,
      #   barmode = 'group')
      return(fig2)
    }
  })
  
  
  output$plot2  <- renderPlotly({
    req(input$loadfile, input$primeFact)
    datMain =  dfmain()
    colr <- input$primeFact
    a <- input$factType
    b <- input$numType
    
    fig1 <- plot_ly(datMain, x = datMain[,a], y = datMain[,b], 
                    color = datMain[,colr], type = "box",
                    line  = list(color = 'rgb(8,48,107)',outliercolor = 'rgb(8,48,107)', outlierwidth = 2, width = 2))
    fig1 <- fig1 %>% layout(boxmode = "group",
                            xaxis = list(title = a),
                            yaxis = list(title = b),
                            paper_bgcolor = '#F2F2F2',
                            plot_bgcolor = '#F2F2F2',
                            font = t)
  })
  
  output$lmplot <- renderPlotly({
    req(input$loadfile, input$primeFact)
    datMain =  dfmain()
    colr <- input$primeFact
    a <- input$lmcol1
    b <- input$lmcol2
    # Showing a linear Model Prediction and Correlation
    fit <- lm(datMain[,b] ~ datMain[,a] , data = datMain)
    fig <- plot_ly(datMain, x = datMain[,a])
    fig <- fig %>% add_markers(y = datMain[,b], color = datMain[,colr], 
                               colors = c('#E08D79','#FFE1A8','#008DD5','#685F74'),
                               marker = list(size = 9,
                                             line = list(color = '#472D30',
                                                         width = 1)))
    fig <- fig %>% add_lines(x = datMain[,a], y = fitted(fit),
                             line = list(color = '#1E555C',
                                         width = 3.2),
                             name = "Linear Model Predcition")
    fig <- fig %>% layout(
      xaxis = list(title = a),
      yaxis = list(title = b),
      paper_bgcolor = '#F2F2F2',
      plot_bgcolor = '#F2F2F2',
      font = t
      
    )})
}

shinyApp(ui = ui, server = server)