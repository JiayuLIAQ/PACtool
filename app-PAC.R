# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(markdown)

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Portable air cleaner (PAC) calculator",
                           
                           tabPanel("Home",
         
fluidRow(column(4,  #style = "background-color:#999999;",
                     # Input values
                     # fluidRow(column(6, 
         # selectInput(inputId = "input_type", 
         #             label = h4("Computing task"),
         #             choices = c('Efficacy', 'Needed PAC numbers')) ,
         h4("Select task"),
         
         radioButtons("input_type", NULL,
                                       choices = c('Estimate the effectiveness' = 'Efficacy',
                                                   'Determine PAC numbers' = 'Needed PAC numbers'),
                                       selected = 'Efficacy'
         ),
         
         h4("Input parameters"),

         selectInput("k", label = "Particle type", 
                          choices = list("UFP" = 1.64, 
                                         "PM2.5" = 1.67, 
                                         "PM10" = 3.35), 
                          selected = "PM2.5"),
         
         uiOutput("ui1"),
         uiOutput("CADR_lim"),
  

         radioButtons("volume_type", label = HTML("Room volume (m<sup>3</sup>)"),
                      choices = c('Select typical room type' = 'room_type',
                                  'Manually type in' = 'type_in'),
                      selected = 'room_type'
         ),
         
         uiOutput("ui2"),
         
         radioButtons("venti_selection", label = p("Ventilation rate (", 
                                                    a("air changes per hour", href="https://en.wikipedia.org/wiki/Air_changes_per_hour", target = "_blank"),
                                                    ", ACH)"), 
                      choices = c('Select typical ventilation rate' = 'venti_type',
                                  'Manually type in' = 'venti_type_in'),
                      selected = 'venti_type'),
         uiOutput("ui3")
         ),
         

         
          # column(8, h5("Figure 1"), plotOutput('plot1'), verbatimTextOutput("txtout")
          column(8, plotOutput('plot1'), h4(textOutput("txtout") )
                 
                 )
         ),
), #tabPanel(), Home
  
  tabPanel("About", 
           # titlePanel("About"), 
           div(includeMarkdown("about-app-PAC.md"),
               align="justify")
  ) #tabPanel(), About
  
                ) # navbarPage()
) # fluidPage()

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  fun_effectiveness <- function(a,k,CADR,V,N) 1-(a+k)/(a+k+N*CADR/V)
  
  fun_num_PAC <- function(a,k,CADR,V,eff) (eff/(100-eff) ) * (a+k) * V/CADR
  
  global_pac_number <- reactiveValues(numVal = 5, numMin = 0, numMax = 50) 
  numVal_pac_number <- reactive({
    if(!is.null(input$N)){
      if(input$N < global_pac_number$numMin) return(global_pac_number$numMin)
      if(input$N > global_pac_number$numMax) return(global_pac_number$numMax)     
      return(input$N)
    }else{
      return(global_pac_number$numVal)
    }
  })
  
  
  
  
  output$ui1 <- renderUI({
    if (is.null(input$input_type))
      return()

    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Efficacy" =  numericInput("N", 
                                            label = "Number of PACs deployed", 
                                      value = numVal_pac_number(),
                                      min = global_pac_number$numMin,
                                      max = global_pac_number$numMax),
           "Needed PAC numbers" = numericInput("eff", 
                                                    label = "Effectiveness target (%)", 
                                                    value = 50)
    )
  })
  output$ui2 <- renderUI({
    if (is.null(input$volume_type))
      return()

    switch(input$volume_type,
           "room_type" =  selectInput("V", label = NULL,
                         choices = list("Small room, 30 m\U00B3" = 30,
                                        "Meeting room, 80 m\U00B3" = 80,
                                        "Large office, 1490 m\U00B3" = 1490),
                         selected = 1490),
           "type_in" =    numericInput("V",
                                             label = NULL,
                                             value = 1000,
                                             min = 0)
    )
  })
  

  output$ui3 <- renderUI({
    if (is.null(input$venti_selection))
      return()
    
    switch(input$venti_selection,
           "venti_type" =  selectInput("a", label = NULL,
                                      choices = list("Mechenical ventilation, 1.2 ACH" = 1.2,
                                                     "Infiltration, 0.16 ACH" = 0.16),
                                      selected = 1.2),
           "venti_type_in" =  numericInput("a", 
                        # label = "Ventilation rate (air changes per hour)", 
                        label = NULL, 
                        value = 1.2)

    )
  })
  
  global_cadr <- reactiveValues(numVal = 444, numMin = 1, numMax = 1000)

  
  numVal_cadr <- reactive({
    if(!is.null(input$CADR)){
      if(input$CADR < global_cadr$numMin) return(global_cadr$numMin)
      if(input$CADR > global_cadr$numMax) return(global_cadr$numMax)     
      return(input$CADR)
    }else{
      return(global_cadr$numVal)
    }
  })
  
  output$CADR_lim <- renderUI(  numericInput("CADR", 
                                             label = p(a("Clean air delivery rate", href="https://en.wikipedia.org/wiki/Clean_air_delivery_rate", target = "_blank"),
                                                       HTML("of one portable air cleaner (m<sup>3</sup>/h)") ), 
                                             value = numVal_cadr(),
                                             min = global_cadr$numMin,
                                             max = global_cadr$numMax)
                                )

  # calculation---------------------
  datasetInput <- reactive({  
    
    if(input$input_type == "Efficacy") {
    
    Output <- fun_effectiveness( as.numeric(input$a),
                                 as.numeric(input$k),
                                 as.numeric(input$CADR),
                                 as.numeric(input$V),
                                 as.numeric(input$N)
                                 )
    
    # print(data.table(`Effectiveness (%)` = as.character( round(Output * 100, digits = 1) ) ) )
    paste0("Effectiveness: ", as.character( round(Output*100, digits = 1) ), "%" ) 
    } else {
 
      Output <- fun_num_PAC(as.numeric(input$a),
                            as.numeric(input$k),
                            as.numeric(input$CADR),
                            as.numeric(input$V),
                            as.numeric(input$eff))
      
      # print(data.table(`PAC numbers` = as.character( round(Output, digits = 0) ) ) )
      paste("Need", as.character( round(Output, digits = 0) ),"portable air cleaners" ) 
    }
  })
 
  # output$plot2<-renderPlot({
  #   ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)

output$plot1<-renderPlot({

  deposition_df <- data.table(
    k = c(1.64,1.67,3.35),
    parti_type = c("UFP","PM2.5","PM10")
  )
  df <- data.table(expand.grid(N = c(1:50),  k = c(1.64, 1.67, 3.35)))[deposition_df, on = "k"]
  
  df[, eff := fun_effectiveness(as.numeric(input$a), 
                                k,
                                as.numeric(input$CADR),
                                as.numeric(input$V),
                                N)]
  
  if(input$input_type == "Efficacy") {
    df1 <- data.table(N = as.numeric(input$N),
                      
                      eff = fun_effectiveness(as.numeric(input$a), 
                                              as.numeric(input$k),
                                              as.numeric(input$CADR),
                                              as.numeric(input$V),
                                              as.numeric(input$N))* 100)
    
    df2 <- data.table(expand.grid(N = c(as.numeric(input$N),0),
                      
                      eff = fun_effectiveness(as.numeric(input$a), 
                                              as.numeric(input$k),
                                              as.numeric(input$CADR),
                                              as.numeric(input$V),
                                              as.numeric(input$N))* 100) )
                      
    g_line <- geom_line(data = df2, aes(N, eff)) 
    g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
    
  }else{
    df1 <- data.table(N = fun_num_PAC(as.numeric(input$a),
                                  as.numeric(input$k),
                                  as.numeric(input$CADR),
                                  as.numeric(input$V),
                                  as.numeric(input$eff)),  
                      eff = as.numeric(input$eff) )
    df2 <- data.table(expand.grid(N = fun_num_PAC(as.numeric(input$a),
                                      as.numeric(input$k),
                                      as.numeric(input$CADR),
                                      as.numeric(input$V),
                                      as.numeric(input$eff)),  
                                eff = c(as.numeric(input$eff),min(df$eff)) 
                      )
                      ) 
                      
    g_line <- geom_line(data = df2, aes(N, eff)) 
    g_point <- geom_point(data = df1, aes(N, eff),shape = 21, fill = NA, color = "red", size = 5) 
  }
  
  ggplot(df) +
    geom_line(aes(N, eff * 100, color = forcats::fct_inorder(parti_type)),size = 1, alpha = 0.7)+
    # geom_line(aes(N, eff * 100, color = parti_type  ) )+
    g_line +
    g_point + 
    
    scale_color_manual("Particle type", values = c("#424D55", "#DCC575", "#C6C6C6") ) +
    scale_y_continuous("Efficiency (%)", expand = c(0,0)) +
    scale_x_continuous("Number of portable air cleaners", expand = c(0,0)) +
    # theme_bw() +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          text = element_text( size = 16),
          legend.key = element_rect(colour = NA, fill = "white"))
  
 } , width = "auto", height = "auto")
# } , width = 6, height = 6)


  # 
  # output$tabledata <- renderTable({
  # 
  #     isolate(datasetInput()) 
  #   
  # })
  
  output$txtout <- renderText({
    paste( datasetInput() )
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
