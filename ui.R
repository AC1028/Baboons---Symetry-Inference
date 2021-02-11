#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    
    
    navbarPage(title = "Baboons",
        tabPanel(title = "Plot of performance",
            fluidPage(
            
            # Application title
            titlePanel("Baboons Performance Plot"),
            
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    sliderInput(inputId="age",label = "Select an age range",min=min(data$Age),max=max(data$Age),step=1,value=c(31,297)),
                    checkboxGroupInput(inputId = "sex", label = "Sex of the monkey", selected = c("female","male"),
                                       unique(data$Sexe)),
                    uiOutput("name_select"),
                    checkboxInput(inputId = "wrap", label = "Multiple Plots?"),
                    checkboxGroupInput(inputId = "question", label = "Please select the question", selected = "paire1",
                                       unique(data$Pair)),
                    checkboxGroupInput(inputId = "blocks", label = "What kind of plots do you want", selected = 1,
                                       c("Blocks"=1,"Cumulative"=2)),
                    selectInput(inputId = "phases", label = "Select the phase: ", selected = unique(data$Condition_Exp)[1],
                                choices = unique(data$Condition_Exp), multiple = FALSE),
                    checkboxInput(inputId = "av", label = "Average of pairs?"),
                    downloadButton(outputId = "dow_performance","Download")
                    
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    plotOutput("learnplot")
                )
            )
        )),
        #tabPanel(title = "A1",
#                 fluidPage(
#                     titlePanel("Link Between Age and Nblock"),
#                     sidebarLayout(
#                         sidebarPanel(
#                            selectInput(inputId = "phases11", label = "Select the phases: ", selected = unique(data$Condition_Exp)[1],
#                                 choices = unique(data$Condition_Exp), multiple = TRUE)
#                 ),
#                         mainPanel(
#                             plotOutput("ageplot")
#                         )
#                 )
#        )
#    
#    
#    
#    ),
    
    tabPanel(title = "Plot of phases",
             fluidPage(
                 titlePanel("Baboons Phases Plot"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(inputId="age1",label = "Select an age range",min=min(data$Age),max=max(data$Age),step=1,value=c(31,297)),
                         checkboxGroupInput(inputId = "sex1", label = "Sex of the monkey", selected = c("female","male"),
                                            unique(data$Sexe)),
                         uiOutput("name_select1"),
                         checkboxInput(inputId = "wrap1", label = "Multiple Plots?"),
                         checkboxGroupInput(inputId = "question1", label = "Please select the question", selected = "paire1",
                                            unique(data$Pair)),
                         checkboxGroupInput(inputId = "blocks1", label = "What kind of plots do you want", selected = 1,
                                            c("Blocks"=1,"Cumulative"=2)),
                         selectInput(inputId = "phase_type", label = "Select the kind of phase: ", selected = unique(data$Phase),
                                     choices = unique(data$Phase), multiple = TRUE),
                         uiOutput("phase_select"),
                         checkboxInput(inputId = "av1", label = "Average of pairs?"),
                         downloadButton(outputId = "dow_performance1","Download")
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("phaseplot")
                     )
                 )
             )),
    tabPanel(title = "2DPlot",
            fluidPage(
                titlePanel("2DPlot"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput(inputId="age2",label = "Select an age range",min=min(data$Age),max=max(data$Age),step=1,value=c(31,297)),
                        checkboxGroupInput(inputId = "sex2", label = "Sex of the monkey", selected = c("female","male"),
                                           unique(data$Sexe)),
                        uiOutput("name2"),
                        selectInput(inputId = "phase_type2", label = "Select the kind of phase: ", selected = unique(data$Phase),
                                    choices = unique(data$Phase), multiple = TRUE),
                        uiOutput("phase_select2"),
                        radioButtons(inputId = "dist_type2",label="Select the kind of distance: ", selected = 1,c("To Origin"=1,
                                                                                                                  "To 0-1"=2,
                                                                                                                  "To 1-0"=3)),
                        downloadButton(outputId = "dow_performance2","Download")
                        
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        plotOutput("twoDplot")
                    )
                )
            )),
    tabPanel(title = "Chuncks plot",
             fluidPage(
                 titlePanel("Baboons Chuncks Plot"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(inputId="age3",label = "Select an age range",min=min(data$Age),max=max(data$Age),step=1,value=c(31,297)),
                         checkboxGroupInput(inputId = "sex3", label = "Sex of the monkey", selected = c("female","male"),
                                            unique(data$Sexe)),
                         uiOutput("name_select3"),
                         selectInput(inputId = "phase_type3", label = "Select the kind of phase: ", selected = unique(data$Phase),
                                     choices = unique(data$Phase), multiple = TRUE),
                         uiOutput("phase_select3"),
                         uiOutput("index_select3"),
                         downloadButton(outputId = "dow_performance3","Download"),
                         numericInput("sat_max",value=0,"Select the maximum saturation")
                         
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("chunckplot")
                     )
                 )
             ))
    )
)
