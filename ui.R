 This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

suppressWarnings(library("shiny"))
suppressWarnings(library("rsconnect"))
suppressWarnings(library("shinythemes"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # App theme
  # shinythemes::themeSelector(),
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel(" Data Science Capstone"),
  h3("Text Prediction Data Product"), br(),
  "rauf", br(), "March 13, 2018", br(), br(),
  
  # SidebarLayout gives two-column layout with a smaller sidebar and a larger main panel
  sidebarLayout(
    sidebarPanel(
      h4("Notes"),
      "This text prediction data product is based on the analysis of a large corpus of text documents.",
      br(), br(),
      "Course 10 tasks:", br(),
      "- Understanding the problem", br(),
      "- Data acquisition and cleaning", br(),
      "- Exploratory analysis", br(),
      "- Statistical modeling", br(),
      "- Predictive modeling", br(),
      "- Creative exploration", br(),
      "- Creating a data product", br(),
      "- Crreating slide deck to pitch product", br()
      
    ),
    
    # Output
    mainPanel(
      "Enter text in box below:", 
      textAreaInput("text", width="100%", label="", value = "Enter text here "),
      uiOutput("wordPred"),
      # uiOutput("wordPred2"),
      br(), br(),
      a(href="https://github.com/jm-carson/DevelopingDataProductsCourseProject", "Github repository", style="float:left;")
      
      # plotOutput("distPlot")
    )
  )
))
