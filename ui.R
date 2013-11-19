library(shiny)

# stop using scientific notation
options(scipen=999)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Q-Cloud Reporting Service (Protoype)"),

  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
	helpText("This is a prototype reporting service for Q-Cloud")
  ),

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Summary", tableOutput("summary")),
      tabPanel("Collection growth", plotOutput("datagrowth")),
      tabPanel("Member statistics", plotOutput("memberstats")),
      tabPanel("New releases", dataTableOutput("newreleases")),
      tabPanel("Largest storage users", tableOutput("topstorageusers")),
      tabPanel("Public collections", tableOutput("publiccollections")),
      tabPanel("Storage data (full)", dataTableOutput("storage"))
    )
  )
))
