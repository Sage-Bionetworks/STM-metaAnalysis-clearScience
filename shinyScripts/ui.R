library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Challenge Leaderboard App"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    selectInput("clinCovar", "Subgroup Patients by Selected Clinical Covariate:",
                choices = c('All Patients',
                            'ER Positive',
                            'ER Negative',
                            'HER2 Positive',
                            'HER2 Negative',
                            'PR Positive',
                            'PR Negative',
                            'Histologic Grade 1',
                            'Histologic Grade 2',
                            'Histologic Grade 3',
                            'Lymph Node Negative',
                            'Lymph Node 1-3',
                            'Lymph Node 4-9',
                            'Lymph Node 10+',
                            'Followup Time 0-5 Years',
                            'Followup Time 5-10 Years',
                            'Followup Time 10+ Years',
                            'Age ≤ 50 Years',
                            'Age 50+ Years',
                            'Tumor Size 0-2 CM',
                            'Tumor Size 2+ CM'
                            )),
    helpText("Select a Clinical Covariate",
             "then click 'Update View'",
             "this will re-sort the leaderboard by",
             "performance in that clinical subgrouping",
             "and display a boxplot of model performance",
             "within that subgroup versus All Patients",
             "in real time."),
    
    submitButton("Update View"),
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    h4('Clinical Subgroup Boxplot'),
    plotOutput("graphics1", width = '100%', height = '500px'),
    
    tableOutput("tableView")
  )
))