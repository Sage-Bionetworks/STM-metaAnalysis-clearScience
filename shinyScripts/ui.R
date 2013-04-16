## ui.R

## Erich S. Huang
## Sage Bionetworks
## Seattle, Washington
## erich.huang@sagebase.org

## REQUIRE
require(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Model Performance by Clinical Covariate"),
  
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
    
    br(),
    
    helpText("This allows one to evaluate how models",
             "generated for prediction across all",
             "breast cancer perform in defined subgroups",
             "of the disease.")
    
    submitButton("Update View")
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    h4('Leaderboard Top 10 By Covariate Selected'),
    tableOutput("tableView"),
    
    h4('Clinical Subgroup Model Performance Boxplot'),
    plotOutput("graphics1", width = '85%', height = '500px'),
    
    h4('Clinical Subgroup Model Performance Density Plot'),
    plotOutput("graphics2", width = '85%', height = '500px')
    
#     helpText('Each point represents a model and its performance',
#              'within the user-selected clinical subgroup of patients.',
#              'On the left, in red, is a boxplot of model performance',
#              'in ALL Patients, and on the right, in blue, is model',
#              'performance within the specific clinical subgroup.'),
  )
))