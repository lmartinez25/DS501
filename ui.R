
# Define UI ----
ui <- page_sidebar(
  # App title ----
  title = "Predicting Employment Status in MA 2023",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    helpText(
      "1. Choose a demographic variable to explore: (results in EDA and Visualize)"
    ),
    selectInput(
      "y2",
      "Select demographic",
      choices = vars,
      selected = "ESR"),
    #submit button
   # submitButton("Create plots"),
    
    helpText(
      "2. Choose 2 demographic variables to compare: (results in Visualize)"
    ),
    #Input: drop down menu to select x
    selectInput(
      "xpick",
      "Select 1st demographic",
      choices = vars,
      selected = "ESR"),
    selectInput(
      "ypick",
      "Select 2nd demographic",
      choices = vars,
      selected = "agecat"),
    #submit button
    #submitButton("Create barplot"),
    
    helpText(
      "",
      "To see model performance refer to 'models' tabs."
      ),
    
    card(
      card_header("Select demographics to use in the models"),
      checkboxGroupInput(
        "selected_vars",
        "Select all that apply",
        choices = vars[-1],
        selected = ''
      ),
      actionButton("run_model","Run Model")
      ),
    
   
  ),
  
  

 navs_tab(
   nav_panel("Visualize", 
             card(verbatimTextOutput(outputId = "percentnum1"),
             card(plotOutput(outputId = "percentPlot1")),
             verbatimTextOutput(outputId = "percentnumE"),
             plotOutput(outputId = "percentPlotE")),
             card(plotOutput(outputId = "distPlot2"),
                  verbatimTextOutput(outputId = "num2"),
                  verbatimTextOutput(outputId = "percentnum2"))
),
   
   nav_panel("Introduction",
p(br(),strong('Introduction:'),br(),introduction),p(strong('Data:'),br(),dataoverview),p(strong('Motivations:'),br(),Motivations),p(strong('Methods:'),br(),Methods1,br()),Methods2, TOC

),

nav_panel("Data details",
          p(br(),strong('Data Details'),Datadetails), 
          card(uiOutput(outputId = "table1")),
          p(strong('Demographic proportions'),Datatables), 
          p(em('Select a variable to explore. Current selection is:')),
          textOutput(outputId = "y2sel"),
          p(br(),em('Proportion of population')),
          card(verbatimTextOutput(outputId = "percentnum1copy")),
          p(em('Proportion of working aged population')),
          card(verbatimTextOutput(outputId = 'percWA1')),
          textOutput(outputId = "proptext")
          
),
   
  nav_panel("EDA", 
            p(br(),EDA1,br(),br(),EDA2),
            p(em("Negative Residuals: Employed proportion vs. Population proportion")),
            p(em("Less common demographics")),  
            card(tableOutput("residTbl1")),
            p(EDA3,br(),br(),EDA4),
            p(em("Negative Residuals: Employed proportion vs. Population proportion")),
            p(em("More common demographics")),  
            card(tableOutput("residTbl2")) ,
            p(em("Positve Residuals: Employed proportion vs. Population proportion")),
              p(em("Less common demographics")),  
            card(tableOutput("residTbl3")) ,
            p(EDA5,br(),br(),EDA6),
            p(em("Positve Residuals: Employed proportion vs. Population proportion")),
              p(em("More common demographics")),  
            card(tableOutput("residTbl4")) ,
            p(em("Negative Residuals: Not in Labor Force proportion vs. Population proportion")),
              p(em("Less common demographics")),  
            card(tableOutput("residTbl5")) ,
            p(EDA7,br(),br(),EDA8),
            p(em("Negative Residuals: Not in Labor Force proportion vs. Population proportion")),
              p(em("More common demographics")),
            card(tableOutput("residTbl6")) ,
            p(em("Positive Residuals: Not in Labor Force proportion vs. Population proportion")),
              p(em("Less common demographics")),
            card(tableOutput("residTbl7")) ,
            p(EDA9,br(),br(),EDA10),
            p(em("Positive Residuals: Not in Labor Force proportion vs. Population proportion")),
              p(em("More common demographics")),
            card(tableOutput("residTbl8")) ,
            p(em("Negative Residuals: Unemployed proportion vs. Population proportion")),
              p(em("Less common demographics")),
            card(tableOutput("residTbl9")) ,
            p(EDA11,br(),br(),EDA12),
            p(em("Negative Residuals: Unemployed proportion vs. Population proportion")),
              p(em("More common demographics")),
            card(tableOutput("residTbl10")) ,
            p(em("Positive Residuals: Unemployed proportion vs. Population proportion")),
              p(em("Less common demographics")),
            card(tableOutput("residTbl11")) ,
            p(EDA13,br(),br(),EDA14),
            p(em("Positive Residuals: Unemployed proportion vs. Population proportion")),
              p(em("More common demographics")),
            card(tableOutput("residTbl12")),
            p(EDA15)
          
            ),

   nav_panel("Models", p(br(),"Below find model assessments (AUC and CCR), tree diagram and logistic regression model summary. Refer to 'methods details' for further explanations."),
             p(em('Select demographics to run model. Current selection is:')),
             verbatimTextOutput("vars_selected1"), #why not showing
             plotOutput("roc_plot_tree"),
             plotOutput("roc_plotlogr"),
             tableOutput("TreeCMtable"),
             verbatimTextOutput('CCR'),
             tableOutput("LogCMtable"),
             verbatimTextOutput('LogCCR'),
             #tableOutput("datatbl"),
             plotOutput("tree_plot"),
             p('LogReg Model summary'),
             #verbatimTextOutput('model_summary_full'),
             verbatimTextOutput('model_summary')
             
             
             ),

nav_panel("Working aged models", p(br(),"Below find model assessments (AUC and CCR), tree diagram and logistic regression model summary. Refer to 'methods details' for further explanations."),
          p(em('Select demographics to run model. Current selection is:')),
          verbatimTextOutput("vars_selected2"),
          plotOutput("roc_plot_treeWA"),
          plotOutput("roc_plotlogrWA"),
          tableOutput("TreeCMtableWA"),
          verbatimTextOutput('CCRWA'),
          tableOutput("LogCMtableWA"),
          verbatimTextOutput('LogCCRWA'),
          #tableOutput("datatbl"),
          plotOutput("tree_plotWA"),
          #verbatimTextOutput('model_summary_fullWA'),
          verbatimTextOutput('model_summaryWA')
          
),

nav_panel("Methods details", p(),MethodDetails1
          
)

))
  
  
    






