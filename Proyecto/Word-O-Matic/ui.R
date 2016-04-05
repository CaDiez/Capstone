# Define UI for dataset viewer application, the screen is divided in three columns,
# Prediction engine tab
# the first contains the input text and option buttons of predicted words
# the second contains a table that shows predicted words and its calculated weights
# the third contains A word cloud representation of the predicted words
# App information tab
# Contains a brief of how does the app was made and works
shinyUI(fluidPage(
  titlePanel(em(strong("Word-O-Matic", style = "color:#00008B; size=4"))),
  h4("A Natural Language Predictor by CGX", style = "color:#202020 "),
  h5("Please wait while Word-O-Matic loads the data (check the progress bar at the top of the page)", style = "color:#202020 "),
  hr(style = "color:#00008B"),
  mainPanel(
    tabsetPanel(
      # Prediction engine tab
      tabPanel("Prediction engine", 
               fluidRow( 
                 #First column, prediction
                 column(5, 
                        br(),
                        strong("Enter text",  style = "color:#202020 "),
                        br(),
                        br(),
                        tags$textarea(id="wt_1", rows=5, cols=40,"Text prediction it is so "),
                        uiOutput("Dynamic"),
                        hr(style = "color:#00008B"),
                        strong("Processing time of the session: ",  style = "color:#202020 "),
                        textOutput('time'),                               
                        br(),
                        strong("Designed by Carlos Alberto Guevara Diez, Mexico, 2016"),
                        br(),
                        br(),
                        a("App Documentation", href="https://github.com/CaDiez/Capstone/tree/master/Proyecto"),
                        br(),
                        a("Review Source Code", href="https://github.com/CaDiez/Capstone/tree/master/Proyecto")
                 ),
                 # second column, the table
                 column(2, offset = 1,
                        br(),
                        strong("Predicted Word Table",  style = "color:#202020 "),
                        br(),
                        br(),
                        uiOutput("matrix")
                 ),
                 # third column word cloud
                 column(3, offset = 1,
                        br(),
                        strong("Predicted WordCloud",  style = "color:#202020 "),
                        plotOutput("plot"),
                        checkboxInput('a_wc', 'Uncheck to hide the graphic (Fastens the app)', TRUE)
                 )
               )
      ), 
      # App information tab      
      tabPanel("App information", 
               fluidRow( 
                 column(8, br(),
                        strong("How does Word-O-Matic works:"),
                        br(),
                        br(),
                        p(strong("- Use: "), "Type anything yow want in the text section, a series of options would appear in buttons beneath the text, click any one to select a word or type it directly in the phrase window. As an output you will se a table with the weights of each  word and a word cloud representation (click the checbox to disable it and fasten the application)."),
                        p(strong("- Calculations: "), "This app uses Good-Turing and 'Stupid Backoff' algorithm for prediction, also adjusted counts have been used to predict unseen words. The model was trained using a random binary sample of 90% from the HC corpora containing English documents from twitter, blogs and news feeds."),
                        p(strong("- Modeling: "), "In this phase I have used N-grams with skipped words (n=2,3, skip=0,1) as features for prediction. The modeling script takes about 4 hours to fit the model. If you want to see the codes please refer to my GitHub Page."),                        
                        br(),
                        br(),
                        strong("Designed by Carlos Alberto Guevara Diez, Mexico, 2016"),
                        br(),
                        br(),
                        a("App Documentation", href="https://github.com/CaDiez/Capstone/tree/master/Proyecto"),
                        br(),
                        a("Review Source Code", href="https://github.com/CaDiez/Capstone/tree/master/Proyecto")
                 )
               )
      )
    )
  )
)
)
