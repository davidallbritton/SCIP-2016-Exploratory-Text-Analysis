## shiny app for exploratory text mining in R
## David Allbritton
## Society for Computer in Psychology, 2015 Annual Meeting
#
# ui.R

# set default values for ngram frequency analysis and creation of dtm:
columnNum.default <- 1
max.words.default  <- 50
# min.freq.default <- 3   goes in server.R instead
minScale.default <- 0.1
maxScale.default <- 2
phraseLength.default <- 1   # set to 1 for single words; 2 for 2-word phrases, etc.  Ngram size.
maxPhraseLength <- 4  #note that this is hard coded in server.R & ui.R separately; have to change it in both places
myStopWords.default <- c("however", "also", "actually", "something", "rather")
# defaults for LDA topic modeling:
topics.default <- 2   # default number of topics for LDA topic modeling.
ndocs.default <- 3   # default number of texts (documents) from LDA topic modeling to display
nterms.default <- 1   # default number of terms from LDA topic model to display.
# select an Excel file in the current working directory for input:
filenames<-list.files(pattern="\\.xlsx$")

# Define UI
navbarPage(
  'Exploratory Text Analysis in R',
  tabPanel("Ngram Frequency",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "inFile",
                           label = "Select Existing Input File:",
                           filenames),
               fileInput('file1', 'Or Upload your own plain text or csv file, one text per line:', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.txt')),
               checkboxInput('useStemming', 'Stemming', value = FALSE),
               sliderInput (
                 'phraseLength',
                 'Words per Phrase (Ngram size)',
                 min = 1,
                 max = 4,
                 value = phraseLength.default,
                 round = TRUE,
                 ticks = FALSE
               ),
               textOutput ("totalObservedNgrams"),
               sliderInput (
                 'maxWords',
                 'Max Ngrams to plot',
                 min = 2,
                 max = 200,
                 max.words.default,
                 step = 1,
                 round = TRUE
               ),
               textOutput ("maxObservedFreq"),
               uiOutput ("setMinFreq"),
               sliderInput (
                 'maxScale',
                 'Max font size for Cloud',
                 min = 0.2,
                 max = 10,
                 value = maxScale.default
               ),
               sliderInput (
                 'minScale',
                 'Min font size for Cloud',
                 min = 0.1,
                 max = 2,
                 value = minScale.default
               ),
               tableOutput("freqTable")
             ),
             mainPanel(
               plotOutput("plot1wordCloud"),
               tableOutput("contents"),
               plotOutput("plot2topNgrams"),
               HTML("<h2>All Ngram Frequencies</h2>"),
               dataTableOutput("table2allNgrams")
             )
           )),
  tabPanel("Topic Modelling with LDA", 
           fluidPage(
             fluidRow(
               column(3, style = "background-color:#EBEDF6",
               htmlOutput("inFile2"),
               htmlOutput("useStemming2"),
               htmlOutput("NgramSize2"),
               sliderInput (
                 'nTopics',
                 'Number of Topics',
                 min = 2,
                 max = 15,
                 value = topics.default,
                 round = TRUE,
                 ticks = FALSE
               ),
               uiOutput("UI_topicChooser"),
               downloadButton('downloadLDAmodel', 'Download Output')
             ),
             column(8, offset = 1,
                       h4("Terms (ngrams) Associated with  each Topic"),
                       dataTableOutput("topicModel_topicSummary")
             )),
             hr(),
             htmlOutput("topicLabel"),
             dataTableOutput("topicModel_displayModel_texts")
             
           )
           )
)