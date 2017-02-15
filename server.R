## shiny app for exploratory text mining in R
## David Allbritton
## Society for Computer in Psychology, 2015 Annual Meeting
#
#  server.R

if (!require(shiny)) {install.packages("shiny"); library(shiny)}

################################
# Define shiny server logic;  reactive stuff starts here ####
shinyServer(function(input, output) {  
  
  ## set default values:
  colNum <- 1  #default is column 1
  stopWordsFile <- stopWordsFile.default <- "stopWords.xlsx"
  min.freq.default <- 3  
  maxPhraseLength <- 4  #note that this is hard coded in server.R & ui.R separately; have to change it in both places
  
  ## select the input file:
  inFileName <-  reactive({
    if (is.null(input$file1)) {
      input$inFile
    }
    else {
      userInfile <- input$file1
      userInfile$name
    }
    })
  
  # load or install the required R packages for reading Excel files and data manipulation:
  if (!require(xlsx)) {install.packages("xlsx"); library(xlsx)}
  # load or install R packages required specifically for doing a word cloud:
  if (!require(wordcloud)) {install.packages("wordcloud"); library(wordcloud)}
  if (!require(tm)) {install.packages("tm"); library(tm)}
  #  detach ggplot2 before using NLP because it masks nlp::annotate
  if (sum(search() == "package:ggplot2")) detach(package:ggplot2)
  if (!require(NLP)) {install.packages("NLP"); library(NLP)}
  if (!require(SnowballC)) {install.packages("SnowballC"); library(SnowballC)}   # for stemming
  if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}  # for joining data framems
  if (!require(topicmodels)) {install.packages("topicmodels"); library(topicmodels)}
  
  
  ### define functions
  # function to clean up the documents vector:
  cleanUpDocs <- function (docs, myStopWords = c("a", "the"))  {
    toSpace <- content_transformer(function(x,pattern) gsub(pattern," ", x))
    toApost <- content_transformer(function(x,pattern) gsub(pattern,"'", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, toApost, "[^a-zA-Z ]")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # remove own custom stop words
    docs <- tm_map(docs, removeWords, myStopWords) 
    docs <- tm_map(docs, stripWhitespace)
    docs
  }
  
  # read input file
  dfInfile <- reactive({
    if (is.null(input$file1)) {   # use server side file if no user uploaded input file
      in_file <- read.xlsx(file=inFileName(), 1, header = F, stringsAsFactors=F)
    }
    else {  # use user uploaded input file
      inFile2 <- input$file1
      in_file <- read.csv(inFile2$datapath, header=F, sep='\n', quote = '', stringsAsFactors=F)
    }
    in_file$Input_Row <- as.numeric(as.character(row.names(in_file)))
    in_file
  })  #reading from sheet 1
  
  
  # read stop words file
  myStopWordsFromFile <- c("a", "the")
  # read stop words file
  if(file.exists(stopWordsFile)) {                        #check existence of stop words file
    dfstopWords <- read.xlsx(file=stopWordsFile, 1)       #reading from sheet 1
    myStopWordsFromFile <- tolower(as.character(dfstopWords[,1]))
    
  }
  
  # get the selected column of texts and clean them up:
  myTexts1 <- reactive (Corpus(VectorSource(dfInfile()[,colNum])))    #default is column 1
  myTexts <- reactive (cleanUpDocs(myTexts1(), myStopWordsFromFile))  # removing stop words, etc.
  myTexts.stemmed <- reactive(tm_map(myTexts(), stemDocument))      # stemmed version
  
  ### create all the frequency lists that will be used for creating output; 
  ### store as one reactive list of lists
  AllFreqListsReactive <- reactive({          ## start creating collection of frequency lists
    
    # initialize some lists to put the results in:
    dtm <- list()                #  document-term matrix
    dtm.stemmed <- list()        #  
    list.freq <- list()          #  ngram frequencies from dtm; will be added to allFreqList
    list.freq.stemmed <- list()  #  will be added to allFreqList
    allFreqList <- list()        #  will be returned and stored as a reactive object; refreshed 
    #  whenever input file changes
    
    #################################
    ####  iterate over N=1..4 for Ngram lengths 
    for(n in 1:maxPhraseLength){          # for each Ngram size from 1 to max length
      # n is the phraselength
      # create document-term matrix of word or ngram counts:  
      dtm[[n]] <- ({
        NgramTokenizer <-
          # define a function for using bigrams or ngrams rather than single words
          function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)  
        DocumentTermMatrix(myTexts(), control = list(tokenize = NgramTokenizer)) 
      })
      dtm.stemmed[[n]] <-  ({
        # define a function for using bigrams or ngrams rather than single words (same as above)
        NgramTokenizer2 <-
          function(x)
            unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)  
        DocumentTermMatrix(myTexts.stemmed(), control = list(tokenize = NgramTokenizer2))
      })
      # create vector of word frequencies:
      freq <- sort(colSums(as.matrix(dtm[[n]])), decreasing=TRUE)    
      freq.stemmed <- sort(colSums(as.matrix(dtm.stemmed[[n]])), decreasing=TRUE)  
      
      # add to the lists of frequencies, one for each ngram size:
      list.freq[[n]] <- freq
      list.freq.stemmed[[n]] <-  freq.stemmed
      
    }  # end for loop over Ngram sizes
    #################################
    
    allFreqList[[1]] <- list.freq           # 1 = unstemmed frequency lists
    allFreqList[[2]] <- list.freq.stemmed   # 2 = stemmed frequency lists
    # Using the same list to hold the DTMs, even though the naming is not ideal:
    allFreqList[[3]] <- dtm           # 1 = unstemmed dtms
    allFreqList[[4]] <- dtm.stemmed   # 2 = stemmed dtms
    
    allFreqList       ## return the list of lists (2) of dataframes (4) to assign to reactive object
  })                                        ## end creating collection of frequency lists
  
  
  ## The next assignment statements reactively choose the frequency data to display based on UI selections
  freq.Reactive <- reactive({  # pick the stemmed vs unstemmed version for the selected ngram size:
    if (input$useStemming) stemmed <- 2 else stemmed <- 1
    AllFreqListsReactive()[[stemmed]][[input$phraseLength]]
  })
  maxFreqReactive <- reactive(max(freq.Reactive()))
  # duplicate the word frequencies as a data frame for displaying in a table:
  fullFreqList.Reactive <- reactive (data.frame(Ngram=names(freq.Reactive()), Frequency=freq.Reactive()))   
  
  ####### Insert stuff for LDA topic modeling
  dtm.Reactive <- reactive({  # pick the stemmed vs unstemmed version for the selected ngram size:
    if (input$useStemming) stemmeddtm <- 4 else stemmeddtm <- 3
    dtm1 <- AllFreqListsReactive()[[stemmeddtm]][[input$phraseLength]]
    keeplines <- rowSums(as.matrix(dtm1)) > 0 # record which documents contain at least one term
    dtm2 <- dtm1[keeplines,]                  # filter out documents containing no terms
    dtm2
  })
  lda_model.Reactive <- reactive({
    LDA(dtm.Reactive(), input$nTopics)
  })
  # create table of terms (ngrams) associated with each lda topic and display summary:
  output$topicModel_topicSummary <- renderDataTable({
    terms(lda_model.Reactive(),100)
  }, options = list(lengthMenu = c(1:5, 10, 15, 20, 50, 100), pageLength = 3, searching = FALSE)
  )
  
  # build a data frame of lda model texts for downloading
  df_lda_model_texts.Reactive <- reactive({
    lda_model <- lda_model.Reactive()        
    fullTexts <- dfInfile()       
    lda_gammas <- as.data.frame(lda_model@gamma)
    names(lda_gammas) <- gsub("V", "Probability for Topic ", names(lda_gammas))
    row.names(lda_gammas) <- names(topics(lda_model))
    lda_gammas$Input_Row <- as.numeric(as.character(row.names(lda_gammas)))
    lda_texts <-  as.data.frame(cbind (Topic = topics(lda_model)))
    lda_texts$Input_Row <- as.numeric(as.character(row.names(lda_texts)))
    lda_texts <- left_join(lda_texts, lda_gammas, by = "Input_Row")
    row.names(lda_texts) <- lda_texts$Input_Row
    temptexts <- select(fullTexts, Input_Row, Text = colNum)
    lda_texts <- right_join(lda_texts, temptexts, by = "Input_Row")
    names(lda_texts) <- gsub("Input_Row", "Input Row", names(lda_texts))
    lda_texts
  }) # end reactive function for building df_lda_model.Reactive for downloading
  
  # build a data frame of lda model terms for downloading
  df_lda_model_terms.Reactive <- reactive({
    lda_model <- lda_model.Reactive()
    lda_terms <- posterior(lda_model)$terms  %>% t()
    as.data.frame(lda_terms)
  })
  
  # download the lda model texts and terms, plus ngram frequencies
  output$downloadLDAmodel <- downloadHandler(
    filename = function() {paste ('LDA_model_', inFileName(), '.xlsx', sep='')},
    content = function(file) {
      write.xlsx(dfInfile(), file,sheetName = "Input File", row.names = F)
      write.xlsx(fullFreqList.Reactive(), file,sheetName = "Ngram Frequencies", append=TRUE, row.names = F)
      write.xlsx(df_lda_model_texts.Reactive(), file,sheetName = "LDA Topic Model texts", append=TRUE, row.names = F)
      write.xlsx(df_lda_model_terms.Reactive(), file,sheetName = "LDA Topic Model terms", append=TRUE, row.names = T)
      outParameters <- data.frame(InputFile = inFileName(), Stemming = input$useStemming, 
                                  NgramSize = input$phraseLength, Topics = input$nTopics)
      write.xlsx(outParameters, file,sheetName = "Analysis parameters", append=TRUE, row.names = F)
      
    }
  )
  
  # create a select UI element for selecting which topic to display:
  output$UI_topicChooser <- renderUI({
    selectInput("selectedTopic", label = "Select Topic to Display", 1:input$nTopics)
  })
  
  # display the topic number along with the top 10 terms for that topic, above texts:
  output$topicLabel <- renderUI({
    termsin <- terms(lda_model.Reactive(), 10)[, as.integer(input$selectedTopic)]  # top 10 terms  
    termsx <- paste(termsin, collapse=", ")
    HTML(paste("<h4><b>TOPIC ", input$selectedTopic, ":</b>", termsx ,"</h4>"))
  })

  # display a table of documents (texts), labeled with terms, for each topic:
  output$topicModel_displayModel_texts <- renderDataTable({
    textsin <- df_lda_model_texts.Reactive()
    textsin <- textsin[textsin$Topic == input$selectedTopic, ]
    # Probability for Topic 1
    # Text
    # Topic
    # Input Row
    ## input$nTerms
    colNumGamma <-  as.numeric(as.character(input$selectedTopic)) + 2
    probsList <- textsin[,colNumGamma]
    textsout <- textsin[, c("Input Row", "Text") ]
    textsout <- cbind("Probability of Topic Membership" = probsList, textsout)
    textsout <- textsout[order(-textsout[,1]),]
    textsout
  }, options = list(lengthMenu = c(1:10, 15, 20, 50, 100), pageLength = 5, rownames=T)
  )
  

  #######  End LDA topic modeling section ###########
  
  #  Only load this after doing NLP stuff, because it masks nlp::annotate
  if (!require(ggplot2)) {install.packages("ggplot2"); library(ggplot2)}
  
  # start writing outputs for the UI to display:
  
  # create the UI element that selects the minimum frequency to plot:
  output$setMinFreq <- renderUI({                      # creating a UI element for choosing min frequency
    sliderInput('minFreq', 'Min frequency to plot',
                min = 1, max = maxFreqReactive(),  min(min.freq.default, maxFreqReactive()), step = 1, round = TRUE)
  })
  output$maxObservedFreq <- renderText (paste0 ("Max Ngram Frequency: ", (max(freq.Reactive()))))
  output$totalObservedNgrams <- renderText (paste0 ("Total unique Ngrams: ", (length(freq.Reactive()))))
  output$freqTable <- renderTable({
    table(freq.Reactive(), dnn = c("Frequency of Ngram (word/phrase) Frequencies"))
  })
  output$plot1wordCloud <- renderPlot({
    cat("updating word cloud.....\n")
    wordcloud(names(freq.Reactive()), freq.Reactive(), min.freq=min(input$minFreq, maxFreqReactive()), max.words=input$maxWords, colors=brewer.pal(6,"Dark2"),random.order=FALSE, scale=c(input$maxScale,input$minScale))
  })
  output$plot2topNgrams <- renderPlot({
    wf <- transform(fullFreqList.Reactive(), Ngram = reorder(Ngram, -Frequency))
    wf <- subset(wf, Frequency>=min(input$minFreq, maxFreqReactive()))
    wf <- wf[1:(min(input$maxWords,length(wf[,1]))),]
    p <- ggplot(wf, aes(Ngram, Frequency))
    p <-p+ geom_bar(stat="identity", width = .7, fill= "#99CCFF")
    p <-p+ theme(axis.text.x=element_text(angle=45, hjust=1, color="blue"))
    p
  })
  output$table2allNgrams <- renderDataTable({
    fullFreqList.Reactive()
  })
  #### More new parts for scip 2016:
  output$inFile2 <- renderText (paste0 ("<i>Input File:</i> ", inFileName(), "<p></p>"))
  
  # for displaying and outputing parameters:
  output$useStemming2 <- renderText(paste0 ("<i>Use Stemming?</i>  ", if(input$useStemming) "Yes" else "No", "<p></p>"))
  output$NgramSize2 <- renderText(paste0 ("<i>Ngram Size:</i> ", input$phraseLength, "<p></p>"))
  
})


