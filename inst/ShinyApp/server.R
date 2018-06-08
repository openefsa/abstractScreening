## Server.R file containing the code that is run
## for the analyses in the application

## Load the required libraries
library(shiny)
  library(plyr)
  library(xlsx)
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(e1071)
  library(kernlab)
  library(nnet)
 library(caret)
library(topicmodels)
library(randomForest)
library(pROC)
library(DMwR)
library(hmeasure)
library(caretEnsemble)
library(DT)
library(pdftools)
library(tabulizer)
library(gbm)
library(ROSE)

options(shiny.maxRequestSize=10000*1024^2)   ## Needed to upload big data files
Sys.setlocale('LC_ALL','C')
#options(encoding = "UTF-8")
##-------------------------------------
#### Start of shiny application code
##-------------------------------------

##--------------------
## Selecting data tab
##--------------------

shinyServer(function(input, output, session) {
  v <- reactiveValues(data = NULL)
  summary_list <- reactiveValues(current=list(),indicator=0)
  fitted_models_ensemble <- reactiveValues(current=list(),selected=NULL,final=NULL,performance_final=NULL,indicator=0,topicsmodel=NULL,timing=NULL)
  model_comparison <- reactiveValues(data=NULL)
  predict_new_data<-reactiveValues(out_csv=NULL)


  clear_list <- eventReactive(input$build_ensemble, {
    fitted_models_ensemble$current = list()
    })



  ## Reading in the data

  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
          }
  })

  inFile2 <- reactive({
    if (is.null(input$file_pdf)) {
      return(NULL)
    } else {
      input$file_pdf
    }
  })

  inFileFull <- reactive({
    if (is.null(input$files_full)) {
      return(NULL)
    } else {
      input$files_full
    }
  })


  FullTexts <- reactive({

    if (is.null(inFileFull())) {
      return(NULL)
    } else {
      opinions <- lapply(inFileFull()$datapath, pdf_text)
      return(opinions)
    }
  })


  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      switch(input$sep,
             "Comma" = read.csv(inFile()$datapath),
             "Tab" = read.table(inFile()$datapath,sep="\t",header=TRUE))
    }
  })

  # myData2 <- reactive({
  #   if (is.null(inFile2())) {
  #     return(NULL)
  #   } else {
  #     switch(input$sep,
  #            "Comma" = read.csv(inFile2()$datapath),
  #            "Tab" = read.table(inFile2()$datapath,sep="\t",header=TRUE))
  #   }
  # })


  output$wordcloud_options<- renderUI({
    if(input$make_wordcloud == FALSE)    return(NULL)
    tagList(tags$h2("Select options for the wordcloud"),
            sliderInput("freq","Minimum Frequency:",min = 1,  max = 50, value = 3),
            sliderInput("max","Maximum Number of Words:", min = 1,  max = 300,  value = 50))
  })


  output$wordcloud_title<- renderUI({
    if(input$make_wordcloud == FALSE)    return(NULL)
    tagList(tags$h2("Wordcloud of the selected input space") )
  })

  output$summary1_title<- renderUI({
    if(input$show == FALSE)    return(NULL)
    tagList(tags$h2("Uploaded data with input space attached") )
  })

  output$summary2_title<- renderUI({
    if(input$show2 == FALSE)    return(NULL)
    tagList(tags$h2("Top 6 words in each topic") )
  })

  output$relevance_title<- renderUI({
    if(is.null(myData()))    return(NULL)
    tagList(tags$h2("Number of relevant papers") )
  })


  output$tuning_method_svm<- renderUI({
    if(input$tune_svm == FALSE)    return(NULL)
    tagList(
      radioButtons("method", label = "Select method for tuning", choices = list("CV", "RepeatedCV"),selected = "CV")
    )

  })

  output$tuning_method_options_svm<- renderUI({
    if(input$tune_svm == FALSE)    return(NULL)
    if(is.null(input$method)){return(NULL)}
    if(input$method=="CV"&input$kernel=="Radial"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "1,2,5"),
        textInput('vec2', 'Enter a grid for sigma parameter (comma delimited)', "0.001,0.01,0.1")
      ) )
    }

    if(input$method=="CV"&input$kernel=="Polynomial"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "1,2,5"),
        textInput('vec2', 'Enter a grid for degree parameter (comma delimited)', "1,2,5"),
        textInput('vec3', 'Enter a grid for scale parameter (comma delimited)', "0.5,1,2")

      ) )
    }

    if(input$method=="CV"&input$kernel=="Linear"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "0.00001,0.0001,0.001,0.01,1,10")
      ) )
    }

    if(is.null(input$method)){return(NULL)}
    if(input$method=="RepeatedCV"&input$kernel=="Radial"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        numericInput('number_rep', "How many repetitions?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "0.01,0.1,1,5"),
        textInput('vec2', 'Enter a grid for sigma parameter (comma delimited)', ".0001,0.001,0.01,0.1")
      ) )
    }
    if(input$method=="RepeatedCV"&input$kernel=="Linear"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        numericInput('number_rep', "How many repetitions?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "1,2,10")
      ) )
    }

    if(input$method=="RepeatedCV"&input$kernel=="Polynomial"){
      return(tagList(
        numericInput('number_CV', "How many CV sets?", 3),
        numericInput('number_rep', "How many repetitions?", 3),
        textInput('vec1', 'Enter a grid for cost parameter (comma delimited)', "1,2,10"),
        textInput('vec2', 'Enter a grid for degree parameter (comma delimited)', "1,2,5"),
        textInput('vec23', 'Enter a grid for  scale (comma delimited)', "0.5,1,2")

      ) )
    }

  })



  output$tuning_metric_svm<- renderUI({
    if(input$tune_svm == FALSE)    return(NULL)
    tagList(
      radioButtons("metric", label = "Select metric for tuning", choices = list("Kappa", "Accuracy","F1"),selected = "Kappa")
    )

  })

  Create_corpus<- reactive({

    if(input$type=="Abstract"){
        if(!is.null(myData())){
      abstracts_final<-myData()
      #abstracts_final<-abstracts_final[abstracts_final$Abstract[]!="",]
      review_corpus = VCorpus(VectorSource(paste(abstracts_final$Title,abstracts_final$Abstract)))
      review_corpus = tm_map(review_corpus, content_transformer(tolower))  #no uppercase
      review_corpus = tm_map(review_corpus, removeNumbers)  #no numerical values
      review_corpus = tm_map(review_corpus, removePunctuation) #remove punctuation
      review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english"))) # remove stopwords
      review_corpus = tm_map(review_corpus, stripWhitespace) # remove whitespace
      review_corpus = tm_map(review_corpus, stemDocument, language = "english")  #stemming (bring back to common base word)
      return(review_corpus)}
    }
      if(input$type=="Full text"){
if(!is.null(FullTexts())){
          opinions<-FullTexts()
          corp <-VCorpus(VectorSource(opinions))

          corp = tm_map(corp, content_transformer(tolower))  #no uppercase
          corp = tm_map(corp, removeNumbers)  #no numerical values
          corp = tm_map(corp, removePunctuation) #remove punctuation
          corp = tm_map(corp, removeWords, c("the", "and", stopwords("english"))) # remove stopwords
          corp =  tm_map(corp, stripWhitespace) # remove whitespace
          corp =  tm_map(corp, stemDocument, language = "english")  #stemming (bring back to common base word)

          for(i in seq(length(corp))){
            meta(corp[[i]], tag = "id")<- inFileFull()$name[i]
          }
          return(corp) }}

  })


final_dtm<-reactive({

      if(input$type=="Abstract"&!is.null(myData())){

      review_corpus<-Create_corpus()
      if(input$selection == "TDM"){
        review_dtm <- DocumentTermMatrix(review_corpus)
        if(input$remove_percentage<100) review_dtm = removeSparseTerms(review_dtm, input$remove_percentage/100)
        if(input$remove_percentage==100) review_dtm = removeSparseTerms(review_dtm, 0.99999)
        return(as.matrix(review_dtm))
      }
      if(input$selection == "TFIDF"){
        review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
        review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, input$remove_percentage/100)
        return(as.matrix(review_dtm_tfidf))
      }
      if(input$selection == "bigram"){
        BigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
        review_dtm_bigram <- t(TermDocumentMatrix(review_corpus, control = list(tokenize = BigramTokenizer)))
        review_dtm_bigram = removeSparseTerms(review_dtm_bigram, input$remove_percentage/100)
        return(as.matrix(review_dtm_bigram))
      }
      if(input$selection == "trigram"){
        TrigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
        review_dtm_trigram <- DocumentTermMatrix(review_corpus, control = list(tokenize = TrigramTokenizer))
        review_dtm_trigram = removeSparseTerms(review_dtm_trigram, input$remove_percentage/100)
        return(as.matrix(review_dtm_trigram))
      }

      if(input$selection == "topics"){
        review_dtm <- DocumentTermMatrix(review_corpus)
        if(input$remove_percentage<100) review_dtm = removeSparseTerms(review_dtm, input$remove_percentage/100)
        if(input$remove_percentage==100) review_dtm = removeSparseTerms(review_dtm, 0.99999)

      #Set parameters for Gibbs sampling
      burnin <- 4000
      iter <- 2000
      thin <- 500
      seed <-1988
      nstart <- 1
      best <- TRUE


      #Number of topics
      k <- input$nr_topics

      raw.sum=apply(review_dtm,1,FUN=sum) #sum by raw each raw of the table
      review_dtm=review_dtm[raw.sum!=0,]

      #Run LDA using Gibbs sampling
      withProgress({
        setProgress(message = "Identifying topics...")
      ldaOut <-LDA(review_dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
      })
      topicProbabilities <- as.data.frame(ldaOut@gamma)
      fitted_models_ensemble$topicsmodel = ldaOut
      return(topicProbabilities)
      }

    }


  if(input$type=="Full text"&!is.null(FullTexts())){
    review_corpus<-Create_corpus()
    if(input$selection == "TDM"){
      review_dtm <- DocumentTermMatrix(review_corpus)
      if(input$remove_percentage<100) review_dtm = removeSparseTerms(review_dtm, input$remove_percentage/100)
      if(input$remove_percentage==100) review_dtm = removeSparseTerms(review_dtm, 0.99999)
      return(as.matrix(review_dtm))
    }
    if(input$selection == "TFIDF"){
      review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
      review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, input$remove_percentage/100)
      return(as.matrix(review_dtm_tfidf))
    }
    if(input$selection == "bigram"){
      BigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
      review_dtm_bigram <- t(TermDocumentMatrix(review_corpus, control = list(tokenize = BigramTokenizer)))
      review_dtm_bigram = removeSparseTerms(review_dtm_bigram, input$remove_percentage/100)
      return(as.matrix(review_dtm_bigram))
    }
    if(input$selection == "trigram"){
      TrigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
      review_dtm_trigram <- DocumentTermMatrix(review_corpus, control = list(tokenize = TrigramTokenizer))
      review_dtm_trigram = removeSparseTerms(review_dtm_trigram, input$remove_percentage/100)
      return(as.matrix(review_dtm_trigram))
    }

    if(input$selection == "topics"){
      review_dtm <- DocumentTermMatrix(review_corpus)
      if(input$remove_percentage<100) review_dtm = removeSparseTerms(review_dtm, input$remove_percentage/100)
      if(input$remove_percentage==100) review_dtm = removeSparseTerms(review_dtm, 0.99999)

      #Set parameters for Gibbs sampling
      burnin <- 4000
      iter <- 2000
      thin <- 500
      seed <-1988
      nstart <- 1
      best <- TRUE


      #Number of topics
      k <- input$nr_topics

      raw.sum=apply(review_dtm,1,FUN=sum) #sum by raw each raw of the table
      review_dtm=review_dtm[raw.sum!=0,]

      #Run LDA using Gibbs sampling
      withProgress({
        setProgress(message = "Identifying topics...")
        ldaOut <-LDA(review_dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
      })
      topicProbabilities <- as.data.frame(ldaOut@gamma)
      fitted_models_ensemble$topicsmodel = ldaOut
      return(topicProbabilities)
    }

  }

  })


  final_data<-reactive({

    if(input$type=="Abstract"){
    if(!is.null(myData())){
      abstracts_final<-myData()
      #abstracts_final<-abstracts_final[abstracts_final$Abstract[]!="",]
      dtm<-final_dtm()
      abstracts_final$Abstract =NULL

      abstracts_final = cbind(abstracts_final,dtm )  # select the desired term-document matrix
      abstracts_final$Indicator<-as.factor(abstracts_final$Indicator)

      if(input$selection!="topics") abstracts_final<-abstracts_final[,c('Indicator',attr(dtm,"dimnames")$Terms)]
      if(input$selection=="topics")  abstracts_final<-abstracts_final[,c('Indicator',paste0("V",1:input$nr_topics))]

    }
    levels(abstracts_final$Indicator)<-c("irrelevant","relevant")
    names(abstracts_final)<-make.names(names(abstracts_final))
    return(abstracts_final)
    }

    if(input$type=="Full text"){
       if(!is.null(FullTexts())){
        abstracts_final<-myData()
        #abstracts_final<-abstracts_final[abstracts_final$Abstract[]!="",]
        dtm<-final_dtm()
        if(input$selection=="topics") use_dtm<-as.data.frame(dtm)
        use_dtm<- as.matrix(dtm)
        use_dtm<-as.data.frame(use_dtm)
        use_dtm$Name_PDF<- inFileFull()$name
        abstracts_final<-merge(abstracts_final,use_dtm,by = "Name_PDF")
        abstracts_final$Indicator<-as.factor(abstracts_final$Indicator)

        if(input$selection!="topics") abstracts_final<-abstracts_final[,c('Indicator',attr(dtm,"dimnames")$Terms)]
        if(input$selection=="topics")  abstracts_final<-abstracts_final[,c('Indicator',paste0("V",1:input$nr_topics))]
      }
      levels(abstracts_final$Indicator)<-c("irrelevant","relevant")
      names(abstracts_final)<-make.names(names(abstracts_final))
      return(abstracts_final)
    }
  })

  ## Summary of the data


  output$relevance_plot <- renderPlot({
    if (is.null(myData())) {return()}
    if(input$type=="Full text" & is.null(FullTexts())){ return()}
    if((input$type=="Full text" & !is.null(FullTexts()))|!is.null(myData())){
      withProgress({
        setProgress(message = "Processing corpus...")
        interim<-final_data()
      })
      abstracts_final<-myData()
      abstracts_final$Indicator<-as.factor(abstracts_final$Indicator)
      levels(abstracts_final$Indicator)<-c("irrelevant","relevant")
      xx<-barplot(table(abstracts_final$Indicator),main="Distribution of abstracts",ylim=c(0,max(table(abstracts_final$Indicator))+500))
      text(x = xx, y = table(abstracts_final$Indicator), label = paste(paste(table(abstracts_final$Indicator),round(prop.table(table(abstracts_final$Indicator)),4),sep=" (percentage= "),")",sep="")
           , pos = 3, cex = 0.8, col = "red")
    }
  })




   output$info_topics <- renderTable({
    if(input$show2==TRUE) {
      as.matrix(terms(fitted_models_ensemble$topicsmodel,6))
    }
  })


  output$summary <- DT::renderDataTable({
    if(input$show==TRUE) {
      dat<-myData()
      ix <- which("Indicator" == colnames(dat))
      clean <- dat[,-ix]
      features<-final_data()
      if(input$selection=="topics"){
      use_features<-as.matrix(features[,-1])
      toptopics <- round(apply(use_features,1,which.max),0)
      features<- cbind(features,toptopics)
      }
      show_data<-cbind(clean,features)
      showing<-data.frame(show_data)
      showing
    }
  })





  # # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)


  output$plot <- renderPlot({
    if(input$make_wordcloud==FALSE){
      return()}
    if(input$make_wordcloud==TRUE){
      final_dtm<-final_dtm()
      if(!is.null(input$freq)&!is.null(input$max)){
      freq = data.frame(sort(colSums(as.matrix(final_dtm)), decreasing=TRUE))
      wordcloud(rownames(freq), freq[,1],
                    min.freq = input$freq, max.words=input$max,scale=c(4,0.2),
                    colors=brewer.pal(4, "Dark2"))
    }}
  })


  splitting_data<-reactive({
    abstracts_final<-final_data()

    set.seed(1988)
    splitIndex <- createDataPartition(abstracts_final$Indicator, p = as.numeric(input$train_percentage/100),
                                      list = FALSE,
                                      times = 1)
    trainSplit <- abstracts_final[splitIndex,]
    testSplit <- abstracts_final[-splitIndex,]
    #testSplit[setdiff(names(trainSplit),names(testSplit))]<-0

    splitted<-list("train" = trainSplit,"test" = testSplit)
    splitted
  })

  fit_svm<-reactive({
    input$build
    # ...but not for anything else


    F1measureCaret<-function (data, lev = NULL, model = NULL,...)
    {
      # adaptation of twoClassSummary

      if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
        stop("levels of observed and predicted data do not match")
      F1Object <- try(confusionMatrix(data = data[, "pred"], reference = data[, "obs"],positive="relevant"),silent=TRUE)
      hmeasH <- if (class(F1Object)[1] == "try-error") {
        NA
      } else {F1Object$byClass["F1"]  #hObject$metrics[c('H')] returns a dataframe, need to return a vector
      }
      out<-hmeasH
      names(out) <- c("F1")
      print(out)
      out
    }

    isolate({

      svm_grid = NULL
      svm_metric = ifelse(input$tune_svm,input$metric,"Kappa")
      if(input$tune_svm & input$kernel == "Linear")  svm_grid<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1,","))))
      if(input$tune_svm & input$kernel == "Radial")  svm_grid<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1,","))), sigma = as.numeric(unlist(strsplit(input$vec2,","))))
      if(input$tune_svm & input$kernel == "Polynomial")  svm_grid<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1,","))), degree= as.numeric(unlist(strsplit(input$vec2,","))), scale= as.numeric(unlist(strsplit(input$vec3,","))))


      if(input$tune_svm){
        cctrl1 <- trainControl(method = input$method, number=input$number_CV, repeats = ifelse(input$method=="RepeatedCV",input$number_rep,0))

      }else{
        cctrl1 <- trainControl(method = "none", number=0)
      }


      if(input$imbalanced_solution!="None") {
        if(input$tune_svm){
          cctrl1 <- trainControl(method = input$method, number=input$number_CV, repeats = ifelse(input$method=="RepeatedCV",input$number_rep,0),sampling = tolower(input$imbalanced_solution))
        }else{
          cctrl1 <- trainControl(method = "none", number=0,sampling = tolower(input$imbalanced_solution))
        }
      }

      if(input$tune_svm){
        if(input$metric == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_svm,input$method,"none"), number=ifelse(input$tune_svm,input$number_CV,0), repeats = ifelse(input$method=="RepeatedCV",input$number_rep,0), summaryFunction = F1measureCaret)}
      }

      if(input$tune_svm&input$imbalanced_solution!="None"){
        if(input$metric == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_svm,input$method,"none"), number=ifelse(input$tune_svm,input$number_CV,0), repeats = ifelse(input$method=="RepeatedCV",input$number_rep,0), summaryFunction = F1measureCaret,sampling = tolower(input$imbalanced_solution))}
      }


      if(input$kernel == "Linear") {use_method = "svmLinear"}
      if(input$kernel == "Radial") {use_method = "svmRadial"}
      if(input$kernel == "Polynomial") {use_method = "svmPoly"}

      withProgress({
        setProgress(message = "Creating input space...")
      splitting_data<- splitting_data()$train
      })

      withProgress({
        setProgress(message = "Building classifier...")
        set.seed(123)

        #if(input$imbalanced_solution=="ROSE") {use_data <- ROSE(Indicator ~ . , data=splitting_data, seed=3)$data}
        #if(input$imbalanced_solution!="ROSE") {use_data <- splitting_data}
        use_data <- splitting_data

        reviews.model<-train(Indicator ~ ., data = use_data, method = use_method,
                             trControl = cctrl1, tuneGrid = svm_grid , metric= svm_metric)
        reviews.model
      })
    })})

  output$fitted_svm2 <- renderPrint({
    if(input$build){

      isolate({ test_SVM <- fit_svm()

      predictors <- names(splitting_data()$train)[names(splitting_data()$train) != 'Indicator']
      withProgress({
        setProgress(message = "Predicting on test set...")
        pred.svm<- predict(test_SVM$finalModel, splitting_data()$test[,predictors])
      })

      tested.svm<-confusionMatrix(data = pred.svm, reference = splitting_data()$test$Indicator,positive="relevant")
      storage<- data.frame("Performance"= round(c(tested.svm$byClass,tested.svm$overall),4))
      print(tested.svm)
      print(storage)
      kernel.used<-  paste0("Kernel=",input$kernel)
      smote.used<-  paste0("Solution_Imbalance=",input$imbalanced_solution)
      cost.used<-  paste(paste0("Cost=",param(test_SVM$finalModel)),collapse = "")
      pars.used<-  paste(paste(names(kpar(kernelf(test_SVM$finalModel))),kpar(kernelf(test_SVM$finalModel)),sep="="),collapse = "__")


      names(storage)<-paste(c(input$selection,kernel.used,smote.used,cost.used,pars.used),collapse = "__")
      k<-length(summary_list$current)+1
      summary_list$current[[k]]<- storage

      })
    }
  })


  output$overall_summary <- renderTable({
    if(input$show_overall_summary & length(summary_list$current)){
      data.frame(summary_list$current)
    }
  },rownames=T)



  #GBM




  output$tuning_method_gbm<- renderUI({
    if(input$tune_gbm == FALSE)    return(NULL)
    tagList(
      radioButtons("method_gbm", label = "Select method for tuning", choices = list("CV", "RepeatedCV"),selected = "CV")
    )

  })





  output$tuning_method_options_gbm<- renderUI({
    if(input$tune_gbm == FALSE)    return(NULL)
    if(is.null(input$method_gbm)){return(NULL)}
    if(input$method_gbm=="CV"){
      return(tagList(
        numericInput('number_CV_gbm', "How many CV sets?", 3),
        textInput('vec1_gbm', 'Enter a grid for n.trees parameter (comma delimited)', "10,20,50"),
        textInput('vec2_gbm', 'Enter a grid for shrinkage parameter (comma delimited)', "0.05,0.1,0.2"),
        textInput('vec3_gbm', 'Enter a grid for n.minobsinnode parameter (comma delimited)', "3,5,10"),
        textInput('vec4_gbm', 'Enter a grid for interaction.depth (comma delimited)', "5,10")
      ) )
    }


    if(input$method_gbm=="RepeatedCV"){
      return(tagList(
        numericInput('number_CV_gbm', "How many CV sets?", 3),
        numericInput('number_rep_gbm', "How many repetitions?", 3),
        textInput('vec1_gbm', 'Enter a grid for n.trees parameter (comma delimited)', "10,20,50"),
        textInput('vec2_gbm', 'Enter a grid for shrinkage parameter (comma delimited)', "0.05,0.1,0.2"),
        textInput('vec3_gbm', 'Enter a grid for n.minobsinnode parameter (comma delimited)', "3,5,10"),
        textInput('vec4_gbm', 'Enter a grid for interaction.depth (comma delimited)', "5,10")
      ) )
    }

  })



  output$tuning_metric_gbm<- renderUI({
    if(input$tune_gbm == FALSE)    return(NULL)
    tagList(
      radioButtons("metric_gbm", label = "Select metric for tuning", choices = list("Kappa", "Accuracy","F1"),selected = "Kappa")
    )

  })




  fit_gbm<-reactive({
    input$build_gbm
    # ...but not for anything else
    isolate({
      gbm_grid = NULL




      F1measureCaret<-function (data, lev = NULL, model = NULL,...)
      {
        # adaptation of twoClassSummary

        if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
          stop("levels of observed and predicted data do not match")
        F1Object <- try(confusionMatrix(data = data[, "pred"], reference = data[, "obs"],positive="relevant"),silent=TRUE)
        hmeasH <- if (class(F1Object)[1] == "try-error") {

        } else {F1Object$byClass["F1"]  #hObject$metrics[c('H')] returns a dataframe, need to return a vector
        }
        out<-hmeasH
        names(out) <- c("F1")
        print(out)
        out
      }


      gbm_metric = ifelse(input$tune_gbm,input$metric_gbm,"Kappa")

      if(input$tune_gbm)  gbm_grid<-expand.grid(n.trees=as.numeric(unlist(strsplit(input$vec1_gbm,","))),shrinkage=as.numeric(unlist(strsplit(input$vec2_gbm,","))),n.minobsinnode = as.numeric(unlist(strsplit(input$vec3_gbm,","))),interaction.depth=as.numeric(unlist(strsplit(input$vec4_gbm,","))))


      if(input$tune_gbm){
        cctrl1 <- trainControl(method = input$method_gbm, number=input$number_CV_gbm, repeats = ifelse(input$method_gbm=="RepeatedCV",input$number_rep_gbm,0))

      }else{
        cctrl1 <- trainControl(method = "none", number=0)
      }


      if(input$imbalanced_solution_gbm!="None") {
        if(input$tune_gbm){
          cctrl1 <- trainControl(method = input$method_gbm, number=input$number_CV_gbm, repeats = ifelse(input$method_gbm=="RepeatedCV",input$number_rep_gbm,0),sampling = tolower(input$imbalanced_solution_gbm))
        }else{
          cctrl1 <- trainControl(method = "none", number=0,sampling = tolower(input$imbalanced_solution_gbm))
        }
      }


      if(input$tune_gbm&input$imbalanced_solution_gbm!="SMOTE"){
        if(input$metric_gbm == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_gbm,input$method_gbm,"none"), number=ifelse(input$tune_gbm,input$number_CV_gbm,0), repeats = ifelse(input$method_gbm=="RepeatedCV",input$number_rep_gbm,0), summaryFunction = F1measureCaret)}
      }

      if(input$tune_gbm&input$imbalanced_solution_gbm!="None"){
        if(input$metric_gbm == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_gbm,input$method_gbm,"none"), number=ifelse(input$tune_gbm,input$number_CV_gbm,0), repeats = ifelse(input$method_gbm=="RepeatedCV",input$number_rep_gbm,0), summaryFunction = F1measureCaret,sampling = tolower(input$imbalanced_solution_gbm))}
      }



      withProgress({
        setProgress(message = "Building classifier...")

        splitting_data<- splitting_data()$train

        #if(input$imbalanced_solution_gbm=="ROSE") {use_data <- ROSE(Indicator ~ . , data=splitting_data, seed=3)$data}
        #if(input$imbalanced_solution_gbm!="ROSE") {use_data <- splitting_data}

        use_data=splitting_data

        set.seed(123)

        garbage <- capture.output(reviews.model<-train(Indicator ~ ., data = use_data, method = "gbm",
                                                       trControl = cctrl1, tuneGrid = gbm_grid , metric= gbm_metric) )
        reviews.model
      })
    })})

  output$fitted_gbm <- renderPrint({
    if(input$build_gbm){

      isolate({
        test_GBM <- fit_gbm()
        predictors <- names(splitting_data()$train)[names(splitting_data()$train) != 'Indicator']
        withProgress({
          setProgress(message = "Predicting on test set...")
          pred.gbm<- predict(test_GBM, splitting_data()$test[,predictors])
        })

        tested.gbm<-confusionMatrix(data = pred.gbm, reference = splitting_data()$test$Indicator,positive="relevant")
        storage<- data.frame("Performance"= round(c(tested.gbm$byClass,tested.gbm$overall),4))
        print(tested.gbm)
        print(test_GBM)
        print(storage)
        method.used<-"GBM"
        smote.used<-  paste0("Solution_Imbalance=",input$imbalanced_solution_gbm)
        pars.used<-paste(paste(c("n.trees=","shrinkage=","n.minobsinnode=","interaction.depth"),c(test_GBM$finalModel$n.trees,test_GBM$finalModel$shrinkage,test_GBM$finalModel$n.minobsinnode,test_GBM$finalModel$interaction.depth),sep=""),collapse = "_")

        names(storage)<-paste(c(method.used,smote.used,pars.used),collapse = "__")
        k<-length(summary_list$current)+1
        summary_list$current[[k]]<- storage

      })
    }
  })

  output$overall_summary_gbm <- renderTable({
    if(input$show_overall_summary_gbm & length(summary_list$current)){
      data.frame(summary_list$current)
    }
  },rownames=T)




  #RF




  output$tuning_method_rf<- renderUI({
    if(input$tune_rf == FALSE)    return(NULL)
    tagList(
      radioButtons("method_rf", label = "Select method for tuning", choices = list("CV", "RepeatedCV"),selected = "CV")
    )

  })


  fit_rf<-reactive({
    input$build_rf
    # ...but not for anything else
    isolate({


      cctrl1 <- trainControl(method = "cv", number=3)
      if(input$imbalanced_solution_rf!="None") {cctrl1 <- trainControl(method ="cv", number=3,sampling = tolower(input$imbalanced_solution_rf))}
      withProgress({
        setProgress(message = "Building classifier...")

        splitting_data<- splitting_data()$train

        #if(input$imbalanced_solution_gbm=="ROSE") {use_data <- ROSE(Indicator ~ . , data=splitting_data, seed=3)$data}
        #if(input$imbalanced_solution_gbm!="ROSE") {use_data <- splitting_data}

        use_data=splitting_data

        set.seed(123)

        garbage <- capture.output(reviews.model<-train(Indicator ~ ., data = use_data, method = "ranger",   trControl = cctrl1) )
        reviews.model
      })
    })})

  output$fitted_rf <- renderPrint({
    if(input$build_rf){

      isolate({
        test_rf <- fit_rf()
        predictors <- names(splitting_data()$train)[names(splitting_data()$train) != 'Indicator']
        withProgress({
          setProgress(message = "Predicting on test set...")
          pred.rf<- predict(test_rf, splitting_data()$test[,predictors])
        })

        tested.rf<-confusionMatrix(data = pred.rf, reference = splitting_data()$test$Indicator,positive="relevant")
        storage<- data.frame("Performance"= round(c(tested.rf$byClass,tested.rf$overall),4))
        print(tested.rf)
        print(test_rf)
        print(storage)
        method.used<-"rf"
        smote.used<-  paste0("Solution_Imbalance=",input$imbalanced_solution_rf)
        names(storage)<-paste(c(method.used,smote.used),collapse = "__")
        k<-length(summary_list$current)+1
        summary_list$current[[k]]<- storage

      })
    }
  })

  output$overall_summary_rf <- renderTable({
    if(input$show_overall_summary_rf & length(summary_list$current)){
      data.frame(summary_list$current)
    }
  },rownames=T)




  #NN




  output$tuning_method_nn<- renderUI({
    if(input$tune_nn == FALSE)    return(NULL)
    tagList(
      radioButtons("method_nn", label = "Select method for tuning", choices = list("CV", "RepeatedCV"),selected = "CV")
    )

  })





  output$tuning_method_options_nn<- renderUI({
    if(input$tune_nn == FALSE)    return(NULL)
    if(is.null(input$method_nn)){return(NULL)}
    if(input$method_nn=="CV"){
      return(tagList(
        numericInput('number_CV_nn', "How many CV sets?", 3),
        textInput('vec1_nn', 'Enter a grid for size parameter (comma delimited)', "1,5,10"),
        textInput('vec2_nn', 'Enter a grid for decay parameter (comma delimited)', "0.1,0.2,0.5")
      ) )
    }


    if(input$method_nn=="RepeatedCV"){
      return(tagList(
        numericInput('number_CV_nn', "How many CV sets?", 3),
        numericInput('number_rep_nn', "How many repetitions?", 3),
        textInput('vec1_nn', 'Enter a grid for size parameter (comma delimited)', "1,5,10"),
        textInput('vec2_nn', 'Enter a grid for decay parameter (comma delimited)', "0.1,0.2,0.5")
      ) )
    }

  })



  output$tuning_metric_nn<- renderUI({
    if(input$tune_nn == FALSE)    return(NULL)
    tagList(
      radioButtons("metric_nn", label = "Select metric for tuning", choices = list("Kappa", "Accuracy","F1"),selected = "Kappa")
    )

  })




  fit_nn<-reactive({
    input$build_nn
    # ...but not for anything else
    isolate({
      nn_grid = NULL




      F1measureCaret<-function (data, lev = NULL, model = NULL,...)
      {
        # adaptation of twoClassSummary

        if (!all(levels(data[, "pred"]) == levels(data[, "obs"])))
          stop("levels of observed and predicted data do not match")
        F1Object <- try(confusionMatrix(data = data[, "pred"], reference = data[, "obs"],positive="relevant"),silent=TRUE)
        hmeasH <- if (class(F1Object)[1] == "try-error") {

        } else {F1Object$byClass["F1"]  #hObject$metrics[c('H')] returns a dataframe, need to return a vector
        }
        out<-hmeasH
        names(out) <- c("F1")
        print(out)
        out
      }


      nn_metric = ifelse(input$tune_nn,input$metric_nn,"Kappa")

      if(input$tune_nn)  nn_grid<-expand.grid(size=as.numeric(unlist(strsplit(input$vec1_nn,","))),decay=as.numeric(unlist(strsplit(input$vec2_nn,","))))



      if(input$tune_nn){
        cctrl1 <- trainControl(method = input$method_nn, number=input$number_CV_nn, repeats = ifelse(input$method_nn=="RepeatedCV",input$number_rep_nn,0))

      }else{
        cctrl1 <- trainControl(method = "none", number=0)
      }


      if(input$imbalanced_solution_nn!="None") {
        if(input$tune_nn){
          cctrl1 <- trainControl(method = input$method_nn, number=input$number_CV_nn, repeats = ifelse(input$method_nn=="RepeatedCV",input$number_rep_nn,0),sampling = tolower(input$imbalanced_solution_nn))
        }else{
          cctrl1 <- trainControl(method = "none", number=0,sampling = tolower(input$imbalanced_solution_nn))
        }
      }




      if(input$tune_nn&input$imbalanced_solution_nn!="SMOTE"){
        if(input$metric_nn == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_nn,input$method_nn,"none"), number=ifelse(input$tune_nn,input$number_CV_nn,0), repeats = ifelse(input$method_nn=="RepeatedCV",input$number_rep_nn,0), summaryFunction = F1measureCaret)}
      }

      if(input$tune_nn&input$imbalanced_solution_nn!="None"){
        if(input$metric_nn == "F1") {cctrl1 <- trainControl(method = ifelse(input$tune_nn,input$method_nn,"none"), number=ifelse(input$tune_nn,input$number_CV_nn,0), repeats = ifelse(input$method_nn=="RepeatedCV",input$number_rep_nn,0), summaryFunction = F1measureCaret,sampling = tolower(input$imbalanced_solution_nn))}
      }



      withProgress({
        setProgress(message = "Building classifier...")

        splitting_data<- splitting_data()$train

        #if(input$imbalanced_solution_gbm=="ROSE") {use_data <- ROSE(Indicator ~ . , data=splitting_data, seed=3)$data}
        #if(input$imbalanced_solution_gbm!="ROSE") {use_data <- splitting_data}

        use_data=splitting_data

        set.seed(5627)

        garbage <- capture.output(reviews.model<-train(Indicator ~ ., data = use_data, method = "nnet",
                                                       trControl = cctrl1, tuneGrid = nn_grid , metric= nn_metric) )
        reviews.model
      })
    })})

  output$fitted_nn <- renderPrint({
    if(input$build_nn){

      isolate({
        test_nn <- fit_nn()
        predictors <- names(splitting_data()$train)[names(splitting_data()$train) != 'Indicator']
        withProgress({
          setProgress(message = "Predicting on test set...")
          pred.nn<- predict(test_nn, splitting_data()$test[,predictors])
        })

        tested.nn<-confusionMatrix(data = pred.nn, reference = splitting_data()$test$Indicator,positive="relevant")
        storage<- data.frame("Performance"= round(c(tested.nn$byClass,tested.nn$overall),4))
        print(tested.nn)
        print(test_nn)
        print(storage)
        method.used<-"NN"
        smote.used<-  paste0("Solution_Imbalance=",input$imbalanced_solution_nn)
        pars.used<-paste(paste(c("size=","decay="),c(test_nn$finalModel$size,test_nn$finalModel$decay),sep=""),collapse = "_")

        names(storage)<-paste(c(method.used,smote.used,pars.used),collapse = "__")
        k<-length(summary_list$current)+1
        summary_list$current[[k]]<- storage

      })
    }
  })

  output$overall_summary_nn <- renderTable({
    if(input$show_overall_summary_nn & length(summary_list$current)){
      data.frame(summary_list$current)
    }
  },rownames=T)



  ## Ensemble

  output$SVM_Linear_Imbalance<- renderUI({
    if(input$SVM_Linear == FALSE)    return(NULL)
    if(input$SVM_Linear == TRUE){
      return(
        checkboxGroupInput("sampling_SVM_Linear", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })


  output$SVM_Linear_Grid<- renderUI({
    if(input$SVM_Linear == FALSE)    return(NULL)
    if(input$SVM_Linear == TRUE){

      return(
        tagList(
          textInput('vec1_linear_ensemble', 'Enter a grid for cost parameter (comma delimited)', "0.00001,0.0001,0.001,0.01,1,10")
        )
      )

    }    })


  output$SVM_Poly_Imbalance<- renderUI({
    if(input$SVM_Polynomial == FALSE)    return(NULL)
    if(input$SVM_Polynomial == TRUE){
      return(
        checkboxGroupInput("sampling_SVM_Polynomial", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })

  output$SVM_Poly_Grid<- renderUI({
    if(input$SVM_Polynomial == FALSE)    return(NULL)
    if(input$SVM_Polynomial == TRUE){

      return(
        tagList(
          textInput('vec1_poly_ensemble', 'Enter a grid for cost parameter (comma delimited)', "1,2,5"),
          textInput('vec2_poly_ensemble', 'Enter a grid for degree parameter (comma delimited)', "1,2,5"),
          textInput('vec3_poly_ensemble', 'Enter a grid for scale parameter (comma delimited)', "0.5,1,2")
        )
      )

    }    })


  output$SVM_Radial_Imbalance<- renderUI({
    if(input$SVM_Radial == FALSE)    return(NULL)
    if(input$SVM_Radial == TRUE){
      return(
        checkboxGroupInput("sampling_SVM_Radial", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })

  output$SVM_Radial_Grid<- renderUI({
    if(input$SVM_Radial == FALSE)    return(NULL)
    if(input$SVM_Radial == TRUE){

      return(
        tagList(
          textInput('vec1_radial_ensemble', 'Enter a grid for cost parameter (comma delimited)', "0.01,0.1,1,5"),
          textInput('vec2_radial_ensemble', 'Enter a grid for sigma parameter (comma delimited)', ".0001,0.001,0.01,0.1")
        )
      )

    }    })


  output$GBM_Imbalance<- renderUI({
    if(input$GBM == FALSE)    return(NULL)
    if(input$GBM == TRUE){
      return(
        checkboxGroupInput("sampling_GBM", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })

  output$GBM_Grid<- renderUI({
    if(input$GBM == FALSE)    return(NULL)
    if(input$GBM == TRUE){

      return(
        tagList(
          textInput('vec1_gbm_ensemble', 'Enter a grid for n.trees parameter (comma delimited)', "150,250"),
          textInput('vec2_gbm_ensemble', 'Enter a grid for shrinkage parameter (comma delimited)', "0.1"),
          textInput('vec3_gbm_ensemble', 'Enter a grid for n.minobsinnode parameter (comma delimited)', "3,5"),
          textInput('vec4_gbm_ensemble', 'Enter a grid for interaction.depth (comma delimited)', "10,20")
        )
      )

    }    })


  output$NN_Imbalance<- renderUI({
    if(input$NN == FALSE)    return(NULL)
    if(input$NN == TRUE){
      return(
        checkboxGroupInput("sampling_NN", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })

  output$NN_Grid<- renderUI({
    if(input$NN == FALSE)    return(NULL)
    if(input$NN == TRUE){

      return(
        tagList(
          textInput('vec1_nn_ensemble', 'Enter a grid for size parameter (comma delimited)', "1,5,10"),
          textInput('vec2_nn_ensemble', 'Enter a grid for decay parameter (comma delimited)', "0.1,0.2,0.5")
        )
      )

    }    })


  output$RF_Imbalance<- renderUI({
    if(input$RF == FALSE)    return(NULL)
    if(input$RF == TRUE){
      return(
        checkboxGroupInput("sampling_RF", "Remedial measure for imbalanced dataset?", choices = c("None","SMOTE","ROSE"), selected = "None",
                           inline = TRUE, choiceNames = NULL)
      )

    }    })


  ## Fit all models in the ensemble and put them together


  fit_ensemble<-reactive({

    validate(
      need({input$SVM_Linear != FALSE |input$SVM_Polynomial != FALSE| input$SVM_Radial != FALSE|input$GBM != FALSE |input$NN != FALSE |input$RF != FALSE} , "Please select at least 1 classifier to include in the ensemble")
    )

    clear_list()

    input$build_ensemble
    # ...but not for anything else

    isolate({

      set.seed(123)
      splitting_data<- splitting_data()$train

      #if(input$imbalanced_solution=="ROSE") {use_data <- ROSE(Indicator ~ . , data=splitting_data, seed=3)$data}
      #if(input$imbalanced_solution!="ROSE") {use_data <- splitting_data}
      use_data <- splitting_data
      timing<-proc.time()
      if(input$SVM_Linear == TRUE & "None" %in% input$sampling_SVM_Linear)  {
        withProgress({
          setProgress(message = "Building SVM Linear without additional sampling...")
          svm_grid_Linear_ensemble<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1_linear_ensemble,","))))
          set.seed(5627)
          svm_Linear_orig <- train(Indicator ~ ., data = use_data, method = "svmLinear",metric = "Kappa",
                                   tuneGrid = svm_grid_Linear_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Linear_orig
          names(fitted_models_ensemble$current)[k]<-"svm_Linear_orig"
        })
      }

      if(input$SVM_Linear == TRUE & "SMOTE" %in% input$sampling_SVM_Linear)  {
        withProgress({
          setProgress(message = "Building SVM Linear with SMOTE...")
          svm_grid_Linear_ensemble<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1_linear_ensemble,","))))
          set.seed(5627)
          svm_Linear_smote <- train(Indicator ~ ., data = use_data, method = "svmLinear",metric = "Kappa",
                                    tuneGrid = svm_grid_Linear_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "smote",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Linear_smote
          names(fitted_models_ensemble$current)[k]<-"svm_Linear_smote"

        })
      }

      if(input$SVM_Linear == TRUE & "ROSE" %in% input$sampling_SVM_Linear)  {
        withProgress({
          setProgress(message = "Building SVM Linear with ROSE...")
          svm_grid_Linear_ensemble<-expand.grid( C = as.numeric(unlist(strsplit(input$vec1_linear_ensemble,","))))
          set.seed(5627)
          svm_Linear_rose <- train(Indicator ~ ., data = use_data, method = "svmLinear",metric = "Kappa",
                                   tuneGrid = svm_grid_Linear_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "rose",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Linear_rose
          names(fitted_models_ensemble$current)[k]<-"svm_Linear_rose"
        })
      }


      if(input$SVM_Polynomial == TRUE& "None" %in% input$sampling_SVM_Polynomial)  {
        withProgress({
          setProgress(message = "Building SVM Polynomial without additional sampling...")

          svm_grid_Poly_ensemble<-  expand.grid( C = as.numeric(unlist(strsplit(input$vec1_poly_ensemble,","))), degree= as.numeric(unlist(strsplit(input$vec2_poly_ensemble,","))), scale= as.numeric(unlist(strsplit(input$vec3_poly_ensemble,","))))
          set.seed(5627)
          svm_Poly_orig <-  train(Indicator ~ ., data = use_data, method = "svmPoly",metric = "Kappa",
                                  tuneGrid = svm_grid_Poly_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL ,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Poly_orig
          names(fitted_models_ensemble$current)[k]<-"svm_Poly_orig"
        })
      }


      if(input$SVM_Polynomial == TRUE& "SMOTE" %in% input$sampling_SVM_Polynomial)  {
        withProgress({
          setProgress(message = "Building SVM Polynomial with SMOTE...")

          svm_grid_Poly_ensemble<-  expand.grid( C = as.numeric(unlist(strsplit(input$vec1_poly_ensemble,","))), degree= as.numeric(unlist(strsplit(input$vec2_poly_ensemble,","))), scale= as.numeric(unlist(strsplit(input$vec3_poly_ensemble,","))))
          set.seed(5627)
          svm_Poly_smote <-  train(Indicator ~ ., data = use_data, method = "svmPoly",metric = "Kappa",
                                   tuneGrid = svm_grid_Poly_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "smote" ,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Poly_smote
          names(fitted_models_ensemble$current)[k]<-"svm_Poly_smote"
        })
      }

      if(input$SVM_Polynomial == TRUE& "ROSE" %in% input$sampling_SVM_Polynomial)  {
        withProgress({
          setProgress(message = "Building SVM Polynomial with ROSE...")

          svm_grid_Poly_ensemble<-  expand.grid( C = as.numeric(unlist(strsplit(input$vec1_poly_ensemble,","))), degree= as.numeric(unlist(strsplit(input$vec2_poly_ensemble,","))), scale= as.numeric(unlist(strsplit(input$vec3_poly_ensemble,","))))
          set.seed(5627)
          svm_Poly_rose <-  train(Indicator ~ ., data = use_data, method = "svmPoly",metric = "Kappa",
                                  tuneGrid = svm_grid_Poly_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "rose" ,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Poly_rose
          names(fitted_models_ensemble$current)[k]<-"svm_Poly_rose"
        })
      }


      if(input$SVM_Radial == TRUE& "None" %in% input$sampling_SVM_Radial)  {
        withProgress({
          setProgress(message = "Building SVM Radial without additional sampling...")

          svm_grid_Radial_ensemble<- expand.grid( C = as.numeric(unlist(strsplit(input$vec1_radial_ensemble,","))), sigma = as.numeric(unlist(strsplit(input$vec2_radial_ensemble,","))))

          set.seed(5627)
          svm_Radial_orig <- train(Indicator ~ ., data = use_data, method = "svmRadial",metric = "Kappa",
                                   tuneGrid = svm_grid_Radial_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Radial_orig
          names(fitted_models_ensemble$current)[k]<-"svm_Radial_orig"
        })
      }

      if(input$SVM_Radial == TRUE& "SMOTE" %in% input$sampling_SVM_Radial)  {
        withProgress({
          setProgress(message = "Building SVM Radial with SMOTE...")

          svm_grid_Radial_ensemble<- expand.grid( C = as.numeric(unlist(strsplit(input$vec1_radial_ensemble,","))), sigma = as.numeric(unlist(strsplit(input$vec2_radial_ensemble,","))))

          set.seed(5627)
          svm_Radial_smote <- train(Indicator ~ ., data = use_data, method = "svmRadial",metric = "Kappa",
                                    tuneGrid = svm_grid_Radial_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "smote",savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- svm_Radial_smote
          names(fitted_models_ensemble$current)[k]<-"svm_Radial_smote"
        })
      }

      if(input$SVM_Radial == TRUE& "ROSE" %in% input$sampling_SVM_Radial)  {
        withProgress({
          setProgress(message = "Building SVM Radial with ROSE...")

          svm_grid_Radial_ensemble<- expand.grid( C = as.numeric(unlist(strsplit(input$vec1_radial_ensemble,","))), sigma = as.numeric(unlist(strsplit(input$vec2_radial_ensemble,","))))

          set.seed(5627)
          svm_Radial_rose <- train(Indicator ~ ., data = use_data, method = "svmRadial",metric = "Kappa",
                                   tuneGrid = svm_grid_Radial_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))

          k<-length(fitted_models_ensemble$current)+1
          print(k)
          fitted_models_ensemble$current[[k]]<- svm_Radial_rose
          names(fitted_models_ensemble$current)[k]<-"svm_Radial_rose"
        })
      }



      if(input$GBM == TRUE& "None" %in% input$sampling_GBM)  {
        withProgress({
          setProgress(message = "Building GBM without additional sampling...")
          GBM_grid_ensemble<- expand.grid(n.trees=as.numeric(unlist(strsplit(input$vec1_gbm_ensemble,","))),shrinkage=as.numeric(unlist(strsplit(input$vec2_gbm_ensemble,","))),n.minobsinnode = as.numeric(unlist(strsplit(input$vec3_gbm_ensemble,","))),interaction.depth=as.numeric(unlist(strsplit(input$vec4_gbm_ensemble,","))))
          set.seed(5627)
          garbage <- capture.output(gbm_orig <- train(Indicator ~ ., data = use_data, method = "gbm",metric = "Kappa",
                                                      tuneGrid = GBM_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- gbm_orig
          names(fitted_models_ensemble$current)[k]<-"GBM_orig"
        })
      }

      if(input$GBM == TRUE& "SMOTE" %in% input$sampling_GBM)  {
        withProgress({
          setProgress(message = "Building GBM with SMOTE...")
          GBM_grid_ensemble<- expand.grid(n.trees=as.numeric(unlist(strsplit(input$vec1_gbm_ensemble,","))),shrinkage=as.numeric(unlist(strsplit(input$vec2_gbm_ensemble,","))),n.minobsinnode = as.numeric(unlist(strsplit(input$vec3_gbm_ensemble,","))),interaction.depth=as.numeric(unlist(strsplit(input$vec4_gbm_ensemble,","))))
          set.seed(5627)
          garbage <- capture.output(gbm_smote <- train(Indicator ~ ., data = use_data, method = "gbm",metric = "Kappa",
                                                       tuneGrid = GBM_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "smote",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- gbm_smote
          names(fitted_models_ensemble$current)[k]<-"GBM_smote"
        })
      }

      if(input$GBM == TRUE& "ROSE" %in% input$sampling_GBM)  {
        withProgress({
          setProgress(message = "Building GBM with ROSE...")
          GBM_grid_ensemble<- expand.grid(n.trees=as.numeric(unlist(strsplit(input$vec1_gbm_ensemble,","))),shrinkage=as.numeric(unlist(strsplit(input$vec2_gbm_ensemble,","))),n.minobsinnode = as.numeric(unlist(strsplit(input$vec3_gbm_ensemble,","))),interaction.depth=as.numeric(unlist(strsplit(input$vec4_gbm_ensemble,","))))
          set.seed(5627)
          garbage <- capture.output(gbm_rose <- train(Indicator ~ ., data = use_data, method = "gbm",metric = "Kappa",
                                                      tuneGrid = GBM_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "rose",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )
          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- gbm_rose
          names(fitted_models_ensemble$current)[k]<-"GBM_rose"
        })
      }


      if(input$NN == TRUE& "None" %in% input$sampling_NN)  {
        withProgress({
          setProgress(message = "Building NN without additional sampling...")
          NN_grid_ensemble<-expand.grid(size = as.numeric(unlist(strsplit(input$vec1_nn_ensemble,","))),
                                        decay = as.numeric(unlist(strsplit(input$vec2_nn_ensemble,","))))

          set.seed(5627)
          garbage <- capture.output( nn_orig <- train(Indicator ~ ., data = use_data, method = "nnet",metric = "Kappa",
                                                      tuneGrid = NN_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))
          )


          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- nn_orig
          names(fitted_models_ensemble$current)[k]<-"NN_orig"
        })
      }


      if(input$NN == TRUE& "SMOTE" %in% input$sampling_NN)  {
        withProgress({
          setProgress(message = "Building NN with SMOTE...")
          NN_grid_ensemble<-expand.grid(size = as.numeric(unlist(strsplit(input$vec1_nn_ensemble,","))),
                                        decay = as.numeric(unlist(strsplit(input$vec2_nn_ensemble,","))))

          set.seed(5627)

          garbage <- capture.output(  nn_smote <- train(Indicator ~ ., data = use_data, method = "nnet",metric = "Kappa",
                                                        tuneGrid = NN_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "smote",savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))

          )

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- nn_smote
          names(fitted_models_ensemble$current)[k]<-"NN_smote"
        })
      }

      if(input$NN == TRUE& "ROSE" %in% input$sampling_NN)  {
        withProgress({
          setProgress(message = "Building NN with ROSE...")
          NN_grid_ensemble<-expand.grid(size = as.numeric(unlist(strsplit(input$vec1_nn_ensemble,","))),
                                        decay = as.numeric(unlist(strsplit(input$vec2_nn_ensemble,","))))

          set.seed(5627)

          garbage <- capture.output(  nn_rose <- train(Indicator ~ ., data = use_data, method = "nnet",metric = "Kappa",
                                                       tuneGrid = NN_grid_ensemble, trControl = trainControl(method = "cv", number = 3, sampling= "rose",savePredictions = "final",classProbs=TRUE,index = createResample(use_data$Indicator,3)))        )




          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- nn_rose
          names(fitted_models_ensemble$current)[k]<-"NN_rose"
        })
      }


      if(input$RF == TRUE& "None" %in% input$sampling_RF)  {
        withProgress({
          setProgress(message = "Building RF without additional sampling...")
          set.seed(5627)

          garbage <- capture.output(rf_orig <- train(Indicator ~ ., data = use_data, method = "ranger",metric = "Kappa",
                                                     trControl = trainControl(method = "cv", number = 3, sampling= NULL,savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- rf_orig
          names(fitted_models_ensemble$current)[k]<-"RF_orig"
        })
      }

      if(input$RF == TRUE& "SMOTE" %in% input$sampling_RF)  {
        withProgress({
          setProgress(message = "Building RF with SMOTE...")
          set.seed(5627)

          garbage <- capture.output(rf_smote <- train(Indicator ~ ., data = use_data, method = "ranger",metric = "Kappa",
                                                      trControl = trainControl(method = "cv", number = 3, sampling= "smote",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- rf_smote
          names(fitted_models_ensemble$current)[k]<-"RF_smote"
        })
      }

      if(input$RF == TRUE& "ROSE" %in% input$sampling_RF)  {
        withProgress({
          setProgress(message = "Building RF with ROSE...")
          set.seed(5627)

          garbage <- capture.output(rf_rose <- train(Indicator ~ ., data = use_data, method = "ranger",metric = "Kappa",
                                                     trControl = trainControl(method = "cv", number = 3, sampling= "rose",savePredictions = "final",classProbs = TRUE,index = createResample(use_data$Indicator,3)))
          )

          k<-length(fitted_models_ensemble$current)+1
          fitted_models_ensemble$current[[k]]<- rf_rose
          names(fitted_models_ensemble$current)[k]<-"RF_rose"

        })
      }

      running_time<-paste("Computation time:", paste(paste(round((proc.time()-timing)[3],2),"seconds"),paste(round((proc.time()-timing)[3]/60,2),"minutes"),sep=", i.e. "))
      fitted_models_ensemble$timing<-running_time
      fitted_models_ensemble$current

    })})


  output$fitted_ensemble2 <- renderPrint({

    test_measures <- function(model, data) {
      tab <- confusionMatrix(data = predict(model, data), reference = splitting_data()$test$Indicator,positive = 'relevant')
      c(tab$overall,tab$byClass)
    }



    if(input$build_ensemble){

      isolate({ fitted_models <- fit_ensemble()
      models_test <- lapply(fitted_models, test_measures, data = splitting_data()$test)
      models_test <- lapply(models_test, as.vector)
      models_test <- do.call("rbind", models_test)
      colnames(models_test) <- c(  "Accuracy"   ,          "Kappa"   ,             "AccuracyLower"   ,     "AccuracyUpper" ,
                                   "AccuracyNull"       ,  "AccuracyPValue"  ,     "McnemarPValue"  ,      "Sensitivity"  ,
                                   "Specificity"     ,     "Pos Pred Value" ,      "Neg Pred Value" ,      "Precision" ,
                                   "Recall"        ,       "F1"      ,             "Prevalence" ,          "Detection Rate"  ,
                                   "Detection Prevalence" ,"Balanced Accuracy")
      models_test <- as.data.frame(models_test)
     models_test
      })
    }
  })


  output$mytable = DT::renderDataTable({
    test_measures <- function(model, data) {
      tab <- confusionMatrix(data = predict(model, data), reference = splitting_data()$test$Indicator,positive = 'relevant')
      #print(tab)
      c(tab$overall,tab$byClass)
    }




    if(input$build_ensemble){

      isolate({ fitted_models <- fit_ensemble()

      models_test <- lapply(fitted_models, test_measures, data = splitting_data()$test)
      models_test <- lapply(models_test, as.vector)
      models_test <- do.call("rbind", models_test)
      colnames(models_test) <- c(  "Accuracy"   ,          "Kappa"   ,             "AccuracyLower"   ,     "AccuracyUpper" ,
                                   "AccuracyNull"       ,  "AccuracyPValue"  ,     "McnemarPValue"  ,      "Sensitivity"  ,
                                   "Specificity"     ,     "Pos Pred Value" ,      "Neg Pred Value" ,      "Precision" ,
                                   "Recall"        ,       "F1"      ,             "Prevalence" ,          "Detection Rate"  ,
                                   "Detection Prevalence" ,"Balanced Accuracy")
      models_test <- as.data.frame(models_test)

      rownames(models_test)<-names(fitted_models_ensemble$current)
      model_comparison$data<-models_test
      models_test[,c(  "Accuracy","Kappa", "F1","Sensitivity" , "Specificity", "Pos Pred Value" , "Neg Pred Value" ,  "Precision" ,
                       "Recall")]
      })
    }
  })



  # output$fitted_ensemble3 <- renderPrint({
  #   validate(
  #     need({input$SVM_Linear != FALSE |input$SVM_Radial != FALSE|input$GBM != FALSE |input$NN != FALSE |input$RF != FALSE} , "Please select at least 1 classifier to include in the ensemble")
  #   )
  #   test_measures <- function(model, data) {
  #     tab <- confusionMatrix(data = predict(model, data), reference = splitting_data()$test$Indicator,positive = 'relevant')
  #     print(tab)
  #     c(tab$overall,tab$byClass)
  #   }
  #
  #
  #
  #   if(input$build_ensemble){
  #
  #     isolate({ fitted_models <- fitted_models_ensemble$current
  #     class(fitted_models) <- "caretList"
  #
  #     #create the ensemble where the error occur.
  #     stackControl <- trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE,sampling='smote')
  #     set.seed(1988)
  #     garbage <- capture.output(fit_ensemble<-caretStack(fitted_models, method="gbm", metric="Kappa", trControl=stackControl,tuneGrid = expand.grid(n.trees=c(100,200,300),shrinkage=c(0.1),n.minobsinnode = c(3,5),interaction.depth=c(10,20))))
  #     #fit_ensemble <- caretEnsemble(fitted_models)
  #     ##predict test set using the ensembel
  #     if(input$build_ensemble2){ return(invisible())}
  #     else{test_measures(fit_ensemble,splitting_data()$test)}
  #     })
  #   }
  # })


  Ensemble_Output <- eventReactive(input$build_ensemble2, {

    s = input$mytable_rows_selected


    if(is.null(s)){s<-1:length(fitted_models_ensemble$current)}

    if (length(s)) {
      cat('These models were selected: \n\n')
      cat(names(fitted_models_ensemble$current)[s], sep = ', ')
      cat('\n \n')
      fitted_models_ensemble$selected<-s
    }

    test_measures <- function(model, data) {
      predictions= predict(model, data)
      tab <- confusionMatrix(data = predictions, reference = splitting_data()$test$Indicator,positive = 'relevant')
      cat("Performance in percentages:\n")
      print(round(prop.table(table(predictions,splitting_data()$test$Indicator)),2))
      cat("\n")
      cat("Performance in absolute values and outcome measures:\n")
      print(tab)
      c(tab$overall,tab$byClass)
    }



    if(length(s)>1 & input$build_ensemble2){
      isolate({
        fitted_models <-   fitted_models_ensemble$current[s]

        class(fitted_models) <- "caretList"

        withProgress({
          setProgress(message = "Building ensemble...")
          stackControl <- trainControl(method="cv", number=5, savePredictions=TRUE, classProbs=TRUE,sampling='smote')
          set.seed(1988)
          garbage <- capture.output(fit_ensemble<-caretStack(fitted_models, method="gbm", metric="Kappa", trControl=stackControl,tuneGrid = expand.grid(n.trees=c(100,200,300),shrinkage=c(0.1),n.minobsinnode = c(1,2),interaction.depth=c(10,20))))
          fitted_models_ensemble$final=fit_ensemble
        })

        fitted_models_ensemble$performancefinal<-test_measures(fit_ensemble,splitting_data()$test)
        return(fitted_models_ensemble$performancefinal)
      })
    }

    if(length(s)==1 & input$build_ensemble2){
      withProgress({
        setProgress(message = "Building ensemble...")
        fit_ensemble <- fitted_models_ensemble$current[[s]]
        fitted_models_ensemble$final=fit_ensemble
              })
      fitted_models_ensemble$performancefinal<-test_measures(fit_ensemble,splitting_data()$test)
            return(fitted_models_ensemble$performancefinal)
          }




  })



  output$x4 = renderPrint({
   ensemble<-Ensemble_Output()
   cat('Performance results of the ensemble \n \n')
   print(ensemble)
   print(fitted_models_ensemble$timing)
  })



  output$plot_ensemble_comparison <- renderPlot({
    if (is.null(model_comparison$data)) {return(NULL)}
    else{
      layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 1))
      par(mai=rep(0.5, 4))
      for(i in 1:3){
        barplot(as.matrix(model_comparison$data[,c("Accuracy","Kappa","Sensitivity", "Specificity","Pos Pred Value" ,"Neg Pred Value" , "Precision" ,
                                                   "Recall" , "F1")[(3*i-2):(3*i)]]),beside=TRUE,col=gray.colors(nrow(model_comparison$data)))
      }
      par(mai=c(0,0,0,0))
      plot.new()
      legend(x="center",ncol=5,rownames(model_comparison$data),fill=gray.colors(nrow(model_comparison$data)) )
        }
  })


   output$Generate_Rdata <- renderUI({
     if(input$build_ensemble2==FALSE){return()}
     downloadButton('saveEnsemble', 'Save the ensemble')
   })


  output$saveEnsemble <- downloadHandler(
    filename <- function(){
      if(input$selection != "topics") model_comparison_list<-list("Individual_models" = fitted_models_ensemble$current, "Individual_performance" = model_comparison$data, "Final_Ensemble" = fitted_models_ensemble$final , "Performance_Ensemble" = fitted_models_ensemble$performancefinal, "Selected" = fitted_models_ensemble$selected , "Input" = input$selection)
      if(input$selection == "topics") model_comparison_list<-list("Individual_models" = fitted_models_ensemble$current, "Individual_performance" = model_comparison$data, "Final_Ensemble" = fitted_models_ensemble$final , "Performance_Ensemble" = fitted_models_ensemble$performancefinal, "Selected" = fitted_models_ensemble$selected , "Input" = input$selection, "Topicsmodel" = fitted_models_ensemble$topicsmodel, "nr_topics"=input$nr_topics)
      paste(paste(rownames(model_comparison_list$Individual_performance)[model_comparison_list$Selected],sep="_",collapse = "_"),"ensemble.Rdata",sep="_")
    },

    content = function(file) {
      if(input$selection != "topics") model_comparison_list<-list("Individual_models" = fitted_models_ensemble$current, "Individual_performance" = model_comparison$data, "Final_Ensemble" = fitted_models_ensemble$final , "Performance_Ensemble" = fitted_models_ensemble$performancefinal, "Selected" = fitted_models_ensemble$selected , "Input" = input$selection)
      if(input$selection == "topics") model_comparison_list<-list("Individual_models" = fitted_models_ensemble$current, "Individual_performance" = model_comparison$data, "Final_Ensemble" = fitted_models_ensemble$final , "Performance_Ensemble" = fitted_models_ensemble$performancefinal, "Selected" = fitted_models_ensemble$selected , "Input" = input$selection, "Topicsmodel" = fitted_models_ensemble$topicsmodel, "nr_topics"=input$nr_topics)
      save(model_comparison_list, file = file)
    }
  )


  dataInput <- reactive({
    loadRData <- function(fileName){
      #loads an RData file, and returns it
      load(fileName)
      get(ls()[ls() != "fileName"])
    }
    if (!is.null(input$f1)) loadRData(input$f1$datapath)
  })


  output$title_ensemble1<- renderUI({
    if(is.null(dataInput()))    return(NULL)
    h1("Basic information on the uploaded model")
  })

  output$title_ensemble2<- renderUI({
    if(is.null(dataInput()))    return(NULL)
    h1("Plot of the performance of the individual models in the ensemble")
  })



  output$datastr <- renderPrint({
    ff <- dataInput()
    if (is.null(dataInput()))  return(invisible())
    else {
      cat('Ensemble loaded \n\n')
      cat('Input space used: \n')
      cat(ff$Input)
      cat('\n\n')
      cat('Models used in the ensemble: \n')
      cat(names(ff$Individual_models)[ff$Selected], sep = ', ')
      cat('\n \n')
      cat("Overall performance of the ensemble: \n\n")
      print(ff$Performance_Ensemble[c("Accuracy","Kappa","Sensitivity", "Specificity","Pos Pred Value" ,"Neg Pred Value" , "Precision" ,
                                      "Recall" , "F1")])
    }

  })


  output$plot_uploaded_ensemble <- renderPlot({
    ff <- dataInput()
    if (is.null(dataInput()))  return(NULL)
    else{
      use<-ff$Individual_performance[ff$Selected, ,drop=FALSE]
      layout(matrix(c(1,2,3,4,4,4), ncol=3, byrow=TRUE), heights=c(4, 1))
      par(mai=rep(0.5, 4))
      for(i in 1:3){
        barplot(as.matrix(use[,c("Accuracy","Kappa","Sensitivity", "Specificity","Pos Pred Value" ,"Neg Pred Value" , "Precision" ,
                                                   "Recall" , "F1")[(3*i-2):(3*i)]]),beside=TRUE,col=gray.colors(nrow(use)))
      }
      par(mai=c(0,0,0,0))
      plot.new()
      legend(x="center",ncol=5,rownames(use),fill=gray.colors(nrow(use)) )
    }
  })



  inFile2 <- reactive({
    if (is.null(input$new_data)) {
      return(NULL)
    } else {
      input$new_data
    }
  })


  NewData <- reactive({
    if (is.null(inFile2())) {
      return(NULL)
    } else {
      switch(input$sep,
             "Comma" = read.csv(inFile2()$datapath),
             "Tab" = read.table(inFile2()$datapath,sep="\t",header=TRUE))
    }
  })


  Create_corpus2<- reactive({
    if(!is.null(NewData())){
      abstracts_final<-NewData()
      #abstracts_final<-abstracts_final[abstracts_final$Abstract[]!="",]
      review_corpus = VCorpus(VectorSource(paste(abstracts_final$Title,abstracts_final$Abstract)))
      review_corpus = tm_map(review_corpus, content_transformer(tolower))  #no uppercase
      review_corpus = tm_map(review_corpus, removeNumbers)  #no numerical values
      review_corpus = tm_map(review_corpus, removePunctuation) #remove punctuation
      review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english"))) # remove stopwords
      review_corpus = tm_map(review_corpus, stripWhitespace) # remove whitespace
      review_corpus = tm_map(review_corpus, stemDocument, language = "english")  #stemming (bring back to common base word)
      return(review_corpus)
      }
  })


  final_dtm2<-reactive({
    if(!is.null(NewData())){
      ff <- dataInput()
      selection=ff$Input
      review_corpus<-Create_corpus2()
      if(selection == "TDM"){
        review_dtm <- DocumentTermMatrix(review_corpus)
        return(as.matrix(review_dtm))
      }
      if(selection == "TFIDF"){
        review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
        return(as.matrix(review_dtm_tfidf))
      }
      if(selection == "bigram"){
        BigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
        review_dtm_bigram <- t(TermDocumentMatrix(review_corpus, control = list(tokenize = BigramTokenizer)))
        return(as.matrix(review_dtm_bigram))
      }
      if(selection == "trigram"){
        TrigramTokenizer <-   function(x) {unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
        review_dtm_trigram <- DocumentTermMatrix(review_corpus, control = list(tokenize = TrigramTokenizer))
        return(as.matrix(review_dtm_trigram))
      }

      if(selection == "topics"){
        review_dtm <- DocumentTermMatrix(review_corpus)
        test.topics <- posterior(ff$Topicsmodel,review_dtm,control=list( seed = 1988))
        use<-data.frame(test.topics$topics)
        names(use)<-paste0("V",1:ncol(use))
        head(use)
        return(use)
      }
    }

  })


  final_data2<-reactive({
    if(!is.null(NewData())){
      ff <- dataInput()
      selection=ff$Input
      k<-ff$nr_topics
      abstracts_final<-NewData()
      #abstracts_final<-abstracts_final[abstracts_final$Abstract[]!="",]
      dtm<-final_dtm2()
      abstracts_final$Abstract =NULL
      abstracts_final = cbind(abstracts_final,dtm )  # select the desired term-document matrix
      if("Indicator" %in% names(abstracts_final)){
      abstracts_final$Indicator<-as.factor(abstracts_final$Indicator)
      if(selection!="topics") abstracts_final<-abstracts_final[,c('Indicator',attr(dtm,"dimnames")$Terms)]
      if(selection=="topics")  abstracts_final<-abstracts_final[,c('Indicator',paste0("V",1:k))]
      levels(abstracts_final$Indicator)<-c("irrelevant","relevant")
      }
      if(!("Indicator" %in% names(abstracts_final))){
        if(selection!="topics") abstracts_final<-abstracts_final[,attr(dtm,"dimnames")$Terms]
        if(selection=="topics")  abstracts_final<-abstracts_final[,c(paste0("V",1:k))]
      }
      }
    names(abstracts_final)<-make.names(names(abstracts_final))
    abstracts_final
  })



  output$performance_newdata <- renderPrint({

if(is.null(NewData())) {return(invisible())}
if(!is.null(NewData())){
  withProgress({
  setProgress(message = "Calculating predictions...")
    ff <- dataInput()
    test_new<-final_data2()



   if(length(ff$Selected)>1) test_new[setdiff(names(ff$Final_Ensemble$models[[1]]$trainingData),names(test_new))]<-0
   if(length(ff$Selected)==1) test_new[setdiff(names(ff$Final_Ensemble$trainingData),names(test_new))]<-0

 predictions<-predict(ff$Final_Ensemble,test_new)

    probs<-predict(ff$Final_Ensemble,test_new,type='prob')


    if(length(ff$Selected)==1) probs<-probs[,"relevant"]
    if(length(ff$Selected)>1) probs<-1-probs



   original_data<-NewData()
   original_data<-original_data[original_data$Abstract[]!="",]
  original_data<-cbind(original_data,predictions,probs)
  original_data<-original_data[order(original_data$probs),]
  predict_new_data$out_csv<-original_data
  cat("The new abstracts have been evaluated and predictions have been made \n\n")
  if("Indicator" %in% names(original_data)){
  cat("Table of predictions: \n\n")
  print(table(original_data$predictions,original_data$Indicator))
  cat("\n \n")
  }
  cat("A csv file containing all abstracts of the data and the predictions of relevance can now be downloaded")
   })
}

  })

  output$downloadData <- downloadHandler(
    filename = function() { paste("predictions", '.csv', sep='') },
    content = function(file) {
      write.csv(predict_new_data$out_csv, file)
    }
  )


  ### Full text screening




    output$test_full<-renderPrint({
    if(!is.null(inFileFull())){
    opinions<-FullTexts()
    corp <-VCorpus(VectorSource(opinions))

    corp = tm_map(corp, content_transformer(tolower))  #no uppercase
    corp = tm_map(corp, removeNumbers)  #no numerical values
    corp = tm_map(corp, removePunctuation) #remove punctuation
    corp = tm_map(corp, removeWords, c("the", "and", stopwords("english"))) # remove stopwords
    corp =  tm_map(corp, stripWhitespace) # remove whitespace
    corp =  tm_map(corp, stemDocument, language = "english")  #stemming (bring back to common base word)

    for(i in seq(length(corp))){
      meta(corp[[i]], tag = "id")<- inFileFull()$name[i]
    }


    review_dtm <- DocumentTermMatrix(corp)
    review_dtm = removeSparseTerms(review_dtm, 0.95)
    inspect(review_dtm)
    }
  })


#### Data Extraction

    output$extracted<-renderPrint({
      if(input$Extract_text){
        opinions<-inFileFull()$datapath
        extract_tables(opinions[3],columns = list(c(10)))
              }
    })




    # output$selected_words <- renderUI({
    #   if(is.null(inFileFull())){return(NULL)}
    #   if(!is.null(inFileFull())){
    #      opinions<-FullTexts()
    #      corp <-VCorpus(VectorSource(opinions))
    #      corp = tm_map(corp, content_transformer(tolower))  #no uppercase
    #      corp = tm_map(corp, removeNumbers)  #no numerical values
    #      corp = tm_map(corp, removePunctuation) #remove punctuation
    #      corp = tm_map(corp, removeWords, c("the", "and", stopwords("english"))) # remove stopwords
    #      corp =  tm_map(corp, stripWhitespace) # remove whitespace
    #      review_dtm <- DocumentTermMatrix(corp)
    #      use_dtm<- as.matrix(review_dtm)
    #      list_of_words<-colnames(use_dtm)
    #   selectizeInput(
    #     'words_selection', 'Which words do you want to extract',
    #     choices = list_of_words
    #   )
    #   }
    # })





    output$highlightedtext <- renderUI({

      input$extracts_words
      indicator_k<-0
      isolate({
      if(!is.null(inFileFull())&length(input$words_selection)){
        strsplit(input$words_selection, " ")[[1]]
        opinions<-inFileFull()$datapath
        cols<-list(tags$h1("Overview of the extracted sentences"))
        for (o in 1:length(opinions)){
        tabs<-extract_text(opinions[o])

        cols<- append(cols,list(
          tags$h2(),
          tags$div(HTML(paste("Looking in pdf file:", tags$span(style="color: red", input$files_full$name[o]), sep = " "))),
          tags$h2())
          )


       loc <- gregexpr(pattern=input$words_selection, unlist(tabs))[[1]]


        if(loc[1]==-1){ cols[length(cols)+1]<-

            list(tags$div(HTML(paste(paste("The selected word ", tags$span(style="color:green", input$words_selection), sep = ""),"was not found",sep=" "))))
        indicator_k<-indicator_k + 1
        }
        if(loc[1]!=-1){
      cols_new<- append(cols,rep( list(list()), 2*length(loc)-1 ) )

        words<- rep( list(list()), length(loc) )

  for( i in 1: length(loc)){
        words[[i]] <- substr(tabs, start=loc[i]-input$length_sentence, stop=loc[i]+input$length_sentence)
        words[[i]]<- strsplit(words[[i]], " ")[[1]]

        words[[i]]<-    unlist(strsplit(words[[i]],"\n"))


        words[[i]] <- c(". . .",words[[i]],". . .")

        if(length(strsplit(input$words_selection, " ")[[1]])>1){

        cols_new[[length(cols) + 2*i-1]]<-lapply(words[[i]], function(word) {
          col <-"black"
          if (grepl(input$words_selection, word)|sum(grepl(word,strsplit(input$words_selection, " ")[[1]])) ) {
            col <- "blue"
          }
           tags$span(style = paste("color:", col), word)
        })
        }

        if(length(strsplit(input$words_selection, " ")[[1]])==1){
          cols_new[[length(cols) + 2*i-1]]<-lapply(words[[i]], function(word) {
            col <-"black"
            if (grepl(input$words_selection, word) ) {
              col <- "blue"
            }
            tags$span(style = paste("color:", col), word)
          })
        }

       if(i<length(loc)) cols_new[[length(cols) + 2*i]] <- tags$h2()
  }
        cols<-cols_new
        }
        }
        cols_new2<-list(tags$h1("Basic information"),paste(paste(paste(paste("The requested word was found in", o-indicator_k),"of the"),o),"uploaded documents"),cols)
        tagList(cols_new2)
      }
      })
    })




})
