
shinyUI(

  navbarPage("Automatic abstract screening",

             tabPanel("Selecting Data", titlePanel("Selecting the data"),
                      fluidRow(sidebarPanel(

                        radioButtons("type", label = "Abstract or Full Text Screening", choices = list("Abstract", "Full text"),selected = "Abstract"),
                        radioButtons("sep", label = "Select delimiter", choices = list("Comma", "Tab"),selected = "Comma"),
                        fileInput('file','Upload file with abstract and indicator of relevance'),


                        conditionalPanel(
                          condition = "input.type == 'Full text'",
                          fileInput("files_full", "Choose pdf files of the full texts", multiple=TRUE, accept = c(".pdf",".PDF"))),

                                             selectInput("selection", "Choose an input space:",
                                                        choices = c("TDM","TFIDF","bigram","trigram","topics")),
                                            conditionalPanel(
                                              condition = "input.selection == 'topics'",
                                              numericInput("nr_topics", label="Select number of topics", 30, min = 10, max = 60)),
                                            sliderInput("remove_percentage","Select percentage of data you want to keep after removing sparse terms", min = 80,  max = 100,  value = 95),
                                            sliderInput("train_percentage","Select percentage of data you want to use in training set", min = 1,  max = 100,  value = 50),

                                            h5("Show some output of the uploaded data"),
                                            checkboxInput('show', 'Show first lines of the uploaded data and final input space', value = FALSE, width = NULL),
                                            conditionalPanel(
                                              condition = "input.selection == 'topics'",
                                              checkboxInput('show2', 'Show info on topics', value = FALSE, width = NULL)
                                                                          ),
                                                                                        checkboxInput('make_wordcloud', 'Create a word cloud', value = FALSE, width = NULL),
                                            uiOutput("wordcloud_options")),
                               mainPanel(h1("Summaries of the uploaded data can be presented here"),
                                         #uiOutput("relevance_title"),
                                         #verbatimTextOutput("relevance1"),
                                         plotOutput("relevance_plot"),
                                         uiOutput("summary1_title"),
                                         DT::dataTableOutput("summary"),
                                         uiOutput("summary2_title"),
                                         tableOutput("info_topics"),
                                         #tableOutput("summary_topics"),
                                         uiOutput("wordcloud_title"),
                                         plotOutput("plot")))),


      tabPanel("Ensemble Classifier", titlePanel("Building an ensemble"),
                      sidebarPanel(
                        checkboxInput("SVM_Linear", "Include SVM Linear", value=FALSE),
                        uiOutput("SVM_Linear_Imbalance") ,
                        uiOutput("SVM_Linear_Grid"),
                        checkboxInput("SVM_Polynomial", "Include SVM Polynomial", value=FALSE),
                        uiOutput("SVM_Poly_Imbalance") ,
                        uiOutput("SVM_Poly_Grid"),
                        checkboxInput("SVM_Radial", "Include SVM Radial", value=FALSE),
                        uiOutput("SVM_Radial_Imbalance") ,
                        uiOutput("SVM_Radial_Grid"),
                        checkboxInput("GBM", "Include GBM", value=FALSE),
                        uiOutput("GBM_Imbalance") ,
                        uiOutput("GBM_Grid"),
                        checkboxInput("NN", "Include NN", value=FALSE),
                        uiOutput("NN_Imbalance") ,
                        uiOutput("NN_Grid"),
                        checkboxInput("RF", "Include RF", value=FALSE),
                        uiOutput("RF_Imbalance") ,
                        actionButton("build_ensemble", "Build classifiers") ,
                        actionButton("build_ensemble2", "Construct ensemble"),
                        uiOutput("Generate_Rdata")

                                            ),
                      mainPanel(h1("Overview of performance measures for the individual classifiers"),
                                DT::dataTableOutput("mytable"),
                                h1("Plot of the performance of the individual models in the ensemble"),
                                plotOutput("plot_ensemble_comparison"),
                                # h1("Performance of the total ensemble (all classifiers)"),
                                # verbatimTextOutput("fitted_ensemble3"),
                                h1("Performance of the selected ensemble (selected classifiers in table)"),
                                verbatimTextOutput('x4')

                      )),

             tabPanel("Predictions on New Data", titlePanel("Determine relevance of new abstracts"),
                      fluidRow(
                        sidebarPanel(
                          fileInput('f1', 'Upload previous model in .Rdata format', accept=c('.RData, .Rds')),
                          fileInput('new_data','Upload your new data here'),
                          downloadButton('downloadData', 'Download predictions')
                                          ),
                               mainPanel(
                              uiOutput("title_ensemble1"),
                              verbatimTextOutput('datastr'),
                              uiOutput("title_ensemble2"),
                              plotOutput("plot_uploaded_ensemble"),
                              verbatimTextOutput('performance_newdata')))),

      navbarMenu("Other options",

                 # tabPanel("Full text screening", titlePanel("Upload the pdf files of the full texts"),
                 #          fluidRow(sidebarPanel( fileInput("files_full", "Choose pdf files of the full texts", multiple=TRUE, accept = c(".pdf",".PDF"))
                 #
                 #          ),
                 #          mainPanel(h1("Summaries of the uploaded data can be presented here"),
                 #                    verbatimTextOutput("test_full")
                 #          ))),
                 #
                 # "----",
                 "Individual Classifiers",
                 tabPanel("SVM", titlePanel("SVM Classifier"),

                          sidebarPanel(radioButtons("kernel", label = "Select kernel", choices = list("Linear", "Polynomial","Radial"),selected = "Linear"),
                                       radioButtons("imbalanced_solution", label = "Remedial measure for imbalanced dataset", choices = list("None", "SMOTE","ROSE"),selected = "None"),
                                       #checkboxInput('smote', 'Perform SMOTE sampling (in case of imbalanced data)', value = FALSE, width = NULL),
                                       checkboxInput('tune_svm', 'Would you like to tune parameters? (if not selected, default values are used)', value = FALSE, width = NULL),
                                       uiOutput("tuning_method_svm"),
                                       uiOutput("tuning_method_options_svm"),
                                       uiOutput("tuning_metric_svm"),
                                       actionButton("build", "Build classifier") ,
                                       checkboxInput("show_overall_summary", "Summarize performance measures for distinct SVM options", value = FALSE, width = NULL)
                          ),
                          mainPanel(h1("Performance on the test set"),
                                    verbatimTextOutput("fitted_svm2"),
                                    tableOutput("overall_summary")
                          )),


                 tabPanel("Gradient Boosting", titlePanel("Gradient Boosting Machine Classifier"),
                          sidebarPanel(
                            radioButtons("imbalanced_solution_gbm", label = "Remedial measure for imbalanced dataset", choices = list("None", "SMOTE","ROSE"),selected = "None"),
                            #checkboxInput('smote_gbm', 'Perform SMOTE sampling (in case of imbalanced data)', value = FALSE, width = NULL),
                            checkboxInput('tune_gbm', 'Would you like to tune parameters? (if not selected, default values are used)', value = FALSE, width = NULL),
                            uiOutput("tuning_method_gbm"),
                            uiOutput("tuning_method_options_gbm"),
                            uiOutput("tuning_metric_gbm"),
                            actionButton("build_gbm", "Build classifier") ,
                            checkboxInput("show_overall_summary_gbm", "Summarize performance measures", value = FALSE, width = NULL)

                          ),
                          mainPanel(h1("Performance on the test set"),
                                    verbatimTextOutput("fitted_gbm"),
                                    tableOutput("overall_summary_gbm")

                          )),

                 tabPanel("Random Forest", titlePanel("Random Forest Classifier"),

                          sidebarPanel(
                            radioButtons("imbalanced_solution_rf", label = "Remedial measure for imbalanced dataset", choices = list("None", "SMOTE","ROSE"),selected = "None"),
                            actionButton("build_rf", "Build classifier") ,
                            checkboxInput("show_overall_summary_rf", "Summarize performance measures", value = FALSE, width = NULL)
                          ),
                          mainPanel(h1("Performance on the test set"),
                                    verbatimTextOutput("fitted_rf"),
                                    tableOutput("overall_summary_rf")
                          )),

                 tabPanel("Neural Network", titlePanel("Neural Network Classifier"),
                          sidebarPanel(
                            radioButtons("imbalanced_solution_nn", label = "Remedial measure for imbalanced dataset", choices = list("None", "SMOTE","ROSE"),selected = "None"),
                            checkboxInput('tune_nn', 'Would you like to tune parameters? (if not selected, default values are used)', value = FALSE, width = NULL),
                            uiOutput("tuning_method_nn"),
                            uiOutput("tuning_method_options_nn"),
                            uiOutput("tuning_metric_nn"),
                            actionButton("build_nn", "Build classifier") ,
                            checkboxInput("show_overall_summary_nn", "Summarize performance measures", value = FALSE, width = NULL)

                          ),
                          mainPanel(h1("Performance on the test set"),
                                    verbatimTextOutput("fitted_nn"),
                                    tableOutput("overall_summary_nn")

                          )),
                  "----",
                 "Data extraction tools",
                 tabPanel("Data extraction", titlePanel("Data extraction"),

                          sidebarPanel(
                            textInput("words_selection", 'Which word do you want to search?', value = "outcome", width = NULL, placeholder = NULL),
                            numericInput("length_sentence", 'How many characters should be shown before and after the querried word?', value = 100),
                            actionButton("extracts_words", "Extract sentences")
                          ),
                          mainPanel(

                                     uiOutput("highlightedtext")


                          )),
                 tabPanel(a("Press here to go to the demo version of EXACT software", href="http://exactdemo.iit.nrc.ca/intro.php",target="_blank"))

                   ))

             )









