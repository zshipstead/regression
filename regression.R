library(shiny)
library(tidyverse)
library(purrr)
library(QuantPsyc)

# Display for verifyVars screen. Replaces numChoice
varsDisplay <- function(varN){
  fluidRow(
    column(5,
           offset = 1,
           h4(textOutput(paste0("numCol", varN)), align = "right"),
    ),
    column(5, 
           radioButtons(paste0("colType", varN), "How should this variable be treated?",
                        choices = list("Continuous", "Categorical"))
           ),
  )
}



ui <- fluidPage(
  tabsetPanel(
    id = "regress",
    type = "hidden",
    tabPanel("ChoicePage", div(h3("Welcome to the Regression App"), align = "center"),
             h3(br()),
             fluidRow(
               column(4,
                      radioButtons("selectData", "Select Data Origin", 
                                   choices = c("Upload .csv File" = "csvUpload",
                                               "Use Demonstration Dataset" = "practiceData"),
                                   selected = character(0))
                      
                      )
             ),
             fluidRow(
               column(4,
                      uiOutput("goToData")
                      )
             )
             ),
    tabPanel("csvUpload", div(h3(textOutput("dataSelectPage")), align = "center"),
             br(),
             fluidRow(
               column(4,
                      uiOutput("selectData")
                      )
             ),
             fluidRow(
               column(1,
                      actionButton("backToStart", "Go Back")
               ),
               column(1,
                      uiOutput("uploadSuccess")
               )
             ),
             fluidRow(
               column(10,
                      offset = 1,
                      dataTableOutput("dataPreview")
                      )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      textOutput("explainDV")
               )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      textOutput("describeData")
               )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      textOutput("describeWM")
               )
             )
             ),
    tabPanel("verifyVars", div(h3("Verify Numeric Input"), align = "center"),
             div(h3(textOutput("notNeeded")), align = "center", style = "color:red"),
             br(),
             fluidRow(
               column(10,
                      uiOutput("numChoice")
                      )
             ),
             fluidRow(
               column(1,
                      offset = 9,
                      actionButton("backToCsv", "Go Back")),
               column(1,
                      actionButton("toSelectVars", "Continue"))
             )
      
    ),
    tabPanel("selectVars", div(h3("Select Variables for Model"), align = "center"),
             br(),
             fluidRow(
               column(5,
                      offset = 1,
                      selectInput("dv", "Choose your Dependent Variable ", choices = NULL)
                      ),
               column(5,
                      offest = 1,
                      uiOutput("modelsHere")
                      )
             ),
             fluidRow(
               column(1,
                      offset = 8,
                      actionButton("backToNumbers", "Go Back")),
               column(1,
                      uiOutput("toEnd")
                      )
             ),
             fluidRow(
               column(10,
                      offset = 1,
                      uiOutput("listModels")
                      )
             )
             ),
    tabPanel("results", div(h3("Regression Results"), align = "center"),
             fluidRow(
               column(1,
                      offset = 1,
                      actionButton("backToVars", "Go Back"))
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      h4(textOutput("removal"))
               )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      h4("Model Results"),
                      tableOutput("modelComp")
                      )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      h4("Model Comparisons (ANOVA)"),
                      uiOutput("multi"),
               )
             ),
             br(),
             fluidRow(
               column(10,
                      offset = 1,
                      h4("Model Coefficients"),
                      tableOutput("coefficients"),
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  ########################
  ## Choose Data Source ##
  ########################
  
  # Create "continue" button when data source selected
  output$goToData <- renderUI({
    req(input$selectData)
    actionButton("goToData", "Continue")
  })
  
  # Store input from initial radio button
  dataSource <- reactive({
    input$selectData
  })
  
  # Go to csv upload page
  observeEvent(input$goToData, {
    updateTabsetPanel(inputId = "regress", selected = "csvUpload")
      # csv upload page title
    if (dataSource() == "csvUpload") {
      output$dataSelectPage <- renderText("Upload a .csv File")
      output$selectData <- renderUI(fileInput("loadData1", "Upload Data (.csv only)", accept = ".csv"))
    } else {
      output$dataSelectPage <- renderText("Select a Demonstration Data Set")
      output$selectData <- renderUI(selectInput("loadData2", "Select Demo Data (Description Below)", 
                                                choices = c("-", "Insurance Costs", "Wine Quality", "Working Memory and Attention"),
                                                selected = character(0)))
    }
      
    
  }) 
  
  #####################
  ## csv Upload Page ##
  #####################
  
  # Back button
  observeEvent(input$backToStart, {
    updateTabsetPanel(inputId = "regress", selected = "ChoicePage")
  })
  
  # Next Page Button...See next section
  
  # Display data preview
  output$dataPreview <- renderDataTable(
    dataUpLoad(), options = list(searching = FALSE, paging = FALSE, scrollY=150, scrollX=800))
  
  
  # Verify that upload is .csv, if so, create "next" button
  dataUpLoad <- reactive({
    if (dataSource() == "csvUpload") {
      req(input$loadData1) 
      ext <- tools::file_ext(input$loadData1$name)
      if (ext == "csv") {
        ###Next page button----------------
        output$uploadSuccess <- renderUI({  
          req(input$loadData1)
          actionButton("verifyNumeric", "Continue")
        })
        ###return data----------------------
        vroom::vroom(input$loadData1$datapath, delim = ",")  
      } else validate("Invalid File: Please Select a .csv file")
    } else {
      # For demonstration data
      req(input$loadData2)
      demoData <- as_tibble(read.csv("RegData.csv"))
      output$uploadSuccess <- renderUI({  
        req(input$loadData2 != "-")
        actionButton("verifyNumeric", "Continue")
      })
      # Load data
      if (input$loadData2 == "Wine Quality") {
        output$explainDV <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "dv_explain")))
        output$describeData <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "wineCred")))
        output$describeWM <- renderText("")
        demoData[2:13] %>% filter(demoData[1] == "wine")
      } else if (input$loadData2 == "Insurance Costs") {
        output$explainDV <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "dv_explain")))
        output$describeData <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "insCred")))
        output$describeWM <- renderText("")
        demoData[14:20] %>% filter(demoData$DataSet == "insurance")
      } else if (input$loadData2 == "Working Memory and Attention") {
        output$explainDV <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "dv_explain")))
        output$describeData <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "wmCred")))
        output$describeWM <- renderText(as.character(demoData[21] %>% filter(demoData[1] == "wm_explain")))
        demoData[22:25] %>% filter(demoData$DataSet == "workingMem")
      } else {
        output$describeData <- renderText("")
        NA
      }
    }
  })
  
  
  ####################  
  ## Verify Numeric ##
  ####################
  
  # Go Back
  observeEvent(input$backToCsv, {
    updateTabsetPanel(inputId = "regress", selected = "csvUpload")
  })
  
  # Continue button on next page
  
  # Set up page (continue button from csv page)
  observeEvent(input$verifyNumeric, {
    updateTabsetPanel(inputId = "regress", selected = "verifyVars")
    if (dataSource() == "practiceData") 
      output$notNeeded <- renderText("(Note: This step can be ignored if using demonstration data)")
    else  output$notNeeded <- renderText("")
    
    # Check for and extract numeric variables
    numericVars <- unlist(lapply(dataUpLoad(), is.numeric))
    if (sum(numericVars) > 0) {
      vD <- NULL
      dataUpLoadNumeric <- dataUpLoad()[, numericVars]
      for (j in 1:ncol(dataUpLoadNumeric)){
        vD <- list(vD, varsDisplay(j))
      }
      
      
      # Make Display
      output$numChoice <- renderUI({
        vD
      })
       
      # Populate Display
      tempName <- ("")
      for (j in 1 : ncol(dataUpLoadNumeric)){
        populateChoices(c(colnames(dataUpLoadNumeric[j]), j))
      }

      } else {
        output$numChoice <- renderUI(div(h4(textOutput({"oops"})), style = "color:red"))
        output$oops <- renderText("Oops! Your .csv file has no numeric variables. Please
                                  upload a new file")
      }
    
  })
  
  # create display 
  populateChoices <- function(choicesVar){
    tempName <- paste0("numCol", choicesVar[2])
    output[[tempName]] <- renderText(choicesVar[1])
  }  
  
  
  ###############
  # Select Vars #
  ###############
  
  #Create Page
  observeEvent(input$toSelectVars, {
    updateTabsetPanel(inputId = "regress", selected = "selectVars")
    
    # Update list of numeric data
    getPossible <- unlist(lapply(correctedDataUpload(), is.numeric))
    possDVs <- correctedDataUpload()[, getPossible]
    
    updateSelectInput(inputId = "dv", choices = colnames(possDVs), selected = (0))
  })
  
  
  # Go back
  observeEvent(input$backToNumbers, {
    updateTabsetPanel(inputId = "regress", selected = "verifyVars")
  })
  
  
  # Make go forward button
  output$toEnd <- renderUI({
    req(input$models > 0 )
    # browser()
    switch(!is.na(input$Model_1) && input$models > 0,
           "TRUE" = actionButton("doRegression", "Continue"),
           "FALSE" = uiOutput("toEnd"))
    
  })
  
  
  # Change selected vars to factors
  correctedDataUpload <- reactive({
    #req(input$toSelectVars)
    # Create listing of radio buttons
    getNums <- unlist(lapply(dataUpLoad(), is.numeric))
    buttons <- dataUpLoad()[, getNums] 
    newData <- dataUpLoad()
    # If radio button = categorical, then change original column to as.factor
    for (i in 1:ncol(buttons)){
      currentButton <- paste0("colType", i)
      if (input[[currentButton]] == "Categorical") {
        nameToChange <- colnames(buttons[i])
        newData[[nameToChange]] <- as.factor(newData[[nameToChange]])
      }
    }
    # change the non-numeric vars to factor
    changeChar <- dataUpLoad()[, getNums == FALSE]
    if (ncol(changeChar) > 0)
      for (i in 1:ncol(changeChar)){
        charToChange <- colnames(changeChar[i])
        newData[[charToChange]] <- as.factor(newData[[charToChange]])
      }
        
    newData
  })
  
  # Create option for number of models
  output$modelsHere <- renderUI({
    switch(input$dv %in% colnames(correctedDataUpload()),
           "TRUE" = numericInput("models", "How Many Models will You Test?",
                               min = 0, value = 0, step = 1),
           "FALSE" = uiOutput("modelsHere")
    )
  })

  
  # List of number of models user wants to create
  numModels <- reactive(paste0("Model_", seq_len(input$models)))
  
  # Create models
  output$listModels <- renderUI({
    # browser()
    req(input$models >= 1 && input$dv != '-')
    map(numModels(), ~
          checkboxGroupInput(.x, paste0("Variables to include in ", .x),
                             choices = colnames(correctedDataUpload()
                                                [,colnames(correctedDataUpload())
                                                  %in% input$dv == FALSE]),
                             selected = input[[.x]], inline = TRUE))
  })
  
  # Initialize last-models count
  observeEvent(input$dv, {
    lastInput$previousModels = 0
  })
  
  # count of last models
  lastInput <- reactiveValues(previousModels = 0)

  
  # Erase old models
  observeEvent(input$models, {
    if (input$models >= lastInput$previousModels) {
      deleteModel <- paste0("Model_", input$models)
      updateCheckboxGroupInput(inputId = deleteModel, selected = "")
    }
    lastInput$previousModels <- input$models 
  })


  ######################
  # Regression Results #
  ######################
  
  
  observeEvent(input$doRegression, {
    updateTabsetPanel(inputId = "regress", selected = "results")
    # Report Missing Data
    output$removal <- renderText(paste0("Listwise removals, due to missing data: ", 
                                        nrow(correctedDataUpload()) - nrow(removeNAs())))
    # Compile models
    unstModels <- storeTestVars()
    stdModels <- stdCoef(unstModels)
    # Model table
    modelsComparisonData <- modelTable(unstModels)
    output$modelComp <- renderTable(modelsComparisonData, digits = 3)
    # ANOVA comparisons
    if (length(unstModels) > 1) {
      aov <- anovaTable(unstModels)
      output$multi <- renderUI(tableOutput("multiModels"))
      output$multiModels <- renderTable(aov, digits = 3)
    } else {
      output$multi <- renderUI(textOutput("notApp"))
      output$notApp <- renderText("Not Applicable")
    }
    # Coefficients
    unstCoeffData <- makeCoeffTable(unstModels, stdModels)
    output$coefficients <- renderTable(unstCoeffData, digits = 3, na = "")
  })
  
  
  # Back to variable selection screen
  observeEvent(input$backToVars, {
    updateTabsetPanel(inputId = "regress", selected = "selectVars")
  })
  
  
                      ########################
                      #### Compile Models ####
                      ########################  
  
  storeTestVars <- function(){
    saveTests <- NULL
    # saveTestsData <- correctedDataUpload()
    saveTestsData <- removeNAs()
    # Cycle models
    for (i in 1 : input$models){
      allIV <- NULL
      currentModel <- paste0("Model_", i)
      # Deconstruct and rebuild model
      if (length(currentModel) > 0){
       for (j in 1 : length(input[[currentModel]])){
          iv <- input[[currentModel]][j]
          allIV <- paste0(allIV, iv)
          if (j < length(input[[currentModel]])) allIV <- paste0(allIV, " + ")
       }
      }
      # Add DV to model
      modelA <- paste0(input$dv, "~", allIV)
      # Analyze model
      saveTests[[currentModel]] <- lm(modelA, data = saveTestsData)

    }
    saveTests
  }
  
  # Examines vars in all models and performs listwise removal of NAs
  removeNAs <- reactive({
    excludeData <- correctedDataUpload()
    excludeData <- excludeData[!is.na(excludeData[input$dv]),]
    for (i in 1 : input$models){
      currentModel <- paste0("Model_", i)
      # Deconstruct and rebuild model
      if (length(currentModel) > 0){
        for (j in 1 : length(input[[currentModel]])){
          currentIV <- input[[currentModel]][j]
          excludeData<- excludeData[!is.na(excludeData[[currentIV]]),]
        }
      }
    }
    excludeData
  })
  
  
  # used for correcting names of standardized coefficients
  rename <- function(x){sub(".*[[[k]]]", "", x)}
  
  # add plusses between columns of dummy vars
  addplus <- function(x){
    sendBack <- NULL
    for (i in 1 : ncol(x)){
      sendBack <- paste0(sendBack, colnames(x)[i], ifelse (i == ncol(x), "", " + "))
    }
    sendBack
  }
  
  # make standardized coefficients
  stdCoef <- function(unstModels){
    stdModels <- NULL
    for (i in 1 : length(unstModels)){
      singleModel <- NULL
      tempData <- removeNAs()
        # check if model lacks factors
      checkForFactors <- try(lm.beta(unstModels[[i]]), silent=TRUE)
      if (class(checkForFactors) == "try-error") {
          # if factors, rebuild the model with dummy vars
        nextIV <- NULL
        for (k in 2:length(unstModels[[i]]$model)) {
          if (is.factor(unstModels[[i]]$model[[k]])) {
              # break up factor variable into columns of dummy vars
            catVar <- as_tibble(model.matrix(~unstModels[[i]]$model[[k]]), data = tempData)
              # scale the dummy vars
            stdCatVar <- sapply(catVar[,2:length(catVar)], scale)
              # rename dummy vars
            colnames(stdCatVar) <-paste0(colnames(unstModels[[i]]$model[k]),
                                         rename(colnames(stdCatVar)[1:ncol(stdCatVar)]))
              # add new columns to data set
            tempData <- cbind(tempData, stdCatVar)
              #add plus
            nextFactor <- ifelse(ncol(stdCatVar) > 1, addplus(stdCatVar), colnames(stdCatVar))

            singleModel <- paste0(singleModel, nextFactor, 
                                  ifelse(k < length(unstModels[[i]]$model), " + ", ""))

          } else singleModel <- paste0(singleModel, colnames(unstModels[[i]]$model[k]), 
                                       ifelse(k < length(unstModels[[i]]$model), " + ", ""))
          
        }
        singleModel <- paste0(colnames(unstModels[[i]]$model[1]), " ~ " , singleModel)
        runModel <- lm(singleModel, data = tempData)
        stdModels[[i]] <- lm.beta(runModel)
      } else stdModels[[i]] <- lm.beta(unstModels[[i]])
    } 
    stdModels
  }
  
                    ##############################
                    #### Make data for tables ####
                    ##############################
  
  modelTable <- function(modelData){
    # Initialize data frame for model table 
    mTable <- data.frame(matrix(ncol = 9, nrow = length(modelData)))
    colnames(mTable) <- c("Model", "R2", "R2_adj", "std_Error",
                             "R2_change", "Model_F", "df1", "df2", "Model_p")
    # Populate data frame
    for (i in 1:length(modelData)){
      mTable$Model[i] <- paste0("Model_", i)
      mTable$R2[i] <- summary(modelData[[i]])$r.squared
      mTable$R2_adj[i] <- summary(modelData[[i]])$adj.r.squared
      mTable$std_Error[i] <- summary(modelData[[i]])$sigma
      if (i > 1) 
        mTable$R2_change[i] <- summary(modelData[[i]])$r.squared - summary(modelData[[i-1]])$r.squared
      mTable$Model_F[i] <- summary(modelData[[i]])$f[1]
      mTable$df1[i] <- as.integer(summary(modelData[[i]])$f[2])
      mTable$df2[i] <- as.integer(summary(modelData[[i]])$f[3])
      mTable$Model_p[i] <- pf(summary(modelData[[i]])$f[1],
                        summary(modelData[[i]])$f[2],
                        summary(modelData[[i]])$f[3],lower.tail=F)
    }
    mTable
  }
  
  anovaTable <- function(aovModels){
    # Initialize data frame for model table
    aovTable <- data.frame(matrix(ncol = 7, nrow = (length(aovModels) - 1)))
    colnames(aovTable) <- c("Models", "Sum_of_Sq", "Res_SS", "df1", "df2", "F",  "p")
    # Populate data frame
    for (i in 2:length(aovModels)){
      compareModels <- anova(aovModels[[i-1]], aovModels[[i]])
      aovTable$Models[i-1] <- paste0("Model_", i-1, " vs. Model_", i)
      aovTable$Sum_of_Sq[i-1]  <- compareModels$`Sum of Sq`[2]
      aovTable$Res_SS[i-1]  <- compareModels$RSS[2]
      aovTable$df1[i-1]  <- as.integer(compareModels$Df[2])
      aovTable$df2[i-1]  <- as.integer(compareModels$Res.Df[2])
      aovTable$F[i-1]  <- compareModels$F[2]
      aovTable$p[i-1]  <- compareModels$`Pr(>F)`[2]
    }
    aovTable
  }
  
  makeCoeffTable <- function(coeffData, stdData){
    tableRows <- 0
    # initialize table
    for (i in 1:length(coeffData)){
      tableRows <- tableRows + length(coeffData[[i]]$coefficients)
    }
    coeffTable <- data.frame(matrix(ncol=7, nrow = tableRows))
    colnames(coeffTable) <- c("Models", "Variable", "B", "Std_Err", "Beta", "t", "p")
    # populate table
    prevRow <- 0
    for (i in 1:length(coeffData)){
      if (i > 1) prevRow <- prevRow + length(coeffData[[i-1]]$coefficients)
      for (k in 1:length(coeffData[[i]]$coefficients)){
        coeffTable$Models[prevRow + k] <- ifelse (k == 1, paste0("Model_", i), "")
        coeffTable$Variable[prevRow + k] <- names(coeffData[[i]]$coefficients[k])
        coeffTable$B[prevRow + k] <- coeffData[[i]]$coefficients[k]
        coeffTable$Std_Err[prevRow + k] <- summary(coeffData[[i]])$coefficients[k,2]
        if (k > 1) coeffTable$Beta [prevRow + k] <- stdData[[i]][k-1]
        coeffTable$t[prevRow + k] <- summary(coeffData[[i]])$coefficients[k,3]
        coeffTable$p[prevRow + k] <- summary(coeffData[[i]])$coefficients[k,4]
      }
    }
    # Reminder: If these are off, start with the call to removeNAs
    coeffTable
  }
  
  
  
}

shinyApp(ui, server)











