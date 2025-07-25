

# Define server logic ----
server <- function(input, output) {
  
  #Data details
  output$table1 = renderUI({
    table = knitr::kable(variables, caption = "Selected and Recoded Variables from Massachusetts 2023 Dataset",format = 'html')
    HTML(table)
  })
  
  
  #Residuals for EDA
  
  #employed small, less likely
  output$residTbl1 = renderTable({
    residuals[residuals[,5]<.20&(residuals$Employed<0|residuals$EmployedU<0),c(1,2,5,6,7,10)]
  })
  
  #employed big, less likely
  output$residTbl2 = renderTable({
    residuals[residuals[,5]>.19&(residuals$Employed<0|residuals$EmployedU<0),c(1,2,5,6,7,10)]
  })
  
  #employed small, more likely
  output$residTbl3 = renderTable({
    residuals[residuals[,5]<.20&(residuals$Employed>0|residuals$EmployedU>0),c(1,2,5,6,7,10)]
  })
  
  #employed big, more likely
  output$residTbl4 = renderTable({
    residuals[residuals[,5]>.19&(residuals$Employed>0|residuals$EmployedU>0),c(1,2,5,6,7,10)]
  })
  
  #not in labor force small, less likely
  output$residTbl5 = renderTable({
    residuals[residuals[,5]<.20&(residuals[,3]<0|residuals[,8]<0),c(1,3,5,6,8,10)]
  })
  
  #not in labor force big, less likely
  output$residTbl6 = renderTable({
    residuals[residuals[,5]>.19&(residuals[,3]<0|residuals[,8]<0),c(1,3,5,6,8,10)]
  })
  
  #not in labor force small, more likely
  output$residTbl7 = renderTable({
    residuals[residuals[,5]<.20&(residuals[,3]>0|residuals[,8]>0),c(1,3,5,6,8,10)]
  })
  
  #not in labor force big, more likely
  output$residTbl8 = renderTable({
    residuals[residuals[,5]>.19&(residuals[,3]>0|residuals[,8]>0),c(1,3,5,6,8,10)]
  })
  
  #unemployed small, less likely
  output$residTbl9 = renderTable({
    residuals[residuals[,5]<.20&(residuals[,4]<0|residuals[,9]<0),c(1,4,5,6,9,10)]
  })
  
  #unemployed big, less likely
  output$residTbl10 = renderTable({
    residuals[residuals[,5]>.19&(residuals[,4]<0|residuals[,9]<0),c(1,4,5,6,9,10)]
  })
  
  #unemployed small, more likely
  output$residTbl11 = renderTable({
    residuals[residuals[,5]<.20&(residuals[,4]>0|residuals[,9]>0),c(1,4,5,6,9,10)]
  })
  
  #unemployed big, more likely
  output$residTbl12 = renderTable({
    residuals[residuals[,5]>.19&(residuals[,4]>0|residuals[,9]>0),c(1,4,5,6,9,10)]
  })
  
  
  #plots for interactive EDA

  
  output$distPlot2 <- renderPlot({
    
    x = as.factor(df[,input$xpick])
    y = as.factor(df[,input$ypick])
    ggplot(df, aes(x = x, fill = y)) +
      geom_bar(position = "dodge") + # Use "stack"  for stacked bars or "dodge" side to side
      labs(
        x = input$xpick,
        fill = input$ypick )
    
  })
    
  #for side bar: table of total number of demogs in both categories
  output$num2 = renderPrint({
     table(df[,c(input$xpick,input$ypick)])

  })
  
  #table of percent that category is of each category of ypick in xpick
  output$percentnum2 = renderPrint({
    perc = table(df[,c(input$xpick,input$ypick)])
    for (i in 1:dim(perc)[2]){perc[,i] = round(perc[,i]/sum(perc[,i]),2)}
    print(perc)
    
  })
  
  #plot of percent of pop for one var y2
  output$percentPlot1 = renderPlot({
    df$total = c(rep(1,dim(df)[1]))
    x = 'total'
    var2 = as.factor(df[,input$y2])
    var1 = as.factor(df[,x])
    
    ggplot(df, aes(x = var1, fill = var2)) +
      geom_bar(position = "fill") + # Use "stack"  for stacked bars or "dodge" side to side
      labs(
        x = "",
        fill = input$y2 ) +
      theme(axis.text.x = element_text(angle = 90, ), legend.position = "top", legend.key.size = unit(0.4, "cm"),
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 7)) 
    
  })
  
  #plot of percent of ESR
  output$percentPlotE = renderPlot({
    df$total = c(rep(1,dim(df)[1]))
    x = 'ESR'
    var2 = as.factor(df[,input$y2])
    var1 = as.factor(df[,x])
    
    ggplot(df, aes(x = var1, fill = var2)) +
      geom_bar(position = "fill") + # Use "stack"  for stacked bars or "dodge" side to side
      labs(
        x = "",
        fill = input$y2 ) +
      theme(axis.text.x = element_text(angle = 90, ), legend.position = "top", legend.key.size = unit(0.4, "cm"),
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 7)) 
    
  })
  
  #table of percent of pop
  output$percentnum1 = renderPrint({
     round(table(df[,input$y2])/dim(df)[1],2) #calculate percent
  })
  
  #table of percent of pop copy
  output$percentnum1copy = renderPrint({
   round(table(df[,input$y2])/dim(df)[1],2) #calculate percent
  })
  
  #table of percent of working aged
  output$percWA1 = renderPrint({ 
    perc = table(df[df$workingage=="U65", input$y2])
   round(perc/sum(df$workingage=="U65"),2)
  })
  
  #table of percent of ESR
  output$percentnumE = renderPrint({
    perc = table(df[,c(input$y2,'ESR')])
    for (i in 1:3){perc[,i] = round(perc[,i]/sum(perc[,i]),2)}
    print(perc)
    
  })
  
  
  
  #text outputs based on inputs
  output$y2sel = renderText({
    input$y2
  })
  
  output$proptext = renderText({
    ProportionsText[vars == input$y2]
  })

  
  #for models
  #input what vars to use
  #react by building models and assessments
  #output tree, summary of glm, comparison of CCR, ROC 
  x <- reactive({
    input$xpick
  })
  
  
  observeEvent(input$run_model, {
    req(input$selected_vars)
    
    output$vars_selected1 = renderPrint({
      input$selected_vars
    })
    
    output$vars_selected2 = renderPrint({
      input$selected_vars
    })
    
    #use selected variables in dataset
   # dd = df[,c('ILF','AGEP',input$selected_vars)]
    dd = df[,c('ILF',input$selected_vars)]
    
    
    
    #split data for training and testing 
    trainRows = caret::createDataPartition(dd[,1], p = 0.8, list=F, times=1)
    train <- dd[trainRows,]
    test <- dd[!row.names(dd) %in% row.names(train),]
    #train$ILF <- factor(train$ILF, levels = c("N", "L"))
    #test$ILF  <- factor(test$ILF,  levels = c("N", "L"))
    
    #run tree model, 
    ib = .000001 #how much fit has to be improved by in order to keep splitting, smaller means more branches, default is .01, maybe go as low as .00001 - may overfit
    tree <- rpart(ILF~.,data=train,control=rpart.control(cp=ib,maxdepth = 6))
    
    
    ###  a new value for cp and then prune and plot the tree:
    cpUser=.000001
    treep=prune(tree,cp=cpUser)
    output$tree_plot = renderPlot({
      rpart.plot(treep,box.palette = "Blues",main="Pruned Tree",cex=.5, space = .5)
    })
    
    
 
    
    #train glm model
    trainLR = glm(ILF ~ ., data=train, family=binomial(link="logit"))
    
    output$model_summary = renderPrint({
    summary(trainLR)  
    })
    
    #predict on test data
    test$Predicted = round(predict(trainLR, test[,-1, drop = FALSE], type="response"), 2)

    
    #ROC curve with area for tree
    pred <- predict(tree,newdata=test,type="prob") #use 'prob' 
    pred2 <- ROCR::prediction(pred[,"L"], test$ILF=="L") 
    perf <- ROCR::performance(pred2, "tpr", "fpr")
    auc <- performance(pred2, "auc")

    output$roc_plot_tree = renderPlot({
      plot(perf, col = "blue", lwd = 2, main = paste("Tree Model on test set, Area Under the Curve:", round(auc@y.values[[1]], 3)))
    })
    
    #ROC curve for LogR
    pred = predict(trainLR, test[,-1, drop = FALSE], type="response")
    pObject = ROCR::prediction(pred, test$ILF=="L" )
    rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
    aucObj = ROCR::performance(pObject, measure="auc")  
    
    output$roc_plotlogr = renderPlot({
    plot(rocObj, main = paste("LogReg Model on test set, Area under the curve:", round(aucObj@y.values[[1]] ,4))) 
    })
    
    #confusion matrix, accuracy for tree

    predP <- predict(treep,newdata=test,type="class")
    t = table(predP,test$ILF)
    #correct classification rate
    CCR = sum(diag(t)) / sum(t)
    
    output$TreeCMtable = renderTable({
      t
    })
    
    output$CCR = renderPrint({
      c('Tree model on test set, CCR: ',round(CCR,2))
    })
    
    
    #confusion matrix, accuracy for LogR
    Phat = predict(trainLR,test,type="response")
    head(Phat)
    prop.table(xtabs(~ ILF, data=test))
    thresh = 0.5
    Logpred = cut(Phat, breaks=c(-Inf, thresh, Inf), labels=c(0, 1))
    #cTab   = xtabs(~ ILF + Logpred, data=test)
    cTab   = table(Logpred,test$ILF)
    LogCCR = sum(diag(cTab)) / sum(cTab)
    LogCCR = sum(diag(cTab)) / sum(cTab)
    
    output$LogCMtable =  renderTable({
      cTab
    })
    
    output$LogCCR = renderPrint({
      c('LogReg on test set, CCR: ',round(LogCCR,2))
    })
   
    #----------------------------------
    #rerun for working aged
    #ddWA = df[df$AGEP < 65,c('ILF','AGEP',input$selected_vars)]
    ddWA = df[df$AGEP < 65,c('ILF',input$selected_vars)]
        trainRowsWA = caret::createDataPartition(ddWA[,1], p = 0.8, list=F, times=1)
    
    trainWA <- ddWA[trainRowsWA,]
    testWA <- ddWA[!row.names(ddWA) %in% row.names(trainWA),]
    trainWA$ILF <- factor(trainWA$ILF, levels = c("N", "L"))
    testWA$ILF  <- factor(testWA$ILF,  levels = c("N", "L"))
    
    #run tree model, 
    ib = .000001 #how much fit has to be improved by in order to keep splitting, smaller means more branches, default is .01, maybe go as low as .00001 - may overfit
    treeWA <- rpart(ILF~.,data=trainWA,control=rpart.control(cp=ib,maxdepth=6))
    
    
    ### user inputs a new value for cp and then prune and re-plot the tree:
    cpUser=.000001
    treepWA=prune(treeWA,cp=cpUser)
    output$tree_plotWA = renderPlot({
      rpart.plot(treepWA,box.palette = "Blues",main="Pruned Tree",cex=.5, space = .5)
    })
    
    
    
    #run LogR on all data CUT
    ILFLogregWA = glm(ILF ~ ., data=ddWA, family=binomial(link="logit"))
    
    output$model_summary_fullWA = renderPrint({
      summary(ILFLogregWA)
    })
    
    
    #trained
    trainLRWA = glm(ILF ~ ., data=trainWA, family=binomial(link="logit"))
    
    output$model_summaryWA = renderPrint({
      summary(trainLRWA)  
    })
    
    #predict on test data
    testWA$PredictedWA = round(predict(trainLRWA, testWA[,-1,drop = FALSE], type="response"), 2)
    
    
    #ROC curve with area for tree
    predWA <- predict(treeWA,newdata=testWA,type="prob") #use 'prob' 
    pred2WA <- ROCR::prediction(predWA[,"L"], testWA$ILF=="L") 
    perfWA <- ROCR::performance(pred2WA, "tpr", "fpr")
    aucWA <- performance(pred2WA, "auc")

    output$roc_plot_treeWA = renderPlot({
      plot(perfWA, col = "blue", lwd = 2, main = paste("Tree test model, Area Under the Curve:", round(aucWA@y.values[[1]], 3)))
    })
    
    #ROC curve for LogR
    predLRWA = predict(trainLRWA, testWA[,-1,drop = FALSE], type="response")
    pObjectWA = ROCR::prediction(predLRWA, testWA$ILF=="L" )
    rocObjWA = ROCR::performance(pObjectWA, measure="tpr", x.measure="fpr")
    aucObjWA = ROCR::performance(pObjectWA, measure="auc")  
    
    output$roc_plotlogrWA = renderPlot({
      plot(rocObjWA, main = paste("LogReg Model on test set, Area under the curve:", round(aucObjWA@y.values[[1]] ,4))) 
    })
    
    #confusion matrix, accuracy for tree

    TreepredWA <- predict(treepWA,newdata=testWA,type="class")
    tWA = table(TreepredWA,testWA$ILF)
    #correct classification rate
    CCRWA = sum(diag(tWA)) / sum(tWA)
    
    output$TreeCMtableWA = renderTable({
      tWA
      })
    
    output$CCRWA = renderPrint({
      c('Tree model on test set, CCRWA: ',round(CCRWA,2))
    })
    
    
    #confusion matrix, accuracy for LogR
    PhatWA = predict(trainLRWA,testWA,type="response")
    head(PhatWA)
    prop.table(xtabs(~ ILF, data=testWA))
    thresh = 0.5
    LogpredWA = cut(PhatWA, breaks=c(-Inf, thresh, Inf), labels=c("N", "L"))
    #cTabWA   = xtabs(~ ILF + LogpredWA, data=testWA)
    cTabWA   = table(LogpredWA,testWA$ILF)
    LogCCRWA = sum(diag(cTabWA)) / sum(cTabWA)

    
    output$LogCMtableWA =  renderTable({
      cTabWA
    })
    
    output$LogCCRWA = renderPrint({
      c('LogReg model on test set, CCRWA: ',round(LogCCRWA,2))
    })
    
    
  })
}

    






