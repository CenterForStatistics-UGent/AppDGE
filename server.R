
server <- function(input, output, session) {
  
  XscaleRange <- reactive({
    c(input$xlim[1],input$xlim[2])})
  XscaleRange2 <- reactive({
    c(input$xlim2[1],input$xlim2[2])})
  YscaleRange <- reactive({
    c(input$ylim[1],input$ylim[2])})
  YscaleRange.ROC <- reactive({
    c(input$ylim.roc[1],input$ylim.roc[2])})
  XscaleRange.ROC <- reactive({
    c(input$xlim.roc[1],input$xlim.roc[2])})
  
  
  biotype <- reactive({ #Zhang and NGP
    input$biotype
  })
  biotype2 <- reactive({ #GTEx
    input$biotype2
  })
  metrics <- reactive({
    input$metrics
  })
  PDE <- reactive({
    input$PDE
  })
  rep.size <- reactive({ #Zhang
    input$rep.size
  })
  thrld <- reactive({
    input$thrld
  })
  
  rep.size2 <- reactive({ #NGP
    input$rep.size2
  })
  rep.size3 <- reactive({ #GTEx
    input$rep.size3
  })
  
  
  # Opt.plot.1 <- reactive({
  #   input$check.opt.plot1
  #   if (input$check.opt.plot1){
  #     return(TRUE)
  #   } else {
  #     return(NULL)
  #   }
  # })
  Opt.plot.2 <- reactive({
    input$check.opt.plot2
    if (input$check.opt.plot2){
      return(TRUE)
    } else {
      return(NULL)
    }
  })
  
  output$perf.plot1.ui <- renderUI({
    plotOutput("perf.Plot1", height = ifelse(length(input$metrics) %in% c(1, 3) & length(input$biotype) == 1, "600px",
                                             ifelse(length(input$metrics) %in% c(1, 3) & length(input$biotype) == 2, "750px",
                                             ifelse(length(input$metrics) %in% c(2, 4), "850px",  "1000px"))),
               width = ifelse((length(input$biotype) == 1 & length(input$metrics) == 1), "60%", "100%"))
    
  })
  
  
  # output$top.DE.table1 <- renderUI({
  #   tableOutput("top.de.table1")
  # })
  # output$top.DE.table2 <- renderUI({
  #   tableOutput("top.de.table2")
  # })
  
  output$top.DE.ROC.curve <- renderUI({
    plotOutput("top.de.ROC", height = "500px", 
               width = ifelse(length(input$biotype)==2 & input$sim !="GTEx", "100%", "50%"))
  }) 
  
  
  observe({
    #toggle(condition = input$check.opt.plot1, selector = "#conditionedPanels li a[data-value=tab2]")
    toggle(condition = input$check.opt.plot2, selector = "#conditionedPanels li a[data-value=tab3]")
    toggle(condition = input$check.opt.plot3, selector = "#conditionedPanels li a[data-value=tab4]")
  })
  
  # observeEvent(input$check.opt.plot1, {
  #   if(input$check.opt.plot1==TRUE){
  #     updateTabsetPanel(session, "conditionedPanels", selected = "tab2") 
  #   }
  # })
  observeEvent(input$check.opt.plot2, {
    if(input$check.opt.plot2==TRUE){
      updateTabsetPanel(session, "conditionedPanels", selected = "tab3") 
    }
  })
  observeEvent(input$check.opt.plot3, {
    if(input$check.opt.plot3==TRUE){
      updateTabsetPanel(session, "conditionedPanels", selected = "tab4") 
    }
  })
  
  
  observeEvent(input$goToHome, {
      updateTabsetPanel(session, "conditionedPanels", selected = "tab1") 
  })
  
  #browse simulation quality assessemnt files
  observeEvent(input$simQualityZhang, {
    browseURL('source_files/QASDZhang.html')
  })
  observeEvent(input$simQualityNGP, {
    browseURL('source_files/QASDNGP.html')
  })
  observeEvent(input$simQualityGTEx, {
    browseURL('source_files/QASDGTEx.html')
  })
  
  observeEvent(input$dispType, ignoreInit = TRUE, ignoreNULL = FALSE, {
    if(is.null(input$dispType)==FALSE) {
      toggleState("show.DE.tool")
      toggleState("showCode")}
  })
  
  observeEvent(input$opt.plot1, ignoreInit = FALSE, ignoreNULL = FALSE, autoDestroy=FALSE,{
    if(input$opt.plot1) {
      enable("ind.thrld") }
    else {
      disable("ind.thrld") }
  })
  
  observeEvent(input$free.axis.DARC, ignoreInit = FALSE, ignoreNULL = FALSE, autoDestroy=FALSE,{
    if(input$free.axis.DARC == TRUE) {
      disable("xlim.DARC")
      disable("ylim.DARC")}
    else {
      enable("xlim.DARC")
      enable("ylim.DARC")}
  })
  
  observeEvent(input$opt.plot2.filt.method, ignoreInit = FALSE, ignoreNULL = FALSE, autoDestroy=FALSE,{
    if(input$opt.plot2.filt.method == "minimum read counts") {
      enable("opt.plot2.min.read")
      disable("opt.plot2.min.cpm")}
    else {
      disable("opt.plot2.min.read")
      enable("opt.plot2.min.cpm")}
  })
  
  
  observeEvent(input$free.axis.MDS, ignoreInit = FALSE, ignoreNULL = FALSE, autoDestroy=FALSE,{
    if(input$free.axis.MDS == TRUE) {
      disable("xlim.MDS")
      disable("ylim.MDS")}
    else {
      enable("xlim.MDS")
      enable("ylim.MDS")}
  })
  
  observeEvent(input$x.axis, ignoreInit = FALSE, ignoreNULL = TRUE, {
    if(input$x.axis == "rep.size") {
      disable("N.sim.sumr")
     enable("PDE.sim.sumr")}
    else if(input$x.axis == "prop.DE") {
      enable("N.sim.sumr")
      disable("PDE.sim.sumr")}
  })
  
  
  output$Optional.Plots1.ui <- renderUI({
    nops <- sum(input$opt.plot1, input$opt.plot2, input$opt.plot3, input$opt.plot4)
    height <- ifelse(nops >=3, "1000px", "600px")
    width  <- ifelse(nops > 1 , "100%", "70%")
    plotOutput("Optional.Plots1", height=height, width = width)
  })
  output$Optional.Plots2.ui <- renderUI({
    plotOutput("Optional.Plots2", height = "500px", width = "100%")
  })
  output$Optional.Plots2.ui.2 <- renderUI({
    if(input$showDGEsummary== "TRUE" & input$optPlot21=="VP"){
      plotOutput("Optional.Plots2.2", height = "300px", width = "100%")}
  })
  
  # output$R.codes <- renderUI({
  #   if(input$showCode == "TRUE"){
  #     verbatimTextOutput("r.codes")}
  # })

  
  #-----------------------------------------------------------------
  ##First Panel
  output$perf.Plot1      <- renderPlot({
    XscaleRange <- XscaleRange() 
    if(input$sim == "Zhang"){
      validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$biotype != '', 'Choose gene biotype.'),
               need(input$rep.size %in% c(2, 3, 4, 5, 7, 10, 15, 20, 30, 40), 
                    'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
      biotype     <- biotype()
      metrics     <- metrics()
      rep.size    <- rep.size()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.Zhang)
      #sim.result2  <- as.data.frame(result.Zhang.sumr)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    }
    else if(input$sim == "NGP Nutlin"){
      validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$biotype != '', 'Choose gene biotype.'))
      biotype     <- biotype()
      metrics     <- metrics()
      rep.size    <- rep.size2()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.NGP)
      #sim.result2  <- as.data.frame(result.NGP.sumr)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    }
    else if(input$sim == "GTEx"){
      validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$biotype != '', 'Only mRNA are studied for GTEx RNA-seq source data'))
      biotype     <- "mRNA"
      metrics     <- metrics()
      rep.size    <- rep.size3()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.GTEx)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    }
    
    if("All" %in% input$DE.tool) {DE.tool = methods.names}
    else                         {DE.tool = as.vector(sapply(input$DE.tool, convert.name2)) }
    
    withProgress(message = '', value = 0, {
      incProgress(0.1, detail = "Retrieving data ...") 
    df.sub       <-    sim.result[sim.result$DE.tool %in% DE.tool    &
                                    sim.result$prop.DE  == PDE         &
                                    sim.result$alpha    == thrld       &
                                    sim.result$rep.size == rep.size, ]
    sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
    df.sub   <- df.sub[ ,c(sel.cols, "rep.size", "prop.DE", "DE.tool", "alpha")]
    df.sub$DE.tool   <- as.vector(sapply(df.sub$DE.tool, convert.name))
    df.sub   <- melt(df.sub, id.vars = c("rep.size", "prop.DE", "DE.tool", "alpha")) 
    # df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
    # df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
    df.sub <- cbind(df.sub, str_split_fixed(as.character(df.sub$variable), "[.]",  n=2)) 
    colnames(df.sub) <- c("rep.size", "prop.DE", "DE.tool", "alpha", "var", "value", "metrics", "biotype")
    df.sub$var <- NULL
    df.sub <- tapply(df.sub$value, list(df.sub$DE.tool, df.sub$metrics, df.sub$biotype),
                     function(y){c(ll=max(0, mean(y)-1.96*sd(y)/sqrt(30)), mean=mean(y), ul=min(1, mean(y)+1.96*sd(y)/sqrt(30)))})
    df.sub <- plyr::adply(df.sub, 1:3)
    df.sub$X1 <- sapply(df.sub$X1, convert.name)
    df.sub$ref.line <- thrld ; df.sub$ref.line[df.sub$X2 != "FDR"] <- NA
    
    # df.sub2 <- sim.result2[ sim.result2$DE.tool %in% DE.tool &  sim.result2$prop.DE==PDE &
    #                           sim.result2$metrics %in% metrics &  sim.result2$biotype %in% biotype &
    #                           sim.result2$rep.size == rep.size &  sim.result2$alpha == thrld, ]
    # 
    
    #dec <- ifelse(input$metrics[1] == "FDR", T, F)
    #df.sub <- df.sub[order(df.sub$X2, df.sub$X3, df.sub$mean, decreasing = dec),]
    df.sub <- by(df.sub, list(df.sub$X2, df.sub$X3), function(y){
      if(all(y$X2 == "FDR")) {y <- y[order(y$mean, decreasing = TRUE),]
      y}
      else if(all(y$X2=="TPR")) {y <- y[order(y$mean, decreasing = FALSE),]
      y}
      else if(all(y$X2 == "FPR")) {y <- y[order(y$mean, decreasing = TRUE),]
      y}
      else if(all(y$X2=="TNR")) {y <- y[order(y$mean, decreasing = FALSE),]
      y}
      else if(all(y$X2=="FNR")) {y <- y[order(y$mean, decreasing = TRUE),]
      y}
    })
    df.sub <- as.data.frame(do.call(rbind, df.sub)) 
    #rownames(df.sub) <- 1:nrow(df.sub)
    df.sub$X1 <- factor(df.sub$X1, levels = unique(df.sub$X1))
    #plot_res(df.sub, XscaleRange)
    
    
    incProgress(0.2, detail = "Loading data ...") 
    plt <- ggplot(df.sub, aes(y=0, x=X1, xend=X1, yend=mean, group = interaction(X2, X3)))+
      #geom_bar(stat="identity", width=0.5, fill="steelblue")+
      geom_segment(stat="identity",  colour="gray", size=1.15)+
      geom_point(size=1.5, aes(y=mean, x=X1))+
      geom_errorbar(aes(ymin=ll, ymax=ul), width=0.5) 
      
      if(input$facet == "wrap"){
        dir = ifelse(input$sim == "GTEx", "h", "v")
        plt <- plt + facet_wrap(~X2+X3 ,  scales="free", dir = dir, 
                                nrow = min(length(metrics), length(biotype))) 
      }
      else{
        plt <- plt + facet_grid(X2~X3) 
      }
    
    plt <- plt + coord_cartesian(ylim = XscaleRange)+
      scale_y_continuous(breaks=seq(XscaleRange[1], XscaleRange[2], length.out = 3), 
                         labels=seq(XscaleRange[1], XscaleRange[2], length.out = 3)) +
      coord_flip(ylim = XscaleRange) + 
      theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
            axis.text.y=element_text(size = 12, colour = "gray30"),
            panel.background = element_rect(fill = "gray90"),
            panel.grid.major = element_line(colour = "white"),
            strip.background = element_rect(colour = "white", fill = "gray60"),
            strip.text = element_text(size = 17, colour = "black"),
            panel.grid.major.y = element_blank(),
            legend.title = element_blank(),
            legend.position="top")+ 
      geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
      labs(y=NULL, x=NULL) 
    
    plt
    
    })
    })
  output$top.de.table1   <- renderTable(striped = TRUE, rownames = TRUE, 
                                        caption = "For mRNA",
                                        caption.placement = getOption("xtable.caption.placement", "top"), 
                                        caption.width = getOption("xtable.caption.width", NULL), {
                                          if(input$sim == "Zhang"){
                                            validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 24'),
                                                     need(input$rep.size %in% c(2, 3, 4, 5, 7, 10, 15, 20, 30, 40), 
                                                          'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
                                            biotype     <- c("mRNA", "lncRNA")
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.Zhang)
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          else if(input$sim == "NGP Nutlin"){
                                            validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 24'))
                                            biotype     <- c("mRNA", "lncRNA")
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size2()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.NGP) 
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          else if(input$sim == "GTEx"){
                                            validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 24'))
                                            biotype     <- "mRNA"
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size2()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.GTEx)
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          
                                          if("All" %in% input$DE.tool) {DE.tool = methods.names}
                                          else                         {DE.tool = as.vector(sapply(input$DE.tool, convert.name2)) }
                                          
                                          df.sub       <-    sim.result[sim.result$DE.tool %in% DE.tool    &
                                                                          sim.result$prop.DE  == PDE         &
                                                                          sim.result$alpha    == thrld       &
                                                                          sim.result$rep.size == rep.size, ]
                                          sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
                                          df.sub   <- df.sub[ ,c(sel.cols, "rep.size", "prop.DE", "DE.tool", "alpha")]
                                          df.sub   <- melt(df.sub, id.vars = c("rep.size", "prop.DE", "DE.tool", "alpha")) 
                                          df.sub$DE.tool   <- as.vector(sapply(df.sub$DE.tool, convert.name))
                                          df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
                                          df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
                                          df.sub <- tapply(df.sub$value, list(df.sub$DE.tool, df.sub$metrics, df.sub$biotype),
                                                           function(y){c(ll=mean(y)-1.96*sd(y)/sqrt(30), mean=mean(y), ul=mean(y)+1.96*sd(y)/sqrt(30))})
                                          df.sub <- plyr::adply(df.sub, 1:3)
                                          df.sub$X1 <- sapply(df.sub$X1, convert.name)
                                          k2  <- input$k
                                          ord <- by(df.sub, list(df.sub$X2, df.sub$X3), function(x) {
                                            if     (all(x$X2 == "FDR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                            else if(all(x$X2 == "TPR")) {as.character(x$X1[order(x$mean, decreasing = TRUE) [1:k2]])}
                                            else if(all(x$X2 == "FNR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                            else if(all(x$X2 == "TNR")) {as.character(x$X1[order(x$mean, decreasing = TRUE) [1:k2]])}
                                            else if(all(x$X2 == "FPR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                          })
                                          
                                          # top.DE.tools <- matrix(NA, nrow=k2, ncol=length(metrics))
                                          # colnames(top.DE.tools) <-  sort(metrics)
                                          # for(i in 1:length(metrics)){
                                          #   top.DE.tools[, i] <- ord[[i]]
                                          # }
                                          # as.data.frame(top.DE.tools)
                                          
                                          
                                          #K tools with the lowest FDR and the highest TPR
                                          lowest_fdr.mRNA <- df.sub$X1[which(df.sub$X2=="FDR"& df.sub$X3 == "mRNA" & 
                                                                               (df.sub$mean <=  thrld | df.sub$mean %in% sort(df.sub$mean[df.sub$X2 =="FDR" & df.sub$X3 == "mRNA"])[1:k2]))]
                                          highest_tpr_among_lowest_fdr.mRNA <- lowest_fdr.mRNA[order(df.sub$mean[df.sub$X1 %in% lowest_fdr.mRNA & df.sub$X2 == "TPR"& df.sub$X3 == "mRNA"], decreasing = TRUE)][1:k2]
                                          
                                          
                                          #K tools with the highest TPR and the lowest FDR
                                          highest_tpr.mRNA <- df.sub$X1[which(df.sub$X2=="TPR"& df.sub$X3 == "mRNA" & 
                                                                                (df.sub$mean %in% sort(df.sub$mean[df.sub$X2 =="TPR" & df.sub$X3 == "mRNA"], TRUE)[1:k2]))]
                                          lowest_fdr_among_lowest_fdr.mRNA <- highest_tpr.mRNA[order(df.sub$mean[df.sub$X1 %in% highest_tpr.mRNA & df.sub$X2 == "FDR"& df.sub$X3 == "mRNA"], decreasing = FALSE)][1:k2]
                                          
                                          list.top.tools.mRNA <- data.table(highest_tpr_among_lowest_fdr.mRNA, lowest_fdr_among_lowest_fdr.mRNA)
                                          colnames(list.top.tools.mRNA) <- c("Actual FDR at most the nominal FDR (order by TPR)", "The highest TPR (order by FDR)")
                                          
                                          #if("mRNA"%in% input$biotype) {list.top.tools.mRNA}
                                          if("mRNA" %in% input$biotype) {
                                            if("FDR" %in% input$metrics || "TPR"%in% input$metrics) {
                                              list.top.tools.mRNA
                                            }
                                            else if("FDR" %in% input$metrics) {
                                              list.top.tools.mRNA[,1]
                                            }
                                            else if("TPR" %in% input$metrics) {
                                              list.top.tools.mRNA[,2]
                                            }
                                          }
                                        })
  
  output$top.de.table2   <- renderTable(striped = TRUE, rownames = TRUE, caption = "For lncRNA",
                                        caption.placement = getOption("xtable.caption.placement", "top"), 
                                        caption.width = getOption("xtable.caption.width", NULL),{
                                          if(input$sim == "Zhang"){
                                            validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$biotype != '', 'Choose gene biotype.'),
                                                     need(input$rep.size %in% c(2, 3, 4, 5, 7, 10, 15, 20, 30, 40), 
                                                          'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
                                            biotype     <- c("mRNA", "lncRNA")
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.Zhang)
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          else if(input$sim == "NGP Nutlin"){
                                            validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 13'),
                                                     need(input$biotype != '', 'Choose gene biotype.'))
                                            biotype     <- c("mRNA", "lncRNA")
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size2()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.NGP)
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          else if(input$sim == "GTEx"){
                                            validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                                                     need(input$metrics != '', 'Select performance comparison metric(s).'),
                                                     need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 13'))
                                            biotype     <- "mRNA"
                                            metrics     <- c("FDR", "TPR")
                                            rep.size    <- rep.size2()
                                            PDE         <- PDE()
                                            thrld       <- thrld()
                                            sim.result  <- as.data.frame(result.GTEx)
                                            #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
                                          }
                                          
                                          if("All" %in% input$DE.tool) {DE.tool = methods.names2}
                                          else                         {DE.tool = as.vector(sapply(input$DE.tool, convert.name2)) }
                                          
                                          df.sub       <-    sim.result[sim.result$DE.tool %in% DE.tool    &
                                                                          sim.result$prop.DE  == PDE         &
                                                                          sim.result$alpha    == thrld       &
                                                                          sim.result$rep.size == rep.size, ]
                                          sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
                                          df.sub   <- df.sub[ ,c(sel.cols, "rep.size", "prop.DE", "DE.tool", "alpha")]
                                          df.sub   <- melt(df.sub, id.vars = c("rep.size", "prop.DE", "DE.tool", "alpha")) 
                                          df.sub$DE.tool   <- as.vector(sapply(df.sub$DE.tool, convert.name))
                                          df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
                                          df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
                                          df.sub <- tapply(df.sub$value, list(df.sub$DE.tool, df.sub$metrics, df.sub$biotype),
                                                           function(y){c(ll=mean(y)-1.96*sd(y)/sqrt(30), mean=mean(y), ul=mean(y)+1.96*sd(y)/sqrt(30))})
                                          df.sub <- plyr::adply(df.sub, 1:3)
                                          df.sub$X1 <- sapply(df.sub$X1, convert.name)
                                          k2  <- input$k
                                          ord <- by(df.sub, list(df.sub$X2, df.sub$X3), function(x) {
                                            if     (all(x$X2 == "FDR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                            else if(all(x$X2 == "TPR")) {as.character(x$X1[order(x$mean, decreasing = TRUE) [1:k2]])}
                                            else if(all(x$X2 == "FNR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                            else if(all(x$X2 == "TNR")) {as.character(x$X1[order(x$mean, decreasing = TRUE) [1:k2]])}
                                            else if(all(x$X2 == "FPR")) {as.character(x$X1[order(x$mean, decreasing = FALSE)[1:k2]])}
                                          })
                                          
                                          # top.DE.tools <- matrix(NA, nrow=input$k, ncol=length(metrics))
                                          # colnames(top.DE.tools) <-  sort(metrics)
                                          # 
                                          # if(length(biotype)==2) {jo = 1} else {jo=0}
                                          # for(i in 1:length(metrics)){
                                          #   top.DE.tools[, i] <- ord[[i+jo]]
                                          # }
                                          # as.data.frame(top.DE.tools)
                                          #K tools with the lowest FDR and the highest TPR
                                          lowest_fdr.lncRNA <- df.sub$X1[which(df.sub$X2=="FDR"& df.sub$X3 == "lncRNA" & 
                                                                                 (df.sub$mean <=  thrld | df.sub$mean %in% sort(df.sub$mean[df.sub$X2 =="FDR" & df.sub$X3 == "lncRNA"])[1:k2]))]
                                          highest_tpr_among_lowest_fdr.lncRNA <- lowest_fdr.lncRNA[order(df.sub$mean[df.sub$X1 %in% lowest_fdr.lncRNA & df.sub$X2 == "TPR"& df.sub$X3 == "lncRNA"], decreasing = TRUE)][1:k2]
                                          
                                          
                                          #K tools with the highest TPR and the lowest FDR
                                          highest_tpr.lncRNA <- df.sub$X1[which(df.sub$X2=="TPR"& df.sub$X3 == "lncRNA" & 
                                                                                  (df.sub$mean %in% sort(df.sub$mean[df.sub$X2 =="TPR" & df.sub$X3 == "lncRNA"], TRUE)[1:k2]))]
                                          lowest_fdr_among_lowest_fdr.lncRNA <- highest_tpr.lncRNA[order(df.sub$mean[df.sub$X1 %in% highest_tpr.lncRNA & df.sub$X2 == "FDR"& df.sub$X3 == "lncRNA"], decreasing = FALSE)][1:k2]
                                          
                                          list.top.tools.lncRNA <- data.table(highest_tpr_among_lowest_fdr.lncRNA, lowest_fdr_among_lowest_fdr.lncRNA)
                                          colnames(list.top.tools.lncRNA) <- c("Actual FDR at most the nominal FDR (order by TPR)", "The highest TPR (order by FDR)")
                                          
                                          #if("lncRNA"%in% input$biotype) {list.top.tools.lncRNA}
                                          if("lncRNA" %in% input$biotype) {
                                            if("FDR" %in% input$metrics || "TPR"%in% input$metrics) {
                                              list.top.tools.lncRNA
                                            }
                                            else if( "FDR" %in% input$metrics) {
                                              list.top.tools.lncRNA[,1]
                                            }
                                            else if("TPR" %in% input$metrics) {
                                              list.top.tools.lncRNA[,2]
                                            }
                                          }
                                        })
  
  output$top.de.ROC   <- renderPlot({
    XscaleRange <- XscaleRange.ROC() 
    YscaleRange <- YscaleRange.ROC() 
    if(input$sim == "Zhang"){
      validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$biotype != '', 'Choose gene biotype.'),
               need(input$rep.size %in% c(2, 3, 4, 5, 7, 10, 15, 20, 30, 40), 
                    'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
      biotype     <- biotype()
      metrics     <- c("FDR", "TPR")
      rep.size    <- rep.size()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.Zhang)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    }
    else if(input$sim == "NGP Nutlin"){
      validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 13'),
               need(input$biotype != '', 'Choose gene biotype.'))
      biotype     <- biotype()
      metrics     <- c("FDR", "TPR")
      rep.size    <- rep.size2()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.NGP)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    }
    else if(input$sim == "GTEx"){
      validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
               need(input$metrics != '', 'Select performance comparison metric(s).'),
               need(input$k != '', 'Add number of top DE tools; value has to be between 1 and 13'))
      biotype     <- "mRNA"
      metrics     <- c("FDR", "TPR")
      rep.size    <- rep.size2()
      PDE         <- PDE()
      thrld       <- thrld()
      sim.result  <- as.data.frame(result.GTEx)
      #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
    } 
    
    if("All" %in% input$DE.tool) {DE.tool = methods.names}
    else                         {DE.tool = as.vector(sapply(input$DE.tool, convert.name2)) }
    
    df.sub       <-    sim.result[sim.result$DE.tool %in% DE.tool    &
                                    sim.result$prop.DE  == PDE         &
                                    sim.result$rep.size == rep.size, ]
    sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
    df.sub   <- df.sub[ ,c(sel.cols, "rep.size", "prop.DE", "DE.tool", "alpha")]
    df.sub   <- melt(df.sub, id.vars = c("rep.size", "prop.DE", "DE.tool", "alpha")) 
    #df.sub$DE.tool   <- as.vector(sapply(df.sub$DE.tool, convert.name))
    df.sub <- cbind(df.sub, str_split_fixed(as.character(df.sub$variable), "[.]",  n=2)) 
    colnames(df.sub) <- c("rep.size", "prop.DE", "DE.tool", "alpha", "var", "value", "metrics", "biotype")
    df.sub$var <- NULL
    df.sub <- tapply(df.sub$value, list(df.sub$DE.tool, df.sub$metrics, df.sub$biotype, df.sub$alpha),
                     function(y){c(ll=mean(y)-1.96*sd(y)/sqrt(length(y)), 
                                   mean=mean(y),
                                   ul=mean(y)+1.96*sd(y)/sqrt(length(y)))})
    df.sub <- plyr::adply(df.sub, 1:4)
    df.sub$X1 <- sapply(df.sub$X1, convert.name)
    
    #head(df.sub)
    df.sub$ll <- NULL ; df.sub$ul <- NULL
    df.sub2 <- dcast(df.sub, X1+X3+X4~X2, value.var="mean")
    #head(df.sub2)
    
    k2  <- input$k
    
    
    # choose.top.K.tools.mRNA <- df.sub2$X1[df.sub2$X3=="mRNA" & df.sub2$X4== thrld & 
    #                                         df.sub2$FDR <= ifelse(sort(df.sub2$FDR[df.sub2$X3=="mRNA" & df.sub2$X4== thrld])[k2+1] <= thrld,
    #                                                               max(df.sub2$FDR[df.sub2$X3=="mRNA"  & df.sub2$X4== thrld & df.sub2$FDR<=thrld]),
    #                                                               sort(df.sub2$FDR[df.sub2$X3=="mRNA" & df.sub2$X4== thrld])[k2]) &
    #                                         df.sub2$TPR >= sort(df.sub2$TPR[df.sub2$X3=="mRNA" & df.sub2$X4== thrld & 
    #                                                                           df.sub2$FDR <= ifelse(sort(df.sub2$FDR[df.sub2$X3=="mRNA" & df.sub2$X4== thrld])[k2+1] <= thrld,
    #                                                                                                 max(df.sub2$FDR[df.sub2$X3=="mRNA"  & df.sub2$X4== thrld & df.sub2$FDR<=thrld]),
    #                                                                                                 sort(df.sub2$FDR[df.sub2$X3=="mRNA" & df.sub2$X4== thrld])[k2])],
    #                                                             decreasing =  TRUE)[k2]]
    # 
    # choose.top.K.tools.lncRNA <- df.sub2$X1[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld & 
    #                                           df.sub2$FDR <= ifelse(sort(df.sub2$FDR[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld])[k2+1] <= thrld,
    #                                                                 max(df.sub2$FDR[df.sub2$X3=="lncRNA"  & df.sub2$X4== thrld & df.sub2$FDR<=thrld]),
    #                                                                 sort(df.sub2$FDR[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld])[k2]) &
    #                                           df.sub2$TPR >= sort(df.sub2$TPR[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld & 
    #                                                                             df.sub2$FDR <= ifelse(sort(df.sub2$FDR[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld])[k2+1] <= thrld,
    #                                                                                                   max(df.sub2$FDR[df.sub2$X3=="lncRNA"  & df.sub2$X4== thrld & df.sub2$FDR<=thrld]),
    #                                                                                                   sort(df.sub2$FDR[df.sub2$X3=="lncRNA" & df.sub2$X4== thrld])[k2])],
    #                                                               decreasing =  TRUE)[k2]]
    
    
    #head(df.sub2)
    if("mRNA" %in% biotype){
    auc.mRNA <- data.frame(auc= as.vector(by(df.sub2[df.sub2$X3=="mRNA", c("FDR", "TPR")], df.sub2[df.sub2$X3=="mRNA", "X1"], 
                           function(x) pracma::trapz(x[,1], x[,2]))), row.names = unique(df.sub2$X1))
    choose.top.K.tools.mRNA <- rownames(auc.mRNA)[auc.mRNA$auc >= sort(auc.mRNA$auc, decreasing = T)[k2]][1:k2]
    df.sub3.mRNA   <- df.sub2[df.sub2$X4 == input$thrld & df.sub2$X3 == "mRNA" & df.sub2$X1 %in% choose.top.K.tools.mRNA, ]
    }
    
    if("lncRNA" %in% biotype){
      auc.lncRNA <- data.frame(auc= as.vector(by(df.sub2[df.sub2$X3=="lncRNA", c("FDR", "TPR")], df.sub2[df.sub2$X3=="lncRNA", "X1"], 
                                                     function(x) pracma::trapz(x[,1], x[,2]))), 
                               row.names = unique(df.sub2$X1))
      choose.top.K.tools.lncRNA <- rownames(auc.lncRNA)[auc.lncRNA$auc >= sort(auc.lncRNA$auc, decreasing = T)[k2]][1:k2]
      df.sub3.lncRNA <- df.sub2[df.sub2$X4 == input$thrld & df.sub2$X3 == "lncRNA" & df.sub2$X1 %in% choose.top.K.tools.lncRNA, ]
    }
    
    #head(auc)                                       
    
    
    
    
    roc.plot.list <- list()
    if("mRNA" %in% biotype){
      roc.plot.list[["roc.plt.mRNA"]] <- ggplot(df.sub2[df.sub2$X1 %in% choose.top.K.tools.mRNA & df.sub2$X3 == "mRNA", ],
                                                aes(x=FDR, y=TPR, group = X1, colour=X1))+
        geom_line(position=position_dodge(0), size=1.3)+ 
        geom_point(data=df.sub3.mRNA, position=position_dodge(0), size=5,  shape = 21, fill="white", stroke = 2,
                   aes(x=FDR, y=TPR, group=X1, colour = X1))+ 
        
        #geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
        coord_cartesian(ylim = YscaleRange, xlim = XscaleRange)+
        scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                           labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
        scale_x_continuous(breaks=seq(XscaleRange[1], XscaleRange[2], length.out = 3), 
                           labels=seq(XscaleRange[1], XscaleRange[2], length.out = 3)) + 
        geom_abline(intercept = 0, slope = 1)+
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 17, colour = "black"),
              plot.title =element_text(size=18, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.title = element_blank(),
              legend.position = c(0.7, 0.2),
              legend.box = "horizontal",
              legend.box.background = element_rect(fill = NA, colour = NA, color=NA ),
              legend.justification = "left") +
        labs(y="TPR", x="actual FDR",  title="Partial ROC curve (mRNA)")
    }
    else{roc.plot.list[["roc.plt.mRNA"]] <- NULL}
    
    if("lncRNA" %in% biotype){
      roc.plot.list[["roc.plt.lncRNA"]] <- ggplot(df.sub2[df.sub2$X1 %in% choose.top.K.tools.lncRNA & df.sub2$X3 == "lncRNA", ],
                                                  aes(x=FDR, y=TPR, group = X1, colour=X1))+
        geom_line(position=position_dodge(0), size=1.3)+ 
        geom_point(data=df.sub3.lncRNA, position=position_dodge(0), size=5,  shape = 21, fill="white", stroke = 2,
                   aes(x=FDR, y=TPR, group=X1, colour = X1))+ 
        
        #geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
        coord_cartesian(ylim = YscaleRange, xlim = XscaleRange)+
        scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                           labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
        scale_x_continuous(breaks=seq(XscaleRange[1], XscaleRange[2], length.out = 3), 
                           labels=seq(XscaleRange[1], XscaleRange[2], length.out = 3)) + 
        geom_abline(intercept = 0, slope = 1)+
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 17, colour = "black"),
              plot.title =element_text(size=18, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.title = element_blank(),
              legend.position = c(0.2, 0.8),
              legend.box = "horizontal",
              legend.box.background = element_rect(fill = NA, colour = NA, color=NA ),
              legend.justification = "left") +
        labs(y="TPR", x="actual FDR",  title="Partial ROC curve (lncRNA)")
    }
    else{roc.plot.list[["roc.plt.lncRNA"]] <- NULL}
    
    
    # remove the null plots from ptlist and wtlist
    to_delete      <- !sapply(roc.plot.list, is.null)
    roc.plot.list  <- roc.plot.list[to_delete] 
     
    if(length(roc.plot.list)==0) return(NULL)
    else{grid.arrange(grobs=roc.plot.list, ncol=length(biotype), nrow = 1)}
  })
  
  output$topDEtools_note <- renderText({
    validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
             need(input$metrics != '', 'Select performance comparison metric(s).'),
             need(input$biotype != '', 'Choose gene biotype.'),
             need(input$rep.size %in% c(2:15, 20, 30, 40), 
                  'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
    paste0("The ROC curve displays the top ", input$k, " DE tools at a given scenario. It chooses the top DE tools based on their area under the ROC curve 
          (combining the actual FDR and TPR ) for each biotype. The open circles on each curve represent the acual FDR and TPR at the choosen nominal threshold (",
           input$thrld*100, "%).")
  })
  
  #-----------------------------------------------------------------
  ##Second Panel
  output$Optional.Plots1 <- renderPlot({ 
    #Opt.plot.1  <- Opt.plot.1()
    #if(Opt.plot.1){ 
      YscaleRange <- YscaleRange()
      XscaleRange2 <- XscaleRange2()
      if(input$sim == "Zhang"){
        validate(need(input$DE.tool != '', 'Select DE tool(s)!'),
                 need(input$metrics != '', 'Select performance comparison metric(s).'),
                 need(input$biotype != '', 'Choose gene biotype.'),
                 need(input$rep.size %in% c(2, 3, 4, 5, 7, 10, 15, 20, 30, 40), 
                      'Numbeer of samples needs to be one of the values in (2, 3, 4, 5, 7, 10, 15, 20, 30, 40).'))
        biotype     <- biotype()
        metrics     <- metrics()
        rep.size    <- rep.size()
        PDE         <- PDE()
        thrld       <- thrld()
        sim.result  <- as.data.frame(result.Zhang)
        #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
      }
      else if(input$sim == "NGP Nutlin"){
        validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                 need(input$metrics != '', 'Select performance comparison metric(s).'),
                 need(input$biotype != '', 'Choose gene biotype.'))
        biotype     <- biotype()
        metrics     <- metrics()
        rep.size    <- rep.size2()
        PDE         <- PDE()
        thrld       <- thrld()
        sim.result  <- as.data.frame(result.NGP)
        #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
      }
      else if(input$sim == "GTEx"){
        validate(need(input$DE.tool  != '', 'Select DE tool(s)!'),
                 need(input$metrics != '', 'Select performance comparison metric(s).'),
                 need(input$biotype != '', 'Only mRNA is allowed'))
        biotype     <- "mRNA"
        metrics     <- metrics()
        rep.size    <- rep.size3()
        PDE         <- PDE()
        thrld       <- thrld()
        sim.result  <- as.data.frame(result.GTEx)
        #sim.result$DE.tool <- as.vector(sapply(sim.result$DE.tool, convert.name))
      }
      
      opt.plots.list <- list()
      if(input$opt.plot1){
        #Performance at different  proportion of true DE genes
        df.sub   <- sim.result[sim.result$DE.tool      == as.vector(sapply(input$sub.DE.tool, convert.name2)) &
                                 sim.result$prop.DE    == PDE &
                                 sim.result$rep.size   == rep.size, ]
        sel.cols <- as.vector(outer(c("FDR", "TPR"), biotype, function(x,y) paste0(x, ".", y)))
        df.sub   <- df.sub[, c(sel.cols, "rep.size", "alpha","DE.tool", "prop.DE")] 
        df.sub <- as.data.frame(do.call("rbind", by(df.sub, df.sub[, c("rep.size", "alpha", "DE.tool", "prop.DE")], function(x) {
          yy   <- x[, sel.cols]
          vals <- apply(as.matrix(yy), 2, mean)
          xx   <- x[, c("rep.size", "alpha", "DE.tool", "prop.DE")]
          vals <- c(vals, xx[!duplicated(xx),])
          names(vals) <- c(paste0(rep(sel.cols, each=1), ".", c("mean")),"rep.size", "alpha", "DE.tool", "prop.DE")
          vals
        })))
        df.sub[, which(colnames(df.sub) != "DE.tool")]  <- sapply(df.sub[, which(colnames(df.sub) != "DE.tool")], function(x) as.numeric(x))
        df.sub[, which(colnames(df.sub) == "DE.tool")]  <- as.character(df.sub[, which(colnames(df.sub) == "DE.tool")])
        
        df.sub         <- melt(df.sub, id.vars = c("rep.size", "alpha", "DE.tool", "prop.DE"))
        df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
        df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
        df.sub$limits  <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[3])
        df.sub$variable <- NULL
        df.sub <- dcast(df.sub, DE.tool+rep.size+biotype+prop.DE+alpha~metrics)
        
        df.sub2 <- df.sub[df.sub$alpha == input$ind.thrld, ]
        w=0.025
        opt.plots.list[["opt.plot1"]] <- 
          ggplot(df.sub, aes(x=FDR, y=TPR, group=biotype, colour = biotype))+
          geom_line(position=position_dodge(0), size=1.3)+
          #geom_point(position=position_dodge(0), size=3)+ 
          geom_point(data=df.sub2, position=position_dodge(0), size=5,  shape = 21, fill="white", stroke = 2,
                     aes(x=FDR, y=TPR, group=biotype, colour = biotype))+ 
          geom_segment(data=df.sub2, aes(x=FDR,         y=rep(0, nrow(df.sub2)), 
                                       xend = FDR, yend = TPR), colour = "red", lty=3, size=1.05)+
          geom_segment(data=df.sub2, aes(x=rep(0, nrow(df.sub2)),  y=TPR, 
                                         xend = FDR, yend = TPR), colour = "red", lty=3, size=1.05)+
          #geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
          coord_cartesian(ylim = YscaleRange, xlim = XscaleRange2)+
          scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                             labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
          scale_x_continuous(breaks=seq(XscaleRange2[1], XscaleRange2[2], length.out = 3), 
                             labels=seq(XscaleRange2[1], XscaleRange2[2], length.out = 3)) +
          scale_color_manual("legend", values = c("orange", "deepskyblue3")) + 
          geom_abline(intercept = 0, slope = 1)+
          theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
                axis.text.y=element_text(size = 15, colour = "gray30"),
                axis.title = element_text(size = 17, colour = "black"),
                plot.title =element_text(size=18, face="bold"),
                panel.background = element_rect(fill = "gray90"),
                panel.grid.major = element_line(colour = "white"),
                strip.background = element_rect(colour = "white", fill = "gray60"),
                strip.text = element_text(size = 17, colour = "black"),
                panel.grid.major.y = element_blank(),
                legend.text= element_text(size=15),
                legend.key.size  = unit(2,"line"),
                legend.key = element_rect(colour = "transparent", fill = "white"),
                legend.title = element_blank(),
                legend.position = c(0.8, 0.2),
                legend.box = "horizontal",
                legend.box.background = element_rect(fill = NA, colour = NA, color=NA ),
                legend.justification = "left") +
        labs(y="TPR", x="actual FDR",  title="Partial ROC curve") #title ="Performance of selected DE tool at different proportion of true DE genes.") 
        
      } else{opt.plots.list[["opt.plot1"]] <- NULL}
      if(input$opt.plot2){
        #Performance at different sample size
        df.sub   <- sim.result[sim.result$DE.tool   == as.vector(sapply(input$sub.DE.tool, convert.name2))  &
                                 sim.result$prop.DE == PDE &
                                 sim.result$alpha   == thrld, ]
        sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
        df.sub   <- df.sub[, c(sel.cols, "rep.size", "alpha","DE.tool", "prop.DE")] 
        df.sub <- as.data.frame(do.call("rbind", by(df.sub, df.sub[, c("rep.size", "alpha", "DE.tool", "prop.DE")], function(x) {
          yy   <- x[, sel.cols]
          vals <- apply(as.matrix(yy), 2, function(y) {c(max(0, mean(y)-1.96*sd(y)/sqrt(30)),
                                                         mean(y), min(1,mean(y)+1.96*sd(y)/sqrt(30)))})
          xx   <- x[, c("rep.size", "alpha", "DE.tool", "prop.DE")]
          vals <- c(vals, xx[!duplicated(xx),])
          names(vals) <- c(paste0(rep(sel.cols, each=3), ".", c("ll", "mean", "ul")),"rep.size", "alpha", "DE.tool", "prop.DE")
          vals
        })))
        df.sub[, which(colnames(df.sub) != "DE.tool")]  <- sapply(df.sub[, which(colnames(df.sub) != "DE.tool")], function(x) as.numeric(x))
        df.sub[, which(colnames(df.sub) == "DE.tool")]  <- as.character(df.sub[, which(colnames(df.sub) == "DE.tool")])
        
        df.sub <- melt(df.sub, id.vars = c("rep.size", "alpha", "DE.tool", "prop.DE"))
        df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
        df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
        df.sub$limits  <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[3])
        df.sub$variable <- NULL
        df.sub <- dcast(df.sub, DE.tool+rep.size+biotype+metrics+prop.DE+alpha~limits)
        
        if(max(as.numeric(df.sub$rep.siz)) == 40) {w=1} else {w=0.25}
        opt.plots.list[["opt.plot2"]] <- ggplot(df.sub, aes(x=rep.size, y=mean, group=interaction(biotype, metrics), 
                                                            colour = metrics, lty=biotype))+
          geom_line(position=position_dodge(0), size=1.3)+
          geom_point(position=position_dodge(0), size=3)+ 
          geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
          coord_cartesian(ylim = YscaleRange)+
          scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                             labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
          scale_color_manual("legend", values = c("orange", "deepskyblue3")) + 
          ggplot2.theme+ 
          labs(y=NULL, x="number of replicates per condition", title="") #,  title ="Performance of selected DE tool at different number of replicates") 
        
        
      } else{opt.plots.list[["opt.plot2"]] <- NULL}
      if(input$opt.plot3){
        #Performance at different  proportion of true DE genes
        df.sub   <- sim.result[sim.result$DE.tool      == as.vector(sapply(input$sub.DE.tool, convert.name2)) &
                                 sim.result$alpha      == thrld &
                                 sim.result$rep.size   == rep.size, ]
        sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
        df.sub   <- df.sub[, c(sel.cols, "rep.size", "alpha","DE.tool", "prop.DE")] 
        df.sub <- as.data.frame(do.call("rbind", by(df.sub, df.sub[, c("rep.size", "alpha", "DE.tool", "prop.DE")], function(x) {
          yy   <- x[, sel.cols]
          vals <- apply(as.matrix(yy), 2, function(y) {c(max(0, mean(y)-1.96*sd(y)/sqrt(30)),
                                                         mean(y), min(1,mean(y)+1.96*sd(y)/sqrt(30)))})
          xx   <- x[, c("rep.size", "alpha", "DE.tool", "prop.DE")]
          vals <- c(vals, xx[!duplicated(xx),])
          names(vals) <- c(paste0(rep(sel.cols, each=3), ".", c("ll", "mean", "ul")),"rep.size", "alpha", "DE.tool", "prop.DE")
          vals
        })))
        df.sub[, which(colnames(df.sub) != "DE.tool")]  <- sapply(df.sub[, which(colnames(df.sub) != "DE.tool")], function(x) as.numeric(x))
        df.sub[, which(colnames(df.sub) == "DE.tool")]  <- as.character(df.sub[, which(colnames(df.sub) == "DE.tool")])
        
        df.sub <- melt(df.sub, id.vars = c("rep.size", "alpha", "DE.tool", "prop.DE"))
        df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
        df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
        df.sub$limits  <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[3])
        df.sub$variable <- NULL
        df.sub <- dcast(df.sub, DE.tool+rep.size+biotype+metrics+prop.DE+alpha~limits)
        df.sub <- df.sub[df.sub$prop.DE != 0, ]
        w=0.025
        opt.plots.list[["opt.plot3"]] <- ggplot(df.sub, aes(x=prop.DE, y=mean, group=interaction(biotype, metrics), 
                                                            colour = metrics, lty=biotype))+
          geom_line(position=position_dodge(0), size=1.3)+
          geom_point(position=position_dodge(0), size=3)+ 
          geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
          coord_cartesian(ylim = YscaleRange)+
          scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                             labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
          scale_color_manual("legend", values = c("orange", "deepskyblue3")) + 
          ggplot2.theme+ 
          labs(y=NULL, x="proportion of true DE genes",  title="") #title ="Performance of selected DE tool at different proportion of true DE genes.") 
        
      } else{opt.plots.list[["opt.plot3"]] <- NULL}
      if(input$opt.plot4){
        #Performance at different FDR threshold
        df.sub   <- sim.result[sim.result$DE.tool      == as.vector(sapply(input$sub.DE.tool, convert.name2))  &
                                 sim.result$prop.DE    == PDE &
                                 sim.result$rep.size   == rep.size, ]
        sel.cols <- as.vector(outer(metrics, biotype, function(x,y) paste0(x, ".", y)))
        df.sub   <- df.sub[, c(sel.cols, "rep.size", "alpha","DE.tool", "prop.DE")] 
        df.sub <- as.data.frame(do.call("rbind", by(df.sub, df.sub[, c("rep.size", "alpha", "DE.tool", "prop.DE")], function(x) {
          yy   <- x[, sel.cols]
          vals <- apply(as.matrix(yy), 2, function(y) {c(max(0, mean(y)-1.96*sd(y)/sqrt(30)),
                                                         mean(y), min(1,mean(y)+1.96*sd(y)/sqrt(30)))})
          xx   <- x[, c("rep.size", "alpha", "DE.tool", "prop.DE")]
          vals <- c(vals, xx[!duplicated(xx),])
          names(vals) <- c(paste0(rep(sel.cols, each=3), ".", c("ll", "mean", "ul")),"rep.size", "alpha", "DE.tool", "prop.DE")
          vals
        })))
        df.sub[, which(colnames(df.sub) != "DE.tool")]  <- sapply(df.sub[, which(colnames(df.sub) != "DE.tool")], function(x) as.numeric(x))
        df.sub[, which(colnames(df.sub) == "DE.tool")]  <- as.character(df.sub[, which(colnames(df.sub) == "DE.tool")])
        
        df.sub <- melt(df.sub, id.vars = c("rep.size", "alpha", "DE.tool", "prop.DE"))
        df.sub$metrics <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[1])
        df.sub$biotype <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[2])
        df.sub$limits  <- sapply(1:nrow(df.sub), function(i) unlist(strsplit(as.character(df.sub$variable[i]), "[.]"))[3])
        df.sub$variable <- NULL
        df.sub <- dcast(df.sub, DE.tool+rep.size+biotype+metrics+prop.DE+alpha~limits)
        
        if(max(as.numeric(df.sub$rep.siz)) == 40) {w=1} else {w=0.25}
        opt.plots.list[["opt.plot4"]] <- ggplot(df.sub, aes(x=alpha, y=mean, group=interaction(biotype, metrics), 
                                                            colour = metrics, lty=biotype))+
          geom_line(position=position_dodge(0), size=1.3)+
          #geom_point(position=position_dodge(0), size=3)+ 
          #geom_errorbar(aes(ymin=ll, ymax=ul), position=position_dodge(0), size=0.75, width=w)+
          geom_ribbon(aes(ymin=ll, ymax=ul), alpha=0.1)+
          coord_cartesian(ylim = YscaleRange, xlim = XscaleRange2)+
          scale_y_continuous(breaks=seq(YscaleRange[1], YscaleRange[2], length.out = 3), 
                             labels=seq(YscaleRange[1], YscaleRange[2], length.out = 3)) +
          scale_x_continuous(breaks=seq(XscaleRange2[1], XscaleRange2[2], length.out = 3), 
                             labels=seq(XscaleRange2[1], XscaleRange2[2], length.out = 3)) +
          scale_color_manual("legend", values = c("orange", "deepskyblue3")) + 
          ggplot2.theme + 
          geom_abline(intercept = 0, slope = 1)+
          labs(y=NULL, x="nominal FDR level",title="") #title ="Performance of selected DE tool at different nominal FDR.") 
        
      } else{opt.plots.list[["opt.plot4"]] <- NULL}
      
     
      
      wtlist         <- c(input$opt.plot1,input$opt.plot2,input$opt.plot3, input$opt.plot4)
      # remove the null plots from ptlist and wtlist
      to_delete      <- !sapply(opt.plots.list, is.null)
      opt.plots.list <- opt.plots.list[to_delete] 
      wtlist         <- wtlist[to_delete]
      if(length(opt.plots.list)==0) return(NULL)
      grid.arrange(grobs=opt.plots.list, ncol=ifelse(length(opt.plots.list)<=2, length(opt.plots.list), 2), 
                   nrow=ifelse(length(opt.plots.list)>2, 2, 1))
    #}
    #else{NULL}
    
  })
  
  
  #-----------------------------------------------------------------
  ##Third Panel
  output$Optional.Plots2 <- renderPlot({
    
    if(input$optPlot21 == 'DARC'){
      withProgress(message = '', value = 0, {
        incProgress(0, detail = "Filtering genes ...")  
      counts.Zhang   <- filter.count(Zhang.source$counts, Zhang.source$group,
                                     method   = input$opt.plot2.filt.method,
                                     min.read = input$opt.plot2.min.read,
                                     min.cpm  = input$opt.plot2.min.cpm)
      counts.NGP     <- filter.count(NGP.source$counts, NGP.source$group,
                                     method   = input$opt.plot2.filt.method,
                                     min.read = input$opt.plot2.min.read,
                                     min.cpm  = input$opt.plot2.min.cpm)  
      counts.GTEx     <- filter.count(GTEx.source$counts, GTEx.source$group,
                                      method   = input$opt.plot2.filt.method,
                                      min.read = input$opt.plot2.min.read,
                                      min.cpm  = input$opt.plot2.min.cpm) 
      incProgress(0.1, detail = "Normalizing count matrix ...")
      ##Normalization of read counts
      norm.counts.Zhang <- normalize.count(counts.Zhang$counts, counts.Zhang$group,
                                           norm.method = input$opt.plot2.norm.method)
      norm.counts.NGP <- normalize.count(counts.NGP$counts, counts.NGP$group,
                                         norm.method = input$opt.plot2.norm.method)
      norm.counts.GTEx <- normalize.count(counts.GTEx$counts, counts.GTEx$group,
                                         norm.method = input$opt.plot2.norm.method)
      
      
      incProgress(0.2, detail = "Calculating mean expression ...")
      mean.norm.counts.Zhang <- data.frame(mean = apply(log(norm.counts.Zhang+1), 1, mean))
      mean.norm.counts.Zhang$biotype <- NA
      mean.norm.counts.Zhang$biotype[rownames(mean.norm.counts.Zhang) %in% Zhang.source$mRNA]   <- "mRNA"
      mean.norm.counts.Zhang$biotype[rownames(mean.norm.counts.Zhang) %in% Zhang.source$lncRNA] <- "lncRNA"
      
      mean.norm.counts.NGP <- data.frame(mean = apply(log(norm.counts.NGP+1), 1, mean))
      mean.norm.counts.NGP$biotype <- NA
      mean.norm.counts.NGP$biotype[rownames(mean.norm.counts.NGP) %in% NGP.source$mRNA]   <- "mRNA"
      mean.norm.counts.NGP$biotype[rownames(mean.norm.counts.NGP) %in% NGP.source$lncRNA] <- "lncRNA"
      
      mean.norm.counts.GTEx <- data.frame(mean = apply(log(norm.counts.GTEx+1), 1, mean))
      mean.norm.counts.GTEx$biotype <- NA
      mean.norm.counts.GTEx$biotype  <- "mRNA"
      
      
      if(input$opt.plot2.biotype == "All"){
        biotype.choice <- c("mRNA", "lncRNA")
      }
      else if(input$opt.plot2.biotype == "mRNA"){
        biotype.choice <- c("mRNA")
      }
      else if(input$opt.plot2.biotype == "lncRNA"){
        biotype.choice <- c( "lncRNA")
      }
      
      incProgress(0.4, detail = "Creating plot for Zhang data ...")
        hist.Zhang <- ggplot(mean.norm.counts.Zhang, aes(x=mean, y=..density..)) + 
          stat_bin(data=subset(mean.norm.counts.Zhang, biotype == biotype.choice[1]),   bins=input$opt.plot2.numb.bins, 
                   fill = cols.biotype[biotype.choice][1], colour="gray90",  alpha = 0.5) +
          stat_bin(data=subset(mean.norm.counts.Zhang, biotype == biotype.choice[2]), bins=input$opt.plot2.numb.bins, 
                   fill = cols.biotype[biotype.choice][2], colour="gray90", alpha = 0.5)+
          scale_fill_manual(name="",values=alpha(cols.biotype, 0.5),
                            labels=biotype.choice, guide="legend") +
          theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
                axis.text.y=element_text(size = 15, colour = "gray30"),
                axis.title = element_text(size = 15, colour = "black"),
                plot.title =element_text(size=17, face="bold"),
                panel.background = element_rect(fill = "gray90"),
                panel.grid.major = element_line(colour = "white"),
                strip.background = element_rect(colour = "white", fill = "gray60"),
                strip.text = element_text(size = 17, colour = "black"),
                panel.grid.major.y = element_blank(),
                legend.text= element_text(size=15),
                legend.key.size  = unit(2,"line"),
                legend.title = element_blank(),
                legend.position = c(0.5, 0.8),
                legend.box = "horizontal",
                legend.justification = "left")+ 
          labs(x="average log-read-counts", y="density", title="Zhang data")
        if(input$free.axis.DARC != TRUE) {
          hist.Zhang <- hist.Zhang + coord_cartesian(ylim = input$ylim.DARC, xlim = input$xlim.DARC)}
        
        incProgress(0.6, detail = "Creating plot for NGP Nutlin data ...")
        hist.NGP <- ggplot(mean.norm.counts.NGP, aes(x=mean, y=..density..)) + 
          stat_bin(data=subset(mean.norm.counts.NGP, biotype == biotype.choice[1]),  bins=input$opt.plot2.numb.bins,
                   fill = cols.biotype[biotype.choice][1], colour="gray90",  alpha = 0.5) +
          stat_bin(data=subset(mean.norm.counts.NGP, biotype == biotype.choice[2]), bins=input$opt.plot2.numb.bins,
                   fill = cols.biotype[biotype.choice][2], colour="gray90", alpha = 0.5)+
          scale_fill_manual(name="",values=alpha(cols.biotype, 0.5),
                            labels=biotype.choice, guide="legend") +
          theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
                axis.text.y=element_text(size = 15, colour = "gray30"),
                axis.title = element_text(size = 15, colour = "black"),
                plot.title =element_text(size=17, face="bold"),
                panel.background = element_rect(fill = "gray90"),
                panel.grid.major = element_line(colour = "white"),
                strip.background = element_rect(colour = "white", fill = "gray60"),
                strip.text = element_text(size = 17, colour = "black"),
                panel.grid.major.y = element_blank(),
                legend.text= element_text(size=15),
                legend.key.size  = unit(2,"line"),
                legend.title = element_blank(),
                legend.position = c(0.5, 0.8),
                legend.box = "horizontal",
                legend.justification = "left")+ 
          labs(x="average log-read-counts", y="density", title="NGP Nutlin data") 
          if(input$free.axis.DARC != TRUE) {
            hist.NGP <- hist.NGP + coord_cartesian(ylim = input$ylim.DARC, xlim = input$xlim.DARC)}

        
        incProgress(0.8, detail = "Creating plot for GTEx data ...")
        hist.GTEx <- ggplot(mean.norm.counts.GTEx, aes(x=mean, y=..density..)) + 
          stat_bin(data=subset(mean.norm.counts.GTEx, biotype == biotype.choice[1]),  bins=input$opt.plot2.numb.bins,
                   fill = cols.biotype[biotype.choice][1], colour="gray90",  alpha = 0.5) +
          stat_bin(data=subset(mean.norm.counts.GTEx, biotype == biotype.choice[2]), bins=input$opt.plot2.numb.bins,
                   fill = cols.biotype[biotype.choice][2], colour="gray90", alpha = 0.5)+
          scale_fill_manual(name="",values=alpha(cols.biotype, 0.5),
                            labels=biotype.choice, guide="legend") +
          theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
                axis.text.y=element_text(size = 15, colour = "gray30"),
                axis.title = element_text(size = 15, colour = "black"),
                plot.title =element_text(size=17, face="bold"),
                panel.background = element_rect(fill = "gray90"),
                panel.grid.major = element_line(colour = "white"),
                strip.background = element_rect(colour = "white", fill = "gray60"),
                strip.text = element_text(size = 17, colour = "black"),
                panel.grid.major.y = element_blank(),
                legend.text= element_text(size=15),
                legend.key.size  = unit(2,"line"),
                legend.title = element_blank(),
                legend.position = c(0.5, 0.8),
                legend.box = "horizontal",
                legend.justification = "left")+ 
          labs(x="average log-read-counts", y="density", title="GTEx data")     
          if(input$free.axis.DARC != TRUE) {
            hist.GTEx <- hist.GTEx + coord_cartesian(ylim = input$ylim.DARC, xlim = input$xlim.DARC)}
        incProgress(0.95, detail = "Loading result ...")
      hist.mean.reads <- list(hist.Zhang, hist.NGP, hist.GTEx)
      grid.arrange(grobs=hist.mean.reads, ncol=length(hist.mean.reads),  nrow=1)
    })}
    
    else if(input$optPlot21 == "BCV"){ 
      withProgress(message = '', value = 0, {
      if(input$opt.plot2.biotype.BCV == "All"){
        biotype.choice <- c("mRNA", "lncRNA")
      }
      else if(input$opt.plot2.biotype.BCV == "mRNA"){
        biotype.choice <- c("mRNA")
      }
      else if(input$opt.plot2.biotype.BCV == "lncRNA"){
        biotype.choice <- c( "lncRNA")
      }
        incProgress(0, detail = "Retrieving Zhang BCV data ...")
        dds.Zhang <- Zhang.DESeq2.results$result[Zhang.DESeq2.results$result$dispOutlier==FALSE &
                                                   log(Zhang.DESeq2.results$result$baseMean) >= -5 &
                                                 Zhang.DESeq2.results$result$biotype %in% biotype.choice, ]
        
        incProgress(0.2, detail = "Creating BCV plot for Zhang ...")
        bcv.Zhang <- ggplot(dds.Zhang, aes(x=log(baseMean), group=as.character(biotype), colour=biotype))+
          geom_point(aes(y=sqrt(dispersion)), size=2)+
          geom_line(aes(y= sqrt(dds.Zhang$dispFit)), col="blue", size=1.25)+
          coord_cartesian(ylim = input$ylim.BCV, xlim = input$xlim.BCV)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
          labs(y="BCV", x="average read count [log]", title = "Zhang data")
        
        incProgress(0.5, detail = "Retrieving NGP Nutlin BCV data ...")
        dds.NGP <- NGP.DESeq2.results$result[NGP.DESeq2.results$result$dispOutlier==FALSE &
                                               log(NGP.DESeq2.results$result$baseMean) >= -2.5 &
                                               NGP.DESeq2.results$result$biotype %in% biotype.choice,]
        incProgress(0.6, detail = "Creating BCV plot for NGP Nutlin ...")
        bcv.NGP <- ggplot(dds.NGP, aes(x=log(baseMean), group=as.character(biotype), colour=biotype))+
          geom_point(aes(y=sqrt(dispersion)), size=2)+
          geom_line(aes(y= sqrt(dds.NGP$dispFit)), col="blue", size=1.25)+
          coord_cartesian(ylim = input$ylim.BCV, xlim = input$xlim.BCV)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          #geom_hline(aes(yintercept = dds.NGP$dispFit), col="red", lty=2)+
          labs(y="BCV", x="average read count [log]", title = "NGP Nutlin data")
        
        incProgress(0.7, detail = "Retrieving GTEx BCV data ...")
        dds.GTEx <- GTEx.DESeq2.results$result[GTEx.DESeq2.results$result$dispOutlier==FALSE &
                                               log(GTEx.DESeq2.results$result$baseMean) >= -2.5 &
                                               GTEx.DESeq2.results$result$biotype %in% biotype.choice,]
        incProgress(0.75, detail = "Creating BCV plot for GTEx ...")
        bcv.GTEx <- ggplot(dds.GTEx, aes(x=log(baseMean), group=as.character(biotype), colour=biotype))+
          geom_point(aes(y=sqrt(dispersion)), size=2)+
          geom_line(aes(y= sqrt(dds.GTEx$dispFit)), col="blue", size=1.25)+
          coord_cartesian(ylim = input$ylim.BCV, xlim = input$xlim.BCV)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          labs(y="BCV", x="average read count [log]", title = "GTEx data")
        
        incProgress(0.9, detail = "Loading results ...")
        BCV.list <- list(bcv.Zhang, bcv.NGP, bcv.GTEx) 
        grid.arrange(grobs=BCV.list, ncol=length(BCV.list),  nrow=1)
      
    })}
    
    else if(input$optPlot21 == "MDS"){
      withProgress(message = '', value = 0, {
        incProgress(0, detail = "Retrieving GTEx BCV data ...")
      pcaData.Zhang <- DESeq2::plotPCA(Zhang.DESeq2.results$DESeq.VSD.data, intgroup=c("grp.Zhang"), returnData=TRUE)
      percentVar.Zhang <- round(100 * attr(pcaData.Zhang, "percentVar"))
      mds.Zhang <- ggplot(pcaData.Zhang, aes(PC1, PC2, color=grp.Zhang)) +
        geom_point(size=3) +
        xlab(paste0("PC1: ",percentVar.Zhang[1],"% variance")) +
        ylab(paste0("PC2: ",percentVar.Zhang[2],"% variance")) + 
        coord_fixed()+
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 17, colour = "black"),
              plot.title =element_text(size=18, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.title = element_blank(),
              legend.position = "bottom")+ 
        labs(title = "Zhang data")
      if(input$free.axis.MDS != TRUE) {
        mds.Zhang <- mds.Zhang + coord_cartesian(ylim = input$ylim.MDS, xlim = input$xlim.MDS)}
      
      pcaData.NGP <- DESeq2::plotPCA(NGP.DESeq2.results$DESeq.VSD.data, intgroup=c("grp.NGP"), returnData=TRUE)
      percentVar.NGP <- round(100 * attr(pcaData.NGP, "percentVar"))
      mds.NGP <- ggplot(pcaData.NGP, aes(PC1, PC2, color=grp.NGP)) +
        geom_point(size=3) +
        xlim(-30, 30)+ylim(-10, 10)+
        xlab(paste0("PC1: ",percentVar.NGP[1],"% variance")) +
        ylab(paste0("PC2: ",percentVar.NGP[2],"% variance")) + 
        coord_fixed()+
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 17, colour = "black"),
              plot.title =element_text(size=18, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.title = element_blank(),
              legend.position = "bottom")+ 
        labs(title = "NGP Nutlin data")
      if(input$free.axis.MDS != TRUE) {
        mds.NGP <- mds.NGP + coord_cartesian(ylim = input$ylim.MDS, xlim = input$xlim.MDS)}
      
      
      pcaData.GTEx <- DESeq2::plotPCA(GTEx.DESeq2.results$DESeq.VSD.data, intgroup=c("grp.GTEx"), returnData=TRUE)
      percentVar.GTEx <- round(100 * attr(pcaData.GTEx, "percentVar"))
      mds.GTEx <- ggplot(pcaData.GTEx, aes(PC1, PC2, color=grp.GTEx)) +
        geom_point(size=3) +
        xlim(-30, 30)+ylim(-20, 20)+
        xlab(paste0("PC1: ",percentVar.GTEx[1],"% variance")) +
        ylab(paste0("PC2: ",percentVar.GTEx[2],"% variance")) + 
        coord_fixed()+
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 17, colour = "black"),
              plot.title =element_text(size=18, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.key = element_rect(colour = "transparent", fill = "white"),
              legend.title = element_blank(),
              legend.position = "bottom")+  
        labs(title = "GTEx data")
      if(input$free.axis.MDS != TRUE) {
        mds.GTEx <- mds.GTEx + coord_cartesian(ylim = input$ylim.MDS, xlim = input$xlim.MDS)}
      
      incProgress(0.5, detail = "Loading results ...")
      MDS.list <- list(mds.Zhang, mds.NGP, mds.GTEx) 
      grid.arrange(grobs=MDS.list, ncol=length(MDS.list),  nrow=1)
      
    })}
    
    else if(input$optPlot21 == "DGER"){
      if(input$opt.plot2.biotype.VP == "All"){
        biotype.choice <- c("mRNA", "lncRNA")
      }
      else if(input$opt.plot2.biotype.VP == "mRNA"){
        biotype.choice <- c("mRNA")
      }
      else if(input$opt.plot2.biotype.VP == "lncRNA"){
        biotype.choice <- c( "lncRNA")
      }
      DGE.results.Zhang <- Zhang.DESeq2.results[[3]]
      DGE.results.Zhang <- DGE.results.Zhang[[paste(input$DGE.tool)]]
      DGE.results.Zhang$test.DE <- ifelse(DGE.results.Zhang$padj < input$nominal.fdr, "SDE", "non SDE")
      
      SDE.summary.Zhang <- melt(table(DGE.results.Zhang$test.DE, DGE.results.Zhang$biotype))
      SDE.summary.Zhang <- ggplot(SDE.summary.Zhang, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        geom_bar(position = "dodge",stat = "count", width = 0.5)+
        scale_fill_manual("legend", values = c("orange", "deepskyblue3")) + 
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 15, colour = "black"),
              plot.title =element_text(size=17, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.justification = "left")+ 
        #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        labs(y="", x="", title = "Zhang data")
      
      
      
      DGE.results.NGP <- NGP.DESeq2.results[[3]]
      DGE.results.NGP <- DGE.results.NGP[[paste(input$DGE.tool)]]
      DGE.results.NGP$test.DE <- ifelse(DGE.results.NGP$padj < input$nominal.fdr, "SDE", "non SDE")
      
      SDE.summary.NGP <- melt(table(DGE.results.NGP$test.DE, DGE.results.NGP$biotype))
      SDE.summary.NGP <- ggplot(SDE.summary.NGP, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        geom_bar(position = "dodge",stat = "count", width = 0.5)+
        scale_fill_manual("legend", values = c("orange", "deepskyblue3")) + 
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 15, colour = "black"),
              plot.title =element_text(size=17, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.justification = "left")+ 
        #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        labs(y="", x="", title = "NGP Nutlin data")
      
      
      DGE.results.GTEx <- GTEx.DESeq2.results[[3]]
      DGE.results.GTEx <- DGE.results.GTEx[[paste(input$DGE.tool)]]
      DGE.results.GTEx$test.DE <- ifelse(DGE.results.GTEx$padj < input$nominal.fdr, "SDE", "non SDE")
      
      SDE.summary.GTEx <- melt(table(DGE.results.GTEx$test.DE, DGE.results.GTEx$biotype))
      SDE.summary.GTEx <- ggplot(SDE.summary.GTEx, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        geom_bar(position = "dodge",stat = "count", width = 0.5)+
        scale_fill_manual("legend", values = c("orange", "deepskyblue3")) + 
        theme(axis.text.x=element_text(size = 15, colour = "gray30"), 
              axis.text.y=element_text(size = 15, colour = "gray30"),
              axis.title = element_text(size = 15, colour = "black"),
              plot.title =element_text(size=17, face="bold"),
              panel.background = element_rect(fill = "gray90"),
              panel.grid.major = element_line(colour = "white"),
              strip.background = element_rect(colour = "white", fill = "gray60"),
              strip.text = element_text(size = 17, colour = "black"),
              panel.grid.major.y = element_blank(),
              legend.text= element_text(size=15),
              legend.key.size  = unit(2,"line"),
              legend.title = element_blank(),
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.justification = "left")+ 
        #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        labs(y="", x="", title = "GTEx data")
      
      
      summary.list <- list(SDE.summary.Zhang, SDE.summary.NGP, SDE.summary.GTEx) 
      grid.arrange(grobs=summary.list, ncol=3,  nrow=1)
    }
    
    else if(input$optPlot21 == "VP"){ 
      if(input$opt.plot2.biotype.VP == "All"){
        biotype.choice <- c("mRNA", "lncRNA")
      }
      else if(input$opt.plot2.biotype.VP == "mRNA"){
        biotype.choice <- c("mRNA")
      }
      else if(input$opt.plot2.biotype.VP == "lncRNA"){
        biotype.choice <- c( "lncRNA")
      }
        dds.Zhang <- as.data.frame(Zhang.DESeq2.results$result[Zhang.DESeq2.results$result$dispOutlier==FALSE &
                                                                 log(Zhang.DESeq2.results$result$baseMean) >= -5 &
                                                                 Zhang.DESeq2.results$result$biotype %in% biotype.choice, ])
        dds.Zhang$test.DE <- ifelse(dds.Zhang$adj.pval<input$fdr.thrld.opt.plot2, "SDE", "non SDE")
        vp.Zhang <- ggplot(dds.Zhang, aes(x=grp.Zhang_MYCN.not.amplified_vs_MYCN.amplified, 
                                          group=interaction(biotype, test.DE), colour=biotype,
                                          shape = test.DE))+
          geom_point(aes(y=-log10(LRTPvalue)), size=2)+
          coord_cartesian(ylim = input$ylim.VP, xlim = input$xlim.VP)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          geom_hline(aes(yintercept = -log10(input$fdr.thrld.opt.plot2)), col="red", lty=2)+
          labs(y="-log10(p-value)", x="log2(fold change)", title = "Zhang data")
        
        # SDE.summary.Zhang <- melt(table(dds.Zhang$test.DE, dds.Zhang$biotype))
        # SDE.summary.Zhang <- ggplot(SDE.summary.Zhang, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        #   geom_bar(position = "dodge")+
        #   scale_fill_manual("legend", values = unlist(cols.biotype[biotype.choice])) +  
        #   coord_flip()+
        #   ggplot2.theme+ 
        #   #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        #   labs(y="", x="", title = "Zhang data")
        
        
        dds.NGP <- as.data.frame(NGP.DESeq2.results$result[NGP.DESeq2.results$result$dispOutlier==FALSE &
                                                             log(NGP.DESeq2.results$result$baseMean) >= -2.5 &
                                                             NGP.DESeq2.results$result$biotype %in% biotype.choice,])
        dds.NGP$test.DE <- ifelse(dds.NGP$adj.pval<input$fdr.thrld.opt.plot2, "SDE", "non SDE")
        vp.NGP <- ggplot(dds.NGP, aes(x=grp.NGP_Nutlin.3_vs_Ethanol, 
                                      group=interaction(biotype, test.DE), colour=biotype,
                                      shape = test.DE))+
          geom_point(aes(y=-log10(LRTPvalue)), size=2)+
          #geom_line(aes(y= sqrt(dds.NGP$dispFit)), col="blue", size=1.25)+
          coord_cartesian(ylim = input$ylim.VP, xlim = input$xlim.VP)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          geom_hline(aes(yintercept = -log10(input$fdr.thrld.opt.plot2)), col="red", lty=2)+
          labs(y="-log10(p-value)", x="log2(fold change)", title = "NGP data")
        
        # SDE.summary.NGP <- melt(table(dds.NGP$test.DE, dds.NGP$biotype))
        # SDE.summary.NGP <- ggplot(SDE.summary.NGP, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        #   geom_bar(position = "dodge")+
        #   scale_fill_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
        #   coord_flip()+
        #   ggplot2.theme + 
        #   #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        #   labs(y="", x="", title = "NGP Nutlin data")
        # 
        
        dds.GTEx <- as.data.frame(GTEx.DESeq2.results$result[GTEx.DESeq2.results$result$dispOutlier==FALSE &
                                                               log(GTEx.DESeq2.results$result$baseMean) >= -2.5 &
                                                               GTEx.DESeq2.results$result$biotype %in% biotype.choice,])
        dds.GTEx$test.DE <- ifelse(dds.GTEx$adj.pval<input$fdr.thrld.opt.plot2, "SDE", "non SDE")
        vp.GTEx <- ggplot(dds.GTEx, aes(x=grp.GTEx_Hypothalamus_vs_Hippocampus, 
                                        group=interaction(biotype, test.DE), colour=biotype,
                                        shape = test.DE))+
          geom_point(aes(y=-log10(LRTPvalue)), size=2)+
          #geom_line(aes(y= sqrt(dds.GTEx$dispFit)), col="blue", size=1.25)+
          coord_cartesian(ylim = input$ylim.VP, xlim = input$xlim.VP)+
          scale_color_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
          ggplot2.theme+ 
          geom_hline(aes(yintercept = -log10(input$fdr.thrld.opt.plot2)), col="red", lty=2)+
          labs(y="-log10(p-value)", x="log2(fold change)", title = "GTEx data")
        
        # SDE.summary.GTEx <- melt(table(dds.GTEx$test.DE, dds.GTEx$biotype))
        # SDE.summary.GTEx <- ggplot(SDE.summary.GTEx, aes(x=Var2, weight=value, group=Var1, fill=Var1))+
        #   geom_bar(position = "dodge")+
        #   scale_fill_manual("legend", values = unlist(cols.biotype[biotype.choice])) + 
        #   coord_flip()+
        #   ggplot2.theme + 
        #   #geom_hline(aes(yintercept = ref.line), col="red", lty=2)+
        #   labs(y="", x="", title = "GTEx data")
        # 
        
        VP.list <- list(vp.Zhang, vp.NGP, vp.GTEx) 
        grid.arrange(grobs=VP.list, ncol=3,  nrow=1)
      
    }
    
    
  })
  
  output$notes.opt.plotts2 <- renderText({
    if(input$optPlot21 == 'DARC'){
      paste0("The histograms demonstrate the distribution of average of log transformed read counts of genes for the three RNA-seq datasets.
      First, genes that have read counts/counts-per-millions (CPM) below the cut off are removed, and then normalization is applied. The user 
             (choices from the left panel) can specify filtration method, cut off value and normalization method. 
             Overall, the average read counts of lncRNAs is lower than that of mRNAs as observed from the Zhang and NGP Nutlin datasets. 
           In both datasets, the average read counts of mRNAs has bimodal distribution. The bimodal shape suggests
            that there are considerable number of low count mRNA genes in both datasets (the left pick). ")
    }
    else if(input$optPlot21 == 'BCV'){
      paste0("Two types of variation are often observed in any RNA-Seq experiment with biological replicates. First, the relative abundance of
              each gene varies across RNA samples, due mainly to biological
             causes. Second, there is measurement error, the uncertainty with which the abundance of
             each gene in each sample is estimated by the sequencing technology. 
             To explore the variability in the three RNA-seq datasets, gene specific biological coefficients of   variation (BCV)  are obtained using DESeq2 bioconductor package. BCV is the CV with which 
             the (unknown) true abundance of the gene varies between replicate RNA samples. It represents the CV that would
             remain between biological replicates if sequencing depth could be increased indefinitely. From negative binomial distribution,
             the dispersion parameter (in square root) is equivalent to BCV and here we plotted the gene-wise (points) and smoothed/trended (solid line) dispersion estimate (in square root)
             against the mean normalized read counts (log scale). The plots show that the extent of biological
             variability among replicates is lower in NGP Nutlin data than in Zhang data. Also, BCV appears to be relatively high for low count genes in all datasets. 
             ")
    }
    else if(input$optPlot21 == 'MDS'){
      paste0("Quality of RNA-seq data (for example the presence of batch effect) can be assessed using the sample-to-sample distance measures, 
for example using multi dimensional scale (MDS) plot or principal component analysis (PCA) of replicates, 
             which visualize the pattern of proximities (i.e., similarities or distances) among a set of replicates. The plot shown above are PCA of replicates 
             obtained using plotPCA() function of DESeq2 package after normalization and variance stabilizing transformation of read counts. In this ordination method, the data points (here, the replicates) are projected onto the 2D plane such 
             that they spread out in the two directions that explain most of the differences. The x-axis is the direction that 
             separates the data points the most. The values of the samples in this direction are written PC1. The y-axis is a direction 
             (it must be orthogonal to the first direction) that separates the data the second most. The values of the samples in this direction 
             are written PC2. The percent of the total variance that is contained in the direction is printed in the axis label. Note that these 
             percentages do not add to 100%, because there are more dimensions that contain the remaining variance (although each of these remaining 
             dimensions will explain less than the two that we see). In both datasets, the first PC component able to distinguish replicates from the two experimental conditions.
             Other sources of variation among samples, for example batch effect, is not noticeable in both datasets. 
             ")
    }
  })
  
  #-----------------------------------------------------------------
  ##Fourth Panel
  output$Optional.Plots3 <- renderPlot({
    if(input$simSumrType == "prop.biotype1"){
      if(input$x.axis == "rep.size"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$prop.DE ==input$PDE.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.mRNA.mRNA", "prop.lncRNA.lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA.mRNA", "mRNA", "lncRNA")
       plt1 <- ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion", title = "Simulations from Zhang data")+
          geom_hline(yintercept = c(0.7026855, 0.2973145))
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$prop.DE ==input$PDE.sim.sumr &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.mRNA.mRNA", "prop.lncRNA.lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA.mRNA", "mRNA", "lncRNA")
        plt2 <-  ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion", title = "Simulations from NGP Nultin data")+
          geom_hline(yintercept =  NGP.source$prop.biotype)
        
       
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
        
      }
      else if(input$x.axis == "prop.DE"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$rep.size ==input$N.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.mRNA.mRNA", "prop.lncRNA.lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA.mRNA", "mRNA", "lncRNA")
        plt1 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes", y="proportion", title = "Simulations from Zhang data")+
          geom_hline(yintercept = c(0.63, 0.37))
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$rep.size ==min(input$N.sim.sumr, 5) &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.mRNA.mRNA", "prop.lncRNA.lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA.mRNA", "mRNA", "lncRNA")
        plt2 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes", y="proportion", title = "Simulations from NGP data")+
          geom_hline(yintercept =  NGP.source$prop.biotype)
        
        
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
      }
    }
    else if(input$simSumrType == "prop.biotype2"){
      if(input$x.axis == "rep.size"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$prop.DE ==input$PDE.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.mRNA_in_DE", "prop.lncRNA_in_DE")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA_in_DE", "mRNA", "lncRNA")
        plt1 <- ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion of biotypes", title = "Simulations from Zhang data")+
          geom_hline(yintercept = c(0.7026855, 0.2973145))
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$prop.DE ==input$PDE.sim.sumr &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.mRNA_in_DE", "prop.lncRNA_in_DE")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA_in_DE", "mRNA", "lncRNA")
        plt2 <- ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion of biotypes", title = "Simulations from NGP data")+
          geom_hline(yintercept =  NGP.source$prop.biotype)
        
        
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
        
      }
      else if(input$x.axis == "prop.DE"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$rep.size ==input$N.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.mRNA_in_DE", "prop.lncRNA_in_DE")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA_in_DE", "mRNA", "lncRNA")
        plt1 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes [setting]", y="proportion of biotypes", title = "Simulations from Zhang data")+
          geom_hline(yintercept = c(0.7026855, 0.2973145))
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$rep.size == min(input$N.sim.sumr, 5) &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.mRNA_in_DE", "prop.lncRNA_in_DE")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.mRNA_in_DE", "mRNA", "lncRNA")
        plt2 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes [setting]", y="proportion of biotypes", title = "Simulations from NGP data")+
          geom_hline(yintercept =  NGP.source$prop.biotype)
        
        
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
      }
    }
    else if(input$simSumrType == "prop.DE_nonDE"){
      if(input$x.axis == "rep.size"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$prop.DE ==input$PDE.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.DE_in_mRNA", "prop.DE_in_lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.DE_in_mRNA", "mRNA", "lncRNA")
        plt1 <- ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion of true DE genes", title = "Simulations from Zhang data")+
          geom_hline(yintercept = input$PDE.sim.sumr)
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$prop.DE ==input$PDE.sim.sumr &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("rep.size", "prop.DE_in_mRNA", "prop.DE_in_lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "rep.size")
        df.sub$biotype = ifelse(df.sub$variable=="prop.DE_in_mRNA", "mRNA", "lncRNA")
        plt2 <- ggplot(df.sub, aes(x=rep.size, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="number of replicates", y="proportion of true DE genes", title = "Simulations from NGP Nutlin data")+
          geom_hline(yintercept = input$PDE.sim.sumr)
        
        
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
        
      }
      else if(input$x.axis == "prop.DE"){
        #Zhang
        df.sub <- result.Zhang[result.Zhang$alpha == 0.05  & result.Zhang$rep.size == input$N.sim.sumr &
                                 result.Zhang$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.DE_in_mRNA", "prop.DE_in_lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.DE_in_mRNA", "mRNA", "lncRNA")
        plt1 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes [setting]", y="proportion of true DE genes [actual]", 
               title = "Simulations from Zhang data")+
          geom_abline(yintercept = 0, slope = 1)
        
        #NGP
        df.sub <- result.NGP[result.NGP$alpha == 0.05  & result.NGP$rep.size == min(input$N.sim.sumr, 5) &
                               result.NGP$DE.tool =="res.DESeq", ]
        df.sub <- df.sub[!duplicated(df.sub), c("prop.DE", "prop.DE_in_mRNA", "prop.DE_in_lncRNA")]
        df.sub <- reshape2::melt(df.sub, id.vars = "prop.DE")
        df.sub$biotype = ifelse(df.sub$variable=="prop.DE_in_mRNA", "mRNA", "lncRNA")
        plt2 <- ggplot(df.sub, aes(x=prop.DE, y=value, group = biotype, colour=biotype))+
          geom_point()+
          coord_cartesian(ylim = input$ylim.simSumr)+
          scale_color_manual("legend", values = unlist(cols.biotype[unique(df.sub$biotype)])) + 
          ggplot2.theme+ 
          labs(x="proportion of true DE genes [setting]", y="proportion of true DE genes [actual]", 
               title = "Simulations from NGP Nutlin data")+
          geom_abline(yintercept = 0, slope = 1)
        
        
        grid.arrange(grobs=list(plt1, plt2), ncol=2,  nrow=1)
      }
    }
    
  })
  output$notes <- renderText({
    if(input$simSumrType == "prop.biotype1"){
      if(input$x.axis == "rep.size"){
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. 
                Each dot represent summary of a single simulation at a particular combination of simulation settings. Since only 
                the Zhang and NGP Nutlin datasets contain annotated mRNAs and lncRNAs, the summary will focus on simulated datasets that 
                started from the two source datasets. For GTEx, it is known that 100% the genes are mRNA.
                The plots show that each simulated count matrix contains sufficient number of mRNA and lncRNA genes. 
               The proportion of mRNA and lncRNA in each individual simulated count matrix is approximately equal to the proportion of mRNA and lncRNA 
               in the source datasets (the solid black line). This fact is true for different settings, 
               such as different number of replicates, and different proportion of true DE genes.")
 
        
      }
      else if(input$x.axis == "prop.DE"){
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. 
                Each dot represent summary of a single simulation at a particular combination of simulation settings. Since only 
                the Zhang and NGP Nutlin datasets contain annotated mRNAs and lncRNAs, the summary will focus on simulated datasets that 
                started from the two source datasets. For GTEx, it is known that 100% the genes are mRNA.
                The plots show that each simulated count matrix contains sufficient number of mRNA and lncRNA genes. 
               The proportion of mRNA and lncRNA in each individual simulated count matrix is approximately equal to the proportion of mRNA and lncRNA 
               in the source datasets (the solid black line). This fact is true for different settings, 
               such as different number of replicates, and different proportion of true DE genes.")
      }
    }
    else if(input$simSumrType == "prop.biotype2"){
      if(input$x.axis == "rep.size"){
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. 
                Each dot represent summary of a single simulation at a particular combination of simulation settings. 
                Since only the Zhang and NGP Nutlin datasets contain annotated mRNAs and lncRNAs, the summary will focus on 
                simulated datasets that started from the two source datasets. For GTEx, it is known that 100% the genes are mRNA.
               The plots show that in each simulated count matrix, there are sufficient number of mRNA and lncRNA genes within the 
               set of DE genes (and non DE genes). For a setting with 0% true DE genes, this proportion summary is applicable only 
                for non-DE genes which is equivalent to the proportion of mRNA and lncRNA in the whole matrix (see the first radio button).
               The proportion of truly DE mRNAs and lncRNAs in each individual simulated count matrix is approximately equal to the proportion of mRNAs and lncRNAs 
               in the source datasets (the solid black line).
               This fact is true for different settings,  such as different number of replicates, and different proportion of true DE genes.")
        
      }
      else if(input$x.axis == "prop.DE"){
        #Zhang
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. 
                Each dot represent summary of a single simulation at a particular combination of simulation settings. 
               Since only the Zhang and NGP Nutlin datasets contain annotated mRNAs and lncRNAs, the summary will focus on 
               simulated datasets that started from the two source datasets. For GTEx, it is known that 100% the genes are mRNA.
               The plots show that in each simulated count matrix, there are sufficient number of mRNA and lncRNA genes within the 
               set of DE genes (and non DE genes). For a setting with 0% true DE genes, this proportion summary is applicable only 
               for non-DE genes which is equivalent to the proportion of mRNA and lncRNA in the whole matrix (see the first radio button).
               The proportion of truly DE mRNAs and lncRNAs in each individual simulated count matrix is approximately equal to the proportion of mRNAs and lncRNAs 
               in the source datasets (the solid black line).
               This fact is true for different settings,  such as different number of replicates, and different proportion of true DE genes.")
      }
    }
    else if(input$simSumrType == "prop.DE_nonDE"){
      if(input$x.axis == "rep.size"){
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. Each dot 
                represent summary of a single simulation at a particular combination of simulation settings. Since only the Zhang and NGP Nutlin 
                datasets contain annotated mRNAs and lncRNAs, the summary will focus on simulated datasets that started from the two source datasets. 
              For GTEx, it is known that 100% the genes are mRNA. The plots show that the proportion of true DE genes within the set of mRNA and lncRNAs
              in each simulated count is also the same to the setting level. 
               This indicates, each gene biotype contains sufficient number of true DE genes with a level approximately equal to the setting level. 
               For example, if the setting is to generate gene expression data with 20% true DE genes, then within each biotype the proportion of 
                true DE genes is also 20%.
               This feature is the same across different replicates and different proportion of true DE genes.")
        
      }
      else if(input$x.axis == "prop.DE"){
        paste0("The above plots show the composition of gene biotypes (mRNA and lncRNA) and DE genes in simulated count matrices. Each dot 
                represent summary of a single simulation at a particular combination of simulation settings. Since only the Zhang and NGP Nutlin 
                datasets contain annotated mRNAs and lncRNAs, the summary will focus on simulated datasets that started from the two source datasets. 
              For GTEx, it is known that 100% the genes are mRNA. The plots show that the proportion of true DE genes within the set of mRNA and lncRNAs
              in each simulated count is also the same to the setting level. 
               This indicates, each gene biotype contains sufficient number of true DE genes with a level approximately equal to the setting level. 
               For example, if the setting is to generate gene expression data with 20% true DE genes, then within each biotype the proportion of 
                true DE genes is also 20%.
               This feature is the same across different replicates and different proportion of true DE genes.")
      }
    }
  })
  
  #-----------------------------------------------------------------
  ##Fifth Panel
  output$tools.table <- renderTable(striped = TRUE, width = "100%", rownames = F, 
                                    colnames =TRUE, hover = TRUE, { 
    if(input$dispType != TRUE){
      temp.table      <- as.data.frame(matrix(NA, ncol=2, nrow = length(input$tool.atrb)))
      temp.table[, 1] <- as.vector(sapply(input$tool.atrb,  attributes.names))
      temp.table[, 2] <- unlist(methods.table2[methods.table2$DE.tool == input$show.DE.tool, input$tool.atrb])
      colnames(temp.table) <- c(" ", " ")
      temp.table
    } 
    else{
      if(sum(c("refTool", "description", "refPackage") %in% input$tool.atrb) >=1){
        y <- c("refTool", "description", "refPackage")
        y <- y[y %in% input$tool.atrb]
        attrib2 <- input$tool.atrb[-which(input$tool.atrb %in% y)]
      }
      else
      {
        attrib2 <- input$tool.atrb
      }
     
      temp.table <- as.data.frame(methods.table2[, attrib2])
      colnames(temp.table) <- as.vector(sapply(attrib2, attributes.names))
      temp.table
    }
  })
  
  
  # output$tools.remark <- renderText({
  #   if(input$dispType == TRUE){ 
  #     paste0("... page under construction")
  #   }
  #   else{
  #     paste0("To see remarks for each tool, click on 'Show DE tools individually.'")
  #   }
  # })
  # output$r.codes <- renderText({
  #     DE.tools.codes(input$show.DE.tool)
  # })
  
  
  #-----------------------------------------------------------------
  ##Last Panel
  output$elucidation1 <- renderUI({
    list1 <- paste0("This web tool is created for easier exploration of a simulation study on the evaluation of statistical 
                    methods for differential gene expression (DGE) analysis in RNA-sequencing data (with a particular 
                    focus on mRNA-seq and lncRNA-seq data) published on _______  and to provide guide for informed selection of DE tools.  <br/> <br/>
                    
                    The performance of 24 pipelines for testing DGE is evaluated with a simulation study under variety of gene-expression experiment scenarios.  
                    Non-parametric procedure was applied to realistically simulate RNA-seq expression data using a SimSeq R software package (version 1.4.0). 
                    The simulation technique involves sub-sampling of replicates from a real RNA-seq dataset with a sufficiently large number of replicates, 
                    which is referred to as the source dataset. In this way, the underlying characteristics of the source dataset, including the count distributions, 
                    variability, and between genes associations are preserved.") 

    list2 <- paste0("Three sets of simulations were performed, each starting from a different source dataset: the Zhang (cancer tissues), NGP nutlin (cultured cell line), 
                    and GTEx (normal tissues) datasets. The degree of homogeneity among the replicates in these source datasets varies, suggesting that they have different levels 
                    of intra-group biological variability, which is also reflected in the simulated gene expression data. Gene expressions are simulated containing a given 
                    proportion of true DE genes (ranging from 0 to 30%) from a total of 10,000 genes. Each gene in every simulated count matrix has a minimum 
                    of one read count in each group.  In addition, counts are simulated for varying numbers of replicates per group (2 to 40, 2 to 5, and 2 to 14 
                    for simulations that start from the Zhang, NGP Nutlin, and GTEx datasets, respectively). For joint simulation of mRNAs and lncRNAs 
                    (for Zhang and NGP Nutilin), each simulated count matrix includes a number of lncRNAs and mRNAs in the set of true DE genes and non-DE 
                    genes, proportional to their numbers in the source dataset (for the Zhang data 71% mRNA and for NGP Nutlin data 66% mRNA). We ran 100 independent 
                    simulations for each scenario. On each simulated dataset, we applied the various DE tools and calculated P-values, adjusted P-values, and other 
                    relevant statistics. These statistics were subsequently used for computing the comparative performance metrics.")
    
    list_space <- paste0(" ")
    HTML(paste(list1, list_space, list2, sep = '<br/>'))
  })
  output$elucidation2 <- renderUI({
    list <- paste0("The first panel ('Performance of all DGE tools') shows the performance of selected (or all) DE tools from one of the three simulations for a specific performance metrics,
           number of replicates per group, gene biotype, proportion of true DE genes, and nominal FDR, which can be chosen by the user. The performance measures
           are shown using bar plots with error bar (95% confidence interval) on the top. Different performance metrics are included, which will be automatically calculated from 
           the simulation results. <br/> <br/>

           The five performance metrics included in this web tool are defined as follows. Note that, these metrics are averaged across independent simulations (100 simulations at a particular scenario), 
           and hence the mean (and the 95% interval estimates) are presented using bar plots (and error bars). The metrics are calculated at a given level of nominal FDR (0 - 100%), which can also be controlled by the user. <br/>
                   => The false discovery rate (FDR) denotes the average proportion of false discoveries among all discoveries. <br/>
                   => True positive rate (TPR), or sometimes called sensitivity, is the average proportion of genes detected as differentially expressed (DE) among truly DE genes. <br/>
                   => False positive rate (FPR) refers to the average proportion of genes detected as DE among genes that are not truly DE. <br/>
                   => True negative rate (TNR) refers to the average proportion of genes not detected as DE among genes that are not truly DE. It is also equal to 1 - FPR. <br/>
                   => False negative rate (FNR) refers to the average proportion of genes not detected as DE among genes that are truly DE. It is also equal to 1 - TPR. <br/> <br/>

             The second panel ('Performance of specific DGE tools') visualizes the performance of specific DE tool at different number of replicates, 
           proportion of true DE genes, and nominal FDR. It also shows the ROC curve to demonstrate the trade-off between the actual FDR and sensitivity. The user can control which plot to see and the axis scales. 
             
                   Two other tab panels are also included showing the observed features of 3 RNA-seq datasets that were used as simulation source and features of the simulated data, respectively. These 
                   panels are inactive at the first run of the application, and the user can control their appearance using check boxes located at the left bottom corner of the first panel.
                   Additionally, another tab panel 'DE Tools' is dedicated to introduce the user to the DE tools evaluated in this web tool. 
                  It includes brief description to each DE tool along with their package name and references. .")
    HTML(list)
  })
  output$elucidation3 <- renderText({
    list1 <- paste0("Contact support: <br/>
                    Alemu Takele Assefa <br/> 
                    Department of Data Analysis and Mathematical Modeling  <br/>
                    Faculty of Bioscience Engineering <br/> 
                    Ghent University <br/> 
                    
                    Coupure Links 653, 9000 Ghent, Belgium <br/> 
                    
                    email: AlemuTakele.Assefa@UGent.be                   alemutak@hotmail.com")
    HTML(paste(list1, sep = '<br/>'))
  })
  
  # observeEvent(input$conditionedPanels,print(input$conditionedPanels))
  # observeEvent(input$showCode, print(input$showCode))
  
  #session$allowReconnect(TRUE)
}