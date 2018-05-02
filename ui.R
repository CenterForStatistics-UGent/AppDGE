ui <- fluidPage(theme = "bootstrap.css",
                tags$head(HTML(
                  "<!-- Global site tag (gtag.js) - Google Analytics -->
                    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-112757510-3'></script>
                    <script>
                    window.dataLayer = window.dataLayer || [];
                  function gtag(){dataLayer.push(arguments);}
                  gtag('js', new Date());
                  
                  gtag('config', 'UA-112757510-3');
                  ga('send', 'pageview');
                  </script>"
                    
                )),
                
                # -- Tracking Code
                tags$script(HTML(
                  "$(document).on('shiny:inputchanged', function(event) {
                  if (event.name === 'sim' || event.name === 'DE.tool' || event.name === 'metrics' || event.name === 'biotype' || event.name === 'rep.size') {
                  ga('send','event', 'input', 
                  'updates', event.name, event.value);
                  }
                  });"
                  )),
  
                useShinyjs(),
                sidebarLayout(
                  sidebarPanel(width = 3,
                               conditionalPanel(condition="input.conditionedPanels=='tab1'",
                                                #-------
                                                fluidRow(
                                                  column(9,
                                                         radioButtons("sim", label = "Choose simulation study", 
                                                                      choices = c("Simulation 1 (cancer tissues, Zhang)"="Zhang", 
                                                                                  "Simulation 2 (cultured cell lines, NGP nutlin)"="NGP Nutlin", 
                                                                                  "Simulation 3 (normal tissues, GTEx)"="GTEx"), selected = "Zhang",
                                                                      inline = FALSE, width = "100%")),
                                                  column(3,
                                                         actionLink("simHelp", "", icon = icon("question-circle")),
                                                         bsPopover("simHelp", "Simulation source", "Three sets of simulations are studied each starting from different source RNA\\-seq data\\: Simulation 1 (starting from the Zhang data), Simulation 2 (starting from the  NGP nutlin data), and Simulation 3 (starting from the  GTEx data). Each source dataset represent different level of biological variability. In terms of variability Sim 1>Sim3>Sim2. Detail information can be obtained in the \\'Description\\' panel.", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  )),
                                                
                                                #-------
                                                fluidRow(
                                                  column(9,
                                                selectInput("DE.tool", label = "Choose DE tool", choices = c("All", methods.names2), 
                                                            selected = "All", multiple=T, width = "100%")),
                                                column(3,
                                                       actionLink("DEToolHelp", "", icon = icon("question-circle")),
                                                       bsPopover("DEToolHelp", "DE tools", "List of DE tools considered in this application. Select \\'All\\' to see results for all DE tools. You can also choos a subset of tools.", 
                                                                 placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                )),
                                                #-------
                                                
                                                fluidRow(
                                                column(9,
                                                       selectInput("metrics", label = "Choose performance metrics", multiple=T,
                                                                   choices = c("TPR", "FDR", "FNR", "TNR", "FPR"), selected = c("FDR", "TPR"), width = "100%")),
                                                column(3,
                                                       actionLink("metricsHelp", "", icon = icon("question-circle")),
                                                       bsPopover("metricsHelp", "Performance metrics", "List of performance metrics. The metrics are calculated over independent simulations. FDR \\= False Discovery Rate, TPR \\= True Positive Rate, TNR \\= True Negative Rate, FNR \\= False Negative Rate, FPR = False Positive Rate. Detail information can be obtained in the \\'Description\\' panel.", 
                                                                 placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                )),
                                                
                                                fluidRow(
                                                  column(9,
                                                         selectInput("biotype", label = "Choose gene biotype", multiple=T,
                                                                     choices = c("mRNA", "lncRNA"), selected = c("mRNA", "lncRNA"), width = "100%")),
                                                  column(3,
                                                         actionLink("biotypeHelp", "", icon = icon("question-circle")),
                                                         bsPopover("biotypeHelp", "Gene biotype", "Two gene biotypes are particularly studied: mRNA and lncRNA. Results are separately shown for each biotype. Simulated gene expression data contains a number of mRNAs and lncRNAs both in the set of truly DE genes and null genes. For the GTEx simulation, only mRNA are studied.", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  )),
                                                #-------
                               
                               
                                               
                                                #Inputs for Zhang and NGP Nutlin simulation
                                                conditionalPanel(
                                                  condition = "input.sim == 'Zhang'",
                                                  #-------
                                                  
                                                  # fluidRow(
                                                  #   column(9,
                                                  #          selectInput("biotype", label = "Choose gene biotype", multiple=T,
                                                  #                      choices = c("mRNA", "lncRNA"), selected = c("mRNA", "lncRNA"), width = "100%")),
                                                  #   column(3,
                                                  #          actionLink("biotypeHelp", "", icon = icon("question-circle")),
                                                  #          bsPopover("biotypeHelp", "Gene biotype", "Two gene biotypes are particularly studied: mRNA and lncRNA. Results are separately shown for each biotype. Simulated gene expression data contains a number of mRNAs and lncRNAs both in the set of truly DE genes and null genes. For the GTEx simulation, only mRNA are studied.", 
                                                  #                    placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  #   )),
                                                  #-------
                                                  
                                                  fluidRow(
                                                    column(9,
                                                           numericInput("rep.size", label = "Number of samples per group", min=2, max=40, value = 20)),
                                                    column(3,
                                                           actionLink("nHelp", "", icon = icon("question-circle")),
                                                           bsPopover("nHelp", "Number of samples", "Number of replicated libraries in each condition or group. We assume equal number of samples for each group, otherwise, the minimum size should be used. For the Zhang simulation, the number of samples can be 2, 3, 4,  5, 7 10, 15, 20, 30, or 40.", 
                                                                     placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                    ))
                                                  #-------
                                                  
                                                ),
                                                #Inputs for NGP Nutlin simulation
                                                conditionalPanel(
                                                  condition = "input.sim == 'NGP Nutlin'", 
                                                  #-------
                                                  
                                                  # fluidRow(
                                                  #   column(9,
                                                  #          selectInput("biotype2", label = "Choose gene biotype", multiple=T,
                                                  #                      choices = c("mRNA", "lncRNA"), selected = c("mRNA", "lncRNA"), width = "100%")),
                                                  #   column(3,
                                                  #          actionLink("bioty2peHelp", "", icon = icon("question-circle")),
                                                  #          bsPopover("biotype2Help", "Gene biotype", "Two gene biotypes are particularly studied: mRNA and lncRNA. Results are separately shown for each biotype. Simulated gene expression data contains a number of mRNAs and lncRNAs both in the set of truly DE genes and null genes. For the GTEx simulation, only mRNA are studied.", 
                                                  #                    placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  #   )),
                                                  #-------
                                                  fluidRow(
                                                    column(9,
                                                           sliderInput("rep.size2", label = "Number of samples per group", min=2, max=5, value = 5, step = 1)),
                                                    column(3,
                                                           actionLink("n2Help", "", icon = icon("question-circle")),
                                                           bsPopover("n2Help", "Number of samples", "Number of replicated libraries in each condition or group. We assume equal number of samples for each group, otherwise, the minimum size should be used. For the NGP Nutlin simulation, the number of samples can be 2, 3, 4, or 5.", 
                                                                     placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                    ))
                                                  #-------
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.sim == 'GTEx'",
                                                  # fluidRow(
                                                  #   column(9,
                                                  #          selectInput("biotype2", label = "Choose gene biotype", multiple=F,
                                                  #                      choices = c("mRNA"), selected = c("mRNA"), width = "100%")),
                                                  #   column(3,
                                                  #          actionLink("bioty3peHelp", "", icon = icon("question-circle")),
                                                  #          bsPopover("bioty3peHelp", "Gene biotype", "Two gene biotypes are particularly studied: mRNA and lncRNA. Results are separately shown for each biotype. Simulated gene expression data contains a number of mRNAs and lncRNAs both in the set of truly DE genes and null genes. For the GTEx simulation, only mRNA are studied.", 
                                                  #                    placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  #   )),
                                                  #-------
                                                
                                                  fluidRow(
                                                    column(9,
                                                           sliderInput("rep.size3", label = "Number of samples per group", min=2, max=14, value = 10, step = 1)),
                                                    column(3,
                                                           actionLink("n3Help", "", icon = icon("question-circle")),
                                                           bsPopover("n3Help", "Number of samples", "Number of replicated libraries in each condition or group. We assume equal number of samples for each group, otherwise, the minimum size should be used. For the GTEx simulation, the number of samples can be any integer between 2 and 14.", 
                                                                     placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                    ))
                                                  #-------
                                                ),
                                                
                                                
                                                fluidRow(
                                                  column(9,
                                                         sliderInput("PDE", label = "Proportion of true DE genes", min = 0, max = 0.3, value = 0.2,
                                                                     step = 0.05, width = "100%")),
                                                  column(3,
                                                         actionLink("PDEHelp", "", icon = icon("question-circle")),
                                                         bsPopover("PDEHelp", "Proportion of true DE genes", "The proportion of true DE genes in the simulated gene expression data. Gene expression data are simulated containing a given proportion of true DE genes. The proportion is between 0 to 30\\%", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  )),
                                                #-------
                                                fluidRow(
                                                  column(9,
                                                         sliderInput("thrld", label = "Nominal FDR threshold", min = 0, max = 0.2, value = 0.01,
                                                                     step = 0.01, width = "100%")),
                                                  column(3,
                                                         actionLink("alphaHelp", "", icon = icon("question-circle")),
                                                         bsPopover("alphaHelp", "Nominal FDR", " The nominal FDR, or often called significance level, referes to the expected (tolerable) proportion of false discoveries among all discoveries. It is used as a thereshold to predict if genes are DE or not, i.e. if a gene has adjusted p-value less than the nominal FDR, then we call it DE. The particular nominal FDR used for the current output is indicated by dashed red line on the FDR plot panel", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  )),
                                                #-------
                                                fluidRow(
                                                  column(9,
                                                         numericInput("k", label = "Number of top DE tools", min=1, max=24, value = 3, width = "100%")),
                                                  column(3,
                                                         actionLink("kHelp", "", icon = icon("question-circle")),
                                                         bsPopover("kHelp", "Number of top DE tools", "This number will be used to select the top DE tools sorted by either their actual FDR or TPR. It can be any integer between 1 and 13.", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                  )),
                                                #-------
                                                
                                                
                                                hr(),
                                                
                                                #Graphics control 
                                                sliderInput("xlim",      label = "Adjust X scales", min = 0, max = 1, value = c(0,1), width = "75%"), 
                                                radioButtons("facet", label = "Plot type", choices = c("wrap", "gride"), selected = "wrap", inline = TRUE, width = "80%"),
                                                
                                                hr(),
                                                sliderInput("xlim.roc",  label = "Adjust X scales (for ROC curve)", min = 0, max = 1, value = c(0,0.5), width = "75%"),
                                                sliderInput("ylim.roc",  label = "Adjust Y scales (for ROC curve)", min = 0, max = 1, value = c(0,1), width = "75%"),
                                                #Optional Pannels 
                                               # checkboxInput("check.opt.plot1", label="Show perofrmance trend", value = T),
                                               #hr(),
                                                checkboxInput("check.opt.plot2", label="Show features of simulation source RNA-seq datasets", value = F, width = "80%"),
                                                checkboxInput("check.opt.plot3", label="Show features of simulated gene expression data", value = F, width = "80%")
                               ),
                               conditionalPanel(condition="input.conditionedPanels=='tab2'",
                                                selectInput("sub.DE.tool", label = "Choose DE tool", choices = methods.names2, 
                                                            selected = "SAMSeq", multiple=F),
                                                checkboxInput("opt.plot1", label = "ROC curve", value = TRUE),
                                                sliderInput("ind.thrld", label = "Indicate the nominal FDR threshold", min = 0, max = 0.2, value = 0.05,
                                                            step = 0.01, width = "100%"),
                                                checkboxInput("opt.plot2", label = "Performance metrics at different number of replicates", value = TRUE), 
                                                checkboxInput("opt.plot3", label = "Performance metrics at different proportion of true DE genes", 
                                                              value = FALSE), 
                                                checkboxInput("opt.plot4", label = "Performance metrics at different nominal FDR", value = FALSE), 
                                                sliderInput("ylim",  label = "Adjust Y scales", min = 0, max = 1, value = c(0,1)),
                                                sliderInput("xlim2",  label = "Adjust X scales", min = 0, max = 1, value = c(0, 0.5))
                               ), 
                               conditionalPanel(condition="input.conditionedPanels=='tab3'",
                                                # selectInput("source.dat", label = "Choose simulation source data", choices = c("Zhang", "NGP Nutlin"), 
                                                #               selected = c("Zhang", "NGP Nutlin"), multiple=T),
                                                radioButtons(inputId="optPlot21", label = "Summary statistics", 
                                                             choices = c("Distribution of average read counts"="DARC",
                                                                         "Estimates of biological coefficient of variation"="BCV", 
                                                                         "Multidimensional scaling plot"="MDS"), #, "Differential gene expression results" = "DGER", "Volcano plot"="VP"), 
                                                             selected = NULL, inline = FALSE),
                                                
                                                hr(),
                                                #checkboxInput("advanceOpts2", label = "Show advanced options", value = TRUE),
                                                conditionalPanel( 
                                                  condition = "input.optPlot21 == 'DARC'",
                                                  radioButtons("opt.plot2.biotype",   label = "Choose biotype", choices = c("All", "mRNA", "lncRNA"), 
                                                               selected = "All", inline = TRUE),
                                                  
                                                  fluidRow(
                                                    column(9,
                                                           selectInput("opt.plot2.filt.method", label = "Filtration method", choices = c("minimum read counts", "CPM"),
                                                                       selected = "minimum read counts")),
                                                    column(3,
                                                           actionLink("optHelp1a", "", icon = icon("question-circle")),
                                                           bsTooltip("optHelp1a", "If \\'minimum read counts\\' is chosen, then genes with total count (per group) less than the minimum read count (see below) will be removed.", 
                                                                     placement ="bottom", trigger = "hover", options = list(container = 'body')),
                                                           actionLink("optHelp1b", "", icon = icon("question-circle")),
                                                           bsTooltip("optHelp1b", "If \\'CPM\\' is chosen, then genes with mean CPM (per group) less than the minimum CPM (see below) will be removed", 
                                                                     placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                    )),
                                                  #-------
                                                  
                                                  numericInput("opt.plot2.min.read", label = "Minimum read counts per condition", value=1, min=0),
                                                  numericInput("opt.plot2.min.cpm", label = "Minimum mean CPM", value=0.001, min=0),
                                                  selectInput("opt.plot2.norm.method", label = "Normalization method",
                                                              choices = c("DESeq", "TMM", "QN", "SAMSeq", "PoissonSeq"),  selected = "DESeq"),
                                                  sliderInput("opt.plot2.numb.bins", label = "Number of bins", min = 1, value= 30, max = 200, step = 1),
                                                  hr(),
                                                  checkboxInput("free.axis.DARC", "Free axis scales", value = TRUE),
                                                  sliderInput("xlim.DARC", label = "Adjust X scales", min = 0, max = 20, value = c(0,12)),
                                                  sliderInput("ylim.DARC", label = "Adjust Y scales", min = 0, max = 1, value = c(0,0.8))
                                                  
                                                ),
                                                conditionalPanel(
                                                  radioButtons("opt.plot2.biotype.BCV",   label = "Choose biotype", choices = c("All", "mRNA", "lncRNA"), 
                                                               selected = "All", inline = TRUE),
                                                  condition = "input.optPlot21 == 'BCV'",
                                                  hr(),
                                                  sliderInput("xlim.BCV", label = "Adjust X scales", min = -5, max = 50, value = c(-3,15)),
                                                  sliderInput("ylim.BCV", label = "Adjust Y scales", min = 0, max = 50, value = c(0,15))
                                                ),
                                                conditionalPanel(
                                                  condition = "input.optPlot21 == 'MDS'",
                                                  hr(),
                                                  checkboxInput("free.axis.MDS", "Free axis scales", value = TRUE),
                                                  sliderInput("xlim.MDS", label = "Adjust X scales", min = -30, max = 30, value = c(-30,30)),
                                                  sliderInput("ylim.MDS", label = "Adjust Y scales", min = -30, max = 30, value = c(-30,30))
                                                ),
                                                conditionalPanel(
                                                  condition = "input.optPlot21 == 'DGER'",
                                                  selectInput("DGE.tool", "DE too to analyse the full(source) dataset", 
                                                              choices = c("DESeq2"="DESeq2", "Wilcoxon rank sum test"="WMW", "limma t-test"="limma.t.test"),
                                                              selected = "deseq2", multiple = FALSE),
                                                  numericInput("nominal.fdr", "Nominal FDR", min=0, max=1, value = 0.05)
                                                ),
                                                
                                                conditionalPanel(
                                                  condition = "input.optPlot21 == 'VP'",
                                                  radioButtons("opt.plot2.biotype.VP",   label = "Choose biotype", choices = c("All", "mRNA", "lncRNA"), 
                                                               selected = "All", inline = TRUE),
                                                  sliderInput("fdr.thrld.opt.plot2", label = "Nominal FDR", min=0, max=1, step = 0.005, value = 0.05),
                                                  #checkboxInput("showDGEsummary", "Show DGE analysis summary", value = FALSE),
                                                  hr(),
                                                  sliderInput("xlim.VP", label = "Adjust X scales", min = -10, max = 10, value = c(-5,5)),
                                                  sliderInput("ylim.VP", label = "Adjust Y scales", min = 0, max = 300, value = c(0,50))
                                                )
                               ),
                               conditionalPanel(condition="input.conditionedPanels=='tab4'",
                                                radioButtons("simSumrType", label = "Choose type of summary", 
                                                                   choices = list(
                                                                     "Proportion of biotypes in the simulated counts"="prop.biotype1",
                                                                     "Proportion of biotypes in the set of DE genes within each simulated counts"="prop.biotype2",
                                                                     "Proportion of DE genes in the set of each biotype within each simulated counts"="prop.DE_nonDE"),
                                                                     #"Level of homogeneity  among replicates of simulated counts" ="homogenity.level",
                                                                     #"Distribution of average gene expression in simulatde counts"="dist.mean.expr"), 
                                                                   selected="prop.biotype1"),
                                                hr(),
                                                
                                                radioButtons("x.axis", label = "Choose X-axis", 
                                                             choices = c("Replicate size at a fixed proportion of DE genes"="rep.size",
                                                                         "Proportion of true DE genes at a fixed replicate size" = "prop.DE"),
                                                               selected = "rep.size"),
                                                numericInput("PDE.sim.sumr", label = "Proportion of true DE genes", min = 0, max = 0.3, value = 0.2),
                                                
                                                fluidRow(
                                                  column(9,
                                                         numericInput("N.sim.sumr",   label = "Number of replicates per group", min = 0, max = 40, value = 5)),
                                                  column(3,
                                                         actionLink("nNGP.help", "", icon = icon("info-circle")),
                                                         bsTooltip("nNGP.help", "If an input more than 5 is entered, then it will show only for a simulation design with 5 replicates per group for simulations that start from the NGP Nutlin data.", 
                                                                   placement ="bottom", trigger = "hover", options = list(container = 'body'))
                                                         )
                                                  ),
                                                sliderInput("ylim.simSumr", label = "Adjust Y-axis", min=0, max = 1, value = c(0,1)),
                                                hr(),
                                                actionLink("simQualityZhang", "Click here to see simulation quality assessment (Zhang)",    icon = icon("external-link-square")),  
                                                actionLink("simQualityNGP", "Click here to see simulation quality assessment (NGP Nutlin)", icon = icon("external-link-square")),
                                                actionLink("simQualityGTEx", "Click here to see simulation quality assessment (GTEx)",      icon = icon("external-link-square")),
                                                bsPopover("simQualityZhang", title = "Quality assessement of simulated data", placement ="bottom", trigger = "hover", options = list(container = 'body'),
                                                          content = "Quality assessment of simulated gene expression data based on quality metrics suggested by Soneson and Robinson 2017. The reports are generated using countsimQC R package. It includes examining the mean-variance relationship, variability, fraction of zero counts, correlations among samples, correlation among genes, and others. It compares the metrics across simulated gene expression data as well as with the simulation source datasets using visual tools and formal statistical test. -------------------- Soneson C, Robinson MD. Towards unified quality verification of synthetic count data with countsimQC. Bioinformatics. 2017 Oct 4."),#      
                                                bsPopover("simQualityNGP", title = "Quality assessement of simulated data", placement ="bottom", trigger = "hover", options = list(container = 'body'),
                                                          content = "Quality assessment of simulated gene expression data based on quality metrics suggested by Soneson and Robinson 2017. The reports are generated using countsimQC R package. It includes examining the mean-variance relationship, variability, fraction of zero counts, correlations among samples, correlation among genes, and others. It compares the metrics across simulated gene expression data as well as with the simulation source datasets using visual tools and formal statistical test. -------------------- Soneson C, Robinson MD. Towards unified quality verification of synthetic count data with countsimQC. Bioinformatics. 2017 Oct 4."),#      
                                                bsPopover("simQualityGTEx", title = "Quality assessement of simulated data", placement ="bottom", trigger = "hover", options = list(container = 'body'),
                                                          content = "Quality assessment of simulated gene expression data based on quality metrics suggested by Soneson and Robinson 2017. The reports are generated using countsimQC R package. It includes examining the mean-variance relationship, variability, fraction of zero counts, correlations among samples, correlation among genes, and others. It compares the metrics across simulated gene expression data as well as with the simulation source datasets using visual tools and formal statistical test. -------------------- Soneson C, Robinson MD. Towards unified quality verification of synthetic count data with countsimQC. Bioinformatics. 2017 Oct 4.")#      
                                                
                                                ),
                                                
                               conditionalPanel(condition="input.conditionedPanels=='tab5'",
                                                selectInput("show.DE.tool", "Choose DE tool", choices = methods.names2, selected = "DESeq"),
                                                
                                                checkboxGroupInput("tool.atrb", label = "Choose attributes", 
                                                                   choices = list(
                                                                     "Tool full name"="DE.tool.full",
                                                                     "Tool short name"="DE.tool", 
                                                                     "Normalization method" = "normMethod",
                                                                     "Count distribution assumption"="countAssum",
                                                                     "Description"="description", 
                                                                     "Possible number of factors to be used" ="numFactors",
                                                                     "Integrated outlier adjustment method" ="outAdjust",
                                                                     "Available in package" ="package",
                                                                     "Package version"="packageVersion",
                                                                     "Tool reference"="refTool", 
                                                                     "Package reference" ="refPackage"), 
                                                                   selected=c("DE.tool", "DE.tool.full", "package", "numFactors", "countAssum",
                                                                              "description", "normMethod", "outAdjust", "packageVersion")),
                                                hr(),
                                                checkboxInput("dispType", "Show all DE tools in a table", value = FALSE)
                                                #checkboxInput("showCode", "Show R code", value = FALSE)
                                                
                               ),
                               hr(),
                               actionButton("goToHome", label = "Go to the first page")
                  ),
                  mainPanel(width=9,
                    tabsetPanel(id = "conditionedPanels",  type = "pills",
                                tabPanel("Performance of all DGE tools", value="tab1",
                                        wellPanel(
                                          h3("Performance of selected DE tool(s)"),
                                          #hr(),
                                          uiOutput("perf.plot1.ui")
                                        ),
                                         
                                         conditionalPanel(
                                           condition = "input.biotype %in% c('mRNA', 'lncRNA') & input.conditionedPanels=='tab1'",
                                           
                                           wellPanel(id = "tPanel", #style = "overflow-y:scroll; max-height: 300px",
                                           h3("Top DE tools"),
                                           #hr(),
                                           uiOutput("top.DE.ROC.curve"),
                                           
                                           hr(),
                                           textOutput("topDEtools_note"))
                                         )),
                                tabPanel("Performance of specific DGE tool", value="tab2",
                                         wellPanel(h3("Performance of selected DE tool across different simulation scenarios"),
                                                   #hr(),
                                                   uiOutput(outputId="Optional.Plots1.ui"))),
                                tabPanel("Features of source datasets", value="tab3", 
                                         wellPanel(h3("Characteristics of source RNA-seq datasets"),
                                         hr(),
                                         uiOutput(outputId = "Optional.Plots2.ui")),
                                         wellPanel(
                                           h3("Note"),
                                           #hr(),
                                           textOutput("notes.opt.plotts2")
                                         )
                                         
                                ),
                                tabPanel("Features of simulated data", value="tab4",
                                         wellPanel(h3("Features of simulated gene expression data"),
                                         hr(),
                                         plotOutput("Optional.Plots3", height="650px", width = "100%")),
                                         
                                         wellPanel(h3("Notes"),
                                                   #hr(),
                                         textOutput("notes"))),
                                
                                tabPanel("DE Tools", value="tab5",
                                         wellPanel(
                                           tableOutput("tools.table")) 
                                         ),
                                tabPanel("Description", value="tab6",
                                         wellPanel(
                                           #h3("Introduction:"),
                                           uiOutput("elucidation1")
                                         ),
                                         wellPanel(
                                           #h3("Author:"),
                                           uiOutput("elucidation2")
                                         ),
                                         wellPanel(
                                           #h3("Author:"),
                                           uiOutput("elucidation3")
                                         ))
                                
                    )
                  )
                )
                # ,
                # tags$head(
                #   tags$style(
                #     HTML("#tools.table table tr td {word-wrap: break-word}")
                #   )
                # )
)