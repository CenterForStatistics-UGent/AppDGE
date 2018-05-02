#Load required packages
suppressPackageStartupMessages(library(shiny))        #CRAN
suppressPackageStartupMessages(library(shinyjs))      #CRAN
suppressPackageStartupMessages(library(shinyBS))      #CRAN
suppressPackageStartupMessages(library(shinythemes))  #CRAN 

suppressPackageStartupMessages(library(reshape2))      #CRAN 
suppressPackageStartupMessages(library(ggplot2))       #CRAN 
suppressPackageStartupMessages(library(data.table))    #CRAN 
suppressPackageStartupMessages(library(stringr))       #CRAN 
suppressPackageStartupMessages(library(pracma))        #CRAN
suppressPackageStartupMessages(library(plyr))          #CRAN 
suppressPackageStartupMessages(library(gridExtra))     #CRAN 
suppressPackageStartupMessages(library(pheatmap))      #CRAN
suppressPackageStartupMessages(library(PoissonSeq))    #CRAN 
suppressPackageStartupMessages(library(samr))          #CRAN 

suppressPackageStartupMessages(library(edgeR))         #Bioconductor
suppressPackageStartupMessages(library(DESeq))         #Bioconductor
suppressPackageStartupMessages(library(DESeq2))        #Bioconductor
suppressPackageStartupMessages(library(limma))         #Bioconductor
suppressPackageStartupMessages(library(preprocessCore)) #Bioconductor


#Load source files
source("source_files/source_data.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)



