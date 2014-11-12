
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(foreign)
library(ggplot2)
#library(dplyr)
library(reshape2)
library(RColorBrewer)

options(shiny.maxRequestSize = -1)

shinyServer(function(input, output) {

  ## Set the event/censor marks. Want open circle for censored, and
  ## x for events
  event.marks=c(1, 4)
  event.labels=c("Cens", "Dead")

  strCol <- brewer.pal(3, "Set1")
  siteCol <- c(strCol[2], strCol[3],strCol[1])
  strCol <- c(strCol[2], strCol[1])

  ### Figure theme.
  theme_set(theme_bw())

  output$contents <- renderPrint({
    summary(readData())
  })

  readData <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    #print(inFile$datapath)

    if(grep(".xpt", inFile)){
      dta <- read.xport(inFile$datapath)
    }else{
      dta <- read.csv(inFile$datapath, header=input$header, sep=input$sep,
                      quote=input$quote)
    }

    ### R is case sensative.
    colnames(dta)<- tolower(colnames(dta))

    # Drop missing value binary variables ("ms_*")
    rmcols <-c(which(colnames(dta) == "id"),
               which(colnames(dta) == "ccfid"),
               which(colnames(dta) == "ptname"),
               which(colnames(dta) == "indicate"),
               grep("ms_", colnames(dta)),
               grep("dt_", colnames(dta)),
               grep("dtn_", colnames(dta)),
               grep("dtx_", colnames(dta)),
               grep("ssn", colnames(dta)))
    #cat(rmcols, "\t rmcols \n")
    # print(rmcols)
    if(length(rmcols) > 0)
      dta <- dta[,-rmcols]

    ## Set modes correctly. For binary variables: transform to logical
    ## Check for range of 0,1
    ## There is probably a better way to do this.
    for(ind in 1:dim(dta)[2]){
      if(!is.factor(dta[,ind]))
        if(length(unique(dta[which(!is.na(dta[,ind])),ind]))<=2) {
          if(sum(range(dta[,ind],na.rm=TRUE) == c(0,1))==2){
            dta[,ind] <- as.logical(dta[,ind])
          }
        }
    }
    for(ind in 1:dim(dta)[2]){
      if(!is.factor(dta[,ind]) & !is.logical(dta[,ind]))
        if(length(unique(dta[which(!is.na(dta[,ind])),ind]))<=6) {
          dta[,ind] <- factor(dta[,ind])
        }
    }

    dta
  })

  output$xvar <- reactive({
    #dta <- readData()
    labs <- readLabs()
    labs
  })

  readLabs <- reactive({

    inFile <- input$file1

    if (is.null(inFile) | !grep(".xpt", inFile))
      return(NULL)

    dta <- lookup.xport(inFile$datapath)

    nms <- tolower(dta[[1]]$name)
    # Pull out the labels, and name them such that they are
    # the same as the dta set.
    dta.labels <- dta[[1]]$label

    ## Fill in empty labels with the variable name
    dta.labels<-sapply(1:length(dta.labels), function(ind){
      if(dta.labels[ind] == "")
        nms[ind]
      else
        dta.labels[ind]
    })

    ## For indexing the labels
    names(dta.labels) = nms
    dta.labels
  })

  output$categorical <- renderPlot({

    dta.st <- readData()
    if(is.null(dta.st)) return(NULL)

    labs <- readLabs()

    print(cbind(labs))

    cls<- sapply(dta.st, class)

    # Somehow determine the number of vars, so we can size
    # the plot reasonably.
    lng <- length(which(cls != "numeric"))

    # We want to stay around 12 or so plots deep.
    # So, we want to calculate ncol
    ncol=round(lng/12)



    print(lng)
    xvr <- xv()
    if(is.null(xvr))return(NULL)

    ## If we have a large range of xvar, we want to adjust the binwidths
    rn <- range(dta.st[,which(colnames(dta.st) == xvr)], na.rm=TRUE)
    bn <- (rn[2]-rn[1])/30
    if(bn < .25) bn <- .25

    dta.tmp<-dta.st[,c(which(cls != "numeric"),
                       which(colnames(dta.st) == xvr))]
    suppressWarnings(plt.dta <- melt(dta.tmp, id=xvr))

    dtaView <- ggplot(plt.dta) +
      geom_histogram(aes_string(x=xvr, fill='value'),
                     alpha=.5, binwidth=bn, color="black")+
      labs(x=labs[xvr], y="")+
      facet_wrap(~variable, ncol=ncol)+
      # scale_fill_brewer(palette="Set1", na.value="lightgrey")+
      theme(legend.position="none")

    dtaView
  })

  xv <- reactive({
    input$xvars
  })

  cen <- reactive({
    input$censor
  })

  output$continuous <- renderPlot({

    dta.st <- readData()
    labs <- readLabs()

    if(is.null(dta.st))return(NULL)

    cls<- sapply(dta.st, class)

    # Somehow determine the number of vars, so we can size
    # the plot reasonably.
    lng <- length(which(cls == "numeric"))

    # We want to stay around 12 plots deep.
    # So, we want to calculate ncol
    ncol=round(lng/10)

    xvr <- xv()
    if(is.null(xvr))return(NULL)

    cn <- cen()
    if(is.null(cn))return(NULL)
    #print(cn)

    dta.tmp<-dta.st[,c(which(cls == "numeric"),
                       which(colnames(dta.st) == cn))]
    suppressWarnings(plt.dta <- melt(dta.tmp, id=c(xvr, cn)))

    dtaView<- ggplot(plt.dta,
                     aes_string(x=xvr, y='value', color=cn, shape=cn)) +
      geom_point(alpha=.5)+
      labs(x=labs[xvr], y="")+
      facet_wrap(~variable,scales = "free_y", ncol=ncol)+
      scale_color_manual(values=strCol, na.value="lightgrey")+
      scale_shape_manual(values=event.marks) +
      theme(legend.position="none")

    dtaView
  })

  output$xvars <- renderUI({
    # Look through the data...
    labs <- readData()

    if(is.null(labs))return(NULL)

    cls<- sapply(labs, class)

    # Somehow determine the number of vars, so we can size
    # the plot reasonably.
    lng <- which(cls == "numeric")

    # x-var should be numeric only...
    nms <- colnames(labs)[lng]
    # print(nms)

    # Search for a set of "defaults"
    if("iv_opyrs" %in% nms){
      sl <- "iv_opyrs"
    }else{
      sl <- nms[grep("opyrs", nms)[1]]
    }

    # Somehow determine the number of vars, so we can size
    # the plot reasonably.
    lng <- which(cls == "logical")

    # censor should be logical only...
    cens <- colnames(labs)[lng]

    # Search for a set of "defaults"
    if("dead" %in% cens){
      csl <- "dead"
    }else if("death" %in% cens){
      csl <- "death"
    }else if("os" %in% cens){
      csl <- "os"
    }else{
      csl <- NULL
    }

    # print(sl)

    if(is.null(labs)){
      NULL
    }else{
      c(
        selectInput("xvars", label="X-axis variable:",
                    choices=nms,
                    selected=sl),
        selectInput("censor", label="censor variables:",
                    choices=cens,
                    selected=csl),
        includeText("varInclude.txt")
      )
    }
  })
})