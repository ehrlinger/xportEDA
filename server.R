#
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

    print(inFile$datapath)
    cat(length(grep(".csv", inFile, ignore.case = TRUE)), "\n")
    if(length(grep(".xpt", inFile,ignore.case = TRUE)) > 0){
      dta <- read.xport(inFile$datapath)
    }else if(length(grep(".csv", inFile, ignore.case = TRUE)) > 0){
      dta <- read.csv(inFile$datapath)
    }else{

      # Try to load this file as an rdata file
      load(inFile$datapath, envir="dta")
      ## Need to look for a data.frame is dta is a list
      if(!is.null(dta) | !is.data.frame(dta)){
        warning("This rda file is not a data.frame.")
        warning("I am a coward, and I refuse to guess what to do with this.")
        return(NULL)
      }
    }

    #cat(dim(dta), "\n")
    ### R is case sensitive.
    dta <- data.frame(dta)
    colnames(dta)<- tolower(colnames(dta))

    # Drop missing value binary variables ("ms_*")
    rmcols <-c(which(colnames(dta) == "id"),
               which(colnames(dta) == "ccfid"),
               which(colnames(dta) == "ptnum"),
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
        # This 10 should be user selectable.... changeable.
        if(length(unique(dta[which(!is.na(dta[,ind])),ind]))<=10) {
          dta[,ind] <- factor(dta[,ind])
        }
    }


    cls <- sapply(dta, class)

    if(length(which(cls=="integer")) > 0){
      for(ind in which(cls=="integer"))
        dta[,ind] <-as.numeric(dta[,ind])
    }

    if(length(grep("date",colnames(dta)))>0){
      for(ind in grep("date",colnames(dta))){
        if(length(grep(".xpt", inFile,ignore.case = TRUE)) > 0){
          dta[,ind] <- as.Date(dta[,ind], origin="1960-01-01")
        }else{
          dta[,ind] <- as.Date(dta[,ind])
        }

      }
    }
    return(dta)
  })

  r <- reactive({
    #dta <- readData()
    labs <- readLabs()
    labs
  })

  readLabs <- reactive({

    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    if(length(grep(".xpt", inFile,ignore.case = TRUE)) > 0){
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
    }else if(length(grep(".csv", inFile, ignore.case = TRUE)) > 0){
      dta <- read.csv(inFile$datapath)
      dta.labels <- nms <- tolower(colnames(dta))
    }else{

      # Try to load this file as an rdata file
      load(inFile$datapath, envir="dta")
      ## Need to look for a data.frame is dta is a list
      if(!is.null(dta) | !is.data.frame(dta)){
        warning("This rda file is not a data.frame.")
        warning("I am a coward, and I refuse to guess what to do with this.")
        dta.labels <- nms <- NULL
      }
      dta.labels <- nms <- tolower(colnames(dta))
    }
    ## For indexing the labels
    names(dta.labels) = nms
    dta.labels
  })

  output$categorical <- renderPlot({

    dta.st <- readData()
    if(is.null(dta.st)) return(NULL)

    labs <- readLabs()
    if(is.null(labs)) labs <- colnames(dta.st)

    #print(cbind(labs))

    cls<- sapply(dta.st, class)

    # Somehow determine the number of vars, so we can size
    # the plot reasonably.
    lng <- length(which(cls != "numeric" & cls != "Date"))

    # We want to stay around 12 or so plots deep.
    # So, we want to calculate ncol
    ncol=round(lng/12)
    if(ncol<1)ncol=1
    #print(lng)
    xvr <- xv()
    if(is.null(xvr))return(NULL)

    ## If we have a large range of xvar, we want to adjust the binwidths
    rn <- range(dta.st[,which(colnames(dta.st) == xvr)], na.rm=TRUE)
    bn <- (rn[2]-rn[1])/30
    if(bn < .25) bn <- .25

    dta.tmp<-dta.st[,c(which(cls != "numeric" & cls != "Date"),
                       which(colnames(dta.st) == xvr))]

    # Want to drop variables with more than 20 factors
    # Again user modifiable?
    fc.cnt <- apply(dta.tmp, 2, function(cl)(if(is.factor(cl)){
      length(levels(cl)) > 20
    }else{
      FALSE
    }
    ))

    if(sum(fc.cnt) > 0)
      dta.tmp <- dta.tmp[,-which(fc.cnt)]

    print(colnames(dta.tmp))
    suppressWarnings(plt.dta <- melt(dta.tmp, id=xvr))

    dtaView <- ggplot(plt.dta) +
      geom_histogram(aes_string(x=xvr, fill='value'),
                     binwidth=bn, color="black")+# alpha slider?
      labs(x=labs[xvr], y="")+
      facet_wrap(~variable, ncol=ncol)+
      #scale_fill_manual(na.value="lightgrey")+
      theme(legend.position="none")

    dtaView
  })

  seld <- reactive({
    input$selected
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
    lng <- length(which(cls == "numeric" | cls == "Date"))

    # We want to stay around 12 plots deep.
    # So, we want to calculate ncol
    ncol=round(lng/10)
    if(ncol<1)ncol=1
    xvr <- xv()
    if(is.null(xvr))return(NULL)

    cn <- cen()
    if(is.null(cn))return(NULL)
    #print(cn)

    dta.tmp<-dta.st[,c(which(cls == "numeric"),which(cls == "Date"),
                       which(colnames(dta.st) == cn))]
    suppressWarnings(plt.dta <- melt(dta.tmp, id=c(xvr, cn)))

    dtaView<- ggplot(plt.dta,
                     aes_string(x=xvr, y='value', color=cn, shape=cn)) +
      geom_point()+ # alpha slider?
      labs(x=labs[xvr], y="")+
      geom_rug(aes_string(x=xvr), data=plt.dta[which(is.na(plt.dta$value)),], color="black")+
      facet_wrap(~variable,scales = "free_y", ncol=ncol)+
      theme(legend.position="none")


    if(is.logical(plt.dta[,cn])){
      dtaView <- dtaView +
        scale_color_manual(values=strCol, na.value="lightgrey")+
        scale_shape_manual(values=event.marks)
    }else{
      cnt <- length(levels(plt.dta[,cn]))

      # We have to choose a palette with at least cnt colors.
      if(cnt<= 12){
        dtaView <- dtaView +
          scale_color_brewer(palette="Set3", na.value="lightgrey")
      }
    }
    dtaView
  })

  output$varPlot <- renderPlot({
    dta <- readData()

    if(is.null(dta))return(NULL)

    labs <- readLabs()
    if(is.null(labs))return(NULL)

    sll <- seld()
    if(is.null(sll))return(NULL)
    xvr <- xv()
    if(is.null(xvr))return(NULL)

    #     print(sll)
    #     print(xvr)

    cls <- class(dta[,which(colnames(dta) == sll)])

    if(cls == "numeric"){
      cn <- cen()
      if(is.null(cn))return(NULL)

      dtaView<- ggplot(dta,
                       aes_string(x=xvr, y=sll, color=cn, shape=cn)) +
        geom_point()+# alpha slider?
        geom_rug(aes_string(x=xvr), data=dta[which(is.na(dta[,sll])),], color="black")+
        labs(x=labs[xvr], y=labs[sll])

      if(is.logical(dta[,cn])){
        dtaView <- dtaView +
          scale_color_manual(values=strCol, na.value="lightgrey")+
          scale_shape_manual(values=event.marks)
      }else{
        cnt <- length(levels(dta[,cn]))

        # We have to choose a palette with at least cnt colors.
        if(cnt<= 12){
          dtaView <- dtaView +
            scale_color_brewer(palette="Set3", na.value="lightgrey")
        }
      }
    }else{
      ## If we have a large range of xvar, we want to adjust the binwidths
      rn <- range(dta[,which(colnames(dta) == xvr)], na.rm=TRUE)
      bn <- (rn[2]-rn[1])/30
      if(bn < .25) bn <- .25

      dtaView <- ggplot(dta) +
        geom_histogram(aes_string(x=xvr, fill=sll),
                       binwidth=bn, color="black")+ # alpha slider?
        labs(x=labs[xvr], y=labs[sll])
    }
    dtaView
  })

  output$selectPlot <- renderUI({
    # Look through the data...
    labs <- readData()

    # x-var should be numeric only...
    nms <- colnames(labs)

    if(is.null(labs)){
      NULL
    }else{
      selectInput("selected", label="Select Plot:",
                  choices=nms)
    }
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
    lng <- which(cls == "logical" | cls == "factor" )

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
        selectInput("censor", label="Event variable:",
                    choices=cens,
                    selected=csl),
        includeText("varInclude.txt")
      )
    }
  })
})