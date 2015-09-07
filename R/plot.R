#' Boxplot for a list of benchdata analysis
#' @export
bplListBenchAnalysis <- function(lS, 
                colors=c('blue', 'red', 'green', 'brown', 'violet', 'orange',
                         'darkgreen'),
                ylim=c(0,0), outliers=T, 
                abl=NULL, xlabels=F, 
                title=NULL, legend=F, legPos='topleft', legTexts=NULL, 
                boxwex=0.15, mgp=c(1.5,0.5,0), grid=T, xlab='', ylab='', txt='',
                ...) {
  levels <- colnames(lS[[1]])
  
  if(is.null(ylim) | ylim[2]-ylim[1]==0) {
    if(outliers) {
      ylim <- range(sapply(lS, range, na.rm=T))
    } else {
      ylim <- range(sapply(lS, function(a) {
        apply(a, 2, function(x) range(boxplot.stats(x)$stats))
      }), na.rm=T)
    }
    ylim[2] <- ylim[2]+0.05*(ylim[2]-ylim[1])
  }
  
  par(mgp=mgp)
  for(i in 1:length(lS)) {
    offset <- (i - ceiling(length(lS)/2)) * (boxwex+0.01)
    boxplot(lS[[i]], ylim=ylim, xlim=c(1.2, length(levels)+0.3+0.5), 
            boxwex=boxwex-0.05, at=(1:length(levels))+0.5+offset, 
            col=colors[i], border=colors[i], axes=F, labels=F, add=i>1, 
            main=ifelse(i==1 & !is.null(title), title, ''),
            xlab=ifelse(i==1,xlab, ''), ylab=ifelse(i==1, ylab, ''), ...)
  }
  
  box()
  if( !is.null(abl) ) {
    sapply(abl, function(h) abline(h=abl, col='black'))
  }
  
  if(is.logical(xlabels) & xlabels) {
    xlabels <- levels
  } 
  axis(1, at=(1:length(levels))+0.5, labels=xlabels)
  
  digs <- 2 - round(log10(ylim[2]-ylim[1]), digits=0)
  
  axis(2)
  
  if(grid) {
    grid()
  }
  if(legend) {
    legTexts <- if(is.null(legTexts)) names(lS) else legTexts
    legend(legPos, legend=sapply(legTexts, function(lt) parse(text=lt)), 
           fill=colors, border=colors, cex=1)
  }
  text(1, ylim[1]+0.97*(ylim[2]-ylim[1]), pos=4, labels=txt) 
}


#' Violin plot for a list of benchdata analysis
#' @export
vioplListBenchAnalysis <- function(lS, 
                                 colors=c('blue', 'red', 'green', 'brown', 'violet', 'orange',
                                          'darkgreen'),
                                 ylim=c(0,0), outliers=T, 
                                 abl=NULL, xlabels=F, 
                                 title=NULL, legend=F, legPos='topleft', legTexts=NULL, 
                                 boxwex=0.15, mgp=c(1.5,0.5,0), grid=T, xlab='', ylab='', txt='',
                                 ...) {
  levels <- colnames(lS[[1]])
  
  
  if(is.null(ylim) | ylim[2]-ylim[1]==0) {
    if(outliers) {
      ylim <- range(sapply(lS, range, na.rm=T))
    } else {
      ylim <- range(sapply(lS, function(a) {
        apply(a, 2, function(x) range(boxplot.stats(x)$stats))
      }), na.rm=T)
    }
    ylim[2] <- ylim[2]+0.05*(ylim[2]-ylim[1])
  }
  
  par(mgp=mgp)
  
  if(is.logical(xlabels)) {
    if(xlabels) {
      xlabels <- levels
    } else {
      xlabels <- rep('', length(levels))
    }
  } 
  
  nPlots <- length(lS)
  plOrder <- c(ceiling(nPlots/2), (1:nPlots)[-ceiling(nPlots/2)])
  #print(plOrder)
  for(i in plOrder) {
    offset <- (i - ceiling(nPlots/2)) * (boxwex+0.01)
    #print(offset)
    lll <- l.(split(lS[[i]], col(lS[[i]])), .[!is.na(.)])
    names(lll) <- NULL
    #print(lll)
    do.call(vioplot, c(lll, 
                       list(ylim=ylim,  
                            wex=boxwex, 
                            at=(1:length(levels))+offset, 
                            col=colors[i], border=colors[i], rectCol='black',
                            colMed='black', 
                            names=xlabels, 
                            add=i!=plOrder[1]
                       )))
  }
  
  title(main=title, xlab=xlab, ylab=ylab)
  
  if( !is.null(abl) ) {
    sapply(abl, function(h) abline(h=abl, col='black'))
  }
  
  digs <- 2 - round(log10(ylim[2]-ylim[1]), digits=0)
  
  if(grid) {
    grid()
  }
  if(legend) {
    legTexts <- if(is.null(legTexts)) names(lS) else legTexts
    legend(legPos, legend=sapply(legTexts, function(lt) parse(text=lt)), 
           fill=colors, border=colors, cex=1)
  }
  text(1, ylim[1]+0.97*(ylim[2]-ylim[1]), pos=4, labels=txt) 
}
