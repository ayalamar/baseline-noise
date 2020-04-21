
source('R/variance.R')
library('effsize')
library('svglite')
#library('car')
library('TOSTER')

# groupcolors <- list( 'young'=c('#ff8200ff','#ff82002f'), 'aging'=c('#c400c4ff','#c400c42f') )

colorset <- list()

colorset[['youngactS']] <- '#e51636ff' # "York red"
colorset[['youngactT']] <- '#e516362f'
colorset[['youngpasS']] <- '#ff8200ff' # orange
colorset[['youngpasT']] <- '#ff82002f'

colorset[['agingactS']] <- '#005de4ff' # blue
colorset[['agingactT']] <- '#005de42f'
colorset[['agingpasS']] <- '#2ab2f2ff' # lighter blue
colorset[['agingpasT']] <- '#2ab2f22f'

colorset[['extra1S']] <- '#c400c4ff' # purple
colorset[['extra1T']] <- '#c400c42f'

plotLocalizationRaw <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig1_localization-data.svg',width=7,height=6,system_fonts=list(sans='Arial'))
  }
  par(mar=c(4.1,4.1,1.1,1.1))
  
  layout(matrix(c(1,2,3,4,5,6,7,8), 4, 2, byrow = TRUE),
         widths=c(1,2), heights=c(1,1,1,1))
  
  plotLocalizationCloud(group='young',task='active')
  plotBaselineBiases(group='young',task='active')
  plotLocalizationCloud(group='young',task='passive')
  plotBaselineBiases(group='young',task='passive')
  plotLocalizationCloud(group='aging',task='active')
  plotBaselineBiases(group='aging',task='active')
  plotLocalizationCloud(group='aging',task='passive')
  plotBaselineBiases(group='aging',task='passive')
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceMLE <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig2_variance-scatter.svg',width=7,height=7,system_fonts=list(sans='Arial'))
    par(mfrow=c(2,2),mar=c(4.1,4.1,1.1,1.1))
  } else {
    par(mfrow=c(1,2),mar=c(4.1,4.1,1.1,1.1))
  }
  
  plotVariancesScatters(group='young')
  plotVariancesScatters(group='aging')
  # plotVariancesRatios(group='young')
  # plotVariancesRatios(group='aging')
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceAge <- function(target='notebook') {
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig3_variance-age.svg',width=7,height=7,system_fonts=list(sans='Arial'))
    par(mfrow=c(2,2))
  } else {
    par(mfrow=c(1,2))
  }
  
  plotVarianceDensityComparison(task='active',groups=c('young','aging'))
  plotVarianceDensityComparison(task='passive',groups=c('young','aging'))
  
  if (target == 'svg') {
    dev.off()
  }
  
}

plotVarianceRecalibration <- function(target='notebook') {

  if (target == 'svg') {
    svglite(file='doc/fig/Fig4_variance-recalibration.svg',width=7,height=7,system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(2,3))
  layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE),
         widths=c(3,3,2), heights=c(1,1))
  
  plotVariancesRecalibrationScatter(group='young',task='active')
  plotVariancesRecalibrationScatter(group='young',task='passive')
  plotRecalibrationCI(group='young')
  plotVariancesRecalibrationScatter(group='aging',task='active')
  plotVariancesRecalibrationScatter(group='aging',task='passive')
  plotRecalibrationCI(group='aging')
  
  
  if (target == 'svg') {
    dev.off()
  }
  
}

# plotVariancesRatios <- function(group='young') {
#   
#   dfs <- loadVarCIs()
#   
#   df <- dfs[[group]]
#   
#   idx <- order(df$p500)
#   
#   plot(-1000,-1000,main=group,xlim=c(0.5,2.0),ylim=c(0,length(idx)+1),axes=F,xlab='variance: passive / active',ylab='participant')
#   # Xs <- c()
#   for (pp.idx in c(1:length(idx))) {
#     
#     X <- c(df$p025[idx[pp.idx]],df$p975[idx[pp.idx]],df$p975[idx[pp.idx]],df$p025[idx[pp.idx]]) / df$a500[idx[pp.idx]]
#     Y <- c(pp.idx, pp.idx, pp.idx+0.75, pp.idx+0.75)
#     
#     color <- rgb(0.5,0.5,0.5,0.5)
#     if (df$p025[idx[pp.idx]] > df$a500[idx[pp.idx]]) {
#       color <- rgb(0,0,1,1)
#     }
#     if (df$p975[idx[pp.idx]] < df$a500[idx[pp.idx]]) {
#       color <- rgb(1,0,0,1)
#     }
#     
#     # Xs <- c(Xs, X)
#     polygon(X,Y,border=NA,col=color)
#     
#     # lines(rep(df$a500[idx[pp.idx]],2),c(pp.idx,pp.idx+0.75),col=rgb(1,0,0))
#     
#   }
#   # print(range(Xs))
#   lines(c(1,1),c(1,length(idx)+0.75),col=rgb(0,0,0))
#   lines(c(5/6,5/6),c(1,length(idx)+0.75),col=rgb(0.5,0.5,0.5,.5),lty=1)
#   lines(c(6/5,6/5),c(1,length(idx)+0.75),col=rgb(0.5,0.5,0.5,.5),lty=1)
#   
#   axis(side=1,at=c(0.5,5/6,1.0,6/5,1.5,2.0),labels=c('1/2','5/6','1/1','6/5','3/2','2/1'))
#   axis(side=2,at=c(1,20,40,60,80))
#   
# }

plotVariancesScatters <- function(group='young',verbose=TRUE) {
  
  dfs <- loadVarCIs()
  
  df <- dfs[[group]]
  
  idx <- order(df$p500)
  
  # plot(-1000,-1000,log-'xy',main=group,xlim=c(1,250),ylim=c(1,250),axes=F,xlab='active variance',ylab='passive variance',asp=1)
  plot(-1000,-1000,main=group,xlim=c(0,15),ylim=c(0,15),axes=F,xlab=bquote(.('active') ~ sigma),ylab=bquote(.('passive') ~ sigma),asp=1)
  
  areacolor <- colorset[[sprintf('%spasT',group)]]
  polygon(c(0,15,0,0),c(0,15,15,0),col=areacolor,border=NA)
  
  logP500 <- sqrt(df$p500)
  logA500 <- sqrt(df$a500)

  regmod <- lm(formula = logP500 ~ logA500)
  # lines(c(0,5.5),(c(0,5.5)*coef(regmod)[2])  + coef(regmod)[1], col=rgb(1,0,1), lty=2 )
  
  X <- seq(0,15,.001)
  Yci <- predict( regmod,
                newdata = data.frame( logA500=X ),
                interval = "confidence" )
  Yupr <- Yci[,'upr']
  Ylwr <- Yci[,'lwr']
  
  polyX <- c(X,rev(X))
  polyY <- c(Yupr,rev(Ylwr))
  polygon(polyX,polyY,col=colorset[['extra1T']],border=NA)
  
  abX <- c(0,15)
  inter <- regmod$coefficients[1]
  slope <- regmod$coefficients[2]
  lines(abX,(slope*abX)+inter,col=colorset[['extra1S']],lty=2)
  
  confirmLME <- 0

  for (pp.idx in c(1:length(idx))) {
    
    # X <- sqrt( c(df$a025[idx[pp.idx]],df$a975[idx[pp.idx]],df$a975[idx[pp.idx]],df$a025[idx[pp.idx]]) )
    # Y <- sqrt( c(df$p025[idx[pp.idx]],df$p025[idx[pp.idx]],df$p975[idx[pp.idx]],df$p975[idx[pp.idx]]) )
    
    color <- rgb(0.5,0.5,0.5)
    if (df$p025[idx[pp.idx]] > df$a500[idx[pp.idx]]) {
      # color <- rgb(0,0,1)
      color <- colorset[[sprintf('%spasS',group)]]
    }
    if (df$p975[idx[pp.idx]] < df$a500[idx[pp.idx]]) {
      # color <- rgb(1,0,0)
      color <- colorset[[sprintf('%sactS',group)]]
    }
    
    if (df$p500[idx[pp.idx]] > df$a500[idx[pp.idx]]) {
      confirmLME <- confirmLME + 1
    }
    # Xs <- c(Xs, X)
    # polygon(X,Y,border=NA,col=color)
    points(sqrt(df$a500[idx[pp.idx]]),sqrt(df$p500[idx[pp.idx]]),col=color)
    
    # lines(rep(df$a500[idx[pp.idx]],2),c(pp.idx,pp.idx+0.75),col=rgb(1,0,0))
    
  }
  # print(range(Xs))
  
  BIexact <- binom.test(x=confirmLME, n=length(logP500))
  
  cortest <- cor.test(logP500,logA500)
  #text(1,17.5,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(logP500)),pos=4)
  text(8,2.5,sprintf('%d/%d (p=%0.3f)\nslope=%0.3f\nr=%0.3f (p<0.001)',confirmLME,length(logP500),BIexact$p.value,slope,cortest$estimate),pos=4)
  # identity line
  lines(c(0,15),c(0,15),col=rgb(0,0,0),lty=1)
  
  # logarithmic stuff:
  # axis(side=1,at=log(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300)),labels=c('','2','','','','','','','','','20','','','','','','','','','200',''))
  # axis(side=2,at=log(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300)),labels=c('','2','','','','','','','','','20','','','','','','','','','200',''))
  
  # SD axis:
  axis(side=1,at=c(0,5,10,15))
  axis(side=2,at=c(0,5,10,15))
  
  if(verbose) {
    
    print(summary(regmod))
    
    print(rbind( data.frame(confint(regmod, '(Intercept)', level=0.95)), 
                 data.frame(confint(regmod, 'a500', level=0.95)) ))
    
    print(t.test(log(df$p500),log(df$a500),paired=TRUE))
    
    print(cohen.d(log(df$p500),log(df$a500),paired=TRUE))
    
    P500 <- df$p500
    A500 <- df$a500
    print("check")
    eqtest <- TOSTpaired.raw(n=length(logP500),
                             m1=mean(logA500),
                             m2=mean(logP500),
                             sd1=sd(logA500),
                             sd2=sd(logP500),
                             cor(logA500,logP500),
                             low_eqbound = -.5,      # lower bound in Cohen's d's
                             high_eqbound = .5)      # upper bound in Cohen's d's
    
    print(eqtest)
    print(mean(logP500-logA500))
    
    #print(mean(df$a500))
    #print(mean(df$p500))
    
  }
}

plotVarianceDensityComparison <- function(task,groups) {
  
  dfs <- loadVarCIs()
  
  # groupcolors <- list( 'young'=c('#ff8200ff','#ff82002f'), 'aging'=c('#c400c4ff','#c400c42f') )
  
  groupttestdata <- list()
  
  densitylinesY <- list()
  densitylinesX <- list()
  
  medians <- list()
  
  legendcolors <- c()
  
  #plot(-1000,-1000,main=sprintf('%s localization',task),xlim=c(0,17),ylim=c(0,0.16),axes=F,xlab=expression(sqrt(sigma^2)),ylab='density')
  plot(-1000,-1000,main=sprintf('%s localization',task),xlim=c(0,17),ylim=c(0,0.16),axes=F,xlab=bquote(.(task) ~ sigma),ylab='density')
  
  for (group in groups) {
    
    df <- dfs[[group]]
    
    varname <- sprintf('%s500',substr(task,1,1))
    
    data <- density(sqrt(df[,varname]),from=0, n=1024)
    
    CI <- t.interval(sqrt(df[,varname]))
    idx <- which(data$x > CI[1] & data$x < CI[2])
    
    # X <- c(data$x[idx], rev(data$x[idx]))
    # Y <- c(data$y[idx], rep(0,length(idx)))
    X <- c(data$x, rev(data$x))
    Y <- c(data$y, rep(0,length(data$y)))
    polygon(X,Y,border=NA,col=colorset[[sprintf('%s%sT',group,substr(task,1,3))]])
    
    densitylinesY[[group]] <- data$y
    densitylinesX[[group]] <- data$x
    thismedian <- median(sqrt(df[,varname]))
    x.idx <- which(abs(data$x - thismedian) == min(abs(data$x - thismedian)))
    medians[[group]] <- c(thismedian,data$y[x.idx])
    groupttestdata[[group]] <- sqrt(df[,varname])
    
  }
  
  for (group in groups) {
    
    lines(densitylinesX[[group]], densitylinesY[[group]], col=colorset[[sprintf('%s%sS',group,substr(task,1,3))]], lty=1)
    lines(rep(medians[[group]][1],2),c(0,medians[[group]][2]), lty=2, col=colorset[[sprintf('%s%sS',group,substr(task,1,3))]])
    
    legendcolors <- c(legendcolors,colorset[[sprintf('%s%sS',group,substr(task,1,3))]])
    
  }
  
  legend(10,.15,legend=groups,col=legendcolors,lty=1,bty='n')
  
  #cat(sprintf('\n*** %s - %s, %s\n', task, groups[[1]], groups[[2]]))
  #print(t.test(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='g'))
  # 
  #print(cohen.d(log(groupttestdata[[groups[1]]]),log(groupttestdata[[groups[2]]]),alternative='g'))
  
  # axis(side=1,at=sqrt(c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400)),labels=c('','2','','','','','','','','','20','','','','','','','','','200','',''))
  axis(side=1,at=c(0,5,10,15))
  axis(side=2,at=c(0,.05,.1,.15))
  
}

t.interval = function(data, variance = var(data, na.rm=TRUE), conf.level = 0.95) {
  
  z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
  
  xbar = mean(data, na.rm=TRUE)
  sdx = sqrt(variance/length(data))
  
  return(c(xbar - z * sdx, xbar + z * sdx))
  
}

plotVariancesRecalibrationScatter <- function(group='young',task='active') {
  
  # load the data for the group:
  locvar   <- read.csv(sprintf('data/%s_varianceCIs.csv',group), stringsAsFactors=F)
  locshift <- read.csv(sprintf('data/%s_rotated_localization.csv',group), stringsAsFactors=F)
  
  # only use the "good" groups?
  if (group == 'young') {
    good_shifts <- c('30implicit','30explicit','cursorjump','60implicit_b','60explicit_b',
                      'sEDS')
    locvar   <- locvar[which(locvar$group %in% good_shifts),]
    locshift <- locshift[which(locshift$group %in% good_shifts),]
  }
  
  # under accepted theory, the variance should predict the shift:
  
  X <- sqrt(locvar[,sprintf('%s500',substr(task,1,1))])
  active_bool <- c('a'=1, 'p'=0)[substr(task,1,1)]
  Y <- locshift$localizationshift_deg[which(locshift$active_bool == active_bool)] * -1
  
  color <- colorset[[sprintf('%s%sS',group,substr(task,1,3))]]
  
  #plot(X,Y,main=sprintf('%s - %s',group,task),xlim=c(0,15),ylim=c(-10,30),axes=F,xlab=expression(sqrt(sigma^2)),ylab='localization shift [°]',col=color)
  plot(X,Y,main=sprintf('%s - %s',group,task),xlim=c(0,15),ylim=c(-10,30),axes=F,xlab=bquote(.(task) ~ sigma),ylab='localization shift [°]',col=color)
  
  #cat(sprintf('\n***%s - %s\n', group, task))
  regr <- lm(Y~X)
  #print(summary(regr))
  abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
  
  cortest <- cor.test(X,Y)
  text(11,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(X)),pos=4)
  
  axis(side=1,at=c(0,5,10,15))
  axis(side=2,at=c(-10,0,10,20,30))
  
}

plotRecalibrationCI <- function(group='young') {
  
  probs=c(.025, .500, .975)
  
  # load the data for the group:
  locshift <- read.csv(sprintf('data/%s_rotated_localization.csv',group), stringsAsFactors=F)
  
  # under accepted theory, the variance should predict the shift:
  
  plot(-1000,-1000,main=sprintf('%s',group),xlim=c(-0.5,1.5),ylim=c(-10,30),axes=F,xlab='movement type',ylab='localization shift [°]')
  
  for (task in c('active', 'passive')) {
    active_bool <- c('a'=1, 'p'=0)[substr(task,1,1)]
    shift <- locshift$localizationshift_deg[which(locshift$active_bool == active_bool)] * -1
    
    shiftCI <- bootstrap.CI(shift, probs=probs, samples=10000, FUN=median)
    
    line_color <- colorset[[sprintf('%s%sS',group,substr(task,1,3))]]
    area_color <- colorset[[sprintf('%s%sT',group,substr(task,1,3))]]
    
    midX <- 1 - active_bool 
    pX <- c(midX-.4,midX+.4,midX+.4,midX-.4)
    pY <- c(shiftCI[1],shiftCI[1],shiftCI[3],shiftCI[3])
    polygon(pX,pY,border=NA,col=area_color)
    lines(c(midX-.4,midX+.4),c(shiftCI[2],shiftCI[2]),col=line_color)
    
  }
  
  axis(side=1,at=c(0,1),labels=c('active','passive'))
  axis(side=2,at=c(-10,0,10,20,30))
  
}

plot2Delipses <- function() {
  
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  #
  # The ellipses follow the arc: participants followed instructions.
  #
  # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  # only use the younger participants
  df <- loadDetrendedData()[['young']]
  
  plot(-1000,-1000,main='',xlab='',ylab='',xlim=c(-15,15),ylim=c(-3,15),asp=1)
  lines(c(-12,12),c(0,0),col='#999999')
  lines(c(0,0),c(0,12),col='#999999')
  lines(cos(seq(0,pi,pi/1000))*12,sin(seq(0,pi,pi/1000))*12,col='#999999')
  
  df <- df[df$active_bool == 1,]
  
  #par(mfrow=c(3,3))
  binedges <- seq(30,150,15)
  for (binno in c(1:(length(binedges)-1))) {
    binstart <- binedges[binno]
    binstop <- binedges[binno+1]
    
    binmiddle <- c(cos((mean(c(binstart,binstop))/180)*pi),sin((mean(c(binstart,binstop))/180)*pi)) * 12
    
    # plot(-1000,-1000,main=sprintf('bin %d (%d - %d)',binno,binstart,binstop),xlab='',ylab='',xlim=c(-15,15),ylim=c(-3,15),asp=1)
    # lines(c(-12,12),c(0,0),col='#999999')
    # lines(c(0,0),c(0,12),col='#999999')
    # lines(cos(seq(0,pi,pi/1000))*12,sin(seq(0,pi,pi/1000))*12,col='#999999')
    
    idx <- which(df$handangle_deg > binstart & df$handangle_deg < binstop)
    coords <- matrix(nrow=length(idx),ncol=2)
    coords[,1] <- df$dx_cm[idx] + binmiddle[1]
    coords[,2] <- df$dy_cm[idx] + binmiddle[2]
    ellipse(center = colMeans(coords), shape = cov(coords), radius = sqrt(qchisq(.90, df=2)), col = 'red', lw=1)
    points(df$dx_cm[idx]+binmiddle[1],df$dy_cm[idx]+binmiddle[2],col='#66bb9944')
  }
  
}

plotLocalizationCloud <- function(group, task) {
  
  df <- loadDetrendedData()[[group]]
  df <- df[which(df$active_bool == c('active'=1, 'passive'=0)[task]),]
  
  #tapX <- (cos((df$handangle_deg/180)*pi) * 12) + df$dx_cm
  #tapY <- (sin((df$handangle_deg/180)*pi) * 12) + df$dy_cm
  
  plot(-1000, -1000, main=sprintf('%s - %s',group,task), xlab='', ylab='', xlim=c(-17,17), ylim=c(0,16), asp=1, axes=F)
  
  # subset the data?
  # idx <- sample(c(1:dim(df)[1]),1000)
  # NO:
  idx <- seq(1:dim(df)[1])
  
  points(df$tapx_cm[idx], df$tapy_cm[idx], col=colorset[[sprintf('%s%sT',group,substr(task,1,3))]])
  
  
  lines(c(-12,12),c(0,0),col='#000000')
  lines(c(0,0),c(0,12),col='#000000')
  lines(cos(seq(0,pi,pi/500))*12,sin(seq(0,pi,pi/500))*12,col='#000000')
  lines(c(0,cos(pi*.25)*12),c(0,sin(pi*.25)*12),col='#000000')
  lines(c(0,cos(pi*.75)*12),c(0,sin(pi*.75)*12),col='#000000')
  
  text(cos(pi*seq(0,1,.25))*16,sin(pi*seq(0,1,.25))*16,c('0°','45°','90°','135°','180°'))
  
  #text(0,-2,sprintf('(%d responses)',dim(df)[1]))
  
  axis(side=1,at=0,labels=c(sprintf('(%d responses)',dim(df)[1])),tick=F)
  
}

plotBaselineBiases <- function(group, task) {
  
  df <- loadDetrendedData()[[group]]
  df <- df[which(df$active_bool == c('active'=1, 'passive'=0)[task]),]
  
  plot(-1000,-1000,main='',xlab='hand angle [°]',ylab='localization error [°]',xlim=c(0,180),ylim=c(-30,30), axes=F)
  
  participants <- unique(df$participant)
  
  angles <- seq(0,180,.5)
  allbias <- matrix(nrow=length(participants),ncol=length(angles))
  
  for (participant_no in c(1:length(participants))) {
    
    P.idx <- which(df$participant == participants[participant_no])
    print(P.idx)
    # tap angles in data are "detrended" (corrected for the bias we want to plot here)
    # so we'll need to work with the original positions again:
    tapx <- df[P.idx,'tapx_cm']
    tapy <- df[P.idx,'tapy_cm']
    print(tapx)
    # I use atan2(X,Y), not atan2(Y,X) 
    # to stay well within the atan2 output range,
    # then just add 90 degrees again:
    localizationangle_deg <- ((atan2(-1*tapx, tapy) / pi) * 180) + 90
    
    # handangle is already in the data and not detrended:
    handangle_deg <- df[P.idx, 'handangle_deg']
    
    spl <- smooth.spline(x=handangle_deg, y=localizationangle_deg-handangle_deg, spar=0.90, keep.data=F )
    angularerror <- predict(spl, x=angles)$y
    angularerror[which(angles < min(handangle_deg))] <- NA
    angularerror[which(angles > max(handangle_deg))] <- NA
    
    lines(angles, angularerror, col=colorset[[sprintf('%s%sT',group,substr(task,1,3))]])
    
    allbias[participant_no,] <- angularerror 
    
  }
  
  lines(c(15,165),c(0,0),lty=1,lw=1,col='#000000')
  lines(angles[90:270],colMeans(allbias,na.rm=T)[90:270],lw=3,col=colorset[[sprintf('%s%sS',group,substr(task,1,3))]])
  
  axis(side=1,at=seq(0,180,45))
  axis(side=2,at=c(-30,0,30))
  
}