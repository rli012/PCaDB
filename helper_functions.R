
### COLORS
google.red <- '#EA4335'
google.yellow <- '#FBBC05'
google.green <- '#34A853'
google.blue <- '#4285F4'

google.colors <- c(google.blue, google.red, google.green, google.yellow)

default.colors <- c('#3266cc','#dc3812','#fe9900','#109619','#990099',
                    '#0099c5','#dd4578','#66aa00','#b82e2e','#316394',
                    '#994499','#21aa98','#aaab12','#6633cc','#e67300',
                    '#329262','#5474a5','#3c3ead','#8b0607','#641066',
                    '#f8756b','#e76af2','#02b0f7','#02bf7d','#8c564a',
                    '#e377c2','#9467bc','#7f7f7f','#bcbd23','#17bed0',
                    '#aec6e8','#ffbc78','#97df89','#ff9897','#c4b0d5',
                    '#c49c94','#f7b7d2','#dadb8d','#9edae5','#ffed6f')

colors <- c(google.colors, default.colors[c(5:20,4,21:23,25:40)])
pie.colors <- c(google.colors, default.colors[c(5:20,4,21:23,25:40)])


boxplotFun <- function(dataForBoxPlot) {
  p <- ggplot(dataForBoxPlot, aes(x=group, y=expr)) + 
    geom_violin(aes(fill=group)) +
    #geom_boxplot(aes(fill=sample), width=0.2,
    #             outlier.shape = NA, outlier.size = NA,#outlier.colour = 'black',
    #             outlier.fill = NA) +
    #geom_boxplot(fill='white', width=0.2,
    #             outlier.shape = NA, outlier.size = NA,#outlier.colour = 'black',
    #             outlier.fill = NA) +
    #stat_summary(fun.y=mean, geom="point", shape=23, size=2, color='white', fill='white') +
    #facet_wrap(~cell, nrow=1) +
    #geom_jitter(size=0.1, width=0.2) +
    #ylim(-0.1,3)+
    xlab('') + ylab(expression('Log'['2']*'CPM')) +
    #ggtitle(paste0('Expression of ', gene.symbol)) +
    guides(fill = guide_legend(nrow=1)) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    #theme(plot.title = element_text(hjust = 0.5, face='bold', size=16)) +
    theme(axis.text.y = element_text(size=12,color='black'),
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.spacing.x = unit(0.1, "cm"),
          axis.title = element_text(size=14),
          strip.text = element_text(angle=90, size=12, face='bold'),
          panel.border = element_rect(colour = "black"))
  
  p <- p + geom_jitter(size=0.1, width=0.2)
  
  return(p)
  
}

kmplotFun <- function(meta.data, expr.data, group, gene.id, quan) {
  
  if (group == 'time_to_death') {
    status <- 'os_status'
    x.title <- 'Overall Survival (months)'
  } else if (group == 'time_to_bcr') {
    status <- 'bcr_status'
    x.title <- 'Relapse-free Survival (months)'
  }  else if (group == 'time_to_metastasis') {
    status <- 'metastasis_status'
    x.title <- 'Metastasis-free Survival (months)'
  } 
  
  keep <- which(meta.data[,'sample_type'] %in% c('Primary','Tumor'))
  
  expr <- expr.data[gene.id,keep]
  
  daysToDeath <- meta.data[keep, group]
  vitalStatus <- meta.data[keep, status]
  
  expr.thres <- quantile(expr, quan/100, na.rm = T) # median(expr,na.rm=T) ##############
  expr.group <- expr > expr.thres
  
  dataForKMPlot <- data.frame(daysToDeath,vitalStatus, expr.group)
  
  nH <- sum(expr.group)
  nL <- sum(!expr.group)
  
  sdf <- survdiff(Surv(daysToDeath, vitalStatus) ~ expr.group)
  pValue <- format(pchisq(sdf$chisq, length(sdf$n)-1, 
                          lower.tail = FALSE),digits=4)
  
  pValue <- format(1-pchisq(sdf$chisq, df=1),digits=3)
  
  HR = (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
  upper95 = exp(log(HR) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
  lower95 = exp(log(HR) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
  
  HR <- format(HR, digits = 3)
  upper95 <- format(upper95, digits = 3)
  lower95 <- format(lower95, digits = 3)
  
  label1 <- paste('HR = ', HR, ' (', lower95, '-', upper95, ')', sep='')
  label2 <- paste('P value = ', pValue, sep='')
  
  fit <- survfit(Surv(daysToDeath, vitalStatus) ~ expr.group, data=dataForKMPlot)
  
  lgdXpos <- 1/1.4
  lgdYpos = 0.9
  
  xpos = max(daysToDeath, na.rm=TRUE)/2.2
  ypos1 = 0.95
  
  p <- ggsurvplot(fit, data=dataForKMPlot, pval = paste(label1, '\n', label2), pval.coord = c(xpos, ypos1),
                  pval.size=4,
                  font.main = c(16, 'bold', 'black'), conf.int = FALSE, 
                  #title = project,
                  #legend = c(lgdXpos, lgdYpos), 
                  #color = c('blue', 'green'),
                  palette= c('blue', 'red'),
                  legend.labs = c(paste('Low Expr (N=',nL,')',sep=''), 
                                  paste('High Expr  (N=',nH,')',sep='')),  
                  legend.title='group',
                  xlab = x.title, ylab = 'Survival probability',
                  #xlab = paste(type,'(months)'), ylab = 'Survival probability',
                  font.x = c(16), font.y = c(16), ylim=c(0,1), #16
                  ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                              panel.grid.major = element_blank(),
                                              panel.grid.minor = element_blank(),
                                              #panel.border = element_rect(colour='black'),
                                              panel.border = element_blank(),
                                              panel.background = element_blank(),
                                              legend.text = element_text(size=12),#14
                                              legend.title = element_blank(),
                                              legend.position = 'top',
                                              axis.text = element_text(size=14, color='black'))) #+
  
  return(p)
  
}


DEvolcanoPlotFun <- function(dataForVolcanoPlot, logFcThreshold, adjPvalThreshold) {
  p <- ggplot(dataForVolcanoPlot, aes(x = logFC, y = -log10(adj.P.Val))) +
    #xlim(-2,2) +
    labs(x=expression(bold('Log'['2']*'(Fold Change)')), 
         y=(expression(bold('-Log'['10']*'(FDR)'))), 
         title=NULL) +
    geom_point(aes(color=Significance), alpha=1, size=2) +
    geom_vline(xintercept = c(-logFcThreshold, logFcThreshold),
               color='darkgreen', linetype='dashed') +
    geom_hline(yintercept = -log10(adjPvalThreshold), 
               color='darkgreen',linetype='dashed')+
    #scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
    #scale_y_continuous(expand = c(0.3, 0)) +
    #scale_color_manual(values = c('#4285F4',"gray", '#FBBC05')) +
    scale_color_manual(values = c('DOWN'=google.green,'NS'="gray", 'UP'=google.red)) +
    #facet_wrap(~Comparison, ncol = 2) +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC > logFcThreshold), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='red', segment.color = 'black') +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC < logFcThreshold*-1), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='green3', segment.color = 'black') +
    
    geom_text_repel(data = subset(dataForVolcanoPlot,
                                  Significance == 'UP' & Signature.Count>=3),
                    aes(label = Symbol),
                    segment.alpha = 0.5, #segment.size = 0.5,
                    min.segment.length = 0,
                    size = 4, color='black', segment.color = 'black') +
    geom_text_repel(data = subset(dataForVolcanoPlot,
                                  Significance == 'DOWN' & Signature.Count>=3),
                    aes(label = Symbol),
                    segment.alpha = 0.5, #segment.size = 0.5,
                    #min.segment.length = 5,
                    size = 4, color='black', segment.color = 'black') +  
  
    theme_bw() +
    theme(axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.text = element_text(size=14, face='bold')) +
    theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
  
  return (p)
}



KMvolcanoPlotFun <- function(dataForVolcanoPlot, logHrThreshold, adjPvalThreshold) {
  
  p <- ggplot(dataForVolcanoPlot, aes(x = HR, y = -log10(P))) +
    #xlim(-2,2) +
    labs(x=expression(bold('Log'['2']*'(Hazard Raio)')), 
         y=(expression(bold('-Log'['10']*'(FDR)'))), 
         title=NULL) +
    geom_point(aes(color=Significance), alpha=1, size=2) +
    geom_vline(xintercept = c(-logHrThreshold, logHrThreshold),
               color='darkgreen', linetype='dashed') +
    geom_hline(yintercept = -log10(adjPvalThreshold), 
               color='darkgreen',linetype='dashed')+
    #scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
    #scale_y_continuous(expand = c(0.3, 0)) +
    #scale_color_manual(values = c('#4285F4',"gray", '#FBBC05')) +
    scale_color_manual(values = c('DOWN'=google.green,'NS'="gray", 'UP'=google.red)) +
    #facet_wrap(~Comparison, ncol = 2) +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC > logFcThreshold), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='red', segment.color = 'black') +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC < logFcThreshold*-1), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='green3', segment.color = 'black') +
    
    geom_text_repel(data = subset(dataForVolcanoPlot,
                                  Significance == 'UP' & Signature.Count>=3),
                    aes(label = Symbol),
                    segment.alpha = 0.5, #segment.size = 0.5,
                    min.segment.length = 0,
                    size = 4, color='black', segment.color = 'black') +
    geom_text_repel(data = subset(dataForVolcanoPlot,
                                  Significance == 'DOWN' & Signature.Count>=3),
                    aes(label = Symbol),
                    segment.alpha = 0.5, #segment.size = 0.5,
                    #min.segment.length = 5,
                    size = 4, color='black', segment.color = 'black') +  
    
    theme_bw() +
    theme(axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.text = element_text(size=14, face='bold')) +
    theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
  
  return (p)
}



corrplotFun <- function(dataForCorrPlot) {
  
  dataForCorrPlot$group <- log2(dataForCorrPlot$group+1)
  
  corr <- cor.test(dataForCorrPlot$expr, dataForCorrPlot$group)$estimate
  p <- cor.test(dataForCorrPlot$expr, dataForCorrPlot$group)$p.value

  anno_text <- data.frame(
    label = paste0('corr = ', round(corr,3), '\n',
                   'p = ', ifelse(p >= 0.01,
                                  formatC(p, digits = 2),
                                  formatC(p, format = "e", digits = 2))),
    x     = max(dataForCorrPlot$expr),
    y     = max(dataForCorrPlot$group)
  )
  
  p <- ggplot(data=dataForCorrPlot, aes(x=expr, y=group)) +
    geom_point(size=1, color='darkred') +
    geom_smooth(method='lm') +
    #geom_text(data    = anno_text,
    #          mapping = aes(x = x, y = y, label = label),
    #          size=4.5) +
    #facet_wrap(~platform) +
    labs(x='Expression Level', y='Preoperative PSA') +
    theme_bw() +
    theme(axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = 'none')+
    theme(axis.text = element_text(size=14),
          axis.title = element_text(size=16),
          strip.text.x = element_text(size=14, face='bold'))
  
  return (p)
  
}


pieplotFun <- function(dataForPiePlot) {
  
  p <- ggplot(dataForPiePlot, aes(x = "", y = num, fill = sam)) +
    geom_bar(width = 1, stat = "identity", color = "white", size=0.5) +
    scale_fill_manual(values = c(google.yellow, google.blue, google.red, google.green)) +
    coord_polar("y", start = 0) + 
    geom_text(aes(label = num), position = position_stack(vjust = 0.5), size=4.5) +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.position = 'right',
          legend.text = element_text(size=12)) +
    theme(plot.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"))
  
  return (p)
}

barplotFun <- function(dataForBarPlot) {
  
  p <- ggplot(data=dataForBarPlot, mapping=aes(x=group, y=num, fill=google.blue)) +
    geom_bar(stat='identity') +
    scale_x_discrete(limits=dataForBarPlot$group) +
    labs(x='', y='Number of samples') + 
    scale_fill_manual(values=c(google.blue)) +
    theme_bw()+theme(axis.line = element_line(colour = "black"),
                     #panel.grid.major = element_blank(),
                     #panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank()) +
    theme(axis.text=element_text(size=12, color='black'),
          axis.text.x = element_text(angle=45, hjust=1),
          axis.title=element_text(size=14)) +
    theme(legend.text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = 'none') +
    theme(plot.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"))
  
  return (p)
}


histogramFun <- function(dataForHistogram) {

  p <- ggplot(data=dataForHistogram, aes(log2(preop_psa+1), fill=google.blue)) + 
    geom_histogram() + 
    scale_fill_manual(values=c(google.blue)) +
    labs(x=expression('Log'[2]*'(Preoperative PSA + 1)'), y='Count') +
    theme_bw()+theme(axis.line = element_line(colour = "black"),
                     #panel.grid.major = element_blank(),
                     #panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank()) +
    theme(axis.text=element_text(size=12, color='black'),
          axis.title=element_text(size=14)) +
    theme(legend.text = element_text(size=12),
          legend.title = element_blank(),
          legend.position = 'none') +
    theme(plot.margin =  margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, unit = "cm"))
    
  return (p)
  
  
}


ExprBoxPlotFun <- function(dataForBoxPlot, colors) {
  p <- ggplot(dataForBoxPlot, aes(x=group, y=expr)) + 
    scale_fill_manual(values=colors) +
    #scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
    geom_boxplot(aes(fill=group), width=0.8,
                outlier.shape = NA, outlier.size = NA,#outlier.colour = 'black',
                outlier.fill = NA) +
    #geom_boxplot(fill='white', width=0.2,
    #             outlier.shape = NA, outlier.size = NA,#outlier.colour = 'black',
    #             outlier.fill = NA) +
    #stat_summary(fun.y=mean, geom="point", shape=23, size=2, color='white', fill='white') +
    #facet_wrap(~dataset, nrow=1, scales = 'free') +
    #geom_jitter(size=0.1, width=0.2) +
    #ylim(-0.1,3)+
    xlab('') + ylab(expression(bold('Log'["2"]*'(Expression Level)'))) + 
    #ggtitle(paste0('Expression of ', gene.symbol)) +
    #guides(fill = guide_legend(nrow=1)) +
    theme_bw() +
    theme(legend.position = 'none') +
    #theme(plot.title = element_text(hjust = 0.5, face='bold', size=16)) +
    theme(axis.text.y = element_text(size=14,color='black', face = 'bold'),
          axis.text.x = element_text(size=14,color='black', face = 'bold', angle = 45, hjust = 1),
          axis.line = element_line(colour = 'black'),
          legend.title = element_blank(),
          legend.text = element_text(size=12),
          legend.spacing.x = unit(0.1, "cm"),
          axis.title = element_text(size=16),
          strip.text = element_text(size=16, face='bold'),
          panel.border = element_blank()) +
    theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 1, unit = "cm"))
  
  #p <- p + geom_jitter(size=0.1, width=0.2)
  
  return(p)
  
}


transcriptomeKMForestplotFunT <- function(dataForForestPlot, sort.var='HR') {
  
  dataForForestPlot$HR <- round(dataForForestPlot$HR,2)
  dataForForestPlot$Lower95 <- round(dataForForestPlot$Lower95,2)
  dataForForestPlot$Upper95 <- round(dataForForestPlot$Upper95,2)
  
  if (sort.var=='HR') {
    o <- order(dataForForestPlot$HR, decreasing = F)
  } else if (sort.var=='P') {
    o <- order(dataForForestPlot$P.Value, decreasing = T)
  }
  
  dataForForestPlot <- dataForForestPlot[o,]
  
  
  dataForForestPlot$P.Value <- ifelse(dataForForestPlot$P.Value >= 0.01, formatC(dataForForestPlot$P.Value, digits = 2),
                                      formatC(dataForForestPlot$P.Value, format = "e", digits = 2))
  
  # rangeb <- range(dataForForestPlot$Lower95, dataForForestPlot$Upper95, na.rm = TRUE)
  # 
  # rangeplot <- rangeb
  # rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # #rangeplot[2] <- rangeplot[2] #+ .15 * diff(rangeb)
  
  rangeb <- c(0,max(dataForForestPlot$Upper95))
  
  rangeplot <- rangeb
  rangeplot[1] <- rangeb[2]*-1*0.8
  rangeplot[2] <- rangeplot[2] + 0.01 * diff(rangeb)
  
  
  
  cpositions=c(0.08, 0.18, 0.28, 0.4)
  
  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_patients <- rangeplot[1] +  cpositions[2] * width
  y_nlevel <- rangeplot[1]  +  cpositions[3] * width
  y_cistring <- rangeplot[1]  +  cpositions[4] * width
  y_stars <- rangeb[2]
  
  p <- ggplot(dataForForestPlot, aes(x=seq_along(Dataset), y=HR)) +
    
    geom_rect(aes(xmin = seq_along(Dataset) - 0.5, xmax = seq_along(Dataset) + 0.5,
                  ymin = rangeplot[1], ymax = rangeplot[2],
                  fill = ordered(seq_along(Dataset) %% 2 + 1))) +
    scale_fill_manual(values = c("#00000033", "#FFFFFF33"), guide = "none") +
    #xlim(c(0.5, length(dataForForestPlot$Project)+1.5)) +
    scale_y_continuous(breaks = sort(unique(c(seq(0, rangeplot[2], 4), 1))), labels = sort(unique(c(seq(0, rangeplot[2], 4), 1)))) +
    scale_x_continuous(limits = c(0, length(dataForForestPlot$Dataset)+2), expand = c(0,0)) +
    #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
    #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
    geom_errorbar(aes(ymin=Lower95, ymax=Upper95),width=0.3, size=0.8, color='black')+ 
    geom_point(color=google.red, size=3, shape=15) + #shape=15, facet_grid(.~type) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Dataset), y=y_variable, label=Dataset, group=NULL),
              size=5) +#, fontface='bold', hjust=0
    # geom_text(data =dataForForestPlot, aes(x=seq_along(Project), y=y_nlevel, label=paste0(HR, ' (', Lower95, '-', Upper95, ')'), group=NULL),
    #           size=4) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Dataset), y=y_patients, label=N, group=NULL),
              size=5) +#, fontface='bold', hjust=0
    geom_text(data =dataForForestPlot, aes(x=seq_along(Dataset), y=y_nlevel, label=HR, group=NULL),
              size=5, vjust=-0.2) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Dataset), y=y_nlevel, label=paste0('(', Lower95, '-', Upper95, ')'), group=NULL),
              size=5, vjust=1.2) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Dataset), y=y_cistring, label=P.Value, group=NULL),
              size=5) +
    
    #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-6.7,-7.3,-2.6,2.6,6.1,5.9), label=P, group=NULL),
    #          size=4.4) +
    geom_hline(yintercept = 1, linetype=2, color='black') +
    geom_hline(yintercept = 0, linetype=1, color='grey') +
    #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
    #          size=4.4) +
    #scale_y_continuous(trans = 'log10',
    #                   breaks = c(0, 1, 2.5,50,250,7500),
    #                   labels = c(0, 1, 2.5,50,250,7500)) +
    coord_flip()+
    #ylim(0,0.05) +
    xlab('')+ylab('Hazard Ratio') +
    #ggtitle(paste0('ROC Analysis of ', mir.name, ' in TCGA')) +
    #xlim(0,100) +
    theme_bw()+
    #theme_set(theme_minimal()) #
    theme(legend.title = element_blank(),
          legend.text = element_text(size=14),
          legend.position = 'right') +
    #theme(plot.title = element_text(color='black', size=18, face = 'bold', hjust = 0.5)) +
    theme(axis.title.x=element_text(size=16, face = 'bold', hjust = 0.71),
          axis.title.y=element_blank(),
          #axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.text = element_text(color='black', size=14, face = 'bold'),
          axis.text.y = element_blank(),
          #axis.text.x = element_text(angle = 0, hjust=0.5),
          strip.text = element_text(size=14)) +
    theme(#axis.line = element_line(colour = "black"),
      axis.line.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) +
    annotate(geom = 'text', x = length(dataForForestPlot$Dataset)+1.2, y=c(y_variable, y_patients, y_nlevel, y_cistring),
             label=c('Dataset','N','Hazard Ratio\n(95% CI)','P Value'), size=5, fontface='bold')
  
  p
  
}



signatureKMForestplotFunT <- function(dataForForestPlot) {
  
  dataForForestPlot$HR <- round(dataForForestPlot$HR,2)
  dataForForestPlot$Lower95 <- round(dataForForestPlot$Lower95,2)
  dataForForestPlot$Upper95 <- round(dataForForestPlot$Upper95,2)
  dataForForestPlot$P.Value <- dataForForestPlot$P
  
  o <- order(dataForForestPlot$HR, decreasing = F)
  dataForForestPlot <- dataForForestPlot[o,]
  
  
  dataForForestPlot$P.Value <- ifelse(dataForForestPlot$P.Value >= 0.01, formatC(dataForForestPlot$P.Value, digits = 2),
                                      formatC(dataForForestPlot$P.Value, format = "e", digits = 2))
  
  #dataForForestPlot$Gene <- factor(dataForForestPlot$Gene, levels=dataForForestPlot$Gene)
  
  # rangeb <- range(dataForForestPlot$Lower95, dataForForestPlot$Upper95, na.rm = TRUE)
  # 
  # rangeplot <- rangeb
  # rangeplot[1] <- rangeplot[1] - diff(rangeb)
  # #rangeplot[2] <- rangeplot[2] #+ .15 * diff(rangeb)
  
  rangeb <- c(0,max(dataForForestPlot$Upper95))
  
  rangeplot <- rangeb
  rangeplot[1] <- rangeb[2]*-1*0.8
  rangeplot[2] <- rangeplot[2] + 0.01 * diff(rangeb)
  
  
  
  cpositions=c(0.05, 0.13, 0.24, 0.37)
  
  width <- diff(rangeplot)
  # y-coordinates for labels:
  y_variable <- rangeplot[1] +  cpositions[1] * width
  y_patients <- rangeplot[1] +  cpositions[2] * width
  y_nlevel <- rangeplot[1]  +  cpositions[3] * width
  y_cistring <- rangeplot[1]  +  cpositions[4] * width
  y_stars <- rangeb[2]
  
  p <- ggplot(dataForForestPlot, aes(x=seq_along(Gene), y=HR)) +
    
    geom_rect(aes(xmin = seq_along(Gene) - 0.5, xmax = seq_along(Gene) + 0.5,
                  ymin = rangeplot[1], ymax = rangeplot[2],
                  fill = ordered(seq_along(Gene) %% 2 + 1))) +
    scale_fill_manual(values = c("#00000033", "#FFFFFF33"), guide = "none") +
    #xlim(c(0.5, length(dataForForestPlot$Project)+1.5)) +
    scale_y_continuous(breaks = sort(unique(c(seq(0, rangeplot[2], 4), 1))), labels = sort(unique(c(seq(0, rangeplot[2], 4), 1)))) +
    scale_x_continuous(limits = c(0, length(dataForForestPlot$Gene)+2), expand = c(0,0)) +
    #geom_segment(aes(y=dataset, x=lower95.coxph, xend=upper95.coxph, yend=dataset), color='black', size=1) +
    #geom_segment(aes(y=6:1-0.1, x=lower95.coxph, xend=lower95.coxph, yend=6:!+0.1), color='black', size=1) +
    geom_errorbar(aes(ymin=Lower95, ymax=Upper95),width=0.3, size=0.8, color='black')+ 
    geom_point(color=google.red, size=3, shape=15) + #shape=15, facet_grid(.~type) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Gene), y=y_variable, label=Gene, group=NULL),
              size=5) +#, fontface='bold', hjust=0
    # geom_text(data =dataForForestPlot, aes(x=seq_along(Project), y=y_nlevel, label=paste0(HR, ' (', Lower95, '-', Upper95, ')'), group=NULL),
    #           size=4) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Gene), y=y_patients, label=N, group=NULL),
              size=5) +#, fontface='bold', hjust=0
    geom_text(data =dataForForestPlot, aes(x=seq_along(Gene), y=y_nlevel, label=HR, group=NULL),
              size=5, vjust=-0.2) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Gene), y=y_nlevel, label=paste0('(', Lower95, '-', Upper95, ')'), group=NULL),
              size=5, vjust=1.2) +
    geom_text(data =dataForForestPlot, aes(x=seq_along(Gene), y=y_cistring, label=P.Value, group=NULL),
              size=5) +
    
    #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(-6.7,-7.3,-2.6,2.6,6.1,5.9), label=P, group=NULL),
    #          size=4.4) +
    geom_hline(yintercept = 1, linetype=2, color='black') +
    geom_hline(yintercept = 0, linetype=1, color='grey') +
    #geom_text(data =dataForForestPlot, aes(x=dataset, y=c(0.35,0.5,0.2,0.45,0.95,0.55), label=p.coxph, group=NULL),
    #          size=4.4) +
    #scale_y_continuous(trans = 'log10',
    #                   breaks = c(0, 1, 2.5,50,250,7500),
    #                   labels = c(0, 1, 2.5,50,250,7500)) +
    coord_flip()+
    #ylim(0,0.05) +
    xlab('')+ylab('Hazard Ratio') +
    #ggtitle(paste0('ROC Analysis of ', mir.name, ' in TCGA')) +
    #xlim(0,100) +
    theme_bw()+
    #theme_set(theme_minimal()) #
    theme(legend.title = element_blank(),
          legend.text = element_text(size=14),
          legend.position = 'right') +
    #theme(plot.title = element_text(color='black', size=18, face = 'bold', hjust = 0.5)) +
    theme(axis.title.x=element_text(size=16, face = 'bold', hjust = 0.71),
          axis.title.y=element_blank(),
          #axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.text = element_text(color='black', size=14, face = 'bold'),
          axis.text.y = element_blank(),
          #axis.text.x = element_text(angle = 0, hjust=0.5),
          strip.text = element_text(size=14)) +
    theme(#axis.line = element_line(colour = "black"),
      axis.line.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()) +
    annotate(geom = 'text', x = length(dataForForestPlot$Gene)+1.2, y=c(y_variable, y_patients, y_nlevel, y_cistring),
             label=c('Gene','N','Hazard Ratio\n(95% CI)','P Value'), size=5, fontface='bold')
  
  p
  
}



scTSNEPlotFun <- function(dataForTSNEPlot, expr=TRUE) {
  
  TSNELabelPos = dataForTSNEPlot %>% group_by(cell.type) %>% select(tSNE.1,tSNE.2) %>% summarize_all(median)

  if (expr==TRUE) {
    p <- ggplot(dataForTSNEPlot,aes(x=tSNE.1,y=tSNE.2,color=expr)) +
      geom_point(size=0.5)+
      scale_color_gradient(low="lightgrey", high="blue") +
      labs(color = 'Normalized\nExpression', x='tSNE_1', y='tSNE_2') +
      theme_bw()+theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_rect(colour='white'),
                       panel.background = element_blank()) +
      theme(axis.text=element_text(size=14, color='black', face = 'bold'),
            axis.title=element_text(size=16, face = 'bold')) +
      theme(legend.text = element_text(size=10, face = 'bold'),
            legend.title = element_text(size=10, face = 'bold'),
            legend.position = 'right')
    
  } else {
    
    ### 
    p <- ggplot(dataForTSNEPlot) + geom_point(aes(x=tSNE.1, y=tSNE.2, 
                                                  color=cell.type), size=0.3) + 
      #scale_fill_manual(values = colors) +
      #scale_color_manual(values=colors) +
      #scale_color_hue() +
      labs(x='tSNE_1', y='tSNE_2') +
      # geom_label_repel(aes(x = tSNE.1, y= tSNE.2, label = cell.type), 
      #                  data=pos) +
      annotate("text", label=TSNELabelPos$cell.type, 
               x=TSNELabelPos$tSNE.1, y=TSNELabelPos$tSNE.2, colour="black", 
               fontface=2, size=3.4) +
      guides(colour = guide_legend(override.aes = list(size=2),
                                   ncol=1)) +
      theme_bw()+theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_rect(colour='white'),
                       panel.background = element_blank()) +
      theme(axis.text=element_text(size=14, color='black', face = 'bold'),
            axis.title=element_text(size=16, face = 'bold')) +
      theme(legend.text = element_text(size=12, face = 'bold'),
            legend.title = element_blank(),
            legend.position = 'none')
    
    
  }
  
  return (p)
  
  
}


scUMAPPlotFun <- function(dataForUMAPPlot, expr=TRUE) {
  
  UMAPLabelPos = dataForUMAPPlot %>% group_by(cell.type) %>% select(UMAP.1,UMAP.2) %>% summarize_all(median)
  
  if (expr==TRUE) {
    p <- ggplot(dataForUMAPPlot,aes(x=UMAP.1,y=UMAP.2,color=expr)) +
      geom_point(size=0.3)+
      scale_color_gradient(low="lightgrey", high="blue") +
      labs(color = 'Normalized\nExpression', x='UMAP_1', y='UMAP_2') +
      theme_bw()+theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_rect(colour='white'),
                       panel.background = element_blank()) +
      theme(axis.text=element_text(size=14, color='black', face = 'bold'),
            axis.title=element_text(size=16, face = 'bold')) +
      theme(legend.text = element_text(size=10, face = 'bold'),
            legend.title = element_text(size=10, face = 'bold'),
            legend.position = 'right')
    
  } else {
    
    p <- ggplot(dataForUMAPPlot) + geom_point(aes(x=UMAP.1, y=UMAP.2, 
                                                  color=cell.type), size=0.3) + 
      #scale_fill_manual(values = colors) +
      #scale_color_manual(values=colors) +
      #scale_color_hue() +
      labs(x='UMAP_1', y='UMAP_2') +
      # geom_label_repel(aes(x = tSNE.1, y= tSNE.2, label = cell.type), 
      #                  data=pos) +
      annotate("text", label=UMAPLabelPos$cell.type, 
               x=UMAPLabelPos$UMAP.1, y=UMAPLabelPos$UMAP.2, colour="black", 
               fontface=2, size=3.4) +
      guides(colour = guide_legend(override.aes = list(size=2),
                                   ncol=1)) +
      theme_bw()+theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.border = element_rect(colour='white'),
                       panel.background = element_blank()) +
      theme(axis.text=element_text(size=14, color='black', face = 'bold'),
            axis.title=element_text(size=16, face = 'bold')) +
      theme(legend.text = element_text(size=12, face = 'bold'),
            legend.title = element_blank(),
            legend.position = 'none')
    
  }
  
  return (p)
  
}


scBubblePlotFun <- function(dataForBubblePlot) {
  
  # library(RColorBrewer)
  # 
  # cols = brewer.pal(4, "Reds")
  # cols = colorRampPalette(cols)(10)
  # col_fun = colorRampPalette(rev(c(cols[10],cols[1])), space = "Lab")(2)
  
  # exprMax <- max(dataForBubblePlot$mean.all)
  # exprMax
  
  p <- ggplot(dataForBubblePlot, mapping=aes(x=cell.type, y=gene, #y=-log10(Benjamini), #y=Fold.Enrichment
                                             color=mean.all,size=percent.expressed)) +
    geom_point()+ #coord_flip() +
    scale_x_discrete(limits=unique(dataForBubblePlot$cell.type)) +
    scale_y_discrete(limits=rev(unique(dataForBubblePlot$gene))) +
    #scale_x_discrete(limits=Order)+
    lims(size = c(0,100)) +
    scale_colour_gradientn(#limits=c(0,2.7), #exprMax+0.07
      colors= c(col_fun[1],col_fun[2])) + #col_fun
    #facet_wrap(~Comparison) +
    #facet_grid(Regulation~Comparison) + # scales=free
    xlab('')+ylab('') +
    guides(size = guide_legend(order=2, title='Percent\nExpressed'),
           colour = guide_colourbar(order=1, title = 'Average\nExpression')) + #'P Value\n(Benjamini)'))
    theme_bw()+theme(axis.line = element_line(colour = "black"),
                     panel.grid.minor = element_blank(),
                     #panel.border = element_rect(colour='black'),
                     panel.border = element_blank(),
                     panel.background = element_blank()) +
    ggtitle("") + theme(plot.title = element_text(hjust = 0.5, size=20)) +
    theme(axis.text=element_text(size=14, color='black', face='bold'),
          axis.text.x =element_text(size=14, color='black', angle=45, hjust = 1),
          axis.title=element_text(size=15)) +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = 'bottom') +
    theme(strip.text = element_text(size = 14),
          legend.key.size = unit(0.8,'cm'))
  
  return(p)
  
}

scViolinPlotFun <- function(dataForViolinPlot) {
  
  p <- ggplot(data=dataForViolinPlot, aes(x=cell.type, y=expr)) +
    geom_violin(aes(fill=cell.type, color=cell.type), lwd=0.1, #size=0.5,
                width=0.8) +
    #coord_flip() +
    # scale_y_continuous(position = "right") +
    # scale_x_discrete(position = 'bottom') +
    #geom_jitter(size=0.01, width = 0.1) +
    #facet_wrap(~gene, nrow=1, strip.position = 'bottom') +
    labs(x='', y=expression(bold('Log'['2']*'(Normalized Value)'))) +
    theme_bw()+
    theme(legend.title = element_blank(),
          legend.position = 'none') +
    #theme_set(theme_minimal()) #
    theme(axis.title=element_text(color = 'black', size = 16, face = 'bold'),
          axis.text = element_text(color='black', size=14, face = 'bold'),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(angle = 45, size=12, 
                                    hjust=0.3),
          strip.background = element_blank()) +
    theme(axis.line = element_line(colour = "black"),
          panel.border = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(panel.spacing = unit(0, "lines")) #+
    #theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
  
  return (p)
}



KMPlotFun <- function(dataForKMPlot, sep='median', type='rfs', 
                      score.type='expr', x.adjust=0, dt) {
  
  if (sep=='1stQu') {
    risk.threshold <- as.numeric(summary(dataForKMPlot$expr)[2])
  } else if (sep=='median') {
    risk.threshold <- as.numeric(summary(dataForKMPlot$expr)[3])
  } else if (sep=='mean') {
    risk.threshold <- as.numeric(summary(dataForKMPlot$expr)[4])
  } else if (sep=='3rdQu') {
    risk.threshold <- as.numeric(summary(dataForKMPlot$expr)[5])
  }
  
  dataForKMPlot$risk.group <- dataForKMPlot$expr > risk.threshold
  
  if (type == 'os') {
    x.title <- 'Overall Survival (months)'
  } else if (type == 'rfs') {
    x.title <- 'Relapse-free Survival (months)'
  }  else if (type == 'mfs') {
    x.title <- 'Metastasis-free Survival (months)'
  }
  
  n.high <- sum(dataForKMPlot$risk.group, na.rm=T)
  n.low <- sum(!dataForKMPlot$risk.group, na.rm=T)
  
  sdf <- survdiff(Surv(dataForKMPlot$time.to.bcr, dataForKMPlot$bcr.status) ~ dataForKMPlot$risk.group)
  p.val <- pchisq(sdf$chisq, length(sdf$n)-1, lower.tail = FALSE)
  #p.val = 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
  
  hr = (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
  upper95 = exp(log(hr) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
  lower95 = exp(log(hr) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
  
  hr <- format(hr, digits = 2, nsmall=2)
  upper95 <- format(upper95, digits = 2, nsmall=2)
  lower95 <- format(lower95, digits = 2, nsmall=2)
  
  p.val <- ifelse(p.val >= 0.01, formatC(p.val, digits = 2), 
                  formatC(p.val, format = "e", digits = 2))
  
  label.hr <- paste('HR = ', hr, ' (', lower95, ' - ', upper95, ')', sep='')
  label.p <- paste('P Value = ', p.val, sep='')
  
  fit <- survfit(Surv(time.to.bcr, bcr.status) ~ risk.group, data=dataForKMPlot)
  
  lgd.xpos <- 0.32+x.adjust
  lgd.ypos = 0.22
  
  p.xpos = max(dataForKMPlot$time.to.bcr, na.rm=TRUE)/50
  p.ypos = 0.05
  
  #title <- 'PFR10YR'
  #type <- 'Relapse-free Survival'
  
  if (score.type=='expr') {
    legend.labs <- c(paste('Low Expression (N=',n.low,')',sep=''), 
                     paste('High Expression (N=',n.high,')',sep=''))
  } else {
    legend.labs <- c(paste('Low Risk (N=',n.low,')',sep=''), 
                     paste('High Risk (N=',n.high,')',sep=''))
  }
  
  
  
  plt <- ggsurvplot(fit, data=dataForKMPlot, pval = paste0(label.hr, '\n', label.p), pval.coord = c(p.xpos, p.ypos),
                    pval.size=4.2,
                    font.main = c(16, 'bold', 'black'), conf.int = FALSE, 
                    title = dt,
                    legend = c(lgd.xpos, lgd.ypos), 
                    #color = c('blue', 'green'),
                    palette= c(google.blue, google.red),
                    legend.labs = legend.labs,  
                    legend.title='', # Group
                    xlab = x.title, ylab = 'Survival Probability',
                    font.x = c(14), font.y = c(14), ylim=c(0,1), #20
                    ggtheme = theme_bw()+ theme(axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                #panel.border = element_rect(colour='black'),
                                                panel.border = element_blank(),
                                                panel.background = element_blank(),
                                                legend.text = element_text(size=12),#16
                                                legend.title = element_blank(), # 16
                                                legend.key = element_blank(),
                                                #legend.box.background = element_blank(),
                                                legend.background = element_blank(),
                                                plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
                                                axis.title = element_text(size = 14, face = 'bold'),
                                                axis.text = element_text(size=12, color='black', face = 'bold'))) # 18
  
  return(plt[[1]])
  
}



piePlotlyFun <- function(dataForPiePlot) {
  
  o <- order(dataForPiePlot$num, decreasing = T)
  dataForPiePlot <- dataForPiePlot[o,]
  
  p <- plot_ly(dataForPiePlot, labels = ~sam, values = ~num, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+value',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste0(sam, '\n', num),
               marker = list(colors = pie.colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) #, height=400, width = 400
  
  p <- p %>% layout(margin = list(l=20, r=20, b=10, t=20))
  
  # p <- p %>% layout(legend = list(orientation = 'h'))
  # p <- p %>% layout(legend = list(x=100, y=0.5))
  
  p
  
}


EnrichmentBarPlotFun <- function(dataForBarPlot) {
  
  p <- ggplot(data=dataForBarPlot, mapping=aes(x=Description, y=-log10(BH.Adj.P), fill='chocolate')) + 
    geom_bar(stat='identity') +
    scale_x_discrete(limits=rev(dataForBarPlot$Description)) +
    ylim(0, max(-log10(dataForBarPlot$BH.Adj.P))) +
    labs(x='', y=expression(bold('-Log'["10"]*'(FDR)'))) + coord_flip() +
    #scale_fill_hue(name='',breaks=kegg$Regulation,
    #               labels=kegg$Regulation) +
    #scale_fill_manual(values = c('orange','dodgerblue')) +
    scale_fill_manual(values=c('chocolate'))+#,breaks=kegg$Reg) +
    #geom_text(aes(label=Count), hjust=1, size=4.5) +
    theme_bw()+theme(axis.line = element_line(colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(colour='white'),
                     panel.background = element_blank()) +
    theme(axis.text=element_text(size=14, color='black', face = 'bold'),
          axis.title=element_text(size=16, face = 'bold')) +
    theme(legend.text = element_text(size=14),
          legend.title = element_blank(),
          legend.position = 'none')
  
  return (p)
  
  
}


EnrichmentBubblePlotFun <- function(dataForBubblePlot) {
  
  p <- ggplot(dataForBubblePlot, mapping=aes(x=Description, y=Fold.Enrichment, #y=-log10(Benjamini), #y=Fold.Enrichment
                                             color=BH.Adj.P,size=Count)) +
    geom_point()+ coord_flip() +
    scale_x_discrete(limits=rev(unique(dataForBubblePlot$Description))) +
    #scale_x_discrete(limits=Order)+
    scale_colour_gradientn(limits=c(0,0.05),
                           colors= c("red","yellow","green")) + #
    #facet_wrap(~Comparison) +
    #facet_grid(Regulation~Comparison) + # scales=free
    xlab('')+ylab('Fold Enrichment') + #ggtitle("") + 
    guides(shape = guide_legend(order=1),
           colour = guide_colourbar(order=2, title = 'FDR')) + #'P Value\n(Benjamini)'))
    theme_bw()+theme(axis.line = element_line(colour = "black"),
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(colour='black'),
                     panel.background = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5, size=20)) +
    theme(axis.text=element_text(size=14, color='black', face = 'bold'),
          axis.text.x =element_text(size=14, color='black', face = 'bold', angle=0, hjust=0.5),
          axis.title=element_text(size=16, face = 'bold')) +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) +
    theme(#strip.text = element_text(size = 14),
      legend.key.size = unit(0.8,'cm'))
  
  
  return (p)
  
}



histPlotlyFun <- function(dataForHistogram) {
  
  p <- plot_ly(x = ~dataForHistogram$x, type='histogram',
               marker = list(color = pie.colors[1],
                             line = list(color = '#FFFFFF', width = 0.5))) %>%
    layout(yaxis = list(title = "Number of Patients",
                        font = list(size=14, color='black'),
                        zeroline = FALSE),
           xaxis = list(title = "Preop PSA",
                        font = list(size=14, color='black'),
                        zeroline = FALSE))
  
  return(p)
  
}


barPlotlyFun <- function(dataForBarPlot) {
  
  p <- plot_ly(x = dataForBarPlot$Gleason,
               y = dataForBarPlot$Count,
               type='bar',
               marker = list(color = pie.colors[1],
                             line = list(color = '#FFFFFF', width = 0.5))) %>%
    layout(yaxis = list(title = "Number of Patients",
                        font = list(size=14, color='black'),
                        zeroline = FALSE),
           xaxis = list(title = "Gleason Score",
                        font = list(size=14, color='black'),
                        zeroline = FALSE))
  
  return(p)
  
}



volcanoPlotFun <- function(dataForVolcanoPlot, logFcThreshold, adjPvalThreshold) {
  p <- ggplot(dataForVolcanoPlot, aes(x = logFC, y = -log10(adj.P.Val))) +
    #xlim(-2,2) +
    labs(x=expression(bold('Log'['2']*'(Fold Change)')), 
         y=expression(bold('-Log'['10']*'(FDR)')), 
         title=NULL) +
    geom_point(aes(color=Significance), alpha=1, size=2) +
    geom_vline(xintercept = c(-logFcThreshold, logFcThreshold),
               color='darkgreen', linetype='dashed') +
    geom_hline(yintercept = -log10(adjPvalThreshold), 
               color='darkgreen',linetype='dashed')+
    #scale_x_continuous(breaks=c(-4,-2,0,2,4,6,8,10)) +
    #scale_y_continuous(expand = c(0.3, 0)) +
    #scale_color_manual(values = c('#4285F4',"gray", '#FBBC05')) +
    scale_color_manual(values = c(google.green,"gray", google.red)) +
    #facet_wrap(~Comparison, ncol = 2) +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC > logFcThreshold), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='red', segment.color = 'black') +
    #geom_text_repel(data = subset(dataForVolcanoPlot, 
    #                              adj.P.Val < adjPvalThreshold & logFC < logFcThreshold*-1), 
    #                segment.alpha = 0.4, aes(label = Symbol), 
    #                size = 3.5, color='green3', segment.color = 'black') +
    
    theme_bw() +
    theme(axis.line = element_blank(),
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position="none") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16),
          strip.text = element_text(size=14, face='bold')) +
    theme(plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm"))
  
  return (p)
}





survModelFun <- function(genes, model, training.geno, training.pheno) {
  
  # filter <- which(apply(training.geno, 2, function(v) sum(v==mean(v))==length(v)))
  # #filter <- which(apply(training.geno, 2, sd)==0)
  # 
  # if (length(filter)>0) {
  #   training.geno <- training.geno[,-filter]
  # }
  
  training.geno <- as.matrix(t(training.geno))
  #training.geno <- scale(training.geno)
  
  training.pheno$time_to_bcr[training.pheno$time_to_bcr<=0] <- 0.01
  
  filter <- which(is.na(training.pheno$bcr_status) | is.na(training.pheno$time_to_bcr))
  
  if (length(filter)>0) {
    
    training.geno <- training.geno[-filter,]
    training.pheno <- training.pheno[-filter,]
  }
  
  genes <- colnames(training.geno)
  
  training.bcr.time <- training.pheno$time_to_bcr
  training.bcr.status <-training.pheno$bcr_status
  
  training.surv.data <- data.frame(training.geno, bcr.time=training.bcr.time, bcr.status=training.bcr.status)
  
  if (model=='CoxPH') {
    
    multi.var <- paste0(genes, collapse = '+')
    
    fml <- as.formula(paste0('Surv(bcr.time, bcr.status) ~ ', multi.var))
    
    coxtest <- coxph(fml, data = training.surv.data)
    summcph <- summary(coxtest)
    coeffs <- as.numeric(summcph$coefficients[,1])
    
  } else if (model=='CoxLasso') {
    
    alpha <- 1
    
    set.seed(777)
    cv.fit <- cv.glmnet(training.geno, Surv(training.bcr.time, training.bcr.status), family="cox", maxit = 1000,
                        alpha=alpha)
    
    coeffs <- coef(cv.fit, s = cv.fit$lambda.min)
    coeffs <- as.numeric(coeffs)
    
  } else if (model=='CoxRidge') {
    
    alpha <- 0
    
    set.seed(777)
    cv.fit <- cv.glmnet(training.geno, Surv(training.bcr.time, training.bcr.status), family="cox", maxit = 1000,
                        alpha=alpha)
    
    coeffs <- coef(cv.fit, s = cv.fit$lambda.min)
    coeffs <- as.numeric(coeffs)
    
  } #else if (model=='plsRcox') {
  #   
  #   ncomps <- 2
  # 
  #   pls.fit <- plsRcox(training.geno,time=training.bcr.time,event=training.bcr.status, nt=ncomps)
  #   
  # }
  
  coeffs <- data.frame(Gene=genes, Symbol=NA, Coefficients=coeffs, stringsAsFactors = F)
  
  return (coeffs)
  
}


# setwd('~/Publications/PCaDB/')
# proj <- 'TCGA-PRAD'
# meta.data <- readRDS('data/PCaDB_Metadata.RDS')
# expr.data <- readRDS('data/PCaDB_Expression.RDS')
# 
# expr <- expr.data[[proj]]
# meta <- meta.data[[proj]]
# 
# pca.signatures <- readRDS('data/PCaDB_Signatures.RDS')
# genes <- pca.signatures$Ensembl.ID[which(pca.signatures$Signature=='Erho')]
# 
# keep <- which(genes %in% rownames(expr))
# samples <- which(meta[,'sample_type'] %in% c('Primary','Tumor'))
# 
# training.geno <- expr[keep, samples]
# training.pheno <- meta[samples,]
# 
# survModelFun(model = 'CoxPH', 
#              training.geno = training.geno, 
#              training.pheno = training.pheno)


readRDSAs <- function(f, var, fn=NULL, ...) {
  req(file.exists(f))
  f.timestamp <- as.numeric(file.mtime(f))
  var.ts <- paste0(var,'_','ts')
  
  if(!exists(var) || !exists(var.ts) || get(var.ts) < f.timestamp) {
    cat(file=stderr(), sprintf('loading %s as variable %s', f, var), "\n")
    assign(var, readRDS(f), envir = .GlobalEnv)		
    assign(var.ts, f.timestamp, envir = .GlobalEnv)
    if(!is.null(fn)) {
      fn(...)
    }
  }
}