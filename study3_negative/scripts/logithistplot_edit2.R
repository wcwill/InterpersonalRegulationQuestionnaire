logithistplot <- function(data,breaks="Sturges",se=TRUE) {
  require(ggplot2);
  col_names=names(data)
  
  
  # get min and max axis values
  min_x <- min(data[,1])
  max_x <- max(data[,1])
  
  # get bin numbers
  hist_both <- hist(data[,1],plot=FALSE,breaks=breaks)
  hist0 <- hist(data[data[,2]==0,1],breaks=hist_both$breaks,plot=FALSE)
  hist1 <- hist(data[data[,2]==1,1],breaks=hist_both$breaks,plot=FALSE)
  max_count=max(c(hist0$counts,hist1$counts))
  
  # create plots
  a <- ggplot(data, aes_string(x = col_names[1], y = col_names[2])) +
    theme_bw(base_size=18) +
    # geom_smooth(method = "gam", family = "binomial", se = se,
                # aes(colour='Spline',fill='Spline'), size=1.5,formula=y~s(x,bs="cr"))+
    geom_smooth(method = "glm", method.args=list(family="binomial"), se = se,
                aes(colour='Logit',fill='Logit'), size=1.5) +
    
    scale_y_continuous(limits=c(0,1), breaks=c(0,1)) +
    scale_x_continuous(limits=c(min_x,max_x)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor=element_blank(),
          panel.background = element_blank()) +
    guides(fill=FALSE)+
    labs(y = "Probability\n", x = paste0("\n",col_names[1]), colour="Method")
  
  y_range_orig=ggplot_build(a)$panel$ranges[[1]]$y.range
  y_range=c(0,1)
  if (y_range[1]<0) y_range[1]=0
  if (y_range[2]>1) y_range[2]=1
  min_y <- min(y_range[1])
  max_y <- max(y_range[2])
  
  # need blank bar up to y_min, bars for 0's, blank bars up to 1's, bars for 1's up to y_max
  hist_df=data.frame(mids=hist_both$mids,
                     counts_below=min_y,
                     counts0=hist0$counts/max_count*diff(y_range)/2,
                     counts_mid=diff(y_range)-hist_both$counts/max_count*diff(y_range)/2,
                     counts1=hist1$counts/max_count*diff(y_range)/2)
  hist.m=melt(hist_df,id=1)
  head(hist.m)
  ggplot()+geom_bar(data=hist.m,aes(mids,value,fill=variable),stat="identity")+coord_cartesian(ylim=y_range)+scale_fill_manual(values=c(NA, "grey", NA,"grey"))+guides(fill=FALSE)
  
  (a <- ggplot(data, aes_string(x = col_names[1], y = col_names[2])) +
     theme_bw(base_size=18) +
     geom_bar(data=hist.m,aes(mids,value,fill=variable),stat="identity")+
     coord_cartesian(ylim=y_range_orig)+
     scale_fill_manual(values=c(NA,NA,"grey","grey","red","blue"))+
     # geom_smooth(method = "gam", family = "binomial", se = se,
                 #aes(colour='Spline',fill='Spline'), size=1.5,formula=y~s(x,bs="cr"))+
     geom_smooth(method = "glm", method.args=list(family="binomial"), se = se,
                 aes(colour='Logit',fill='Logit'), size=1.5) +
     guides(fill=FALSE)+
     labs(y = "Probability\n", x = paste0("\n",col_names[1]), colour="Method"))
  a
}