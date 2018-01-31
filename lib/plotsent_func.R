f.plotsent<-function(In.list, InFile, InTerm, President){
  
  col.use=c("lightgray", "red2", "darkgoldenrod1", 
            "chartreuse3", "blueviolet",
            "darkgoldenrod2", "dodgerblue3", 
            "darkgoldenrod1", "darkgoldenrod1",
            "black", "darkgoldenrod2")
  
  In.list$topemotion=apply(select(In.list, 
                                  anger:positive), 
                           1, which.max)
  In.list$topemotion.v=apply(select(In.list,
                                    anger:positive), 
                             1, max)
  In.list$topemotion[In.list$topemotion.v<0.05]=0
  In.list$topemotion= In.list$topemotion + 1
  
  temp=In.list$topemotion.v
  In.list$topemotion.v[temp<0.05]=1
  
  df=In.list%>%filter(File==InFile, Term==InTerm)%>%
    select(sent.id, word.count, 
           topemotion, topemotion.v)
  
  ptcol.use=alpha(col.use[df$topemotion], sqrt(sqrt(df$topemotion.v)))
  
  plot(df$sent.id, df$word.count, 
       col=ptcol.use,
       type="h", 
       ylim=c(-10, max(In.list$word.count)),
       main=President)
}
