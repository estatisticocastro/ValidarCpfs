graphic_lp<-function(objective.in,const.mat,const.dir,const.rhs)
{
  pontos=list()
  for(i in 1:(nrow(const.mat)-1))
  {
    for(j in (i+1):nrow(const.mat))
    {
      x_intersecao=(const.rhs[j]-const.mat[j,2]*(const.rhs[i]/const.mat[i,2]) )/
        (const.mat[j,1]-const.mat[j,2]*const.mat[i,1]/const.mat[i,2])
      y_intersecao=(const.rhs[i]-const.mat[i,1]*x_intersecao)/const.mat[i,2]
      
      if(is.nan(x_intersecao))
      {
        x_intersecao=(const.rhs[i]-const.mat[i,2]*(const.rhs[j]/const.mat[j,2]) )/
          (const.mat[i,1]-const.mat[i,2]*const.mat[j,1]/const.mat[j,2])
        y_intersecao=(const.rhs[j]-const.mat[j,1]*x_intersecao)/const.mat[j,2]
        
      }
      if(is.finite(x_intersecao) & is.finite(y_intersecao))
      {
        z_intersecao=objective.in[1]*x_intersecao+objective.in[2]*y_intersecao
        
        aux_maior_igual=sum(const.mat[(const.dir==">="),1]*x_intersecao+
                              const.mat[(const.dir==">="),2]*y_intersecao >=
                              const.rhs[(const.dir==">=")])
        
        aux_menor_igual=sum(const.mat[(const.dir=="<="),1]*x_intersecao+
                              const.mat[(const.dir=="<="),2]*y_intersecao <=
                              const.rhs[(const.dir=="<=")])
        
        if(aux_maior_igual+aux_menor_igual==nrow(const.mat))
        {
          pontos[[length(pontos)+1]]=data.frame(x=x_intersecao,y=y_intersecao,z=z_intersecao)
        }
      }
    }
    
  }
  pontos_df=do.call(rbind.data.frame, pontos)
  pontos_df=pontos_df[with(pontos_df,order(x,y)),]
  
  
  x_lim=c(2*min(unlist(lapply(pontos, function(p) p$x)))-max(unlist(lapply(pontos, function(p) p$x))),
          2*max(unlist(lapply(pontos, function(p) p$x)))-min(unlist(lapply(pontos, function(p) p$x))))
  y_lim=c(2*min(unlist(lapply(pontos, function(p) p$y)))-max(unlist(lapply(pontos, function(p) p$y))),
          2*max(unlist(lapply(pontos, function(p) p$y)))-min(unlist(lapply(pontos, function(p) p$y))))
  
  plot(x = (x_lim[2]-x_lim[1])/2,y=(y_lim[2]-y_lim[1])/2,col="white",xlim = x_lim,ylim = y_lim,xlab="x",ylab="y")
  
  for(i in 1:(nrow(const.mat)))
  {
    if(sum(const.mat[i,]!=0)==2)
    {
      curve((const.rhs[i]-const.mat[i,1]*x)/const.mat[i,2],
            from=x_lim[1],to=x_lim[2],add = T,col=2)  
      
      text(x=10,y = (const.rhs[i]-const.mat[i,1]*10)/const.mat[i,2],cex = 0.8,col=2,adj = c(0,0),
           labels = paste0(const.mat[i,1],"x + ",const.mat[i,2],"y ",const.dir[i]," ",const.rhs[i]))
    }  else if (const.mat[i,2]==0)
    {
      abline(v = const.rhs[i],col=2)
    }  else if (const.mat[i,1]==0)
    {
      abline(h = const.rhs[i],col=2)
    }
    
  }
  
  polygon(x = pontos_df$x,y=pontos_df$y,col="grey",fillOddEven = FALSE)
  for(i in 1:nrow(pontos_df))
  {
    points(pontos_df[i,]$x,pontos_df[i,]$y)
    text(x=pontos_df[i,]$x,y = pontos_df[i,]$y,adj = c(0,1),cex = 0.8,pos = rep(c(2,2,4,4),nrow(pontos_df))[i],
         labels = paste0(" (x=",pontos_df[i,]$x,", y=",pontos_df[i,]$y,", z=",pontos_df[i,]$z,")")) 
  }
  
}
