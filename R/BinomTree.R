if (getRversion() >= "2.15.1") utils::globalVariables(c("opttype","exercisetype","Stock","X","Time","r","Div","nsteps"))

BinomTree <-
function(){
  
  my.draw <- function(panel){
    
    Stock <- as.numeric(panel$Stock)
    X <- as.numeric(panel$X)
    Time <- as.numeric(panel$Time)
    r <- as.numeric(panel$r)
    sigma <- as.numeric(panel$sigma)
    Div <- as.numeric(panel$Div)
    nsteps <- as.numeric(panel$nsteps)
    
    timepoints=nsteps+1
    # keep some extra space along all 4 sides for the matrix to be plotted neatly
    extra = 1
    # gap=1
    nrows=2*(nsteps)+1+2*extra
    ncols=2*(nsteps)+1+2*extra
    
    dt = Time/nsteps
    u = exp(sigma*sqrt(dt))
    d= 1/u
    p = (exp((r-Div)*dt)-d)/(u-d)
    
    
    S= matrix(data=NA,nrow=nrows,ncol=ncols)
    
    
    
    # S[1,ceiling(ncols/2)]=100
    
    
    # row will start from the third row bcos the
    # first price is in row 2
    # col will go as seq(from=(nrows - row + 1),to=(ncols - nrows + row),by=2)
    startrow=2
    S[startrow,nsteps+2]=Stock # initial price
    startcol=2    
    
    for (row in seq(from=startrow+2,to=(nrows-1),by=2)) {
      for (col in seq(from=(startcol + timepoints - row/2),by=2,length.out=row/2))
      {
        if (col <= ceiling(ncols/2)){
          S[row,col]=round(S[row-2,col+1]*d,2)
        }
        else {
          S[row,col]=round(S[row-2,col-1]*u,2) 
        } 
      }
    }
    
    
    # value the option     
    C= matrix(data=NA,nrow=nrows,ncol=ncols)
    
    # first calculate terminal values of C- will depend on opttype
    lastrow = nrows-extra
    
    for (col in seq(from=(startcol + timepoints - lastrow/2),by=2,length.out=lastrow/2)){
      if (panel$opttype == "Call"){  
        C[lastrow,col]=round(pmax(S[lastrow,col]-X,0),2)
      }
      else{
        C[lastrow,col]=round(pmax(X-S[lastrow,col],0),2)
      }
    }
    
    # then go backward from last row to second row
    
    for (row in seq(from=(lastrow-2),to=startrow,by=-2)){
      for (col in seq(from=(startcol + timepoints - row/2),by=2,length.out=row/2)){
        C[row,col]=round(exp(-r*dt)*(C[row+2,col+1]*p + C[row+2,col-1]*(1-p)),2)
      }
    }
    
    # if american, it is lower of the two below (for non terminal nodes)
    if (panel$exercisetype == "American"){  
      for (row in seq(from=(lastrow-2),to=startrow,by=-2)){
        for (col in seq(from=(startcol + timepoints - row/2),by=2,length.out=row/2)){
          if (panel$opttype == "Call"){  
            C[row,col]=pmax(pmax(S[row,col]-X,0),C[row,col])
          }
          else{
            C[row,col]=pmax(pmax(X-S[row,col],0),C[row,col])
          }
        }}
    }
    else{
      
    }
    
    
    
    # set graphs options
    if (nsteps>= 2){
      cex=0.9
    }
    else{
      cex=1
    }
    
    
    maxlt = nrows
    
    if (length(dev.list()) == 0) 
      x11()
    plot(2:maxlt, 2:maxlt, type="n",ylab="",xlab="", 
         axes=FALSE, frame = FALSE)
    
    for (i in 2:nrows){
      for (j in 2:ncols){
        if (panel$plot == "Stock Tree"){
          topaste = "Stock"  
          text(i, j, S[i,j],cex=cex) # ,col="red") 
        }
        else{
          topaste = paste(panel$exercisetype, panel$opttype)
          text(i, j, C[i,j],cex=cex) # ,col="red")  
        }
      }
    }
    title(main = paste(nsteps,"Step ", topaste, " Tree"))
    panel
  }   
  
  
  
  
  my.panel <- rp.control(title = "CRR Binomial Tree")
  
  rp.radiogroup(panel = my.panel, variable= opttype,
                vals = c("Call", "Put"), 
                action = my.draw, title = "Type of Option")
  
  rp.radiogroup(panel = my.panel, variable= exercisetype,
                vals = c("European", "American"), 
                action = my.draw, title = "Exercise style")
  
  rp.textentry(panel=my.panel,variable=Stock,action=my.draw,title="Stock price     ",initval=100)
  rp.textentry(panel=my.panel,variable=X,action=my.draw,title="Strike price     ",initval=110)
  rp.textentry(panel=my.panel,variable=Time,action=my.draw,title="Time               ",initval=0.25)
  rp.textentry(panel=my.panel,variable=sigma,action=my.draw,title="Volatility         ",initval=0.25)
  rp.textentry(panel=my.panel,variable=r,action=my.draw,title="Risk free rate  ",initval=0.04)
  rp.textentry(panel=my.panel,variable=Div,action=my.draw,title="Dividend rate ",initval=0)
  rp.doublebutton(panel = my.panel, showvalue=TRUE, variable= nsteps, step = 1, range = c(1, 5),initval=3,
                  title = "No. of Steps", action = my.draw)
  rp.radiogroup(panel = my.panel, variable= plot,
                vals = c("Stock Tree", "Option Tree"), 
                action = my.draw, title = "Plot Type")
  rp.do(my.panel, my.draw)
}
