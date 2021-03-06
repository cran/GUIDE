ABMPaths <-
function(){

    initialprice <- 100 # lets keep this fixed
    time <- 1
    steps <- 250
    dt <- time/steps
    #   paths <- 6



    my.draw <- function(panel) {
      #  mu and sigma are mean and variance of change in price rather than of returns
      mu <- panel$mu
      sigma <- panel$sigma
      mudt <- mu*dt
      sigmasqrtdt <- sigma*sqrt(dt)
      paths <- panel$paths

      #   ds = S + S(mudt +sigmasqrtdt*epsilon)
      ABMStockprices <- function(initialprice,steps,paths,sigmasqrtdt,mudt){
        prices <- matrix(data=NA, nrow=steps+1,ncol=paths)
        prices[1,] <- initialprice
        for (i in 2:(steps+1)){
          prices[i,] <- prices[i-1,]+ (mudt + sigmasqrtdt*rnorm(paths))
        }
        return(prices)
      }

      prices <-ABMStockprices(initialprice,steps,paths,sigmasqrtdt,mudt)

      x.axis <- seq(0,1,length=steps+1)

      #if (length(dev.list()) == 0)
        #dev.new()
        #x11()
      my.title <- paste(paths, " Arithmetic Brownian motions", "(mu=", mu, ", sigma=", sigma,")")
      matplot(prices,main= my.title,xlab="time", ylab="price",type='l',lwd=2)
      panel
    }

    my.redraw <- function(panel) #not needed bcos we are not using tkr plot
    {
      rp.tkrreplot(panel, my.tkrplot)
      panel
    }

    my.panel <- rp.control(title = "Arithmetic Brownian Motion", mu = 20, sigma = 40,paths=1,size=c(500,400))
    rp.doublebutton(panel = my.panel, variable= mu, step = 8, range = c(0, 40),
                    showvalue=TRUE, title = "Drift", action = my.redraw)
    rp.doublebutton(panel = my.panel, variable= sigma, step = 8, range = c(0, 80),
                    showvalue=TRUE, title = "Volatility",  action = my.redraw)
    rp.doublebutton(panel = my.panel, variable= paths, step = 1, range = c(1, 10),
                    showvalue=TRUE, title = "Paths", action = my.redraw)
    rp.tkrplot(my.panel, my.tkrplot, my.draw, pos = "right", hscale=2,vscale=1.75) # doesnt appear very good
    #rp.do(my.panel, my.draw)
  }
