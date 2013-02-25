\name{BinomTree}
\alias{BinomTree}
\title{
Plot a CRR stock / Option Tree
}
\description{
Function to plot a CRR Binomial Tree

}
\usage{
BinomTree()
}
\details{
The user inputs are as follows:\cr
Type of Option:  chosen between Call/Put\cr
Exercise style:  chosen between European/American \cr
Stock Price: to be entered in numbers for e.g. 120.50\cr
Strike price: to be entered in numbers for e.g. 110.50\cr
Time in number of years: to be entered in decimals. For e.g. 0.25 for a quarter year\cr
Volatility(or sigma) per annum: to be entered in decimals. For e.g. 0.25 for 25 percent \cr
Risk free rate per annum: to be entered in decimals. For e.g. 0.05 for 5 percent \cr
Dividend yield: to be entered in decimals. For e.g. 0.02 for 2 percent\cr
No of steps: Clicking on the '+' and '-' respectively increases and decreases the value.\cr
Plot type: chosen between Stock tree/ option tree
}


\value{
A plot of Stock Tree / Option Tree with user specified parameters.
}

\references{
John C. Hull, "Options, Futures, and Other Derivatives", 8/E, Prentice Hall, 2012. 
}
\author{
S Subramanian <ssubramanian@sssihl.edu.in>
}

\seealso{
\code{\link[GUIDE]{blackscholes}}
}

