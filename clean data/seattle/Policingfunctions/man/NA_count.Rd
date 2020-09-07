\name{NA_count}
\alias{NA_count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
How many NAs in each column?
}
\description{
counts NAs per column
}
\usage{
NA_count(metadata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata}{
NA_count<-function(metadata){

  count_temp<-data.frame(matrix(NA,nrow=nrow(metadata),ncol=ncol(metadata)))

  #look for number of NAs in each column
  for (j in (1:ncol(metadata)))
  {
    for (i in 1:length(metadata[,2])){

      if(is.na(metadata[,j][i])){
        count_temp[,j][i]<-1
      }
      else{
        count_temp[,j][i]<-0
      }
    }
  }
    SummaryofNA_count<-rbind(colSums(count_temp))
    rownames(SummaryofNA_count)<-"NumberofNA"
    colnames(SummaryofNA_count)<-colnames(metadata)
    return(SummaryofNA_count)
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
dataframe
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Katherine Manbeck
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (metadata)
{
    count_temp <- data.frame(matrix(NA, nrow = nrow(metadata),
        ncol = ncol(metadata)))
    for (j in (1:ncol(metadata))) {
        for (i in 1:length(metadata[, 2])) {
            if (is.na(metadata[, j][i])) {
                count_temp[, j][i] <- 1
            }
            else {
                count_temp[, j][i] <- 0
            }
        }
    }
    SummaryofNA_count <- rbind(colSums(count_temp))
    rownames(SummaryofNA_count) <- "NumberofNA"
    colnames(SummaryofNA_count) <- colnames(metadata)
    return(SummaryofNA_count)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
