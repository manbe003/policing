\name{LevelsOfValues}
\alias{LevelsOfValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
number of levels per column
}
\description{
evaluates how many unique values there are per column in a dataset
}
\usage{
LevelsOfValues(metadata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  LevelsOfValues<-function(metadata) {
  Vector_length<-as.data.frame(metadata)
  SummaryofLevels<-data.frame(matrix(NA,nrow=1,ncol=ncol(Vector_length)))

for (j in (1:ncol(metadata)))
  {
    SummaryofLevels[,j]<-length(unique(metadata[,j][!is.na(metadata[,j])]))
}
rownames(SummaryofLevels)<-"NumberofLevels"
colnames(SummaryofLevels)<-colnames(metadata)
return(SummaryofLevels)
}


}


}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
    Vector_length <- as.data.frame(metadata)
    SummaryofLevels <- data.frame(matrix(NA, nrow = 1, ncol = ncol(Vector_length)))
    for (j in (1:ncol(metadata))) {
        SummaryofLevels[, j] <- length(unique(metadata[, j][!is.na(metadata[,
            j])]))
    }
    rownames(SummaryofLevels) <- "NumberofLevels"
    colnames(SummaryofLevels) <- colnames(metadata)
    return(SummaryofLevels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
