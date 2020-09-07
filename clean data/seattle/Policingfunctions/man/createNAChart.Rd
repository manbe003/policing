\name{createNAchart}
\alias{createNAchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
create chart describing distinctness of variables.
}
\description{
Returns a chart that describes: 1) sample size; 2) number of NAs in each column; 3) number of distinct values in each column; 4) number of cases where NA is unique in the row; 5) number of cases where NA is in one other column in the row.
}
\usage{
createNAchart(x)
}

\arguments{
  \item{x}{
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


#make a chart that will do all 5 columns at the same time!
  createNAChart<-function(metadata){
    NAChart<-as.data.frame(matrix(NA,nrow=nrow(metadata),ncol=5))
    NAChart<-rbind(nrow(metadata),NA_count(metadata), LevelsOfValues(metadata), PercentNAUnique(metadata,1),PercentNAUnique(metadata,2))
   rownames(NAChart)<-cbind('N',"Number of NAs", "Number of Levels", "Number of only NA", "Number of 1 of 2 NAs")
    return(NAChart)
  }}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data frame

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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
