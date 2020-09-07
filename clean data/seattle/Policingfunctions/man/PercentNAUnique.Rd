\name{PercentNAUnique}
\alias{PercentNAUnique}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
look for how many NAs are in a given row of the dataset
}
\description{
look for numbers of cases where NA is unique in the column
Where NumTotalNAsAllowable = 0 if only looking for unique NAs, = 1 if looking for NAs with overlap with
}
\usage{
PercentNAUnique(metadata, NumTotalNAsAllowableInRow)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{metadata}{
 only one other variable, etc.
PercentNAUnique<-function(metadata, NumTotalNAsAllowableInRow){
  Vector_length<-as.data.frame(metadata)
  PercentNAvsNonNA<-as.data.frame(matrix(NA,nrow=nrow(Vector_length),ncol=(ncol(Vector_length))))
  NAOverlap<-as.data.frame(matrix(NA,nrow=1,ncol=ncol(Vector_length)))

  for (j in (1:ncol(metadata))){
    for (i in (1:nrow(metadata))){

      if(is.na(metadata[,j][i])==TRUE){

        PercentNAvsNonNA[,j][i]<-rowSums(is.na(metadata[i,]))
      }
      else{
        PercentNAvsNonNA[,j][i]<-0
      }
    }
    if(NumTotalNAsAllowableInRow !=0){
     NAOverlap[,j]<-((sum(PercentNAvsNonNA[,j]==NumTotalNAsAllowableInRow))/(sum(PercentNAvsNonNA[,j]!=0)))
    }
    else
      {
      return("Error: please input a non zero number into function call line - 1 means the variable is distinct as a NA")
      }
  }
  rownames(NAOverlap)<-"NumberUniqueNAs"
  colnames(NAOverlap)<-colnames(metadata)
  return(NAOverlap)
}

}
  \item{NumTotalNAsAllowableInRow}{
%%     ~~Describe \code{NumTotalNAsAllowableInRow} here~~
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
function (metadata, NumTotalNAsAllowableInRow)
{
    Vector_length <- as.data.frame(metadata)
    PercentNAvsNonNA <- as.data.frame(matrix(NA, nrow = nrow(Vector_length),
        ncol = (ncol(Vector_length))))
    NAOverlap <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(Vector_length)))
    for (j in (1:ncol(metadata))) {
        for (i in (1:nrow(metadata))) {
            if (is.na(metadata[, j][i]) == TRUE) {
                PercentNAvsNonNA[, j][i] <- rowSums(is.na(metadata[i,
                  ]))
            }
            else {
                PercentNAvsNonNA[, j][i] <- 0
            }
        }
        if (NumTotalNAsAllowableInRow != 0) {
            NAOverlap[, j] <- ((sum(PercentNAvsNonNA[, j] ==
                NumTotalNAsAllowableInRow))/(sum(PercentNAvsNonNA[,
                j] != 0)))
        }
        else {
            return("Error: please input a non zero number into function call line - 1 means the variable is distinct as a NA")
        }
    }
    rownames(NAOverlap) <- "NumberUniqueNAs"
    colnames(NAOverlap) <- colnames(metadata)
    return(NAOverlap)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
