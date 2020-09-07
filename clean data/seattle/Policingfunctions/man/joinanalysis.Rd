\name{joinanalysis}
\alias{joinanalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Analyze the overlap between two columns of data}
\description{
This tells, for two given columns: 1) how many instances of a given value from dataset 1 occur in dataset 1, how many instances of that value from dataset 1 occur in dataset 2, and how many instances of a given value from dataset 2 occur in both datasets 1 and 2.}
\usage{
joinanalysis(x)
}

\arguments{
  \item{x}{
join_analysis<-function(column_name_fromdataset1, column_name_fromdataset2){
  Dataset1<-as.numeric(column_name_fromdataset1)
  Dataset2<-as.numeric(column_name_fromdataset2)
  count<-merge(aggregate(. ~ ind, stack(table(Dataset1)), sum),
                       aggregate (. ~ ind, stack (table(Dataset2)), sum),
                       by = "ind", all.x = TRUE
                               )
  count[is.na(count)]<-0
return(count)
  }}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
data frame
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
