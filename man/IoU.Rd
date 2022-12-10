\name{IoU}
\alias{IoU}
\title{Intersection over Union}
\usage{

IoU(confMat)

}
\description{

IoU functions computes the Intersection over Union (Jaccard index) from a
    confusionMatrix output from the caret package.

}
\examples{

# Prediction and reference vectors
pred <- c(1,1,3,1,2,2,3,4,4,1,2,3,2,2,3,4,4,2,2,1)
refe <- c(1,3,3,2,2,3,3,4,3,1,2,3,2,3,2,4,4,2,2,1)

# Create confusionMatrix using caret
# install.packages("caret")
library(caret)
cm <- confusionMatrix(as.factor(pred), as.factor(refe), mode = "prec_recall")
print(cm)

# Use IoU to compute Intersection over Union from the confusionMatrix object
IoU(cm)

}
