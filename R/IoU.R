#' IoU functions computes the Intersection over Union (Jaccard index) from a
#' confusionMatrix output from the caret package
#' IoU is computed according to: IoU = TP / (TP+FP+FN)
#' https://en.wikipedia.org/wiki/Jaccard_index

#' @export
#' @param confMat confusionMatrix object from caret package

IoU <- function(confMat){

  # Arguments:
  # confMat: confusion matrix object from caret::confusionMatrix

  # Value:
  # The function prints the IoU of each class and the macro-average IoU

  if(class(confMat) != "confusionMatrix"){

    message("confMat must be object of type 'confusionMatrix' created with caret::confusionMatrix")

  } else {

    m <- confMat$table

    # List of true positives (TP)
    tp <- list()
    for(i in 1:ncol(m)){
      tp[[i]] <- m[i,i]
    }
    tp <- unlist(tp)

    # List of false positives (FP)
    fp <- list()
    for(i in 1:ncol(m)){
      fp[[i]] <- sum(m[,i])-tp[[i]]
    }
    fp <- unlist(fp)

    # List of false negatives (FN)
    fn <- list()
    for(i in 1:ncol(m)){
      fn[[i]] <- sum(m[i,])-tp[[i]]
    }
    fn <- unlist(fn)

    # List of IoU
    IoU <- list()
    for(i in 1:length(tp)){
      IoU[[i]] <- tp[[i]] / (tp[[i]]+fp[[i]]+fn[[i]])
    }
    IoU <- unlist(IoU)

    # Mean IoU (macro-averaged)
    macroMean_IoU <- mean(IoU)
    # Mean IoU (micro-averaged)
    microMean_IoU <- sum(tp)/(sum(tp)+sum(fp)+sum(fn))

    print("Intersection over Union (IoU) by class: ")

    # Print results
    for(i in 1:length(IoU)){
      print(paste0("Class ", i, " :", IoU[[i]]))
    }

    print("Average IoU: ")

    print(paste0("IoU (macro-average): ", macroMean_IoU))
    print(paste0("IoU (micro-average): ", microMean_IoU))
  }

}

