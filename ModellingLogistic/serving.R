#!/usr/bin/env Rscript
library(data.table)
library(rjson)
library(readr)

# need to access the hashing function to convert the json input into a hashed matrix
source("preprocessor.R")

#* @post /infer
#* @serializer json list(auto_unbox=TRUE)
function(req) {
  
    df <- req$postBody
    parsed_df <- rjson::fromJSON(df)
    dfr <-  as.data.frame(do.call(cbind, parsed_df))

    model <- readr::read_rds("./../ml_vol/model/artifacts/model.rds")
    thefeatures <- readr::read_rds("./../ml_vol/model/artifacts/features.rds")
    modelmat_test <- hashing(df=dfr, features=thefeatures)
     predicted <- predict(model,newdata=modelmat_test, type="response")
     predicted <- data.table(predicted)
     names(predicted) <- "probabilities"
     # where the probabilities returned are <0.5 put 0 otherwise 1.
     predicted <- setDT(predicted)[, predictions:=0][probabilities>0.5, predictions:=1]
     #glm_pred = cbind(idField, predicted)
     # glm_pred <- dcast(glm_pred, id ~ predictions, value.var = "predictions")
     # colnames(glm_pred)[2:3]<-paste("class",colnames(glm_pred)[2:3],sep="_")
     predicted

}