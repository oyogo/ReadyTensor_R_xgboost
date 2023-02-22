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
    #dfr <-  as.data.frame(do.call(cbind, parsed_df))
    dfr <- data.table::rbindlist(parsed_df$instances)
    model <- readr::read_rds("./../ml_vol/model/artifacts/model.rds")
    thefeatures <- readr::read_rds("./../ml_vol/model/artifacts/features.rds")
    id <- readr::read_rds("./../ml_vol/model/artifacts/id.rds")
    modelmat_pred <- hashing(df=dfr, features=thefeatures)
     predicted <- predict(model,newdata=modelmat_pred, type="response")
     predicted <- data.table(predicted)
     names(predicted) <- "probabilities"
     predicted <- cbind(dfr,predicted)
     
     # where the probabilities returned are <0.5 put 0 otherwise 1.
     predicted <- setDT(predicted)[, predictions:=0][probabilities>0.5, predictions:=1]
     cols <- c(eval(id),"probabilities","predictions")
     predicted <- predicted[,..cols]
     predicted

}