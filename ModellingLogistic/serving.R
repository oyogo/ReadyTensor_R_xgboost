#!/usr/bin/env Rscript
library(data.table)
library(rjson)
library(readr)
#json list(auto_unbox=TRUE)

#* @post /infer
#* @serializer json list(auto_unbox=TRUE)
function(req) {
  
    df <- req$postBody
    parsed_df <- rjson::fromJSON(df)
    dfr <-  as.data.frame(do.call(cbind, parsed_df))
    num_cols <- names(dfr)
    dfr <- setDT(dfr)[,(num_cols):= lapply(.SD, as.numeric), .SDcols = num_cols]
    
    model <- readr::read_rds("./../ml_vol/model/artifacts/model.rds")
    idField <- subset(dfr, select=id)
    dfr <- subset(dfr, select=-id)
    predicted <- predict(model,newdata=dfr, type="response")
    predicted <- data.table(predicted)
    names(predicted) <- "probabilities"
    # where the probabilities returned are <0.5 put 0 otherwise 1.
    predicted <- setDT(predicted)[, predictions:=0][probabilities>0.5, predictions:=1]
    glm_pred = cbind(idField, predicted)
    glm_pred
    # glm_pred <- dcast(glm_pred, id ~ predictions, value.var = "predictions")
    # colnames(glm_pred)[2:3]<-paste("class",colnames(glm_pred)[2:3],sep="_")
    # glm_pred

    
    
    # multipart <- mime::parse_multipart(req)
    # dat <- read.csv(file=multipart$upload$datapath)
    # dat <- subset(dat,select=-X)
    # idField <- subset(dat, select = id)
    # model <- readRDS("./../ml_vol/model/artifacts/model.rds")
    # predicted <-  predict(model, newdata=dat, type="response")
    # predicted <- data.table(predicted)
    # names(predicted) <- "probabilities"
    # # where the probabilities returned are <0.5 put 0 otherwise 1.
    # predicted <- predicted[, predictions:=0][probabilities<0.5, predictions:=1]
    # 
    # # add the ID colum to the predictions
    # glm_pred = cbind(idField, predicted)
    # glm_pred <- data.table(glm_pred)
    # glm_pred <- dcast(glm_pred, id ~ predictions, value.var = "predictions")
    # colnames(glm_pred)[2:3]<-paste("class",colnames(glm_pred)[2:3],sep="_")
    # glm_pred
}