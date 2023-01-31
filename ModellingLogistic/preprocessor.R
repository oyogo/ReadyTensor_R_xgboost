
#####*****Preprocessing script*****######
#####* This is where we process the data, say imputing NA's with mean plus any other transformation that can be done. 
#####* This function will be sourced in the training script before fitting the model. 

library(data.table) # Opted for this, 1. Because its really fast 2. dplyr conflicted with plumber
library(rjson) # for handling json data
library(FeatureHashing)
library(stringr)
library(dplyr)

preprocessing <- function(fname_train,fname_schema,genericdata,dataschema){ 
  

  #names(genericdata) <- gsub("%","x",names(genericdata))

  # get the response variable and store it as a string to a variable
  varr <- dataschema$inputDatasets$binaryClassificationBaseMainInput$targetField
  
# drop the id field 
## get the field name and store it as a variable
idfieldname <- dataschema$inputDatasets$binaryClassificationBaseMainInput$idField

# get the predictor fields from the dataschemaa. 
predictor_fields <- data.frame(dataschema$inputDatasets$binaryClassificationBaseMainInput$predictorFields)

# convert the dataframe to data.table for munging :- don't want to use dplyr
predictor_fields <- setDT(predictor_fields)

# melt the data.table into long format so as to filter numeric columns. 
predictor_fields <- melt(predictor_fields,measure.vars=patterns(fieldNames="fieldName",dataTypes="dataType"))
#predictor_fields$fieldNames <- gsub("%", "x", predictor_fields$fieldNames)
# filter the numeric columns 
num_vars <- predictor_fields[dataTypes=="NUMERIC",.(fieldNames)]

# categorical variables
cat_vars <- predictor_fields[dataTypes=="CATEGORICAL",.(fieldNames)]

catcols <- as.vector(cat_vars$fieldNames)
v <- num_vars$fieldNames

# loop through the numeric columns and replace na values with mean of the same column in which the na appears.
for (coll in v){

 genericdata <-  genericdata[, (coll) := lapply(coll, function(x) {
    x <- get(x)
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  })]

}

# for the missing values in categorical variables replace them with mode
my_mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (cat_coll in cat_vars) {
  genericdata <- as.data.frame(genericdata)
  genericdata[is.na(genericdata[,cat_coll]),cat_coll] <- my_mode(genericdata[,cat_coll], na.rm = TRUE)

}

# genericdata <- mutate(genericdata, Churn=
#                    case_when(Churn == "Yes" ~ 1,
#                              Churn == "No" ~ 0,TRUE ~ as.numeric(Churn)))

#genericdata <- setDT(genericdata)[, eval(varr) := fifelse(eval(varr)=="yes",1,0)]#[,eval(varr) := as.numeric(eval(varr))]
#genericdata <- genericdata[,eval(varr):=as.numeric(eval(varr))]



# # encode the categorical variables and create a matrix for the xgboost training
# modelmat <- hashed.model.matrix(c(catcols,v),
#                              genericdata, hash.size = 2 ^ 10,
#                              create.mapping = TRUE)
# 
# return(list(modelmat,genericdata,varr,idfieldname))
return(unique(genericdata$Churn))
  
}

#head(preprocessing(genericdata = genericdata, dataschema = dataschema)[[1]])
