# install.packages("rjson")
# install.packages("caret")
# install.packages("stopwords")
# install.packages("stringi")
# install.packages("stringr")
library("rjson")
library("caret")
library("stopwords")
library("stringr")
library("stringi")
library(e1071)

filename = "Absolute-path-to-file"
fileContent = lapply(readLines(filename), fromJSON)

wholeDataset = data.frame(matrix(unlist(fileContent), nrow = length(fileContent), byrow = TRUE))
names(wholeDataset) = c("category", "headline", "authors", "link", "short_description", "date")

dataset = wholeDataset[wholeDataset$category == "POLITICS",][1:50,]
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "CRIME",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "ENTERTAINMENT",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "SPORTS",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "TRAVEL",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "BUSINESS",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "TECH",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "COLLEGE",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "EDUCATION",][1:50,])
dataset = rbind(dataset, wholeDataset[wholeDataset$category == "RELIGION",][1:50,])

categories = c("POLITICS", "CRIME", "ENTERTAINMENT", "SPORTS", "TRAVEL", "BUSINESS", "TECH", "COLLEGE",
               "EDUCATION", "RELIGION")

getUniqueWords = function(preparedDataset) {
  textWithoutPunctuationNumbers = gsub("[0-9]+", "", gsub("[[:punct:] ]+", " ", paste(split(preparedDataset[, c("headline", "short_description")], "")[[1]], collapse = " ")))
  uniqueWords = unique(strsplit(tolower(textWithoutPunctuationNumbers), split = " ")[[1]])
  
  stopwords_regex = paste(stopwords("en"), collapse = "\\b|\\b")
  stopwords_regex = paste0("\\b", stopwords_regex, "\\b")
  uniqueWords = str_replace_all(uniqueWords, stopwords_regex, "")
  uniqueWords = stri_remove_empty(str_replace_all(uniqueWords, "\\b[[:lower:]]\\b", ""))
  
  return(uniqueWords)
}

uniqueWords = c()
for (i in 1:length(categories)) {
  newsFromCurrentCategory = as.matrix(sapply(dataset[dataset$category == categories[i],], as.character))
  uniqueWords = unique(c(uniqueWords, getUniqueWords(newsFromCurrentCategory)))
}

dataTable = data.frame("politicsOccurences" = integer(), "crimeOccurences" = integer(), "entertainmentOccurences" = integer(),
                       "sportsOccurences" = integer(), "travelOccurences" = integer(), "businessOccurences" = integer(),
                       "techOccurences" = integer(), "collegeOccurences" = integer(), "educationOccurences" = integer(),
                       "religionOccurences" = integer())

categoriesHeadlinesAndDescriptions = list()
for (i in 1:length(categories)) {
  currentCategoryHeadlinesAndDescriptions = as.matrix(sapply(dataset[dataset$category == categories[i], c("headline", "short_description")],
                                                             as.character))
  categoriesHeadlinesAndDescriptions[[i]] = currentCategoryHeadlinesAndDescriptions
}

for (i in 1:length(uniqueWords)) {
  for (j in 1:length(categories)) {
    currentCategoryHeadlinesAndDescriptions = categoriesHeadlinesAndDescriptions[[j]]
    currentWordCount = 0
    
    for (k in 1:nrow(currentCategoryHeadlinesAndDescriptions)) {
      currentNew = currentCategoryHeadlinesAndDescriptions[k,]
      currentNewHeadlineAndDescription = paste(currentNew["headline"], currentNew["short_description"], sep = " ")
      currentWordCount = currentWordCount + length(grep(uniqueWords[i], currentNewHeadlineAndDescription, ignore.case = TRUE))
    }
    
    dataTable[i, j] = currentWordCount
  }
}

chisq = chisq.test(dataTable, simulate.p.value = TRUE)
# library(corrplot)
# corrplot(chisq$residuals, is.cor = FALSE)

contrib = 100 * (chisq$residuals ^ 2 / chisq$statistic)

features = list()
index = 1
for (i in 1:nrow(contrib)) {
  currentRowMaxValueIndex = which(contrib[i,] == max(contrib[i,]), arr.ind = TRUE)[[1]]
  if (contrib[i, currentRowMaxValueIndex] > 0.05) { # Pick only features which are significant
    features[[index]] = list(uniqueWords[i], contrib[i, currentRowMaxValueIndex])
    index = index + 1
  }
}

features = sapply(features[order(-sapply(features, "[[", 2))], "[[", 1) # Sorting features in decreasing order and extracting
                                                                        # only names of features.

data = data.frame(matrix(ncol = length(features) + 1, nrow = 0))
colnames(data) = features
data$category = factor(c(), levels = unique(dataset$category))

for (i in 1:nrow(dataset)) {
  currentNewHeadlineAndDescription = paste(as.character(dataset[i, "headline"]), as.character(dataset[i, "short_description"]),
                                           sep = " ")
  currentNewHeadlineAndDescription = tolower(gsub("[0-9]+", "", gsub("[[:punct:] ]+", " ", currentNewHeadlineAndDescription)))
  for (j in 1:length(features)) {
    data[i, j] = str_count(currentNewHeadlineAndDescription, features[[j]])
  }
  
  data[i, "category"] = dataset[i, "category"]
}

Naive_Bayes_Model = naiveBayes(category~., data = data)
NB_Predictions = predict(Naive_Bayes_Model, data)

# Confusion matrix to check accuracy
resultsTable = table(NB_Predictions, data$category)
Naive_Bayes_Model_Accuracy = sum(diag(resultsTable)) / sum(resultsTable)
Naive_Bayes_Model_Accuracy
