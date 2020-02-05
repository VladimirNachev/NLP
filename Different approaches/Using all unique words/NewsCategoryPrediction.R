# install.packages("rjson")
# install.packages("caret")
# install.packages("stopwords")
# install.packages("stringi")
# install.packages("stringr")
library("rjson")
library("caret")
library("stopwords")
library("stringi")
library("stringr")
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

convertedDataset = as.matrix(sapply(dataset, as.character))

textWithoutPunctuationNumbers = gsub("[0-9]+", "", gsub("[[:punct:] ]+", " ", paste(split(convertedDataset[, c("headline", "short_description")], "")[[1]], collapse = " ")))
uniqueWords = unique(strsplit(tolower(textWithoutPunctuationNumbers), split = " ")[[1]])

stopwords_regex = paste(stopwords("en"), collapse = "\\b|\\b")
stopwords_regex = paste0("\\b", stopwords_regex, "\\b")
uniqueWords = str_replace_all(uniqueWords, stopwords_regex, "")
uniqueWords = stri_remove_empty(str_replace_all(uniqueWords, "\\b[[:lower:]]\\b", ""))

dataTable = data.frame(matrix(ncol = length(uniqueWords) + 1, nrow = 0))
colnames(dataTable) = uniqueWords
dataTable$category = factor(c(), levels = unique(dataset$category))

countWordOccurences = function(word, text) {
  textWithoutPunctuation = tolower(gsub("[[:punct:] ]+", " ", text))
  return(str_count(textWithoutPunctuation, word))
}

uniqueWordsLength = length(uniqueWords)
datasetLength = nrow(dataset)
for (i in 1:datasetLength) {
  for (j in 1:uniqueWordsLength) {
    dataTable[i, j] = countWordOccurences(uniqueWords[j], paste(as.character(dataset[i, "short_description"]),
                                                as.character(dataset[i, "headline"]), sep = " "))
  }
  
  dataTable[i, "category"] = dataset[i, "category"]
}

Naive_Bayes_Model = naiveBayes(category~., data = dataTable)
NB_Predictions = predict(Naive_Bayes_Model, dataTable)

# Confusion matrix to check accuracy
resultsTable = table(NB_Predictions, dataTable$category)
Naive_Bayes_Model_Accuracy = sum(diag(resultsTable)) / sum(resultsTable)
Naive_Bayes_Model_Accuracy
