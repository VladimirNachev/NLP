# install.packages("rjson")
# install.packages("caret")
library("rjson")
library("caret")

filename = "Absolute-path-to-file"
fileContent = lapply(readLines(filename), fromJSON)

dataset = data.frame(matrix(unlist(fileContent), nrow = length(fileContent), byrow = TRUE))
names(dataset) = c("category", "headline", "authors", "link", "short_description", "date")

politicsSample = dataset[dataset$category == "POLITICS",][3406:6810,]
crimeSample = dataset[dataset$category == "CRIME",]
entertainmentSample = dataset[dataset$category == "ENTERTAINMENT",][3406:6810,]

sample = rbind(crimeSample, politicsSample, entertainmentSample)

crimeKeywords = c("death", "dead", "kill", "killed", "killer", "murder", "murdered", "murderer", "shot", "shotgun", "shoot",
                  "shooting", "gun", "riffle", "abuse", "autopsy", "bomb", "attack", "attacks", "terrorist", "terrorism", "terrosism",
                  "explosion", "radioactive", "machine gun", "automat", "theft", "thief", "robbery", "steal", "burglary",
                  "stealing", "violate", "violation", "evil", "jail", "prison", "drugs", "crime", "criminal", "forgery", "kidnap",
                  "kidnapping", "punish", "punishment", "assault", "fraud", "hijacking", "hacking", "manslaughter", "genocide",
                  "smuggling", "offender", "offence", "vandalism", "vandal", "trafficking", "trafficker", "narcotraficant",
                  "narcotic", "narcotics", "law", "conflict", "illegal", "witness")
politicsKeywords = c("donald", "trump", "trump's", "department", "obligation", "nomination", "democrat", "democrats", "democrats'",
                    "republican", "republicans", "party", "parties", "us", "politics", "political", "politic", "decision", "law",
                    "laws", "elective", "elections", "barak", "obama", "hillary", "clinton", "bush", "government", "democracy",
                    "communism", "parliament", "liberal", "conservator", "conflict", "mandate", "vote", "voting", "impeachment",
                    "electoral", "reform", "conservatism", "liberalism", "capitalism", "capitalist", "nationalization", "consensus",
                    "debate", "debates", "socialism", "prosecutor", "american", "americans", "puerto", "rico", "mexico", "aid",
                    "california", "california's", "president", "president's", "stalin", "media", "talk", "taxpayers", "nation",
                    "nation's", "national", "signs", "people", "NAFTA", "country", "corporation", "corporations", "tyranny",
                    "dictator", "dictatorial", "federal", "u.s.", "governor", "governator", "immigrants", "immigrant", "unesco",
                    "policy", "scandal", "tax", "taxes", "allegation", "allegations", "regulate", "united", "states", "congress",
                    "congressman", "congressmen", "loan", "aristocracy", "capital", "million", "mideast", "russia", "press",
                    "journalist", "reporter", "deputy", "iran", "deal", "korea", "saudi", "arabia", "europe", "union", "africa",
                    "cuba", "diplomat", "diplomats", "diplomacy", "counselor", "counselors", "popularity", "rating")
entertainmentKeywords = c("entertain", "entertained", "entertaining", "entertainment", "art", "arts", "music", "musician", "guitar", "guitars",
                          "actor", "actors", "actor's", "actress", "actresses", "movie", "movies", "oscar", "oscars", "funny",
                          "funniest", "hilarious", "dance", "dances", "dancing", "celebrity", "celebrities", "film", "films",
                          "song", "songs", "singer", "singers", "singer's", "marry", "married", "marries", "girlfriend",
                          "girlfriend's", "girlfriends", "boyfriend", "boyfriend's", "boyfriends", "artwork", "artworks", "humor",
                          "humour", "compliment", "compliments", "show", "shows", "tv", "television", "radio", "radios", "watch",
                          "series", "mini-series", "award", "awards", "award-winning", "pop", "star", "stars", "rock", "metal",
                          "album", "albums", "cover", "producer", "producers", "produced", "producing", "photo", "photos", "spent",
                          "spending", "comedy", "comedian", "director", "directors", "director's", "story", "stories",
                          "criticism", "critic", "critics", "critical", "date", "dates", "karaoke", "bar", "bars", "scene",
                          "scenes", "farewell", "season", "seasons", "hollywood", "surprise", "surprising", "surprisingly",
                          "surprised", "mogul", "masterpiece", "meme", "memes", "drama", "amaze", "amazing", "amazingly", "amazed",
                          "troll", "old", "laughs", "laugh", "laughing", "laughter", "rap", "rapping", "rapper", "rappers",
                          "rapper's", "stage", "stages", "culture", "performance", "performed", "performer", "happy", "happier",
                          "happiest", "drug", "drugs", "play", "plays", "playing", "played", "role", "roles", "youtube")


keywords = list(crimeKeywords, politicsKeywords, entertainmentKeywords)

data = data.frame("numberOfCrimeKeywords" = integer(), "numberOfPoliticsKeywords" = integer(),
                  "numberOfEntertainmentKeywords" = integer(),
                  "category" = factor(levels = c("crime", "politics", "entertainment")))

for (i in 1:nrow(sample)) {
  newsHeadlineAndDescription = strsplit(as.character(sample[i, "headline"]), " ")[[1]]
  newsHeadlineAndDescription = c(newsHeadlineAndDescription, strsplit(as.character(sample[i, "short_description"]), " ")[[1]])
  
  data[i, 1:(ncol(data) - 1)] = 0
  data[i, "category"] = tolower(sample[i, "category"])
  for (j in 1:length(keywords)) {
    data[i, j] = length(grep(paste(keywords[j][[1]], collapse = "|"), newsHeadlineAndDescription, ignore.case = TRUE))
  }
}

# Run algorithms using 10-fold cross validation
control = trainControl(method = "cv", number = 10)
metric = "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda = train(category~., data = data, method = "lda", metric = metric, trControl = control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart = train(category~., data = data, method = "rpart", metric = metric, trControl = control)
# kNN
set.seed(7)
fit.knn = train(category~., data = data, method = "knn", metric = metric, trControl = control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm = train(category~., data = data, method = "svmRadial", metric = metric, trControl = control)
# Random Forest
set.seed(7)
fit.rf = train(category~., data = data, method = "rf", metric = metric, trControl = control)

# Summarize accuracy of models
results = resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

# install.packages("e1071")
library("e1071")

# Fitting the Naive Bayes model
Naive_Bayes_Model = naiveBayes(category~., data = data)
NB_Predictions = predict(Naive_Bayes_Model, data)

# Confusion matrix to check accuracy
resultsTable = table(NB_Predictions, data$category)
Naive_Bayes_Model_Accuracy = sum(diag(resultsTable)) / sum(resultsTable)
Naive_Bayes_Model_Accuracy
  
