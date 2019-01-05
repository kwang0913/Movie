#### Setup env ####
library(recommenderlab)
data("MovieLense")

#### real scheme ####
scheme_split <- evaluationScheme(MovieLense, method="split", k=5, given=-5, goodRating=5)
scheme_cross <- evaluationScheme(MovieLense, method="cross", k=5, given=-5, goodRating=5)
scheme_boots <- evaluationScheme(MovieLense, method="bootstrap", k=5, given=-5, goodRating=5)

#### real algorithm ####
realalgorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=NULL),
  "item-based CF" = list(name="IBCF", param=NULL),
  "ALS Implicit" = list(name="ALS_implicit", param=NULL),
  "re" = list(name="RERECOMMEND", param=NULL),
  "SVD" = list(name="SVD", param=NULL),
  "Funk SVD" = list(name="SVDF", param=NULL),
  "ALS" = list(name="ALS", param=NULL)
)

#### real result ####
results <- evaluate(scheme_split, realalgorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=1:9, legend="topleft")
results <- evaluate(scheme_cross, realalgorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=1:9, legend="topleft")
results <- evaluate(scheme_boots, realalgorithms, type = "topNList", n=c(1, 3, 5, 10, 15, 20))
plot(results, annotate=1:9, legend="topleft")

#results <- evaluate(scheme, algorithms, type = "ratings")

#### bin setup ####
MovieLense_binary <- binarize(MovieLense, minRating=3.5)
MovieLense_binary <- MovieLense_binary[rowCounts(MovieLense_binary)>20,colCounts(MovieLense_binary)>50]

#### bin scheme ####
binscheme_split <- evaluationScheme(MovieLense_binary, method="split", k=5, given=-5, goodRating=5)
binscheme_cross <- evaluationScheme(MovieLense_binary, method="cross", k=5, given=-5, goodRating=5)
binscheme_boots <- evaluationScheme(MovieLense_binary, method="bootstrap", k=5, given=-5, goodRating=5)

#### bin algorithm ####
binalgorithms <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=NULL),
  "item-based CF" = list(name="IBCF", param=NULL),
  "ALS Implicit" = list(name="ALS_implicit", param=NULL),
  "Association Rule" = list(name="AR", param=NULL)
)

#### bin result ####
results_binary <- evaluate(binscheme_split, binalgorithms, type = "topNList", n=c(1,3,5,10,15,20))
plot(results_binary, legend="topright")
results_binary <- evaluate(binscheme_cross, binalgorithms, type = "topNList", n=c(1,3,5,10,15,20))
plot(results_binary, legend="topright")
results_binary <- evaluate(binscheme_boots, binalgorithms, type = "topNList", n=c(1,3,5,10,15,20))
plot(results_binary, legend="topright")

#### Explore result1 ####
tr <- getData(scheme_boots, "train")
tst_known <- getData(scheme_boots, "known")
tst_unknown <- getData(scheme_boots, "unknown")

rcmnd <- Recommender(tr, "ALS_implicit")
pred <- predict(rcmnd, tst_known, type="ratings")

as(tst_unknown, "matrix")[1:8,1:5]
as(pred, "matrix")[1:8,1:5]

pred <- predict(rcmnd, tst_known, type="topNList")
pred@itemLabels[pred@items[[1]]]

rcmnd <- Recommender(tr, "POPULAR")
pred <- predict(rcmnd, tst_known, type="ratings")

as(tst_unknown, "matrix")[1:8,1:5]
as(pred, "matrix")[1:8,1:5]

pred <- predict(rcmnd, tst_known, type="topNList")
pred@itemLabels[pred@items[[1]]]

#### Explore result2 ####
tr <- getData(binscheme_boots, "train")
tst_known <- getData(binscheme_boots, "known")
tst_unknown <- getData(binscheme_boots, "unknown")

rcmnd <- Recommender(tr, "ALS_implicit")
pred <- predict(rcmnd, tst_known, type="ratings")

as(tst_unknown, "matrix")[1:8,1:5]
as(pred, "matrix")[1:8,1:5]

pred <- predict(rcmnd, tst_known, type="topNList")
pred@itemLabels[pred@items[[1]]]

rcmnd <- Recommender(tr, "POPULAR")
pred <- predict(rcmnd, tst_known, type="topNList")
pred@itemLabels[pred@items[[1]]]