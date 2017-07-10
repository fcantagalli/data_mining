#package link: https://cran.r-project.org/web/packages/arules/arules.pdf
#tutorial: https://en.wikibooks.org/w/index.php?title=Data_Mining_Algorithms_In_R/Frequent_Pattern_Mining/The_Apriori_Algorithm
#https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
#pre processamento https://cran.r-project.org/doc/contrib/de_Jonge+van_der_Loo-Introduction_to_data_cleaning_with_R.pdf

library(arules)
library(arulesViz)

setwd("~/Code/data_mining/associacao")

#preciso pesquisar como carregar os dados para o algoritmo
#cogumeloDataSet<-read.csv("agaricuslepiota.txt", header = FALSE,na.string='?')


#checar se ha missing na ou nan
#for (i in 1:ncol(cogumeloDataSet)) {
#    print(table(is.na(cogumeloDataSet[, i])))
#    print(table(is.nan(cogumeloDataSet[, i])))
#} #nao ha

#Load Dataset
#This one is just a mock dataset
#data("Adult")
#rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
#summary(rules)

##############################################################################
contraceptivoDataset <- as.matrix(read.csv("contraceptivo",
                                            header = TRUE,
                                            col.names = c("Wife_age",
                                                           "Wife_education",
                                                           "Husband_education",
                                                           "Number_children_born",
                                                           "Wife_religion",
                                                           "Wife_working",
                                                           "Husband_occupation",
                                                           "Standard_living_index",
                                                           "Media_exposure",
                                                           "Contraceptive_used"),
                                              colClasses = c('numeric',
                                                             'factor',
                                                             'factor',
                                                             'numeric',
                                                             'raw',
                                                             'raw',
                                                             'factor',
                                                             'factor',
                                                             'raw',
                                                             'factor')))

dcontraceptivoDataset = transform(contraceptivoDataset,
                                   Wife_age = as.factor(Wife_age),
                                   Wife_education = as.factor(Wife_education),
                                   Husband_education = as.factor(Husband_education),
                                   Number_children_born = as.factor(Number_children_born),
                                   Wife_religion = as.factor(Wife_religion),
                                   Wife_working = as.factor(Wife_working),
                                    Husband_occupation = as.factor(Husband_occupation),
                                    Standard_living_index = as.factor(Standard_living_index),
                                    Media_exposure = as.factor(Media_exposure),
                                    Contraceptive_used = as.factor(Contraceptive_used))

                                   
        
for (i in 1:ncol(dcontraceptivoDataset)) {
    print(table(is.na(dcontraceptivoDataset[, i])))
    print(table(is.nan(dcontraceptivoDataset[, i])))
}
#data_train_matrix <- as.matrix(scale(contraceptivoDataset))
print(contraceptivoDataset)

#write.table(contraceptivoDataset, file = "contraceptivo.csv", sep = ",", quote = TRUE, row.names = FALSE)
# I intend to create a csv file, so I use 'sep=","' to separate the entries by a comma, 'quote=TRUE' to quote all the entries, and 'row.names=F to prevent the creation of an extra column containing the row names (which is the default behavior of write.table() )

# Now place the dataset into a "data" directory (either via R or via the operating system, doesn't make any difference):
 #dir.create("data")
# create the directory
#file.rename(from = "contraceptivo.csv", to = "data/contraceptivo.csv")
# move the file


# Now we can finally load the dataset:
#data("contraceptivo")
# data(mydataset) works as well, but quoted is preferable - less risk of conflic
#Contraceptivos = as(contraceptivo, "transactions")
rules = apriori(dcontraceptivoDataset, parameter = list(support = 1, confidence = 1, minlen = 2))
rules
plot(rules)
inspect(head(sort(rules, by = "lift"), 250))
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

#sem alteração nos resultados
rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.9, confidence = 0.9, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))

#sem altereções
rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.8, confidence = 0.8, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))


rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.7, confidence = 0.7, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.6, confidence = 0.6, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.4, confidence = 0.4, maxlen = 2, minlen=2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")
plot(rules, method = "matrix", measure = c("support"))

rules = apriori(dcontraceptivoDataset, parameter = list(support = 1, confidence = 1, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")





rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.7, confidence = 0.7, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.6, confidence = 0.6, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.3, confidence = 0.3, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

rules = apriori(dcontraceptivoDataset, parameter = list(support = 0.35, confidence = 0.6, minlen = 5))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")




####################################################################################
##Cogumelos################

#write.table(cogumeloDataSet, file = "cogumelo.csv", sep = ",", quote = TRUE, row.names = FALSE)
# I intend to create a csv file, so I use 'sep=","' to separate the entries by a comma, 'quote=TRUE' to quote all the entries, and 'row.names=F to prevent the creation of an extra column containing the row names (which is the default behavior of write.table() )

# Now place the dataset into a "data" directory (either via R or via the operating system, doesn't make any difference):
#dir.create("data")
# create the directory
#file.rename(from = "cogumelo.csv", to = "data/cogumelo.csv")
# move the file

# Now we can finally load the dataset:
#data("agaricuslepiota")
# data(mydataset) works as well, but quoted is preferable - less risk of conflic
#Cogumelos = as(agaricuslepiota, "transactions")
#rules = apriori(Cogumelos, parameter = list(minlen=2))
#rules = apriori(Cogumelos, parameter = list(support = 0.001, confidence = 0.001))
#rules
#inspect(head(sort(rules, by = "lift"), 250))
#plot(rules)
#head(quality(rules))
#plot(rules, measure = c("support", "lift"), shading = "confidence")
#plot(rules, shading = "order", control = list(main = "Two-key plot"))
#plot(rules, shading="order",control=list(main = "Two-key plot"))

###########-------------------------------
#Congress
congressDataset <- as.matrix(read.csv("congress", header = FALSE, col.names = c(
                                                                            "party",
                                                                            "handicapped_infants", 
                                                                            "water_project_cost_sharing", 
                                                                            "adoption_of_the_budget_resolution",
                                                                            "physician_fee_freeze", 
                                                                            "el_salvador_aid", 
                                                                            "religious_groups_in_schools",
                                                                            "anti_satellite_test_ban",
                                                                            "aid_to_nicaraguan_contras", 
                                                                            "mx_missile", "immigration", 
                                                                            "synfuels_corporation_cutback",
                                                                            "education_spending", 
                                                                            "superfund_right_to_sue", 
                                                                            "crime", "duty_free_exports",
                                                                            "export_administration_act_south_africa")))

dcongressDataset = transform(congressDataset,
                                   party = as.factor(party),
                                    handicapped_infants = as.factor(handicapped_infants),    
                                    water_project_cost_sharing = as.factor(water_project_cost_sharing),
                                    adoption_of_the_budget_resolution = as.factor(adoption_of_the_budget_resolution),
                                    physician_fee_freeze = as.factor(physician_fee_freeze),
                                    el_salvador_aid = as.factor(el_salvador_aid),
                                    religious_groups_in_schools = as.factor(religious_groups_in_schools),
                                    anti_satellite_test_ban = as.factor(anti_satellite_test_ban),
                                    aid_to_nicaraguan_contras = as.factor(aid_to_nicaraguan_contras),
                                    mx_missile = as.factor(mx_missile),
                                    immigration = as.factor(immigration),
                                    synfuels_corporation_cutback = as.factor(synfuels_corporation_cutback),
                                    education_spending = as.factor(education_spending),
                                    superfund_right_to_sue = as.factor(superfund_right_to_sue),
                                    crime = as.factor(crime),
                                    duty_free_exports = as.factor(duty_free_exports),
                                    export_administration_act_south_africa = as.factor(export_administration_act_south_africa))
#minimo
rules = apriori(dcongressDataset, parameter = list(support = 0.5, confidence = 0.5, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "matrix", measure = c("support"))

#sem altereções
rules = apriori(congressDataset, parameter = list(support = 0.8, confidence = 0.8, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

rules = apriori(congressDataset, parameter = list(support = 0.7, confidence = 0.7, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "lift"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

rules = apriori(congressDataset, parameter = list(support = 0.6, confidence = 0.6, minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(dcongressDataset, parameter = list(support = 0.3, confidence = 0.3,  minlen = 2))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")
plot(rules, method = "matrix", measure = c("support"))

rules = apriori(congressDataset, parameter = list(support = 1, confidence = 1, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")





rules = apriori(congressDataset, parameter = list(support = 0.7, confidence = 0.7, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(congressDataset, parameter = list(support = 0.6, confidence = 0.6, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


rules = apriori(congressDataset, parameter = list(support = 0.3, confidence = 0.3, minlen = 3))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")

rules = apriori(dcongressDataset, parameter = list(support = 0.35, confidence = 0.6, minlen = 6))
rules
plot(rules)
head(quality(rules))
inspect(head(sort(rules, by = "support"), 250))
plot(rules, measure = c("support", "lift"), shading = "confidence")
plot(rules, shading = "order", control = list(main = "Two-key plot"))
plot(rules, method = "grouped", measure = c("support"), shadding = "confidence")


loadCongressDataset <- function() {
  dataset <- read.csv("congress", header = FALSE, col.names = c(
    "party", "handicapped_infants", "water_project_cost_sharing", "adoption_of_the_budget_resolution",
    "physician_fee_freeze", "el_salvador_aid", "religious_groups_in_schools", "anti_satellite_test_ban",
    "aid_to_nicaraguan_contras", "mx_missile", "immigration", "synfuels_corporation_cutback",
    "education_spending", "superfund_right_to_sue", "crime", "duty_free_exports", "export_administration_act_south_africa"
  ))
  #dataset$V2 <- sapply(dataset$V2, function (item) { paste("handicapped-infants-", item) })
  #dataset$V3 <- sapply(dataset$V3, function (item) { paste("handicapped-infants-", item) })
  for (column in names(dataset)) {
    dataset[,column] <- replace(as.character(dataset[,column]), dataset[,column] == "?", "absent")
  }
  write.table(dataset, file = "congress.csv", sep = ",", quote = TRUE, row.names = FALSE)
  dataset <- read.csv("congress.csv", header = FALSE, stringsAsFactors = TRUE ,col.names = c(
    "party", "handicapped_infants", "water_project_cost_sharing", "adoption_of_the_budget_resolution",
    "physician_fee_freeze", "el_salvador_aid", "religious_groups_in_schools", "anti_satellite_test_ban",
    "aid_to_nicaraguan_contras", "mx_missile", "immigration", "synfuels_corporation_cutback",
    "education_spending", "superfund_right_to_sue", "crime", "duty_free_exports", "export_administration_act_south_africa"
  ))
  return(dataset)
}

concatStrings <- function(x2, x1) {
  paste(x1, x2)
}