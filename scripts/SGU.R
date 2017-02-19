loadlibraries <- function() {
    library(readxl)
    library(dplyr)
    library(reshape2)
    
}

readSGUData <- function() {
    #dfEpisodes <- read.xlsx('SGU_Science_or_Fiction.xlsx', sheetIndex = 1, startRow =1,
    #                   colIndex = seq(1,9), colClasses = c("numeric", rep("character",5), "numeric", 
    #                                                      "character", "character"))
    #dfAnswers <- read.xlsx('SGU_Science_or_Fiction.xlsx', sheetIndex = 1, startRow =1,
    #                    colIndex = seq(10,15))
    
    dfSGUData <- readxl::read_excel('SGU_Science_or_Fiction.xlsx', sheet = 1 )
    
    dfEpisodes <- dfSGUData[, 1:9]
    dfEpisodes <- dfEpisodes[complete.cases(dfEpisodes$Episode),]
    
    dfGuesses <- dfSGUData[ , c(1, 10:15)]
    featureColNames <- names(dfGuesses)[2:7]
    dfGuesses <- melt(dfGuesses, id = "Episode", measure.vars = featureColNames,
                            variable.name = "Panelist", value.name = "Guess")
    dfGuesses <- dfGuesses[complete.cases(dfGuesses), ]
    print(names(dfEpisodes))
    
    dfCorrectAns <- select(dfEpisodes, Episode, FictionItem)
    
    dfGuesses <- merge(dfGuesses, dfCorrectAns, by.x="Episode", by.y="Episode", all=TRUE)
    
    dfGuesses <- mutate(dfGuesses, isCorrect = (Guess == FictionItem))
    
    dfPerformance <- group_by(dfGuesses, factor(Panelist))
    print(arrange(summarize(dfPerformance, wins = sum(isCorrect==TRUE), 
                            guesses = n(), winPct = wins/guesses), desc(winPct)))
    
    #xlsx_example <- readxl_example('SGU_Science_or_Fiction.xlsx')
    #read_excel(xlsx_example)
    
    #dfEpisodes <- read_excel('SGU_Science_or_Fiction.xlsx', sheet =1)
    return(dfGuesses)
}