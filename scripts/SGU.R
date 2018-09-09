
library(readxl)
library(dplyr)
library(reshape2)
    


readSGUData_old <- function() {
    #dfEpisodes <- read.xlsx('SGU_Science_or_Fiction.xlsx', sheetIndex = 1, startRow =1,
    #                   colIndex = seq(1,9), colClasses = c("numeric", rep("character",5), "numeric", 
    #                                                      "character", "character"))
    #dfAnswers <- read.xlsx('SGU_Science_or_Fiction.xlsx', sheetIndex = 1, startRow =1,
    #                    colIndex = seq(10,15))
    
    dfSGUData <- readxl::read_excel('SGU_Science_or_Fiction.xlsx', sheet = 1 )
    
    dfEpisodes <- dfSGUData[, 1:9]
    dfEpisodes <- dfEpisodes[complete.cases(dfEpisodes$Episode),]
    
    dfGuesses <- dfSGUData[ , c(1, 10:16)]
    featureColNames <- names(dfGuesses)[2:8]
    dfGuesses <- melt(dfGuesses, id = "Episode", measure.vars = featureColNames,
                            variable.name = "Panelist", value.name = "Guess")
    dfGuesses <- dfGuesses[complete.cases(dfGuesses), ]
    print(names(dfGuesses))
    
    
    dfCorrectAns <- select(dfEpisodes, Episode, FictionItem, Theme)
    
    dfGuesses <- merge(dfGuesses, dfCorrectAns, by.x="Episode", by.y="Episode", all=TRUE)
    
    dfGuesses <- mutate(dfGuesses, isCorrect = (Guess == FictionItem))
    
    dfPerformance <- group_by(dfGuesses, factor(Panelist))
    
    print(arrange(summarize(dfPerformance, wins = sum(isCorrect==TRUE), 
                            guesses = n(), winPct = wins/guesses), desc(winPct)))
    
    print(arrange(summarize(filter(dfPerformance, !is.na(Theme)), wins = sum(isCorrect==TRUE), 
                            guesses = n(), winPct = wins/guesses), desc(winPct)))
    
    print(arrange(summarize(filter(dfPerformance, is.na(Theme)), wins = sum(isCorrect==TRUE), 
                            guesses = n(), winPct = wins/guesses), desc(winPct)))
    
    #xlsx_example <- readxl_example('SGU_Science_or_Fiction.xlsx')
    #read_excel(xlsx_example)
    
    #dfEpisodes <- read_excel('SGU_Science_or_Fiction.xlsx', sheet =1)
    return(dfGuesses)
}

readSGUSummary <- function() {

    dfSGUSummary <- readxl::read_excel('SGU_Science_or_Fiction.xlsx', sheet = 3 )
    dfSGUSummary <- as.data.frame(dfSGUSummary)
    rownames(dfSGUSummary) <- dfSGUSummary$Statistic
    dfSGUSummary <- select(dfSGUSummary, -Statistic)
    Rogues <- c("Steve", "Bob", "Evan", "Cara", "Jay")
    dfSGUSummary <- select(dfSGUSummary, c(Rogues, "ALL"))
    dfSGUSummary <- t(dfSGUSummary)
    
    return(dfSGUSummary)
    
}

calcSGUSummary <- function() {
    pathtofile <- file.path(getwd(),'SGU_Science_or_Fiction.xlsx')
    dfEpisodeData <- readxl::read_excel(pathtofile, sheet = 1 )
    dfEpisodeData <- as.data.frame(dfEpisodeData)
    dfEpisodeData <- select(dfEpisodeData, 1:8)
    
    
    dfItemsSelected <- readxl::read_excel(pathtofile, sheet =2)
    dfItemsSelected <- as.data.frame(dfItemsSelected)
    dfItemsSelected <- select(dfItemsSelected, c('Episode', 'Panelist', 'ItemSelected', 'AnsweringOrder'))
    
    #dfItemsSelected <- melt(dfEpisodeData, id.vars = 'Episode', measure.vars = 10:21, na.rm = TRUE, value.name = 'ItemSelected', variable.name = 'Panelist')
    dfFictionItems <- select(dfEpisodeData, c('Episode', 'FictionItem'))
    dfItemsSelected <- inner_join(dfItemsSelected, dfFictionItems, by='Episode')
    dfItemsSelected$Correct <- mapply(ifelse, test = (dfItemsSelected$ItemSelected == dfItemsSelected$FictionItem), yes = 1, no = 0 )
    
    # Append number correct for each episode
    dfCorrect <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% summarise(CorrectAnswers = sum(Correct)))
    dfEpisodeData <- inner_join(dfEpisodeData, dfCorrect, by='Episode')
    
    # Append panelist who answered first for each episode
    dfFirstPanelist <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% filter(AnsweringOrder==1) %>% select(Episode, FirstPanelist = Panelist))
    dfEpisodeData <- inner_join(dfEpisodeData, dfFirstPanelist, by='Episode')
    
    # Append number of panelists for each episode
    dfTotalPanelists <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% summarise(TotalPanelists = n()) %>% select(Episode, TotalPanelists))
    dfEpisodeData <- inner_join(dfEpisodeData, dfTotalPanelists, by='Episode')
    
    # Solo Wins
    dfSoloWins <- dfItemsSelected[dfItemsSelected$Episode %in% dfEpisodeData[dfEpisodeData$CorrectAnswers==1, 'Episode'] & dfItemsSelected$Correct==1, c('Episode', 'Panelist')]
    dfSoloWins %>% group_by(Panelist) %>% summarise(SoloWins = n())
    
    # Host Sweeps
    dfHostSweeps <- dfEpisodeData[dfEpisodeData$CorrectAnswers==0, c('Episode', 'Host')]
    dfHostSweeps %>% group_by(Host) %>% summarize(HostSweeps = n())
    
    dfPanelistPerf <- dfItemsSelected %>% select(Episode, AnsweringOrder, Correct, Panelist) %>% group_by(Panelist)
    # Overall Performance
    dfPanelistPerf %>% summarise(pctAnsweringFirst = sum(Correct)/n())
    # Panelist Performance When Answering First
    dfPanelistPerf %>% filter(AnsweringOrder==1) %>% summarise(pctAnsweringFirst = sum(Correct)/n())
    # Panelist Performance with Themes
    dfPanelistPerf %>% filter(Episode %in% dfEpisodeData[!is.na(dfEpisodeData$Theme), 'Episode']) %>% summarise(pctThemes = sum(Correct)/n())
    # Panelist Performance without Themes
    dfPanelistPerf %>% filter(Episode %in% dfEpisodeData[is.na(dfEpisodeData$Theme), 'Episode']) %>% summarise(pctThemes = sum(Correct)/n())
    # Panelist Performance When Answering First
    print(dfEpisodeData %>% group_by(FirstPanelist) %>% summarise(PanelPerformance = sum(CorrectAnswers)/sum(TotalPanelists)))
    View(dfEpisodeData)
    # Longest Winning Streak
    df <- dfItemsSelected %>% filter(Panelist =='Bob')
    
    # not working yet max(diff(cumsum(df$Correct), 1))
}  
    