
library(readxl)
library(dplyr)
library(reshape2)
library(rvest)
library(qdapRegex)
library(stringr)
    
scrapeAnswerText <- function() {
    url <- "https://www.theskepticsguide.org/podcast/sgu/1000"
    webpage <- read_html(url)
   
    #Using CSS selectors to scrap the rankings section
    item_html <- html_nodes(webpage, xpath = "//div[@class='podcast-segment' and contains(., 'Science or Fiction')]/child::*")
    
    #item_html <- html_nodes(webpage, xpath=".//div[contains(., 'Kod umowy:') and contains(@class, 'col-sm-3')]/following-sibling::div[2]") %>% 
        #html_text()
    
    #Converting  to text
    item_data <- html_text(item_html)
    
    #Remove any urls
    item_data <- rm_url(item_data)
    
    #Remove section heading 'Science or Fiction'
    question_block <- toString(item_data[2])
    
    #Split into individual questions
    questions <- str_split(question_block, "Item #. ")
    
    #Remove first item from list (always blank) as a consequence of split
    questions <- questions[[1]][-1]
    
    # Remove the Science and Fiction identifiers
    mysub <- function(question_text) {
        tmp <- sub("^Fiction ", "", question_text)
        tmp <- sub("^Science ", "", tmp)
        return(tmp)
    }
    
    questions <- unlist(lapply(questions, mysub ))
    print(questions)
}

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
    pathtofile <- file.path(".",'SGU_Science_or_Fiction.xlsx')
    Rogues <- c("Steve", "Bob", "Evan", "Cara", "Jay")
    Episode_range = 652:697
    
    dfEpisodeData <- readxl::read_excel(pathtofile, sheet = 1 )
    dfEpisodeData <- as.data.frame(dfEpisodeData)
    dfEpisodeData <- select(dfEpisodeData, 1:8) %>% filter(Episode %in% Episode_range)
    
    
    dfItemsSelected <- readxl::read_excel(pathtofile, sheet =2)
    dfItemsSelected <- as.data.frame(dfItemsSelected)
    dfItemsSelected <- select(dfItemsSelected, c('Episode', 'Panelist', 'ItemSelected', 'AnsweringOrder'))
    dfItemsSelected <- filter(dfItemsSelected, Episode %in% Episode_range)
    
    
    dfFictionItems <- select(dfEpisodeData, c('Episode', 'FictionItem', 'Theme'))
    dfItemsSelected <- inner_join(dfItemsSelected, dfFictionItems, by='Episode')
    dfItemsSelected$Correct <- mapply(ifelse, test = (dfItemsSelected$ItemSelected == dfItemsSelected$FictionItem), yes = 1, no = 0 )
    dfItemsSelected$Theme <- mapply(ifelse, test = !is.na(dfItemsSelected$Theme), yes = 'With Theme', no = 'Without Theme')
    dfPanelistPerf <- dfItemsSelected %>% select(Episode, AnsweringOrder, Correct, Panelist, Theme) %>% filter(Panelist %in% Rogues) 
    dfOverallPerf <- dfItemsSelected %>% select(Episode, AnsweringOrder, Correct, Panelist, Theme) # %>% filter(Panelist %in% Rogues)
    
    
    dfDuoPerf <- inner_join(dfItemsSelected, dfItemsSelected, by='Episode')
    View(dfDuoPerf)
    dfDuo <- dfDuoPerf %>% filter(Panelist.x %in% Rogues) %>% filter(Panelist.y %in% Rogues) %>% group_by(Panelist.x, Panelist.y, ItemSelected.x == ItemSelected.y) %>% summarise('Percent Correct' = 100*sum(Correct.x)/n())
   
    dfDuo <-  rename(dfDuo, Rogue = Panelist.x) #, Peer == Panelist.y, Agree == 'ItemSelected.x == ItemSelected.y', 'Percent Correct' = pctCorrect) # names(dfDuo)<-c('Rogue', 'Peer', 'Agree', 'Percent Correct')
    dfDuo <-  rename(dfDuo, Peer = Panelist.y)
    dfDuo <-  rename(dfDuo, Agree = 'ItemSelected.x == ItemSelected.y')
    View(dfDuo)
    

    dfDuoWide <- dfDuoWide %>% filter(Agree==FALSE) %>% dcast( Rogue ~ Peer , value.var ='Percent Correct')
    View(dfDuoWide)
    
    # Append number correct for each episode
    dfCorrect <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% summarise(CorrectAnswers = sum(Correct)))
    dfEpisodeData <- inner_join(dfEpisodeData, dfCorrect, by='Episode')
    
    # Append panelist who answered first for each episode
    dfFirstPanelist <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% filter(AnsweringOrder==1) %>% select(Episode, FirstPanelist = Panelist))
    dfEpisodeData <- inner_join(dfEpisodeData, dfFirstPanelist, by='Episode')
    
    # Append number of panelists for each episode
    dfTotalPanelists <- as.data.frame(dfItemsSelected %>% group_by(Episode) %>% summarise(TotalPanelists = n()) %>% select(Episode, TotalPanelists))
    dfEpisodeData <- inner_join(dfEpisodeData, dfTotalPanelists, by='Episode')
    
    # Overall Performance
    print(dfPanelistPerf %>% summarise(pctAnsweringFirst = sum(Correct)/n()))
    stat <- dfPanelistPerf %>% summarise(pctAnsweringFirst = sum(Correct)/n())
    print(stat['pctAnsweringFirst']*100)
    #print(percent(stat['pctAnsweringFirst']))
    
    # Panelist Performance When Answering First
    dfPanelistPerf %>% filter(AnsweringOrder==1) %>% summarise(pctAnsweringFirst = sum(Correct)/n())
    # Panelist Performance with Themes
    dfPanelistPerf %>% filter(Episode %in% dfEpisodeData[!is.na(dfEpisodeData$Theme), 'Episode']) %>% summarise(pctThemes = sum(Correct)/n())
    # Panelist Performance without Themes
    dfPanelistPerf %>% filter(Episode %in% dfEpisodeData[is.na(dfEpisodeData$Theme), 'Episode']) %>% summarise(pctThemes = sum(Correct)/n())
    # Panelist Performance When Answering First
    dfEpisodeData %>% group_by(FirstPanelist) %>% summarise(PanelPerformance = sum(CorrectAnswers)/sum(TotalPanelists))
    # Longest Winning Streak
    wins = list()
    losses = list()
    for (rogue in Rogues) {
        df <- dfItemsSelected %>% filter(Panelist == rogue)
        x = rle(df$Correct)
        wins[rogue] = max(x$lengths[x$values == 1])
        losses[rogue] = max(x$lengths[x$values == 0])
    }
    dfConsecWins <- t(as.data.frame(wins))
    dfConsecLosses <- t(as.data.frame(losses))

   #Panelist Sweeps
    print(dfEpisodeData[dfEpisodeData$CorrectAnswers == dfEpisodeData$TotalPanelists,"Episode"])
    print(dfItemsSelected[dfItemsSelected$Episode %in% dfEpisodeData[dfEpisodeData$CorrectAnswers == dfEpisodeData$TotalPanelists,"Episode"],])
}  
    