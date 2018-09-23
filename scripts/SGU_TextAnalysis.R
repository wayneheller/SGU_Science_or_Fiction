###############################################################################
#                                                                             #
# Text Analysis Code for Science or Fiction.  Includes:                       #
# Webscraping of question text from www.theskepticsguide.org                  #
# 9/16/18                                                                     #
###############################################################################

# 
# 1) If output file exists, read the last episode retrieved, if not create it
# 2) Loop through all episodes of interest.  Starting with 600 when I began 
# recording results
# 3) test if webpage exists, if not exit
# 4) scrap page for question text
# 5 Append to existing file
# 6 Repeat
# 7 Close file
# 8 Run some sanity checks for dupes

SGUQuestionFile <- "SGU_SorF_QuestionText.csv"
EpisodesWithoutSorF <- c(628, 642)
    
# Append item of questions file
appendQuestion <- function(questionfilename, episodeNumber, itemNumber, question_type, question_text) {
    if (file.exists(questionfilename)) {
        appends = TRUE
    }
    else { appends = FALSE
    }
    
    #conn <- file(questionfilename, open = openmode, encoding = "UTF-8" )
    x <- NULL
    x$EpisodeNumber <- episodeNumber
    x$ItemNumber <- itemNumber
    x$ScienceOrFiction <- question_type
    x$QuestionText <- question_text
    print(appends)
    write.table(x, questionfilename, append = appends, col.names  = !appends, sep=",", quote = TRUE, row.names = FALSE)
    
}

getLastEpisode <- function(questionfilename){
    conn <- file(questionfilename, open = "r") 
    lastLine <- readLines(conn, n = -1, warn = TRUE, skipNul = TRUE)
    lastEpisode <- str_split(lastLine[length(lastLine)], ",")
    close(conn)
    return(as.integer(lastEpisode[[1]][1]))
}

getEpisodeUrl <- function(EpisodeNumber) {
    return(paste0("https://www.theskepticsguide.org/podcast/sgu/", EpisodeNumber))
}

# Remove the Science and Fiction identifiers
rmScienceFiction <- function(question) {
    if (grepl("^Fiction", question)) {
        question_text <- sub("^Fiction ", "", question)
        question_type <- "Fiction"
    }
    else if (grepl("^Science", question)) {
        question_text <- sub("^Science ", "", question)
        question_type <- "Science"
    }
    else {
        question_text <- question
        question_type <- "Unknown"
    }
    return(c(question_type, question_text))
}

scrapeEpisodePage <- function(EpisodeNumber) {
    
    url <- getEpisodeUrl(EpisodeNumber)
    
    # Read SGU Show Notes Page
    webpage <- read_html(url)
    
    #Using CSS selectors to scrap the rankings section
    item_html <- html_nodes(webpage, xpath = "//div[@class='podcast-segment' and contains(., 'Science or Fiction')]/child::*")
    
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
    
    #questions <- unlist(lapply(questions, rmScienceFiction ))

    #questions <- unlist(lapply(questions, rmScienceFiction))
    
    return(questions)
}

refreshQuestionFile <- function() {
    if (file.exists(SGUQuestionFile)) {
        episodeNumber = getLastEpisode(SGUQuestionFile) + 1
    }
    else {episodeNumber <- 600
    }
    print(episodeNumber)
    if (episodeNumber %in% EpisodesWithoutSorF) {episodeNumber <- episodeNumber + 1}
    questions <- scrapeEpisodePage(episodeNumber)
    while (!is.null(questions)) {
        itemNumber <- 1
        for (question in questions) {
            parsedQuestion <- rmScienceFiction(question)
            appendQuestion(SGUQuestionFile, episodeNumber, itemNumber, parsedQuestion[1], parsedQuestion[2])
            itemNumber <- itemNumber + 1
        }
        episodeNumber <- episodeNumber + 1
        
        if (episodeNumber %in% EpisodesWithoutSorF) {episodeNumber <- episodeNumber + 1}
        
        questions <- scrapeEpisodePage(episodeNumber)
        
        if (episodeNumber > 610)  {break}
    }
    checkQuestionFile()
}

checkQuestionFile <- function() {
    dfQuestions <- read.csv(SGUQuestionFile)
    dfCounts <- dfQuestions %>% group_by(EpisodeNumber) %>% summarise(itemCount = n()) %>% arrange(desc(itemCount))
    print("Check for Episodes with More than 4 Questions...")
    print(dfCounts[dfCounts$itemCount > 4, ])
    print("Check for Episodes with Less than 3 Questions...")
    print(dfCounts[dfCounts$itemCount < 3, ])
}