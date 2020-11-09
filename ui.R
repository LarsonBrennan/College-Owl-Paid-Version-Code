library(firebase)
library(shiny)
library(devtools)
library(data.table)
library(dplyr)
library(rdrop2)
library(readr)
library(tidytext)
library(tidyr)
library(textdata)
library(ggplot2)
library(tidyverse)
library(quanteda)
library(SentimentAnalysis)
library(igraph)
library(ggraph)
library(plotly)
library(httr)
library(formattable)
library(stringi)
library(memoise)
library(widyr)
library(topicmodels)
library(syuzhet)
library(LDAvis)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(stringr)
library(topicmodels)
library(tokenizers)
library(readtext)
library(languageR)
library(zipfR)
library(shinyWidgets)
library(twitteR)
library(grid)
library(gridExtra)
library(shinyjs)
options(scipen = 999)


fluidPage(
  useShinyjs(),
  theme = "bootstrap.min.css",
  tags$head(HTML("<script type='text/javascript'>document.addEventListener('contextmenu', function(e){
                 e.preventDefault();
                 }, false)</script>")),
  includeScript("www/disable.js"),
  usePaddle(),
  useFirebase(),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(h5("1st: LOGIN"),useFirebaseUI()),
        tabPanel(h5("2nd: SUBSCRIBE"), h5(uiOutput("subscriber"))),
        tabPanel(h5("3rd: LOGOUT"), h5(uiOutput("user_out")))
      ),

      htmlOutput("school_selector", style="color:#FFA319;font-size:125%"),
      htmlOutput("state_selector", style="color:#FFA319;font-size:125%"),
      htmlOutput("county_selector", style="color:#FFA319;font-size:125%"),
      fileInput("file1", em(h4("CLICK HERE TO USE PDF"), style="color:#FFA319;font-size:125%"),
                multiple = FALSE,
                accept = c(".pdf")),
      textAreaInput("text", em(h4("ESSAY INPUT:"), style="text-align:center;color:#FFA319;font-size:100%"),  width = "225px", height = "600px")
    ),
    mainPanel(



      tabsetPanel(
        tabPanel(h5("School Common Words"), br(plotOutput("universitymostUsedSingleWordOutput")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This graph shows the most used words from the school. On the left side are the most
                         used words, and on the bottom of the chart is the total frequency of each word. Use this chart
                         to see what words are commonly associated with the school. Use this information to see if some of the
                         commonly used words, and ideas related to those words should be included in your essay. This information will
                         help you in writing your essay by giving you a better idea of what the popular words and ideas the school uses,
                         so you can use that information to make your essay sound more knowledeable with respect to the school.",style="font-size:125%")))),
        tabPanel(h5("Unique Words From a School"), br(plotOutput("universityTFIDFSingleWord")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: The graph is displaying a weighted score of the unique but frequently used words or phrases from the school, and how much they relate to the school.
                         On the left side going up are the uncommon unique words or phrases, and on the bottom is the weighted score
                         of the word or phrase. The higher the score is on the bottom, the more it is related to the school. This chart should be used to see what commonly used unique words from the school are,
                         so you can get an idea if they or the ideas they are related to should be incorporated in your essay. By knowing more
                         unique words affiliated with the school, you can use more obscure words or ideas that the school is affiliated with
                         so you sound like a good potential contributing member to the culture of the school.",style="font-size:125%")))),
        tabPanel(h5("School Word Associations"), br(plotOutput("universityBigrams")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This chart is showing common word associations used by the school. On the left side are the word
                         associations, and on the bottom is the strength of the word associations. The further to the right a score is on the
                         hoizontal axis, the stronger the association. Use this information to see if you should include some of the unique
                         word associations, or the ideas they are related to in your essay. This will help you sound like a good fit for the school,
                         and shows that you have done research on the school as well - which in turns makes you a better candidate for getting admitted.",style="font-size:125%")))),
        tabPanel(h5("School Text Categories"), br(plotOutput("universityTopicModel")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This feature groups the words used by the school into two categories. Match the factor(topic) number on the right side with '1' or '2' on the two graphs to the left; this will help you differentiate between the topics.
                         Commonly one graph displays either words associated with campus life or words asscoiated with research. The other graph usually displays
                         other words that the school is associated with, but are not directly affiliated with research or campus life.
                         On the left side of both graphs are the words, and on the bottom is a statistic used to determine how much the word
                         relates with the category. The higher the value, the more the word is associated with that category. It is suggested
                         to use this informaiton to see what general categories - like dorm life - are associated with the school, and write
                         about those categories, and include the words or ideas associated with the category - so you sound more informed about the school.",style="font-size:125%")))),
        tabPanel(h5("School Common Positive and Negative Words"), br(plotOutput("universityPosAndNegWords")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: The two charts being displayed show the most commonly used positive and negative words. The negative words
                         are shown as black bars on the left chart, and the positive words are shown as orange bars on the right chart.
                         For both charts the most used words are on the left side of the graphs, and the frequency of each word is on the
                         bottom. It is suggested to use this information to see what positive and negative words the school is using
                         in their text data. It is okay to use the negative words from the data, if it serves a purpose related to the school.
                         Using the positive words from the school will help you diversify your essay with positive words that the school frequently uses. It also helps create a tone that is similar with the tone that the school portrays itself as through their text data.",style="font-size:125%")))),


        tabPanel(h5("Sentiment of Words From a School"), br(plotOutput("universitySentimentHistogramAggregate")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: The graph is showing the overall sentiment of the message that the school is showing to the public. A negative sentiment
                         will have negative score, and positive sentiment will have a positive score. Most school have sentiment scores around 0,
                         which indicates neutral sentiment. Sentiment scores above 0.5 show very high positivity in their message, and scores below 0.5
                         indicate very negative messages. The density along the left side of the graph shows the occurence of each sentiment score. The goal
                         should be to try and match the sentiment of the school through one's essay, or exceed the positive sentiment of the school.
                         Doing this will help your essay fit the the general mood for what the school wants to portray itself as to the public, and will
                         make you sound like someone that would match the social environment of the school. You can also use this information to help match the tone of the school in your essay.",style="font-size:125%")))),
        tabPanel(h5("Readability of Words From a School"), br(plotOutput("universityreadabilitygraph")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This chart displays the overall readability of the promotion materials a school uses. Scores below 50 indicate more reading difficulty, while scores above 50 indicate easier readability. On the chart itself, the density along the left side is just the frequency of the
                         readability scores along the bottom part of the graph. The goal should be to match the readability graph of
                         the school one is applying to, or have higher readability. Doing this will help your essay be easier to read for the target audience - the school.",style="font-size:125%"))))
                 ),


      tabsetPanel(
        br(h5("INFORMATION IF RED TEXT OCCURS: If no essay data is input, then words will be displayed in red showing that essay data needs to be input.",style="color:#FFA319")),
        tabPanel(h5("Essay Common Words"), br(plotOutput("mostUsedSingleWordOutput")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This chart shows the most commonly used words in a provided essay. By seeing what the most used words are in your essay you
                         can see if you essay is sounding the same because of the overuse of certain words. The graph is also helpful in seeing if there is a general theme occuring within your essay. On the
                         vertical axis are the most commonly found words, and on the horizontal axis are the frequencies of the words.",style="font-size:125%")))),
        tabPanel(h5("Unique Words From Essay"), br(plotOutput("tfidfSingleWordOutput")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: The graph above displays the commonly used unique key words in your essay, via a weighted statistic. The graph also helps see if these more unique words are contributing to a unique idea that you are trying to convey in your essay.
                         On the vertical axis are the unique key words found in the essay, and on the horizontal axis are the statistics used to determine just how key the unique words are in your document. The higher the number the more key the word is.",style="font-size:125%")))),
        tabPanel(h5("Essay Word Associations"), br(plotOutput("essayBigrams")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: The 'Essay Word Associations' chart shows unique word associations used in your essay.  The chart also helps to see if you are overusing phrases in the essay, and if these word associations
                         are contributing to the general idea you want your essay to convey. On the vertical axis are the word associations, and on the horizontal axis are numbers indicating the strength of the associations with in the essay. The bigger the metric, higher is the association between the two words.",style="font-size:125%")))),
        tabPanel(h5("Essay Common Positive and Negative Words"), br(plotOutput("essayPosAndNegWords")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This graph displays the most commonly used positive and negative words in the essay. Try to minimize negative words to create
                         a more positive sounding essay, and maximize a wide variety of positive words to make your essay sound diverse and favorable.",style="font-size:125%")))),
        tabPanel(h5("Sentiment of Words From Essay: Graph"), br(plotOutput("sentimentHistogramAggregate")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This graph displays the overall senitment of your essay. Generally what one wants is to have a graph that is more centered above
                         0 on the horizontal axis, which will indicate a more positive sounding essay. Most essays have neutral sentiment centered around
                         0 on the horizontal axis - so it is best to either match that central tendency or be to the right of it above 0. The goal is to have the essay sound positive in tone.",style="font-size:125%")))),
        tabPanel(h5("Sentiment of Words From Essay: Table"),  br(br(p("INFORMATION ABOUT THE TABLE: The table below shows the sentiment score of each sentence in the essay. If the sentiment score for a given
                                                                      sentence is less than 0, then orange is used to highlight that the sentence sounds negative in tone. If the score is at or
                                                                      above 0, then the score will be displayed in black. The goal to have less negative sounding sentences, thus having
                                                                      sentence scores above or at 0 is suggested.",style="font-size:125%"))), br(div(formattableOutput("sentimentTable"),style="font-size:125%"))),
        tabPanel(h5("Essay Alternative Sentiments: Graph"), br(plotOutput("alternativeSentimentGraph")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This graph displays the alternatives emotions found in the essay from the other sentiment graphs and table. It offers more
                         sentiments that can show very tones for how your essay reads. Use the chart to see if there any other emotions that you can look to include or get rid of in your essay.",style="font-size:125%")))),

        tabPanel(h5("Readability of Words From Essay: Graph"), br(plotOutput("readabilitygraph")),
                 br(br(p("INFORMATION ABOUT THE GRAPH: This graph shows the overall readability of the essay. What one usually wants is to have the central tendency of the readability around 50 on the horizontal axis,
                         or be above that score. The higher the score, the more readable the essay is.",style="font-size:125%")))),
        tabPanel(h5("Readability of Words From Essay: Table"),  br(br(p("INFORMATION ABOUT THE TABLE: The table below shows the readability of each sentence in the essay. If the essay has a readability score less than 50 the score will
                                                                        be highlighted in orange, and if it is above 50 it will be displayed in black. This feature is to highlight when there are potentially difficult
                                                                        sentences to read.",style="font-size:125%"))), br(div(formattableOutput("dataReadability"),style="font-size:125%"))),
        tabPanel(h5("Repeated Words: Table"), br(br(p("This table shows the range of lexical diversity throughout the essay among each sentence. What we are looking to see is that words are not being repeated in each sentence of the essay often.
                                                      A score of 1 will indicate that no words are being repeated in each sentence of the essay, while a score less than 1 will indicate that there are some words being repeated. The goal should be to
                                                      not repeat words very often in the essay, so the essay does not sound the same.",style="font-size:125%"))), br(div(formattableOutput("lexicalDiversity"),style="font-size:125%"))),
        tabPanel(h5("Repeated Words: Graph"), br(plotOutput("lexicalDiversityBoxAndWhis")), br(br(p("This graph shows the range of lexical diversity occuring throughout the essay. It uses the scores from the sentences in the 'Repeated Words:Table'
                                                                                                    tool and combines them into one chart so you can see where the outlier scores are. The scores are represented by a black dot,
                                                                                                    with a bigger dot indicating more high scores of the same type, and a smaller dot representing smaller scores of a similar type. The goal should be to have most scores equal to 1 so the sentences
                                                                                                    do not have many repeated words.",style="font-size:125%")))),
        tabPanel(h5("Repeated Sentences: Graph"), br(plotOutput("essayHeatMap")), br(br(p("The above chart provides a way to see if sentences throughout the essay are correlated with each other, and thus sound similar. If no color is shown on the chart, it means that there are no sentences that sound similar. The tool uses a correlation of 0.3 or higher to let you know when
                                                                                          sentences sound similar. If the correlation is higher than 0.3, then the sentences correlated with each other will be displayed on both the horizontal and vertical axes. On both the horizontal and vertical axes there will be text stating 'text1.' with a number following, those are sentences that are correlated with each other.
                                                                                          Take the numbers from horizontal and vertical axes that are correlated with each other and go to to the 'Repeated Sentence: Sentence Number' tab on the page to find the sentences to see the similarity, and what can be done to make them less similar sounding.",style="font-size:125%")))),
        tabPanel(h5("Repeated Sentence: Sentence Number"), br(br(p("INFORMATION ABOUT THE TABLE: The table below shows the sentences in the essay, and assigns an ascending value to each sentence. This table is to be used with the 'Repeated Sentences Graph'.",style="font-size:125%"))), br(div(formattableOutput("tableSentenceNumber"),style="font-size:125%")))


        )

        )
                 )

        )

