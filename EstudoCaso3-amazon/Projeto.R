# Mini-Projeto 1 - Analisando Reviews na Amazon.com e Criando App com Shiny

# Carregando os pacotes (pode ser necessário instalar alguns desses pacotes)
library(shiny)
library(magrittr)
library(ggplot2)
library(lattice)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(NLP)
library(tidyr)
library(shinythemes)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(tidytext)
library(DT)
library(plyr)



# Definindo UI com o shiny

ui <- fluidPage(
  
  theme=shinytheme("slate"),
  
  titlePanel("Faça Upload do Arquivo com as Reviews"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      # Carregando um arquivo
      fileInput("file", "Selecione um Arquivo CSV",options(shiny.maxRequestSize=200*1024^2),
                multiple = TRUE),
      
      tags$hr(),
      
      # Header
      checkboxInput("header", "Header", TRUE),
      
      # Separador
      radioButtons("sep", "Separador",
                   choices = c(Vírgula = ",",
                               PontoeVírgula = ";",
                               Tab = "\t"),
                   selected = ","),
      
      tags$hr(),
      
      # Entrada: Selecione o número de linhas para exibir 
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Painel Principal
    mainPanel(
      
      
      tabsetPanel(
        tabPanel("Dados",
                 h4("Tabela"),
                 tableOutput("first")
        ),
        tabPanel("Reviews Após Limpeza",DT::dataTableOutput("contents"),options = list(scrollX = TRUE)),
        tabPanel(" Contagem de Termos / Word Cloud",
                 tabsetPanel(
                   tabPanel("Tabela Inicial",  DT::renderDataTable("Freq")),
                   tabPanel("Word Cloud Inicial", plotOutput("plot")),
                   tabPanel("Tabela Após a Limpeza", tableOutput("Freq2")),
                   tabPanel("Word Cloud Após a Limpeza", plotOutput("plot2")))
        ),
        
        
        tabPanel("Sentiment Scores", tabsetPanel(
          tabPanel("Sentiment Scores",tableOutput("sentiment")))),
        
        tabPanel(" Tabela de Produtos", tabsetPanel(
          tabPanel("Número de Usuários e Scores Médios",tableOutput("tab1")),
          tabPanel("Top 6 Mais Avaliados",tableOutput("tab2")))),
        tabPanel("Scatter plots",tableOutput("scp"))
        
        
        
      )
      
    )
    
  )
)

server <- function(input, output) {
  output$first <- renderTable({
    
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  output$contents <-  DT::renderDataTable({
    
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    temp <- data.frame(text=sapply(corpus.clean, identity), 
                       stringsAsFactors=F)
    df$Text = temp
    names(df)[10]<-paste("Review")
    
    subdf <- df[,c(1:3,10)]
    
    return(subdf) 
  })
  
  output$plot <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    dtm <- DocumentTermMatrix(corpus)
    dtm = removeSparseTerms(dtm, 0.99)
    findFreqTerms(dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(8, "Dark2"), 
              random.color=TRUE)
  })
  
  output$Freq <-  DT::renderDataTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    # Tabela 1
    corpus <- Corpus(VectorSource(df$Text))
    
    dtm<- DocumentTermMatrix(corpus)
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    table <- data.frame(word=names(freq), freq=freq)   
    return(table)
    
  })
  output$plot2 <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    # Removendo pontuação, números, palavras irrelevantes e espaços em branco
    corpus.clean <- corpus %>%    
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    dtm = removeSparseTerms(dtm, 0.99)
    findFreqTerms(dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(8, "Dark2"), 
              random.color=TRUE)
  })
  output$Freq2 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    dtm = removeSparseTerms(dtm, 0.99)
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
    table <- data.frame(word=names(freq), freq=freq)   
    return(table)
    
  })
  
  # Análise de sentimentos
  output$sentiment <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    # Removendo pontuação, números, palavras irrelevantes e espaços em branco
    corpus.clean <- corpus %>%  
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positivo",ifelse(dfscores$Sentiment_score< 0, "Negativo",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
  })
  
  output$tab1 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    # Removendo pontuação, números, palavras irrelevantes e espaços em branco
    corpus.clean <- corpus %>%  
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positivo",ifelse(dfscores$Sentiment_score< 0, "Negativo",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    return(tab1)
  })
  output$tab2 <- renderTable({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    # Removendo pontuação, números, palavras irrelevantes e espaços em branco
    corpus.clean <- corpus %>%  
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positivo",ifelse(dfscores$Sentiment_score< 0, "Negativo",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    tab2<-head(arrange(tab1,desc(count)), n = 6)
    return(tab2)
  })
  
  output$scp <- renderPlot({
    req(input$file)
    
    df <- read.csv(input$file$datapath,
                   header = input$header,
                   sep = input$sep)
    corpus <- Corpus(VectorSource(df$Text))
    
    # Removendo pontuação, números, palavras irrelevantes e espaços em branco
    corpus.clean <- corpus %>%  
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    dtm <- DocumentTermMatrix(corpus.clean)
    t <- Terms(dtm)
    t_count <- tidy(dtm)
    ap_sentiments <- t_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = df, y = b, by.x = "row.names", by.y="Id", all.x = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positivo",ifelse(dfscores$Sentiment_score< 0, "Negativo",NA))
    
    dfscores <- dfscores[complete.cases(dfscores), ]
    dfscores<-dfscores[,-1]
    tab1<-ddply(dfscores,c(.(ProductId)),summarise,count=length(UserId),mean=mean(Sentiment_score))
    tab2<-head(arrange(tab1,desc(count)), n = 6)
    scd<-merge(x = dfscores, y = tab2, by="ProductId", all.y =TRUE)
    p<-ggplot(scd, aes(x= scd$Sentiment_score, y=scd$Score, color=scd$ProductId)) +
      geom_point(aes(color=ProductId)) +
      facet_wrap( ~ scd$ProductId) +
      ggtitle(" Sentiment Scores Vs Classificação do Usuário  ") +
      labs(x ="Sentiment Score", y = "Avaliação do Usuário", fill = "ProductID") +
      theme(
        plot.title = element_text(color="Blue", size=20, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    print(p)
  })
  
  output$text <- renderUI({
    str1 <- paste("Depois de normalizar os dados, podemos ver que o Wordcloud mostra termos muito importantes")
    str2 <- paste("Análise de Sentimento pode ser melhorada",
                  input$range[1], "to", input$range[2])
    HTML(paste(str1, str2, sep = '<br/>'))})
}


# Executando a App
shinyApp(ui = ui, server = server)