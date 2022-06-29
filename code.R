
library(tidiverse)

actas.web <- c("https://8cfe.congresoforestal.es/es/actas",
               paste0("https://8cfe.congresoforestal.es/es/actas?page=",1:20))

titles.CFE8 <- c(NULL)

for(a in 1:length(actas.web)){
  
  actas <- rvest::read_html(actas.web[a])
  
  page <- 
    actas %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  
  positions <- page %>%
    str_detect(".pdf")
  
  titles <- page[positions]
  
  titles.CFE8 <- c(titles.CFE8, titles)
  
  cat(a, " ")
}


keywords.CFE8 <- c(NULL)


for(i in 1:length(titles.CFE8)){

  tryCatch({
    
    pdf <- pdftools::pdf_text(pdf = paste0("https://8cfe.congresoforestal.es/sites/default/files/actas/", 
                                           titles.CFE8[i]))
    
    keywords <- sub(".*Palabras clave\n", "", pdf[2])
    
    keywords <- sub("\n\n.*", "", keywords)
    
    keywords <- keywords %>%
      str_split("[:punct:]") %>%
      unlist() %>%
      str_squish() %>%
      str_to_title() %>%
      stringi::stri_remove_empty()
    
    keywords.CFE8 <- c(keywords.CFE8, keywords)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  cat(i, " ")
}

#save(keywords.CFE8, file = "C:/Users/alber/Desktop/CFE8_keywords/keywords.RData")

freq <- tibble(word = keywords.CFE8) %>%
  group_by(word) %>%
  count() %>%
  filter(!str_detect(word, "\\d")) %>%
  arrange(desc(n)) %>%
  mutate(word = str_replace(word, " ", "")) %>%
  ungroup()

wordcloud2::wordcloud2(data = freq %>% filter(n > 2), size=.4, shape = "diamond", ellipticity = 2)






