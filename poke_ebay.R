pacman::p_load(RSelenium,dplyr,rvest,lubridate,tidyverse)
chrome <-function(ip){
  remDr = remoteDriver(remoteServerAddr = ip, port = 4445L, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr
}
remDr <- chrome("138.68.229.207")

remDr$navigate("https://www.ebay.com/b/Pokemon-Trading-Card-Game-Cards-Merchandise/183466/bn_1904919")

suppressWarnings(remDr$findElement('xpath','/html/body/div[3]/div[3]/div[4]/section[7]/section/ul[1]/li[2]/div/button')$clickElement())
suppressWarnings(remDr$findElement('xpath','/html/body/div[3]/div[3]/div[4]/section[7]/section/ul[1]/li[2]/div/div/div/button')$clickElement())
poke_set_list <- data.frame(editions = tolower(remDr$getPageSource()[[1]] %>% read_html() %>% html_nodes(".x-refine__multi-select-label") %>% html_text()))
if(nrow(poke_set_list) == 0){
  remDr$findElement("class name",'x-see-all__container')$clickElement()
  Sys.sleep(1)
  remDr$findElement("id",'c3-mainPanel-Set')$clickElement()
  Sys.sleep(3)
  poke_set_list <- data.frame(editions = tolower(remDr$getPageSource()[[1]] %>% read_html() %>% html_nodes(".x-refine__multi-select-label") %>% html_text()))
}

remDr$navigate("https://www.ebay.com/b/10-Graded-PSA-Pokemon-Individual-Cards-in-English/2611/bn_93430953?rt=nc&LH_Complete=1&LH_Sold=1&listingOnly=1")



poke_html <- remDr$getPageSource()[[1]]%>% read_html()
poke_sales_dates <- mdy(gsub("SOLD  ","",poke_html %>% html_nodes(".s-item__title-tag") %>% html_text()))
poke_title_line <- tolower(poke_html %>% html_nodes(".s-item__title--has-tags") %>% html_text())
poke_sales_price <- as.numeric(gsub("^\\$","",poke_html %>% html_nodes(".s-item__price") %>% html_text())) + coalesce(as.numeric(gsub("free","",gsub(" shipping","",gsub("^\\$","",tolower(poke_html %>% html_nodes(".s-item__logisticsCost") %>% html_text()))))),0)
poke_bind <- data.frame(title = poke_title_line,price = poke_sales_price,dos = poke_sales_dates) %>%
  mutate(grader = "psa",grade = 10 )

poke_bind <- poke_bind %>% mutate(title = gsub("10(\\s*)psa(\\s*)","",gsub("  tcg","",gsub(" htf","",gsub(" 💎","",gsub(" -\\s*"," ",gsub("\\swotc","",gsub(" gem\\s*mt(\\.)*","",gsub("(^|\\s)psa(\\s|\\-)10","",gsub("\\s*pok(e|é)mon(\\s*card)*","",gsub("\\s*gem\\s*mint","",title)))))))))) )
poke_bind <- poke_bind[!grepl("\\slot\\s",poke_bind$title),]

poke_bind_cleanse <- poke_bind %>% mutate(yor = trimws(str_extract(poke_bind$title,"\\s*\\d{4}\\s+")),
                                          title = gsub("\\s*\\d{4}\\s+","",title),
                                          card_number = str_extract(gsub("1st ","",poke_bind$title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
  
  #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
  mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(poke_bind$title)),"(\\[a-z]+\\d+)"),card_number)) %>%
  mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(poke_bind$title)),"(\\s\\#\\s*\\d+)"),card_number)) %>%
  mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(poke_bind$title)),"([a-z]{2}\\d+)"),card_number)) %>%
  mutate(card_number = str_extract(card_number,"\\d+")) %>%
  
  mutate(set_number = gsub("\\/","",trimws(str_extract(gsub("1st ","",poke_bind$title),"(\\/\\d+\\s*)"))) ) %>%
  
  #Rarity Section
  mutate(rarity = ifelse(grepl("secret rare",title)==T,"secret rare",
                         ifelse(grepl("ultra rare",title)==T,"ultra rare",
                                ifelse(grepl("holo",title)==T,"holofoil rare",
                                       ifelse(grepl("uncommon",title)==T,"uncommon",
                                              ifelse(grepl("\\s*full\\s*art\\s*",title)==T,"ultra rare",NA)))))) %>%
  
  #Edition (Do not name Set bc SQL will hate you) Below
  mutate(editions = "")

for(i in 1:nrow(poke_set_list)){
  for(q in 1:nrow(poke_bind_cleanse)){
    if(grepl(poke_set_list$editions[i],poke_bind_cleanse$title[q])==T){poke_bind_cleanse$editions[q] = poke_set_list$editions[i]}
    
    if((grepl("1999",poke_bind_cleanse$yor[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "base set"}
    
    if((grepl("unlimited",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "base set"}
    
    if((grepl("(^|\\s+)dark\\s+",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "team rocket"}
    
    if((grepl("rocket",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "team rocket"}
    
    if((grepl("sword\\s*(&|and)\\s*shield",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "sword & shield"}
    
    if((grepl("(sun\\s*(&|and)\\s*moon)|((^|\\s+)s\\&m\\s+)|(\\ssm\\s)",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "sun & moon"}
    
    if((grepl("champion(\\')*s\\s*path",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "champions path"}
      
    if((grepl("hid(d)*en\\s*fates",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] == "") ){poke_bind_cleanse$editions[q] = "hidden fates"}
    
    if( (grepl("1st edition",poke_bind_cleanse$title[q])==T) & (poke_bind_cleanse$editions[q] != "") & (grepl("[a-z]+\\sfirst edition",poke_bind_cleanse$editions[q])!=T) ){poke_bind_cleanse$editions[q] = paste(poke_bind_cleanse$editions[q],"first edition")}else if( (grepl("first edition",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] != "")  & (grepl("[a-z]+\\sfirst edition",poke_bind_cleanse$editions[q])!=T)){poke_bind_cleanse$editions[q] = paste(poke_bind_cleanse$editions[q],"first edition")}else if( (grepl("1st ed",poke_bind_cleanse$title[q])==T)  & (poke_bind_cleanse$editions[q] != "")  & (grepl("[a-z]+\\sfirst edition",poke_bind_cleanse$editions[q])!=T)){poke_bind_cleanse$editions[q] = paste(poke_bind_cleanse$editions[q],"first edition")}
    
    
    if(poke_bind_cleanse$editions[q] == "base set"){
        if((grepl("shadowless",poke_bind_cleanse$title[q])==T) & (grepl("[a-z]+\\sshadowless",poke_bind_cleanse$editions[q])!=T) ){poke_bind_cleanse$editions[q] = paste(poke_bind_cleanse$editions[q],"shadowless")}
      }
  }
}

poke_bind_cleanse <- poke_bind_cleanse %>% mutate(title = (title %>% gsub("\\." , '',.) %>%
                                           gsub("\\(.*\\)"  , '',.) %>%
                                           gsub("holo", '',.) %>%
                                           gsub("psa", '',.) %>%
                                           gsub("full art" , '',.) %>%
                                           gsub("\\/" , '',.) %>%
                                           gsub("ultra" , '',.) %>%
                                           gsub("secret" , '',.) %>%
                                           gsub("holofoil" , '',.) %>%
                                           gsub("[a-z]{2}\\\\[a-z]{2}" , '',.) %>%
                                           gsub("\\:" , '',.) %>%
                                           gsub("\\*" , '',.) %>%
                                           gsub("\\!" , '',.) %>%
                                           gsub("\\s*[0-9]+\\s*" , '',.) %>%
                                           gsub("\\#\\d*" , '',.) %>%
                                           gsub("st edition" , '',.) %>%
                                           gsub("mp","",.)%>%
                                        gsub("foil","",.)%>%
                                        gsub("\\s+set","",.)%>%
                                        gsub("rare","",.)%>%
                                        gsub("game","",.)%>%
                                        gsub("lp","",.)%>%
                                        gsub("nm","",.)%>%
                                        gsub("hp","",.)%>%
                                        gsub("vmax","",.)))

for(i in 1:nrow(poke_set_list)){
  for(q in 1:nrow(poke_bind_cleanse)){
    if(grepl(poke_set_list$editions[i],poke_bind_cleanse$title[q])==T){poke_bind_cleanse$title[q] = gsub(poke_set_list$editions[i],"  ",poke_bind_cleanse$title[q])}
  }
}


pokemon_pagination <- rbind(pokemon_pagination,poke_bind_cleanse)
