pacman::p_load(bigrquery,tidyverse,httr,rvest)
invisible(pokemon_ebay_db <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "pokemon-ebay",
    billing = "pokemon-ebay"
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
})


sets <- read_html("https://cardmavin.com/pokemon/pokemon-card-set-symbols") %>% 
  html_node("table") %>% 
  html_table(fill = TRUE) %>% 
  select(X1,X2) %>% 
  mutate(X1 = tolower(gsub(" Series","",X1)),
         X2 = trimws((X2 %>% 
                 gsub("© ","",.) %>%
                 gsub("[A-Za-z]+","",.) %>%
                 gsub("&","",.) %>%
                 gsub("\\s*\\-\\s*\\d*","",.))) ) %>%
  distinct() %>%
  `colnames<-` (c("editions","year")) 

for(i in 1:nrow(sets)){
    if(sets$year[i]==""){sets$year[i] = sets$year[i+1]}
}

sets <- sets %>% arrange(year)

set_collection <- NULL
for(i in 1:nrow(sets)){
    
    url_link_input <- gsub(" ","-",sets$editions[i])
    
    url_link_input <- if(grepl("promos",url_link_input)){url_link_input}else if(grepl("base-set-2",url_link_input)){url_link_input}else{ paste(gsub("\\-set$","",url_link_input),"-set-list",sep="") }
  
    url = paste("https://cardmavin.com/pokemon/",url_link_input,sep="")
    if(grepl("aquapolis",url)){url = gsub("-set-list","-card-set",url)}
    if(grepl("southern-islands",url)){url = gsub("-set-list","",url)}
    if(grepl("ruby-and-sapphire",url)){url = gsub("-and","",url)}
    if(grepl("fire-red-and-leaf-green",url)){url = gsub("fire-red-and-leaf-green","firered-leafgreen",url)}
    if(grepl("ex-trainer-kit---latias-&-latios",url)){url = gsub("ex-trainer-kit---latias-&-latios-set-list","xy-trainer-kit-latias-and-latios",url)}
    if(grepl("\\/pop",url)){url = gsub("-set-list","",gsub("\\/pop-","\\/pop\\/pop-series-",url))}
    if(grepl("\\-\\-+",url)){url = gsub("-set-list","",gsub("&","and",gsub("\\-\\-+","\\-",url)))}
    if(grepl("trainer-kit-–-manaphy-&-lucario-set-list",url)){url = gsub("trainer-kit-–-manaphy-&-lucario-set-list","diamond-pearl\\/diamond-and-pearl-trainer-kit-manaphy-and-lucario",url)}
    if(grepl("trainer-kit-–-gyarados-&-raichu-set-list",url)){url = gsub("trainer-kit-–-gyarados-&-raichu-set-list","heartgold-soulsilver\\/heartgold-soulsilver-trainer-kit-gyarados-and-raichu",url)}
    if(grepl("heartgold-and-soulsilver",url)){url = gsub("heartgold-and-soulsilver","heartgold-soulsilver",url)}
    if(grepl("trainer-kit-–-excadrill-&-zoroark-set-list",url)){url = gsub("trainer-kit-–-excadrill-&-zoroark-set-list","black-white-trainer-kit-excadrill-and-zoroark",url)}
    if(grepl("xy\\-\\-+promos",url)){url = paste(gsub("\\-\\-+","\\-",url),"-set-list",sep="")}
    if(grepl("trainer-kit-–-bisharp-&-wigglytuff-set-list",url)){url = gsub("trainer-kit-–-bisharp-&-wigglytuff-set-list","xy-trainer-kit-bisharp-and-wigglytuff",url)}
    if(grepl("trainer-kit-–-sylveon-&-noivern-set-list",url)){url = gsub("trainer-kit-–-sylveon-&-noivern-set-list","xy-trainer-kit-sylveon-and-noivern",url)}
    if(grepl("trainer-kit-–-latias-&-latios-set-list",url)){url = gsub("trainer-kit-–-latias-&-latios-set-list","xy-trainer-kit-latias-and-latios",url)}
    if(grepl("trainer-kit-–-pikachu-libre-&-suicune-set-list",url)){url = gsub("trainer-kit-–-pikachu-libre-&-suicune-set-list","xy-trainer-kit-pikachu-libre-and-suicune",url)}
    if(grepl("trainer-kit-–-lycanroc-&-alolan-raichu-set-list",url)){url = gsub("trainer-kit-–-lycanroc-&-alolan-raichu-set-list","sun-moon-trainer-kit-lycanroc-and-alolan-raichu",url)}
    
    
    inspection_approved <- "yes"
    
    if(tryCatch({url %>% read_html()} ,error = function(e){"no"}) == "no"){inspection_approved = "no"}
    
    if(inspection_approved == "yes"){
    card_names <- url %>% 
      read_html() %>% 
      html_nodes("td.column-4") %>% 
      html_text()
    
    legend_set_removal <- tryCatch({if(url %>% 
                             read_html() %>% 
                             html_nodes("table")%>% .[2] %>% 
                             html_table(fill = T) %>% as.data.frame() %>% ncol() > 1){url %>% 
      read_html() %>% 
      html_nodes("table")%>% .[2] %>% 
      html_table(fill = T) %>% as.data.frame() %>% select(X2)%>% 
      mutate(X2 = (X2 %>% gsub("Pokemon card rarity symbol","", .))) %>%
      mutate(X2 = str_split(X2,"\n")) %>% unnest(X2) %>% nrow()}else{0}
    } ,error = function(e){0})
    
    rarity <- tryCatch({url %>% read_html() %>% 
      html_nodes("svg") %>% 
      html_attr("class") %>% 
      gsub("icon rarity-","",.) %>% 
      gsub("-"," ",.) %>% 
      as.data.frame() %>% 
      filter(. != "icon large") %>%
      slice_head(n = nrow(.)-legend_set_removal)}, error = function(e){NA})
    
    if(nrow(rarity) > length(card_names)){ rarity = as.data.frame(rarity[c(1:(nrow(rarity)-(nrow(rarity)-length(card_names))) ),]) }
    
    edition <- data.frame(card_names = card_names,
                          edition = "",
                          rare = ifelse(nrow(rarity) == 0,"", rarity),
                          card_number = "") %>%
      
      `colnames<-` (c("card_names","edition","rarity","card_number")) %>%
    
      mutate(edition = sets$editions[i],
             card_number = seq(length(card_names)),
             set_number = max(card_number),
             set_year = sets$year[i],
             set_id = i)
    
    set_collection <- rbind(set_collection, edition)
    }else(if(i == nrow(sets)){break}else{next})
}

#set_collection %>% select(edition) %>% distinct()

base_first <- set_collection %>% filter(edition == "base set") %>% mutate(edition = paste(edition,"1st ed"))
base_shadowless <- set_collection %>% filter(edition == "base set") %>% mutate(edition = paste("shadowless"))
base_unlimited <- set_collection %>% filter(edition == "base set") %>% mutate(edition = paste("unlimited"))
jungle_first <- set_collection %>% filter(edition == "jungle") %>% mutate(edition = paste(edition,"1st ed"))
fossil_first <- set_collection %>% filter(edition == "fossil") %>% mutate(edition = paste(edition,"1st ed"))
team_rocket_first <- set_collection %>% filter(edition == "team rocket") %>% mutate(edition = paste(edition,"1st ed"))
gym_heroes_first <- set_collection %>% filter(edition == "gym heroes") %>% mutate(edition = paste(edition,"1st ed"))
gym_challenge_first <- set_collection %>% filter(edition == "gym challenge") %>% mutate(edition = paste(edition,"1st ed"))
neo_genesis_first <- set_collection %>% filter(edition == "neo genesis") %>% mutate(edition = paste(edition,"1st ed"))
neo_discovery_first <- set_collection %>% filter(edition == "neo discovery") %>% mutate(edition = paste(edition,"1st ed"))
neo_revelation_first <- set_collection %>% filter(edition == "neo revelation") %>% mutate(edition = paste(edition,"1st ed"))
neo_destiny_first <- set_collection %>% filter(edition == "neo destiny") %>% mutate(edition = paste(edition,"1st ed"))
`1st_collection` <- do.call("rbind",list(base_first,base_shadowless,base_unlimited,jungle_first,fossil_first,team_rocket_first,gym_heroes_first,gym_challenge_first,
                                         neo_genesis_first,neo_discovery_first,neo_revelation_first,neo_destiny_first))
first_collection <- `1st_collection` %>% mutate(edition = gsub("1st","first",edition)) %>% filter(edition != "shadowless" & edition != "unlimited")


set_collection <- do.call("rbind", list(set_collection,`1st_collection`,first_collection))

set_collection <- unique(set_collection) %>% arrange(set_year) %>% mutate(card_id = seq(nrow(.)))

wotc_era <- set_collection %>% filter(set_year <= 2003) %>%distinct()

# PSA Checkpoints 10-1 -----------------------------------------------------

psa_grade = 10

overall_output_10 = NULL
  for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
      for(i in 1:length(results$ebay_response)){
        title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
        title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
        title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                                 ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                        ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                               ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                      ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                             ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                    ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
        
        
        listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
        listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
        listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
        listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
          results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
        }
        listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
        
        card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
        card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
        card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
          results$ebay_response[[i]]$`_source`$data$Item$HitCount
        }
        
        seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
        seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
          results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
        }
        seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
        
        seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
        seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
          results$ebay_response[[i]]$`_source`$data$Item$PostalCode
        }
        
        tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
        
        
        scraped_card_number <- title_line %>% as.data.frame() %>%  
          mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
          mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
          select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
    
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
        
        psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
        
        listing_info = cbind(title_line = title_line,
                                  title_name = wotc_era$card_names[p],
                                  title_set = wotc_era$edition[p],
                                  title_rarity = wotc_era$rarity[p],
                                  title_language = title_language,
                                  title_picture = title_picture, 
                                  listing_type = listing_type,
                                  listing_qty = listing_qty,
                                  listing_price = listing_price,
                                  listing_shipping = listing_shipping,
                                  listing_total_price = listing_total_price,
                                  listing_psa = psa_check,
                                  card_upload_date = card_upload_date,
                                  card_sale_date = card_sale_date,
                                  card_views = card_views,
                                  seller_username = seller_username,
                                  seller_storefront = seller_storefront, 
                                  seller_location = seller_location,
                                  seller_zip = seller_zip)
        listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                       ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                              ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                     ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                            ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                   ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
          mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
        search_results <- rbind(search_results,listing_info)
        colnames(listing_info)
        colnames(search_results)
        search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
  
        
        #colnames(search_results)
        #colnames(overall_output)
        overall_output_10 <- rbind(overall_output_10,search_results)
        overall_output_10 <- overall_output_10 %>% distinct()
      }
    }

  Sys.sleep(.25)
  }

backup_10 <- overall_output_10 

psa_grade = 9
overall_output_9 = NULL
  for(p in 1:nrow(wotc_era)){
    
    url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
    results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
    
    search_results <- NULL
    
    if(length(results$ebay_response) == 0){next}else{
      for(i in 1:length(results$ebay_response)){
        title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
        title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
        title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                                 ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                        ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                               ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                      ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                             ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                    ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
        
        
        listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
        listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
        listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
        listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
          results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
        }
        listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
        
        card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
        card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
        card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
          results$ebay_response[[i]]$`_source`$data$Item$HitCount
        }
        
        seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
        seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
          results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
        }
        seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
        
        seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
        seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
          results$ebay_response[[i]]$`_source`$data$Item$PostalCode
        }
        
        tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
        
        
        scraped_card_number <- title_line %>% as.data.frame() %>%  
          mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
          mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
          select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
          
          #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
          mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
          mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
          mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
          mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
        
        psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
        
        listing_info = cbind(title_line = title_line,
                             title_name = wotc_era$card_names[p],
                             title_set = wotc_era$edition[p],
                             title_rarity = wotc_era$rarity[p],
                             title_language = title_language,
                             title_picture = title_picture, 
                             listing_type = listing_type,
                             listing_qty = listing_qty,
                             listing_price = listing_price,
                             listing_shipping = listing_shipping,
                             listing_total_price = listing_total_price,
                             listing_psa = psa_check,
                             card_upload_date = card_upload_date,
                             card_sale_date = card_sale_date,
                             card_views = card_views,
                             seller_username = seller_username,
                             seller_storefront = seller_storefront, 
                             seller_location = seller_location,
                             seller_zip = seller_zip)
        listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                       ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                              ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                     ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                            ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                   ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
          mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
        search_results <- rbind(search_results,listing_info)
        colnames(listing_info)
        colnames(search_results)
        search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
        
        
        #colnames(search_results)
        #colnames(overall_output)
        overall_output_9 <- rbind(overall_output_9,search_results)
        overall_output_9 <- overall_output_9 %>% distinct()
      }
    }
    
    Sys.sleep(.25)
  }

backup_9 <- overall_output_9 

psa_grade = 8
overall_output_8 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_8 <- rbind(overall_output_8,search_results)
      overall_output_8 <- overall_output_8 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_8 <- overall_output_8 

psa_grade = 7
overall_output_7 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_7 <- rbind(overall_output_7,search_results)
      overall_output_7 <- overall_output_7 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_7 <- overall_output_7 

psa_grade = 6
overall_output_6 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_6 <- rbind(overall_output_6,search_results)
      overall_output_6 <- overall_output_6 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_6 <- overall_output_6 

psa_grade = 5
overall_output_5 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_5 <- rbind(overall_output_5,search_results)
      overall_output_5 <- overall_output_5 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_5 <- overall_output_5 

psa_grade = 4
overall_output_4 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_4 <- rbind(overall_output_4,search_results)
      overall_output_4 <- overall_output_4 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_4 <- overall_output_4


psa_grade = 3
overall_output_3 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_3 <- rbind(overall_output_3,search_results)
      overall_output_3 <- overall_output_3 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_3 <- overall_output_3

psa_grade = 2
overall_output_2 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_2 <- rbind(overall_output_2,search_results)
      overall_output_2 <- overall_output_2 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_2 <- overall_output_2

psa_grade = 1
overall_output_1 = NULL
for(p in 1:nrow(wotc_era)){
  
  url <- paste("https://mavin.io/search?q=",gsub(" " ,"+",wotc_era$card_names[p]),"+",gsub(" ","+",wotc_era$edition[p]),"+psa+",psa_grade,"&format=json&bt=sold&spq=0",sep="")
  results <- GET(url) %>% content(., as="parsed", encoding = "UTF-8")
  
  search_results <- NULL
  
  if(length(results$ebay_response) == 0){next}else{
    for(i in 1:length(results$ebay_response)){
      title_line <- results$ebay_response[[i]]$`_source`$data$Item$Title
      title_picture <- results$ebay_response[[i]]$`_source`$data$Item$PictureURL[[1]]
      title_language <- ifelse(grepl("(japanese|\\s*jpn\\s*|\\sjap\\s)",tolower(title_line))==T,"japanese",
                               ifelse(grepl("(french)",tolower(title_line))==T,"french",
                                      ifelse(grepl("(german)",tolower(title_line))==T,"german",
                                             ifelse(grepl("(chinese)",tolower(title_line))==T,"chinese",
                                                    ifelse(grepl("(spanish)",tolower(title_line))==T,"spanish",
                                                           ifelse(grepl("(portuguese)",tolower(title_line))==T,"portuguese",
                                                                  ifelse(grepl("(dutch)",tolower(title_line))==T,"dutch","english")))))))
      
      
      listing_type <- results$ebay_response[[i]]$`_source`$data$Item$ListingType
      listing_qty <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$QuantitySold)){1}else{results$ebay_response[[i]]$`_source`$data$Item$QuantitySold}
      listing_price <- results$ebay_response[[i]]$`_source`$data$Item$CurrentPrice$Value
      listing_shipping <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value)){0}else{
        results$ebay_response[[i]]$`_source`$data$Item$ShippingCostSummary$ListedShippingServiceCost$Value
      }
      listing_total_price <- results$ebay_response[[i]]$`_source`$totalPrice
      
      card_upload_date <- results$ebay_response[[i]]$`_source`$data$Item$StartTime
      card_sale_date <- results$ebay_response[[i]]$`_source`$data$Item$EndTime
      card_views <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$HitCount)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$HitCount
      }
      
      seller_username <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID)){NA}else{results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}
      seller_storefront <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName)){results$ebay_response[[i]]$`_source`$data$Item$Seller$UserID}else{
        results$ebay_response[[i]]$`_source`$data$Item$Storefront$StoreName
      }
      seller_storefront <- if(is.null(seller_storefront)){NA}else{seller_storefront}
      
      seller_location <- results$ebay_response[[i]]$`_source`$data$Item$Location
      seller_zip <- if(is.null(results$ebay_response[[i]]$`_source`$data$Item$PostalCode)){NA}else{
        results$ebay_response[[i]]$`_source`$data$Item$PostalCode
      }
      
      tcg_genre <- tryCatch({results$ebay_response[[i]]$`_source`$data$Item$ItemSpecifics$NameValueList[[14]]$Value[[1]]},error = function(e){NA})
      
      
      scraped_card_number <- title_line %>% as.data.frame() %>%  
        mutate(title = gsub("(\\d+x|x\\d+)","",gsub("\\d{4}(\\s|$)","",gsub("(PSA|psa)\\s*\\d+","",.)))) %>% 
        mutate(title = ifelse(grepl(wotc_era$edition[p],tolower(title)), gsub(wotc_era$edition[p],"",tolower(title)),title)) %>%
        select(title) %>% mutate(card_number = str_extract(gsub("1st ","",title),"(\\d+[a-z]+) | (\\#\\d+) | (\\s*\\d+\\/)")) %>%
        
        #Separating out the mutates to keep kpi structure clear in mind: Set Number Below
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\[a-z]+\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"(\\s\\#*\\s*\\d+)"),card_number)) %>%
        mutate(card_number = ifelse(is.na(card_number),str_extract(gsub("1st ","",trimws(title)),"([a-z]{2}\\d+)"),card_number)) %>%
        mutate(card_number = str_extract(card_number,"\\d+")) %>% select(card_number)
      
      psa_check <- tryCatch({if( (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric()) == psa_grade){psa_grade}else{(right(str_extract(tolower(title_line),"psa\\s\\d+"),1) %>% as.numeric())}},error = function(e){psa_grade})
      
      
      listing_info = cbind(title_line = title_line,
                           title_name = wotc_era$card_names[p],
                           title_set = wotc_era$edition[p],
                           title_rarity = wotc_era$rarity[p],
                           title_language = title_language,
                           title_picture = title_picture, 
                           listing_type = listing_type,
                           listing_qty = listing_qty,
                           listing_price = listing_price,
                           listing_shipping = listing_shipping,
                           listing_total_price = listing_total_price,
                           listing_psa = psa_check,
                           card_upload_date = card_upload_date,
                           card_sale_date = card_sale_date,
                           card_views = card_views,
                           seller_username = seller_username,
                           seller_storefront = seller_storefront, 
                           seller_location = seller_location,
                           seller_zip = seller_zip)
      listing_info <- listing_info %>% as.data.frame() %>% mutate(title_set = ifelse((title_set == "base set" & grepl("base set 2",title_line)),"base set 2",
                                                                                     ifelse((title_set == "base set" & grepl("shadowless",title_line)),"shadowless",
                                                                                            ifelse((title_set == "base set unlimited" & grepl("base set 2",title_line)),"base set 2",
                                                                                                   ifelse((title_set == "base set" & grepl("\\sII\\s",title_line)),"base set 2",
                                                                                                          ifelse((title_set == "base set" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",
                                                                                                                 ifelse((title_set == "shadowless" & grepl("(1(st|ST)|first\\s)",title_line)),"base set 1st ed",title_set))))))) %>%
        mutate(title_number = wotc_era$card_number[p],scraped_id = scraped_card_number$card_number,title_id =wotc_era$card_id[p] )
      search_results <- rbind(search_results,listing_info)
      colnames(listing_info)
      colnames(search_results)
      search_results <- search_results[!grepl("(psa\\s*\\d\\/\\d+)|(possible)|(regrade)|(\\?)|(unplayed)|(scans)|(psa\\s*\\d+\\s*-\\s*\\d+)|(\\sEX\\s)",tolower(search_results$title_line)),]
      
      
      #colnames(search_results)
      #colnames(overall_output)
      overall_output_1 <- rbind(overall_output_1,search_results)
      overall_output_1 <- overall_output_1 %>% distinct()
    }
  }
  
  Sys.sleep(.25)
}

backup_1 <- overall_output_1

# Resulting Analysis ------------------------------------------------------

overall_output <- rbind(overall_output_10,overall_output_9,overall_output_8,overall_output_7,overall_output_6,
                        overall_output_5,overall_output_4,overall_output_3,overall_output_2,overall_output_1)

overall_output <- overall_output %>% mutate(title_set = ifelse((tolower(title_set) == "base set" & grepl("base set 2",tolower(title_line))),"base set 2",
                                             ifelse((tolower(title_set) == "base set" & grepl("shadowless",tolower(title_line))),"shadowless",
                                                    ifelse((tolower(title_set) == "unlimited"),"base set",
                                                           ifelse((tolower(title_set) == "base set" & grepl("\\sII\\s",tolower(title_line))),"base set 2",
                                                                  ifelse((tolower(title_set) == "base set 2" & grepl("base set 2",tolower(title_line)) == F),"base set",
                                                                    ifelse((tolower(title_set) == "base set 2" & grepl("base set 2",tolower(title_line)) == F & grepl("(1st|first)\\s*ed",tolower(title_line))),"base set 1st ed",
                                                                         ifelse((tolower(title_set) == "shadowless" & grepl("non(\\-*|\\s*)shadowless",tolower(title_line))),"base set",tolower(title_set))))))))
                          
                          ) %>% mutate(
                            listing_qty = as.numeric(listing_qty),
                            listing_price = as.numeric(listing_price),
                            listing_shipping = as.numeric(listing_shipping),
                            listing_total_price = as.numeric(listing_total_price),
                            listing_psa = as.numeric(listing_psa),
                            card_views = as.numeric(card_views),
                            scraped_id = as.numeric(scraped_id)
                          ) %>% filter(!grepl('\\strio(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\spair(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\sduo(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\sduo(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s1996(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2004(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2005(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2006(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2007(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2008(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2009(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2010(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2011(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2012(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2013(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2014(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2015(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2016(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2017(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2018(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2019(\\s|$)',tolower(title_line))
                          ) %>% filter(!grepl('\\s2020(\\s|$)',tolower(title_line))
                          ) %>% mutate(
                            title_set = ifelse((tolower(title_set) == "jungle" & grepl("(1st|first\\s)",tolower(title_line))),"jungle 1st ed",
                                               ifelse((tolower(title_set) == "fossil" & grepl("(1st|first\\s)",tolower(title_line))),"fossil 1st ed",
                                                      ifelse((tolower(title_set) == "team rocket" & grepl("(1st|first\\s)",tolower(title_line))),"team rocket 1st ed",
                                                             ifelse((tolower(title_set) == "gym heroes" & grepl("(1st|first\\s)",tolower(title_line))),"gym heroes 1st ed",
                                                                    ifelse((tolower(title_set) == "gym challenge" & grepl("(1st|first\\s)",tolower(title_line))),"gym challenge 1st ed",
                                                                           ifelse((tolower(title_set) == "neo genesis" & grepl("(1st|first\\s)",tolower(title_line))),"neo genesis 1st ed",
                                                                                  ifelse((tolower(title_set) == "neo discovery" & grepl("(1st|first\\s)",tolower(title_line))),"neo discovery 1st ed",
                                                                                         ifelse((tolower(title_set) == "neo revelation" & grepl("(1st|first\\s)",tolower(title_line))),"neo revelation 1st ed",
                                                                                                ifelse((tolower(title_set) == "neo destiny" & grepl("(1st|first\\s)",tolower(title_line))),"neo destiny 1st ed",tolower(title_set) )))))))))
                            
                          ) %>% mutate(
                            title_set = gsub("","",gsub("base set unlimited","base set",gsub("first\\sed","1st edition",gsub("1st ed","1st edition",title_set))))
                          )

overall_output = overall_output %>% mutate(listing_psa = (str_extract(str_extract(tolower(title_line),"psa.*\\d+"),"\\d+") %>% as.numeric())) %>% select( - title_rarity) %>% distinct()  

title_number_correction = overall_output %>% arrange(title_number)
overall_output$title_number = title_number_correction$title_number[match(paste(overall_output$title_name,overall_output$title_set),paste(title_number_correction$title_name,title_number_correction$title_set))]

title_id_correction = overall_output %>% arrange(title_id)
overall_output$title_id = title_number_correction$title_id[match(paste(overall_output$title_name,overall_output$title_set),paste(title_number_correction$title_name,title_number_correction$title_set))]

title_id_correction = overall_output %>% arrange(title_id)
overall_output$title_id = title_number_correction$title_id[match(paste(overall_output$title_name,overall_output$title_set),paste(title_number_correction$title_name,title_number_correction$title_set))]


overall_output = overall_output %>% distinct() 



con = pokemon_ebay_db("wolfoftinstreet@gmail.com")
bq_auth("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "pokemon-ebay", dataset = "ebay_pokedex", table = "kanto")
bq_table_upload(x=mybq, values = overall_output, fields=as_bq_fields(overall_output),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")



overall_cleaned_output <- overall_output %>% distinct() %>% 
  arrange(desc(listing_total_price)) %>% 
  separate(.,card_upload_date,into = c("upload_date","upload_time"), sep="T") %>%
  separate(.,card_sale_date,c("sale_date","sale_time"),sep="T") %>%
  mutate(upload_time = (gsub("\\.0.*","",upload_time)),
         sale_time = (gsub("\\.0.*","",sale_time)),
         upload_date = as.Date(upload_date),
         sale_date = as.Date(sale_date),
         scraped_id = as.numeric(scraped_id),
         title_set = gsub("","",gsub("base set unlimited","base set",gsub("first\\sed","1st edition",gsub("1st ed","1st edition",title_set))))) %>% 
  filter(title_number == scraped_id | is.na(scraped_id)==T)


overall_rejected_outputs <- overall_output %>% distinct() %>% 
  arrange(desc(listing_total_price)) %>% 
  separate(.,card_upload_date,into = c("upload_date","upload_time"), sep="T") %>%
  separate(.,card_sale_date,c("sale_date","sale_time"),sep="T") %>%
  mutate(upload_time = (gsub("\\.0.*","",upload_time)),
         sale_time = (gsub("\\.0.*","",sale_time)),
         upload_date = as.Date(upload_date),
         sale_date = as.Date(sale_date),
        scraped_id = as.numeric(scraped_id)) %>%
  #select(title_line, title_number, scraped_id) %>%
  filter(title_number != scraped_id | is.na(scraped_id)==F)

# overall_output %>% distinct() %>% filter(title_set == "team rocket") %>% distinct()
# overall_cleaned_output %>% filter(title_set == "team rocket") %>% distinct()
# wotc_era %>% mutate(edition = gsub("","",gsub("base set unlimited","unlimited",gsub("first\\sed","1st edition",gsub("1st ed","1st edition",edition))))) %>% select(edition) %>% distinct()

set_sales <- NULL
for(g in 10:1){
  for(edits in unique(overall_cleaned_output$title_set)){
    edition = edits
    grade = g
    ytd = overall_cleaned_output %>% filter(title_set == paste(edits) & listing_psa == g & sale_date >= "2020-01-01") %>%nrow()
    ya_ytd = overall_cleaned_output %>% filter(title_set == paste(edits) & listing_psa == g & sale_date >= "2019-01-01" & sale_date <= Sys.Date()-366) %>%nrow()
    edition_performance <- cbind(edition,grade,ytd,ya_ytd)
    set_sales <- rbind(set_sales, edition_performance)
  }
}

psa_grades = overall_cleaned_output %>% select(listing_psa) %>% group_by(listing_psa) %>%  mutate(listing_psa = as.numeric(listing_psa)) %>% distinct() %>% filter(listing_psa <= 10) %>% arrange(desc(listing_psa))
grades_current = overall_cleaned_output %>% filter(sale_date >= "2020-01-01") %>% select(listing_psa) %>% group_by(listing_psa) %>%  mutate(psa_count = n(), listing_psa = as.numeric(listing_psa)) %>% distinct()  %>% filter(listing_psa <= 10) %>% arrange(desc(listing_psa)) %>% ungroup() %>% select(psa_count)
grades_ya = overall_cleaned_output %>% filter(sale_date >= "2019-01-01" & sale_date <= Sys.Date()-366) %>% select(listing_psa) %>% group_by(listing_psa) %>%  mutate(psa_count = n(), listing_psa = as.numeric(listing_psa)) %>% distinct()  %>% filter(listing_psa <= 10) %>% ungroup() %>% arrange(desc(listing_psa))

psa_grade_perf <- data.frame(grades = psa_grades,grades_current = grades_current$psa_count,grades_ya = grades_ya$psa_count ) %>% mutate(grade_growth = (grades_current-grades_ya)/grades_ya)

sales_tbl <- set_sales %>% as_tibble() %>% mutate(ytd = as.numeric(ytd),
                                     ya_ytd = as.numeric(ya_ytd),
                                     ytd_growth = round((ytd-ya_ytd)/(ya_ytd),3),
                                     ytd_growth = ifelse(is.infinite(ytd_growth),1,ytd_growth))

sales_tbl$grade_totals = psa_grade_perf$grades_current[match(sales_tbl$grade, psa_grade_perf$listing_psa)]
sales_tbl$grade_totals_ya = psa_grade_perf$grades_ya[match(sales_tbl$grade, psa_grade_perf$listing_psa)]
sales_tbl$grade_growth = psa_grade_perf$grade_growth[match(sales_tbl$grade, psa_grade_perf$listing_psa)]

edition_performance <- sales_tbl %>% mutate(set_v_grade_growth = ytd_growth - grade_growth,
                     ytd_growth = scales::percent(round(ytd_growth,1)),
                     grade_growth = scales::percent(round(grade_growth,1)),
                     set_v_grade_growth = scales::percent(round(set_v_grade_growth,1) ))


con = pokemon_ebay_db("wolfoftinstreet@gmail.com")
bq_auth("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "pokemon-ebay", dataset = "ebay_pokedex", table = "kanto_editions")
bq_table_upload(x=mybq, values = edition_performance, fields=as_bq_fields(edition_performance),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")


ytd_ovr_tbl <- overall_cleaned_output %>% select(title_id,title_name,title_set,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views), Key = paste(title_name,title_set,listing_psa),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  filter(sale_date >= "2020-01-01") %>% group_by(Key,listing_psa)%>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
  summarize(card_sale_count_ytd = n(),
            Median_Retail = median(listing_total_price),
            Average_Retail = mean(listing_total_price),
            Max_Retail = max(listing_total_price),
            Latest_Sale_Date = max(sale_date),
            Median_Views = median(card_views),
            Average_Views = mean(card_views),
            Max_Views = max(card_views),
            Total_Views = sum(card_views),
            Median_Days_Listed = round(median(listing_completion_days),0),
            Average_Days_Listed = round(mean(listing_completion_days),0),
            Max_Days_Listed = round(max(listing_completion_days),0),
            Min_Days_Listed = round(min(listing_completion_days),0)) %>% arrange(desc(listing_psa),desc(Total_Views)) %>% ungroup()



latest_sale_ytd <- overall_cleaned_output %>% select(title_id,title_name,title_set,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views), Key = paste(title_name,title_set,listing_psa),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  filter(sale_date >= "2020-01-01") %>% group_by(title_id,Key,listing_psa) %>% filter(sale_date == max(sale_date)) %>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
  summarize(Total_Retail = sum(listing_total_price),
            Latest_Sale_Date = max(sale_date),
            Total_Views = sum(card_views),
            Total_Days_Listed = round(sum(listing_completion_days),0)) %>% arrange(desc(listing_psa),title_id) %>% ungroup()

ytd_ovr_tbl$Total_Days_Last_Sale = latest_sale_ytd$Total_Days_Listed[match(ytd_ovr_tbl$Key,latest_sale_ytd$Key)]

graded_sales <- overall_cleaned_output %>% select(title_id,title_name,title_set,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views), Key = paste(title_name,title_set,listing_psa),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  group_by(Key,listing_psa)%>% mutate(listing_total_price = as.numeric(listing_total_price)) %>% ungroup()

con = pokemon_ebay_db("wolfoftinstreet@gmail.com")
bq_auth("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "pokemon-ebay", dataset = "ebay_pokedex", table = "kanto_grades")
bq_table_upload(x=mybq, values = graded_sales, fields=as_bq_fields(graded_sales),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")


ya_ytd_ovr_tbl <- overall_cleaned_output %>% select(title_id,title_name,title_set,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views), Key = paste(title_name,title_set,listing_psa),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  filter(sale_date >= "2019-01-01" & sale_date <= Sys.Date()-366) %>% group_by(Key,listing_psa)%>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
  summarize(card_sale_count_ytd = n(),
            Median_Retail = median(listing_total_price),
            Average_Retail = mean(listing_total_price),
            Max_Retail = max(listing_total_price),
            Latest_Sale_Date = max(sale_date),
            Median_Views = median(card_views),
            Average_Views = mean(card_views),
            Max_Views = max(card_views),
            Total_Views = sum(card_views),
            Median_Days_Listed = round(median(listing_completion_days),0),
            Average_Days_Listed = round(mean(listing_completion_days),0),
            Max_Days_Listed = round(max(listing_completion_days),0),
            Min_Days_Listed = round(min(listing_completion_days),0)) %>% arrange(desc(listing_psa),desc(Total_Views)) %>% ungroup()

ya_latest_sale_ytd <- overall_cleaned_output %>% select(title_id,title_name,title_set,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views), Key = paste(title_name,title_set,listing_psa),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  filter(sale_date >= "2019-01-01" & sale_date <= Sys.Date()-366) %>% group_by(title_id,Key,listing_psa) %>% filter(sale_date == max(sale_date)) %>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
  summarize(Total_Retail = sum(listing_total_price),
            Latest_Sale_Date = max(sale_date),
            Total_Views = sum(card_views),
            Total_Days_Listed = round(sum(listing_completion_days),0)) %>% arrange(desc(listing_psa),title_id) %>% ungroup()


ytd_sellers_tbl <- overall_cleaned_output %>% select(seller_storefront,seller_username,seller_location,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
  mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
  group_by(seller_storefront,seller_username,seller_location)%>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
  summarize(Location_Transactions = n(),
            Location_Revenue = sum(listing_total_price),
            Location_Median_Revenue = median(listing_total_price),
            Location_Average_Revenue = mean(listing_total_price),
            Location_Median_Views = median(card_views),
            Location_Average_Views = mean(card_views)) %>%
  arrange(desc(Location_Revenue)) %>% ungroup()

con = pokemon_ebay_db("wolfoftinstreet@gmail.com")
bq_auth("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "pokemon-ebay", dataset = "ebay_pokedex", table = "kanto_addresses")
bq_table_upload(x=mybq, values = ytd_sellers_tbl, fields=as_bq_fields(ytd_sellers_tbl),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")


# ya_ytd_sellers_tbl <- overall_cleaned_output %>% select(seller_storefront,seller_username,seller_location,listing_psa,listing_total_price,title_language,card_views,upload_date,sale_date) %>%
#   mutate(listing_psa = as.numeric(listing_psa), card_views = as.numeric(card_views),listing_completion_days = as.numeric(sale_date - upload_date,units = "days")) %>% 
#   filter(sale_date >= "2019-01-01" & sale_date <= Sys.Date()-366) %>% group_by(seller_storefront,seller_username,seller_location)%>% mutate(listing_total_price = as.numeric(listing_total_price)) %>%
#   summarize(Location_Transactions = n(),
#             Location_Revenue = sum(listing_total_price),
#             Location_Median_Revenue = median(listing_total_price),
#             Location_Average_Revenue = mean(listing_total_price),
#             Location_Median_Views = median(card_views),
#             Location_Average_Views = mean(card_views)) %>%
#   arrange(desc(Location_Revenue)) %>% ungroup()





