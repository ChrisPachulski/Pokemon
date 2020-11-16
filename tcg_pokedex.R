#Functions & packages####
invisible(right <- function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
}) #Recreating the right function from Excel 
invisible(left <- function(text, num_char) {
    substr(text, 1, num_char)
}) #Recreating the left function from Excel 
invisible(pokemon_ebay <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "pokemon-ebay",
        dataset = "kanto",
        billing = "pokemon-ebay"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
})
invisible(chrome <-function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr
})
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium)



#TCG- Market####
Start_Time <- Sys.time()
A <- 0
B <- 100
C <- 100
Pokemon_Best_Sellers <- NULL
body <- paste('{
    "algorithm": "salesrel",
    "context": {
          "cart": {},
          "shippingCountry": "US"
              },
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "pokemon"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Amazing Rare",
                "Secret Rare",
                "Holo Rare",
                "Ultra Rare",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B 
B <- 200
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
for(i in 1:C){
    Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
    MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    hasFoil = ""
    Direct_Listings <- 0
    Total_Copies <- NULL
    Potential_Direct_Copies <- NULL
    limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
    if(limit >0){
        for(j in 1:limit){
            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                    dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                else{
                    Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                    Total_Copies <- rbind(Total_Copies,Copies)
                }
            }
        }
    }
    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
    Total_Copies <- sum(Total_Copies)
    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
    Line_Item <- cbind(Name,Set,Rarity,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
    Pokemon_Best_Sellers <- rbind(Pokemon_Best_Sellers, Line_Item)
}

for(i in 1:97){
    body <- paste('{
        "algorithm": "salesrel",
            "context": {
              "cart": {},
              "shippingCountry": "US"
                  },
        "from": "',A,'",
        "size": "',B,'",
        "filters": {
            "range": {},
            "term": {
                "productLineName": [
                "pokemon"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Amazing Rare",
                "Secret Rare",
                "Holo Rare",
                "Ultra Rare",
                "Rare"
                ]
            }
        }
    }',
                  sep="")
    A <- A + 100
    B <- 100
    C <- 100
    TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    # repeat{
    #     if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
    #     if((length(TCG_Results_1[[1]]$results) != 0)) break
    # }
    C = length(TCG_Results_1[[1]]$results)
    if(C == 0)break
    for(i in 1:C){
        Name <- gsub("\\s\\/\\/.*","",TCG_Results_1[[1]]$results[[i]]$productName)
        Set <- TCG_Results_1[[1]]$results[[i]]$setName
        Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
        MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
        Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
        MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
        Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
        Direct_Listings <- 0
        hasFoil = ""
        Total_Copies <- NULL
        Potential_Direct_Copies <- NULL
        limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
        if(limit >0){
            for(j in 1:limit){
                if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                    Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                    if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                        dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                        Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                    else{
                        Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                        Total_Copies <- rbind(Total_Copies,Copies)
                    }
                }
            }
        }
        Potential_Direct_Copies <- sum(Potential_Direct_Copies)
        Total_Copies <- sum(Total_Copies)
        if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
        Line_Item <- cbind(Name,Set,Rarity,hasFoil,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
        Pokemon_Best_Sellers <- rbind(Pokemon_Best_Sellers, Line_Item)
    }
    #if(A >= 9990) break
    Sys.sleep(.25)
}


Pokemon_Best_Sellers <- unique(Pokemon_Best_Sellers) %>% as.data.frame()
Pokemon_Best_Sellers <- Pokemon_Best_Sellers %>% mutate(Rank = seq(nrow(Pokemon_Best_Sellers))) %>%
    mutate(Key = trimws(paste(Name,Set,Rarity,hasFoil,sep=""))) %>% relocate(Key, .before = Name) %>%
    mutate(MKT_EST = as.numeric(MKT_EST),
           Listings = as.numeric(Listings),
           MKT = as.numeric(MKT),
           Product_ID = as.numeric(Product_ID),
           Direct_Listings = as.numeric(Direct_Listings),
           Potential_Direct_Copies = as.numeric(Potential_Direct_Copies),
           Total_Copies = as.numeric(Total_Copies))

#Pokemon_Best_Sellers %>% glimpse()
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
sheet_write(
    Pokemon_Best_Sellers,
    ss = drive_get("Pokemon_TCG"),
    sheet = "pokemon_sr"
)

con = pokemon_ebay("wolfoftinstreet@gmail.com")
bq_table = "tcg_best_sellers"
mybq <- bq_table(project = "pokemon-ebay", dataset = "tcg_best_seller", table = paste(gsub("-","_",Sys.Date()),"_",bq_table,sep=""))
bq_table_upload(x=mybq, values = Pokemon_Best_Sellers, fields=as_bq_fields(Pokemon_Best_Sellers),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print(paste("BQ Pokemon_All_Editions Upload Successful!"))

remDr <- chrome("64.225.20.203")
remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=pokemon&page=1")
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[2]/div[2]/div[2]')$clickElement()
# webElem <- remDr$findElements("css", "iframe")
# remDr$switchToFrame(webElem[[1]])
stacked_text <- NULL
stacked_qty <- NULL
numbers <- remDr$findElements('css','.search-filter__option-count')
for(i in 1:length(numbers)){
    text <- numbers[[i]]$getElementText()
    stacked_qty <- rbind(stacked_qty,text)}

options <- remDr$findElements('css','.checkbox__option-value')
for(i in 1:length(options)){
    text <- options[[i]]$getElementText()
    stacked_text <- rbind(stacked_text,text)}

stacked_text <- cbind(stacked_text,stacked_qty)
stacked_backup <- stacked_text
#stacked_text <- stacked_backup
stacked_text <- stacked_text %>% as.data.frame() %>% slice(-c(1:45))
rownames(stacked_text) <- seq(nrow(stacked_text))
cutoff <- which(grepl("^Cards$",stacked_text$V1))
stacked_text <- tryCatch({stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("V1" = "editions", "V2" = "qty"))},error = function(e){stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("editions" = "V1", "qty" = "V2"))}) %>% 
    mutate(api_editions  = gsub("---","-",gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions)))))))) %>% mutate(qty = ceiling(as.numeric(qty)/100) ) %>%
    mutate(api_editions = ifelse(grepl("mcdonalds-promos-2011",api_editions)==T,paste(api_editions,"_new",sep=""),api_editions)) %>%
    mutate(api_editions = ifelse(grepl("mcdonalds-promos-2012",api_editions)==T,paste(api_editions,"_new",sep=""),api_editions)) %>%
    mutate(api_editions = paste('"',api_editions,'"',sep=""))
stacked_text = stacked_text[!grepl( "World Championship Decks",stacked_text$editions),]

#View(stacked_text)
# stacked_text = stacked_text[!grepl("\"spellslinger-starter-kit\"",stacked_text$api_editions),]
# stacked_text = stacked_text[!grepl("\"box-sets\"",stacked_text$api_editions),]
# stacked_text = stacked_text[!grepl("\"magic-the-gathering-apparel\"",stacked_text$api_editions),]
stacked_text = stacked_text %>% na.omit()
length(stacked_text$editions[142])
Pokemon_All_Editions <- NULL

Start_Time <- Sys.time()
for(q in 1:length(stacked_text$editions)){
    Best_Sellers_SR <- NULL
    Best_Sellers_RR <- NULL 
    A <- 0
    B <- 100
    for(p in 1:stacked_text$qty[q]){
        body <- paste('{
            "algorithm": "salesrel",
            "context": {
                  "cart": {},
                  "shippingCountry": "US"
                      },
            "from": "',A,'",
            "size": "',B,'",
            "filters": {
                "range": {},
                "term": {
                    "productLineName": [
                        "pokemon"
                    ],
                    "productTypeName": [
                        "Cards"
                    ],
                    "setName": [
                        ',stacked_text$api_editions[q],'
                    ]
                }
            }
        }',
                      sep="")
        A <- A + 100
        B <- 100
        C <- 100
        TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        while(TCG_Results$status_code != 200){
            Sys.sleep(4)
            TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        }
        TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
        C = length(TCG_Results_1[[1]]$results)
        if(C == 0)break
        for(i in 1:C){
            Name <- TCG_Results_1[[1]]$results[[i]]$productName
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- ifelse(is.null(TCG_Results_1[[1]]$results[[i]]$rarityName), TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName, TCG_Results_1[[1]]$results[[i]]$rarityName)
            number = TCG_Results_1[[1]]$results[[i]]$customAttributes$number
            MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
            MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
            Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
            Direct_Listings <- 0
            Total_Copies <- NULL
            Potential_Direct_Copies <- NULL
            limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
            if(limit >0){
                for(j in 1:limit){
                    if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                        Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                        else{
                            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Total_Copies <- rbind(Total_Copies,Copies)
                        }
                    }
                }
            }
            Potential_Direct_Copies <- sum(Potential_Direct_Copies)
            Total_Copies <- sum(Total_Copies)
            if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
            Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = number, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                    Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "SR")
            Best_Sellers_SR <- rbind(Best_Sellers_SR, Line_Item)
        }
        Sys.sleep(.25)
    }
    
    A <- 0
    B <- 100
    for(p in 1:stacked_text$qty[q]){
        body <- paste('{
            "algorithm": "revenuerel",
            "context": {
                  "cart": {},
                  "shippingCountry": "US"
                      },
            "from": "',A,'",
            "size": "',B,'",
            "filters": {
                "range": {},
                "term": {
                    "productLineName": [
                        "pokemon"
                    ],
                    "productTypeName": [
                        "Cards"
                    ],
                    "setName": [
                        ',stacked_text$api_editions[q],'
                    ]
                }
            }
        }',
                      sep="")
        A <- A + 100
        B <- 100
        C <- 100
        TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        while(TCG_Results$status_code != 200){
            Sys.sleep(4)
            TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
        }
        TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
        C = length(TCG_Results_1[[1]]$results)
        if(C == 0)break
        for(i in 1:C){
            Name <- TCG_Results_1[[1]]$results[[i]]$productName
            Set <- TCG_Results_1[[1]]$results[[i]]$setName
            Rarity <- ifelse(is.null(TCG_Results_1[[1]]$results[[i]]$rarityName), TCG_Results_1[[1]]$results[[i]]$customAttributes$rarityDbName, TCG_Results_1[[1]]$results[[i]]$rarityName)
            number = TCG_Results_1[[1]]$results[[i]]$customAttributes$number
            MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
            Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
            MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
            Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
            Direct_Listings <- 0
            Total_Copies <- NULL
            Potential_Direct_Copies <- NULL
            limit <- if(length(TCG_Results_1[[1]]$results[[i]]$listings) < 3){length(TCG_Results_1[[1]]$results[[i]]$listings)}else{3}
            if(limit >0){
                for(j in 1:limit){
                    if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
                        Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
                        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
                            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
                        else{
                            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
                            Total_Copies <- rbind(Total_Copies,Copies)
                        }
                    }
                }
            }
            Potential_Direct_Copies <- sum(Potential_Direct_Copies)
            Total_Copies <- sum(Total_Copies)
            if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
            Line_Item <- data.frame(Card_name = Name,Set = Set,Rarity = Rarity,number = number, MKT_EST = MKT_EST,Vendor_Listings = Listings,MKT= MKT,
                                    Product_ID = Product_ID,Direct_Listings = Direct_Listings,Potential_Direct_Copies = Potential_Direct_Copies,Total_Copies = Total_Copies, Methodology = "RR")
            Best_Sellers_RR <- rbind(Best_Sellers_RR, Line_Item)
        }
        Sys.sleep(.25)
    }

    Best_Sellers_SR <- unique(Best_Sellers_SR)
    Best_Sellers_RR <- unique(Best_Sellers_RR)
        
    Pokemon_Edition_Rankings_SR <- Best_Sellers_SR %>% as.data.frame() %>% mutate(Set_Rank = as.numeric(seq(nrow(Best_Sellers_SR)))) %>% 
        #mutate(Rarity = ifelse(Rarity == "Mythic","M", ifelse(Rarity == "Rare", "R", ifelse(Rarity == "Uncommon", "U", ifelse(Rarity == "Common", "C", ifelse(Rarity == "Land","L",ifelse(Rarity == "Promo","P",ifelse(Rarity == "Special","S",ifelse(Rarity == "Token","T",Rarity))))))))) %>% 
        #mutate(Card_name = gsub("\\s\\(.*","",Card_name),Set = Sets_V2$mtgjson[match(Set,Sets_V2$TCG_Key)]) %>%
        mutate(Key = paste(Card_name,Set,Rarity,sep=""), 
               Ovr_Rank = as.numeric(ifelse(!is.na(Pokemon_Best_Sellers$Rank[match(Key,Pokemon_Best_Sellers$Key)]),Pokemon_Best_Sellers$Rank[match(Key,Pokemon_Best_Sellers$Key)],NA )), Date = Sys.Date()) %>%
        select(Key,everything())
    
    Pokemon_Edition_Rankings_RR <- Best_Sellers_RR %>% as.data.frame() %>% mutate(Set_Rank = as.numeric(seq(nrow(Best_Sellers_RR)))) %>% 
        #mutate(Rarity = ifelse(Rarity == "Mythic","M", ifelse(Rarity == "Rare", "R", ifelse(Rarity == "Uncommon", "U", ifelse(Rarity == "Common", "C", ifelse(Rarity == "Land","L",ifelse(Rarity == "Promo","P",ifelse(Rarity == "Special","S",ifelse(Rarity == "Token","T",Rarity))))))))) %>% 
        #mutate(Card_name = gsub("\\s\\(.*","",Card_name),Set = Sets_V2$mtgjson[match(Set,Sets_V2$TCG_Key)]) %>%
        mutate(Key = paste(Card_name,Set,Rarity,sep=""), 
               Ovr_Rank = as.numeric(ifelse(!is.na(Pokemon_Best_Sellers$Rank[match(Key,Pokemon_Best_Sellers$Key)]),Pokemon_Best_Sellers$Rank[match(Key,Pokemon_Best_Sellers$Key)],NA )), Date = Sys.Date()) %>%
        select(Key,everything())
    
    
    Pokemon_Edition_Rankings <- rbind(Pokemon_Edition_Rankings_SR,Pokemon_Edition_Rankings_RR) %>% data.frame() %>%
        mutate(across(.cols = MKT_EST:Rank, .fns = as.numeric)) %>% replace_na(list(Ovr_Rank = 0))
    Sys.sleep(.75)
    
    bq_table = unique(tolower(gsub("\\)","",gsub("\\(","",gsub("___","_",gsub("-","_",gsub("&","and",gsub("’","",gsub("\\.","",gsub(":","",gsub("'","",gsub(" ","_",Pokemon_Edition_Rankings$Set))))))))))))
    
    con = pokemon_ebay("wolfoftinstreet@gmail.com")
    bq_auth("wolfoftinstreet@gmail.com")
    mybq <- bq_table(project = "pokemon-ebay", dataset = bq_table, table = paste(gsub("-","_",Sys.Date()),"_",bq_table,sep=""))
    bq_table_upload(x=mybq, values = Pokemon_Edition_Rankings, fields=as_bq_fields(Pokemon_Edition_Rankings),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
    print(paste("BQ", stacked_text$editions[q],"Upload Successful!"))
    Pokemon_All_Editions = rbind(Pokemon_All_Editions,Pokemon_Edition_Rankings)
}

bq_table = "tcg_collection"
mybq <- bq_table(project = "pokemon-ebay", dataset = "tcg_pokedex", table = paste(gsub("-","_",Sys.Date()),"_",bq_table,sep=""))
bq_table_upload(x=mybq, values = Pokemon_All_Editions, fields=as_bq_fields(Pokemon_All_Editions),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print(paste("BQ Pokemon_All_Editions Upload Successful!"))
    

End_Time <- Sys.time()
print(paste("TCG Pokedex Collection Lasted:",round(End_Time - Start_Time,2)))

