pacman::p_load(httr,tidyverse,rvest,bigrquery,anytime)
x = card
detect_outliers <- function(x){
  #if(missing(x$p10)) stop ("Arg must be a vector")
  #if(!is.numeric(x$p10))stop("Arg must be numeric")
  
  data_tbl <- tibble(data=x$p8)
  limits <- data_tbl %>% summarise(
    quantile_lo = quantile(data,probs=.25,na.rm=T),
    quantile_hi = quantile(data,probs=.75,na.rm=T),
    iqr = IQR(data,na.rm=T),
    limit_lo = (quantile_lo - 1.5 * iqr),
    limit_hi = (quantile_hi + 1.5 * iqr)
  )
  output <- data_tbl %>% mutate(outlier = case_when(
    data < limits$limit_lo ~ T,
    data > limits$limit_hi ~ T,
    T ~ F
  ))
  new_data = cbind(x,output$outlier)
  new_data = new_data %>% filter( output$outlier != "TRUE")
  return(new_data)
  
}

pokemon_price_bigquery_export <- NULL
edition_roster_pokemon_price = read_html("https://www.pokemonprice.com/Sets") %>% html_nodes('a') %>% html_attr("href") %>% as_tibble() %>% filter(grepl("CardsForSet",value)
  ) %>% mutate(value = gsub("/CardsForSet/","",value)
  ) %>% separate(value,c("set_code","structured_set_name"), sep="/\\d{4}\\-pokemon\\-"
  ) %>% mutate(readable_set_name = gsub("^game base ii$","base set 2",gsub("^game$","base set",gsub("-"," ",structured_set_name))),
               phone_number = paste("https://www.pokemonprice.com/api/cardsforset/",set_code,sep=""),
               area_code = paste("https://www.pokemonprice.com/CardDetails/",set_code,sep="")
  ) 

#edition_roster_pokemon_price$area_code[a] %>% read_html()  %>% html_nodes(xpath = '//*[@id="prices"]')
                

for(a in 1:nrow(edition_roster_pokemon_price)){
  pokemon_price_card_ids = GET(edition_roster_pokemon_price$phone_number[a]
  ) %>% content("parsed"
  ) %>% html_nodes("span"
  ) %>% html_attr("id"
  )
  
  pokemon_price_card_names = GET(edition_roster_pokemon_price$phone_number[a]
  ) %>% content("parsed"
  ) %>% html_nodes("a"
  ) %>% html_text(
  ) %>% gsub(" Holo","",.
  ) %>% trimws(
  )
  
  pokemon_card_numbers = GET(edition_roster_pokemon_price$phone_number[a]
  ) %>% content("parsed"
  ) %>% html_text(
  ) %>% gsub("\\d{4}(\\s|\\))","",.
  ) %>% gsub("\\s\\d{4}","",.
  ) %>% gsub("\\'\\d{2}(\\-\\'\\d{2})*","",.
  ) %>% str_extract_all(.,"(\\d{2}\\/\\d{2}|H\\d{2}|\\d{3}|\\d{2}[A-Za-z]|([A-Z]{1}|\\!|\\?)\\/\\d{2}|[A-Z]{2}\\d{2}|ONE|TWO|THREE|FOUR)"
  ) %>%unlist(
  ) %>% as.data.frame(
  ) %>% filter(. != "909" &  .!= "10t" & .!= "20t" & .!= "DY90"
  ) %>% as.vector(
  )
  
  pokemon_price_set_tbl = data.frame(phone_number = paste("https://www.pokemonprice.com/api/pricedata/",pokemon_price_card_ids,sep=""),
                                     identifier = pokemon_price_card_ids, 
                                     card_name = pokemon_price_card_names, 
                                     card_number = pokemon_card_numbers$.)
  for(b in 1:nrow(pokemon_price_set_tbl)){
    pokemon_price_unique_card_pricing <- GET(pokemon_price_set_tbl$phone_number[b]) %>% content("parsed")
    if(is.null(pokemon_price_unique_card_pricing)){next}else{
      aggregated_sales_line_df = NULL
      for(c in 1:length(pokemon_price_unique_card_pricing)){
        sale_line = data.frame(
          set = edition_roster_pokemon_price$set_code[a],
          card_id = pokemon_price_set_tbl$identifier[b],
          card_name = pokemon_price_set_tbl$card_name[b],
          card_number = pokemon_price_set_tbl$card_number[b],
          dos = anytime(pokemon_price_unique_card_pricing[[c]]$SaleDate),
          sold = 1,
          p10 = pokemon_price_unique_card_pricing[[c]]$PSA10,
          p9 = pokemon_price_unique_card_pricing[[c]]$PSA9,
          p8 = pokemon_price_unique_card_pricing[[c]]$PSA8,
          p7 = pokemon_price_unique_card_pricing[[c]]$PSA7,
          p6 = pokemon_price_unique_card_pricing[[c]]$PSA6,
          p5 = pokemon_price_unique_card_pricing[[c]]$PSA5,
          p4 = pokemon_price_unique_card_pricing[[c]]$PSA4,
          p3 = pokemon_price_unique_card_pricing[[c]]$PSA3,
          p2 = pokemon_price_unique_card_pricing[[c]]$PSA2,
          p1 = pokemon_price_unique_card_pricing[[c]]$PSA1
        )
        
        aggregated_sales_line_df = rbind(aggregated_sales_line_df, sale_line)
        aggregated_sales_line_df = aggregated_sales_line_df %>% arrange(desc(dos))
      }
    }
    pokemon_price_bigquery_export = rbind(pokemon_price_bigquery_export,aggregated_sales_line_df)
    Sys.sleep(.05)
  }
  Sys.sleep(.05)
}


pokemon_json_export <- pokemon_price_bigquery_export %>% left_join(edition_roster_pokemon_price, by = c("set" = "set_code" )
) %>% select(-set,-card_id,-structured_set_name,-phone_number
) %>% select(card_name, readable_set_name,everything())

#write_json(pokemon_json_export, "/home/cujo253/pokemon_history.json")

pokemon_json_export %>% glimpse()
pokemon_json_export %>% filter(card_name == "Lapras 1st Edition", p10 != 0) %>% select(dos,p10) %>% arrange(desc(dos)) %>% 
  plot_time_series(dos,p10,.title = "Lapras 1st Edition PSA 9",.smooth = F, .interactive = F)


detect_outliers <- function(x){
  #if(missing(x$p10)) stop ("Arg must be a vector")
  #if(!is.numeric(x$p10))stop("Arg must be numeric")
  
  data_tbl <- tibble(data=x$p10)
  limits <- data_tbl %>% summarise(
    quantile_lo = quantile(data,probs=.25,na.rm=T),
    quantile_hi = quantile(data,probs=.75,na.rm=T),
    iqr = IQR(data,na.rm=T),
    limit_lo = (quantile_lo - 1.5 * iqr),
    limit_hi = (quantile_hi + 1.5 * iqr)
  )
  output <- data_tbl %>% mutate(outlier = case_when(
    data < limits$limit_lo ~ T,
    data > limits$limit_hi ~ T,
    T ~ F
  ))
  new_data = cbind(x,output$outlier)
  new_data = new_data %>% filter( output$outlier != "TRUE")
  return(new_data)
  
}
card = pokemon_json_export %>% filter(card_name == "Charizard",grepl("evolution",readable_set_name)==T, p10 != 0, grepl("2020",dos)==T) %>% select(dos,p10) %>% arrange(desc(dos))
detect_outliers(card) %>% select(dos,p10) %>%plot_time_series(dos,p10,.title = "Charizard PSA 10",.smooth = F, .interactive = F)
