# Functions

MakeEthnicOnomap <- function(onolytic, geographic){
  ## Take vectors onolytic and geographic to recode ethnicity and return this as a vector
  require(car)
  if(length(onolytic) != length(geographic)) warning("Input vectors should be of same length")
  onolytic <- car::recode(onolytic,
                          "'NOT FOUND'=NA; 'INTERNATIONAL'=NA; 'UNCLASSIFIED'=NA; 'VOID'=NA; 'VOID - FORENAME'=NA; 'VOID INITIAL'=NA")
  eth <- rep("Other", length(onolytic))
  eth[is.na(onolytic)] <- NA
  eth[onolytic=="CHINESE" |
        onolytic=="HONG KONGESE" |
        onolytic=="SINGAPORESE"] <- "Chinese"
  eth[geographic=="EAST ASIA" &
        eth != "Chinese"] <- "Other Asia & Pacific"
  
  eth[geographic=="SOUTH ASIA"] <- "South Asian"
  
  eth[geographic=="BRITISH ISLES"] <- "Britain&Ireland"
  eth[geographic=="CENTRAL EUROPE" |
        geographic=="EASTERN EUROPE" |
        geographic=="NORTHERN EUROPE" |
        geographic=="SOUTHERN EUROPE" |
        onolytic=="AFRIKAANS"] <- "Other Europe"
  
  eth[geographic=="AFRICA" &
        onolytic != "AFRIKAANS" &
        onolytic != "LIBYAN"] <- "Black African"
  eth[geographic=="MIDDLE EAST" &
        onolytic != "MUSLIM"] <- "East Med"
  eth[onolytic == "MUSLIM"] <- "South Asian" # "Muslim, not localized"
  
  eth[onolytic == "BLACK CARIBBEAN"] <- "Black Caribbean"
  
  ## reduce to 5 categories: White, South Asian, Chinese, Black, Other
  ethnic5 <- car::recode(eth, "'Black African'='Black'; 'Black Caribbean'='Black';  'Britain&Ireland'='White';  'Other Europe'='White';  'East Med'='Other'; 'Other Asia & Pacific'='Other'; 'Muslim, not localized'='Other'")
  ethnic5 <- relevel(as.factor(ethnic5), ref="White")
  ethnic5
}

ExtractEst <- function(model){
  # Take estimate and 95% CI
  a <- summary(model)
  a <- a$conf.int
  a [, c(1, 3, 4), drop = FALSE]
}

ConvertListModelSmriesDf <- function(modellist){
  ## Convert model summaries into a single dataframe
  modeldfs <- map(modellist, function(x) {
    params <- rownames(x)
    x <- as_tibble(x)
    names(x) <- c("est", "lci", "uci")
    x$params <- params
    x %>% 
      select(params, everything())
  })
  modeldfs <- bind_rows(modeldfs, .id = "model")
  modeldfs_neat <- modeldfs %>% 
    mutate_at(vars(est, lci, uci), function(x)  formatC(round(x, 2), digits = 2, format = "f", flag = "0")) %>% 
    mutate(res = paste0(est, " (", lci, "-", uci, ")"))
  modeldfs$res <- modeldfs_neat$res
  rm(modeldfs_neat)
  modeldfs
}

Prioritise <- function(x, preferences = c("pf_any", "undetermined", "npf")) {
  ## Choose worst/best case from a vector based on preferences
  # can use inside group_by as returns a single value
  # Mimics case_when, but less typing
  if(!any(preferences %in% x)) return(NA_character_)
  i <- 1
  pref <- preferences[i]
  while (!pref %in% x) {
    i = i+1
    pref <- preferences[i]
  }
  pref
}