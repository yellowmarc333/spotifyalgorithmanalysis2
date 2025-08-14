clean <- function(dt_raw) {
  assertDataTable(dt_raw)

  dt <- cleanColnames(dt_raw, camelCaseSep = "_", verbose = TRUE, camelCase = TRUE)
  names(dt)
  
  setnames(dt, 
           c("PopularityIndexOnlyuseSpotifyforDeveloperslinkedbelowOnlyenterwholenumbersnopercentordecimal",
             "Whatgenreofmusicmostaccuratelydescribesthisartistproject",
             "Whatisthedominantwaythisartisthaspromotedthissong",
             "Howmanysongsdoyouhaveinradiorightnow",
             "Howmanysongshasthisartisteverreleased",
             "Areyouindependentorsignedtoalabel",
             "IsthissongoptedintoSpotifyDiscoveryMode"),
           c("PopularityIndex",
             "WhatGenreOfMusicMostAccuratelyDescribesThisArtistProject",
             "WhatIsTheDominantWayThisArtistHasPromotedThisSong",
             "HowManySongsDoYouHaveInRadioRightNow",
             "HowManySongsHasThisArtistEverReleased",
             "AreYouIndependentOrSignedToALabel",
             "IsThisSongOptedIntoSpotifyDiscoveryMode"))
  
  # numerical conversions ####
  # todo: clean some columns by hand
  intend_num_cols <- c("StreamsLast28Days",                                           
                       "ListenersLast28Days",                                          
                       "SavesLast28Days",                                              
                       "PlaylistAddsLast28Days",
                       "StreamsLast7Days",
                       "ListenersLast7Days",
                       "SavesLast7Days",
                       "PlaylistAddsLast7Days",
                       "StreamsAllTime",
                       "RadioStreamsLast28Days",
                       "RadioStreamsLast7Days",
                       "DiscoverWeeklyStreamsLast28Days",
                       "DiscoverWeeklyStreamsLast7Days",
                       "ReleaseRadarStreamsLast28Days",
                       "ReleaseRadarStreamsLast7Days",
                       "CurrentSpotifyFollowers",
                       "PopularityIndex",
                       "HowManySongsDoYouHaveInRadioRightNow",
                       "HowManySongsHasThisArtistEverReleased")
  
  symbols <- c(",", ".", ";", "%", "&", " ", "`", "?")
  pattern <- paste0("[", paste0(symbols, collapse = ","), 
                    "]")
  
  dt[, `:=`((intend_num_cols), mapply(function(x, name) {
    x <- gsub(pattern = pattern, replacement = "", x = x)
    is.na(x) <- x == ""
    x <- as.numeric(x)
    return(x)
  }, .SD, intend_num_cols, SIMPLIFY = FALSE)), 
  .SDcols = intend_num_cols]
  assert(all(sapply(dt[, .SD, .SDcols = intend_num_cols], class) ==
               "numeric"))
  

  # intended_mix_cols <- c("NumberOfBlogsThatCoveredTheSong")
  # symbols <- c(",", ".", ";", "%", "&", "`", "?", "<", ">")
  # pattern <- paste0("[", paste0(symbols, collapse = ","), 
  #                   "]")
  # dt[, `:=`((paste0(intended_mix_cols, "_num")),
  #           mapply(function(x, name) {
  #             x <- gsub(pattern = pattern, replacement = "", x = x)
  #             is.na(x) <- x == ""
  #             x <- as.numeric(x)
  #             return(x)
  #           }, .SD, intended_mix_cols, SIMPLIFY = FALSE)), 
  #    .SDcols = intended_mix_cols]
  # assert(all(sapply(dt[, .SD, .SDcols = 
  #                        paste0(intended_mix_cols, "_num")], class) ==
  #              "numeric"))
  
  intended_POS_col <- c("ReleaseDate")

  format <- c("%Y-%m-%d")
  # format <- c("%Y-%m-%d %H:%M:%S")
  
  date_time_vec_form <- as.POSIXct(strptime(
    dt[[intended_POS_col]], format, tz = "UTC"))
  dt[, (intended_POS_col) := date_time_vec_form]
  
  intended_POS_col <- c("Timestamp")
  dt[, Timestamp := as.POSIXct(Timestamp, format = "%m/%d/%Y %H:%M:%S")]
  
  

  # format <- c("%m-%d-%Y %H:%M:%S")
  # 
  # date_time_vec_form <- as.POSIXct(strptime(
  #   dt[[intended_POS_col]], format, tz = "UTC"))
  # dt[, (intended_POS_col) := date_time_vec_form]
  
  # names(dt)
  
  indended_char_cols <- c("ArtistName",                                                   
                          "SongName",
                          "WhatGenreOfMusicMostAccuratelyDescribesThisArtistProject",
                          "WhatIsTheDominantWayThisArtistHasPromotedThisSong",
                          "ReleaseConsistency",
                          "AreYouIndependentOrSignedToALabel",
                          "IsThisSongOptedIntoSpotifyDiscoveryMode")
  # assert(length(unique(dt[["PopularityIndexSource"]])) <= 3)
  
  return(dt)
  
}
