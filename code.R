library("magrittr")

# see http://livefreeordichotomize.com/2017/07/18/the-making-of-we-r-ladies/
doc.raw <- RCurl::getURL("https://raw.githubusercontent.com/rladies/starter-kit/master/Current-Chapters.md")
twitters <- stringr::str_match_all(doc.raw, "twitter.com/(.*?)/")[[1]][,2]
twitters <- unique(twitters)
twitters <- stringr::str_replace_all(twitters, "\\].*", "") 

# folder for all pics
dir.create("pics")

# helper function for saving one pic
# name is actually a path from the project root
save_pic <- function(url, name){
  magick::image_read(url) %>%
    magick::image_write(name)
}

# function for downloading pics from one account
download_pics <- function(chapter){
  # create folder
  dir.create(paste0("pics/", chapter))
  
  # get medial urls
  tweets <- rtweet::get_timeline(chapter, n = 18000,
                                 include_rts = FALSE, 
                                 filter_media = TRUE)
 
  urls <- tweets$media_url
  urls <- urls[!is.na(urls)]
  # no gifs
  urls <- urls[!stringr::str_detect(urls, "tweet\\_video\\_thumb")]
  
  no_urls <- length(urls)
  if(no_urls > 0){
    purrr::walk2(urls, paste0("pics/", chapter, "/", 1:no_urls, ".jpg"),
                 save_pic)
  }
}

# get pics from all chapters
purrr::walk(twitters, download_pics)
