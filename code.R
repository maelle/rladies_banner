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
  try(magick::image_read(url) %>%
    magick::image_write(name),
    silent = TRUE)
}

# function for downloading pics from one account
download_pics <- function(chapter){
  print(chapter)
  
  # get medial urls
  tweets <- try(rtweet::get_timeline(chapter, n = 18000,
                                     include_rts = FALSE, 
                                     filter_media = TRUE))
  
  if(!is(tweets, "try-error")){
    urls <- tweets$media_url
    urls <- urls[!is.na(urls)]
    # no gifs
    urls <- urls[!stringr::str_detect(urls, "tweet\\_video\\_thumb")]
    
    no_urls <- length(urls)
    if(no_urls > 0){
      purrr::walk2(urls, paste0("pics/", chapter, 1:no_urls, ".jpg"),
                   save_pic)
    }
  }
 
  
}

# get pics from all chapters
purrr::walk(twitters, download_pics)

# helper function to get image info
get_info <- function(path){
  df <- magick::image_read(path) %>%
      magick::image_info() 
  df$path <- path
  df$pic <- stringr::str_replace(path, "pics\\/", "")
  df$chapter <- stringr::str_replace(df$pic, "[1-9]*\\.jpg", "")
  return(df)
}

# remove duplicates
pics_info <- purrr::map_df(dir("pics", full.names = TRUE),
                           get_info)

unique_pics <- pics_info %>% 
  dplyr::group_by(width, height, filesize, chapter) %>%
  dplyr::summarize(path = path[1], pic = pic[1])

# now add borders to each pic from one of the main colours
# not the first one so that very similar pics can get different borders
dir.create("formatted_pics")
set.seed("42")
format_image <- function(path){
  image <- magick::image_read(path)
  info <- magick::image_info(image)
  
  
  
  direction <- ifelse(info$height > info$width,
                      "height", "width")
  scale_number <- as.numeric(info[direction]/500)
  image <- magick::image_scale(image, paste0(info["width"]/scale_number,
                                             "x", 
                                             info["height"]/scale_number))
  newinfo <- magick::image_info(image)
  
  # colours
  colours <- try(rPlotter::extract_colours(path), silent = TRUE)
  
  # one pic at least was problematic but it was bicolor and actually
  # a gif
  if(!is(colours, "try-error")){
    colour <- sample(x = colours, size = 1)
    
    image <- magick::image_border(image, colour, paste0((500-newinfo$width)/2, "x",
                                                        (500-newinfo$height)/2))
    info <- magick::image_info(image)
    # odd numbers out!
    if(info$height/2 != floor(info$height/2)){
      image <- magick::image_crop(image, "0x500+0")
    }
    if(info$width/2 != floor(info$width/2)){
      image <- magick::image_crop(image, "500x0+0")
    }
    magick::image_write(image,
                        stringr::str_replace(path, "pics", "formatted_pics"))
  }
  
  
}

purrr::walk(unique_pics$path, format_image)


# re-format logo
magick::image_read("logo.svg") %>%
  magick::image_save("logo.jpeg")

# mosaic making time!

composeMosaicFromImageRandomOptim(
  originalImageFileName = "logo.jpeg",
  outputImageFileName = "header.jpg",
  inputTileSize = 500,
  imagesToUseInMosaic = "formatted_pic", 
  removeTiles = TRUE,
  fracLibSizeThreshold = 0.01
)