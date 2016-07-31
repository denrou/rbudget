library(rvest)
library(RSelenium)
library(dplyr)
library(stringr)
library(RCurl)
library(purrr)
library(plyr)
library(png)
library(base64enc)

#' @title connect_boursorama
#' 
#' @description 
#' connect to the boursorama bank webpage
#' 
#' @param account_number
#' an integer corresponding to the cient account number
#' 
#' @param password
#' an integer corresponding to the password which connect to the account
#' 
#' @value
#' The webpage get after the submit request
connect_boursorama <- function(account_number = NULL, password = NULL) {
  
  checkForServer()
  startServer(log = FALSE, invisible = FALSE)
  remDr <- remoteDriver()
  remDr$open()
  remDr$navigate("https://clients.boursorama.com/connexion/")
  
  # Find element where to put the account_number and print it
  webelem <- remDr$findElements("id", "form_login")[[1]]
  webelem$clickElement()
  remDr$sendKeysToActiveElement(list(account_number))
  
  # rvest is easier to work with, so we fetch the html source
  page_source <- remDr$getPageSource()[[1]]
  
  # A number is associated with a key of 3 letters which will be passed
  key <- page_source %>% 
    read_html() %>% 
    html_nodes(".sasmap__key") %>% 
    html_attrs() %>% 
    map_chr(~ .x[2])
  
  # Numbers are displayed as a png image (encoded in base64)
  # We need to fetch each image to determine to which number it corresponds
  image <- page_source %>% 
    read_html() %>% 
    html_nodes(".sasmap__key") %>% 
    html_nodes("img") %>% 
    html_attrs() %>% 
    str_replace("data:image/png;base64,", "") %>% 
    map(base64decode) %>% 
    map(readPNG) %>% 
    # readPNG returns a 4 dimension array, each dimension being a color (+alpha)
    # alply is the equivalent of apply (apply(data, 3) means separate by array)
    map(alply, 3) %>% 
    # add all colors together to get a one dimension array representing the image
    map(function(i) {
      i[[1]] + i[[2]] + i[[3]] + i[[4]]
    }) %>% 
    # some noise exist on the png image
    map(~ replace(., . < 3.9, 0))
  
  # Compare each image to a model
  load("boursorama_password_number_images.RData")
  numbers <- image %>% 
    # for each image, calculate the sum of the differences between the image and each modele images
    map(function(i) {
      x <- modele %>% 
        map(function(m) {
          sum(i-m)
        }) %>% 
        unlist
      # Indexes of the modele's images correspond to the number they represent.
      x <- which(abs(x) == min(abs(x)))
      ifelse(x == 10, 0, ifelse(x == 11, NA, x))
    }) %>% 
    unlist
  
  # Numbers in the website are not displayed in order, so we need to find this order
  split_code <- strsplit(password, "")[[1]] %>% as.numeric()
  index <- map_int(split_code, function(i) which(numbers == i))
  
  # For each number of the code, we can click on the right image
  webElem <- remDr$findElements("class", "sasmap__key")
  for (elem in index) {
    webElem[[elem]]$clickElement()
  }
  
  # Finally, we submit the page
  webElem <- remDr$findElements("css selector", ".button.button--lg")[[1]]
  webElem$clickElement()
}