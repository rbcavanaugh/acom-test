# read in item parameters for full ACOM
acom_itpar_content <- read.csv(file = "acom_Tscaled_ItemParameters_Content.csv",na.strings = "NA")
acom_itpar_content$itnum <- as.numeric(seq(1:59)) 

acom_itpar <- acom_itpar_content[1:59,c(4:7,3)]
acom_itpar <- data.matrix(acom_itpar)

bank = acom_itpar[,1:4]

# define options for content balancing
acom_cb_names <- c("talk","comp","gen","writ","nam")
acom_cb_props <- c(0.355932203, 0.271186441, 0.13559322, 0.13559322, 0.101694915)
acom_cbControl <- list(names = acom_cb_names, props = acom_cb_props)
acom_cb_group <- acom_itpar_content$content_area
test.cbList(acom_cbControl, cbGroup = acom_cb_group)

goCAT <- function(v){
  
  dat = v$results %>% drop_na(response) %>%
    mutate(response_num = case_when(
      merge_cats == 1 & response_num == 1 ~ 0,
      merge_cats == 1 & response_num == 2 ~ 1,
      merge_cats == 1 & response_num == 3 ~ 2,
      
      merge_cats == 2 & response_num == 2 ~ 1,
      merge_cats == 2 & response_num == 3 ~ 2,
      
      merge_cats == 3 & response_num == 3 ~ 2,
      
      merge_cats == 4 & response_num == 2 ~ 1,
      merge_cats == 4 & response_num == 3 ~ 1,
      TRUE ~ response_num
    ))
    
  print(dat$response_num)
  
  done = dat$itnum
  
  if(all(is.na(dat$response_num))){
    cur_theta = NA
    cur_sem = NA
    theta = 50
  } else {
  
      #estimate theta and sem
      cur_theta <- thetaEst(it = dat[,4:7], x = dat$response_num, method = "EAP", model = "GRM", D = 1, priorDist = "norm", priorPar = c(50, 10),
                            parInt = c(10, 90, 33))
      
      theta = cur_theta
      
      cur_sem <- semTheta(cur_theta, it = dat[,4:7], x = dat$response_num, method = "EAP", model = "GRM", D = 1, priorDist = "norm",
                        priorPar = c(50, 10), parInt = c(10, 90, 33))
      
  }

  # 
  it_next <- nextItem(itemBank = bank,
                      model = "GRM",
                      theta = theta,
                      out = done,
                      x = NULL,
                      criterion = "MFI",
                      method = "EAP",
                      priorDist = "norm", priorPar = c(50, 10),
                      D = 1, range = c(10, 90), parInt = c(10, 90, 33), infoType = "observed",
                      randomesque = 1, random.seed = NULL, rule = "length", thr = 20, SETH = NULL,
                      AP = 1, nAvailable = NULL, maxItems = 59, cbControl = acom_cbControl, cbGroup = acom_cb_group)
  # 
  
  data_out = list(theta = cur_theta,
                  sem = cur_sem,
                  next_item = it_next$item)
  
  print(data_out)
  return(data_out)
  
}

response_to_numeric <- function(select, clarify){
  # cannot do because of my communication problem = 0
  # cannot do for some other reason = NA
  # not very = 0
  # somewhat = 1
  # mostly = 2
  # completely = 3
  
  df = tibble(response = select, clarify = clarify) 
  #print(df)
  
  df %>%
    mutate(response_num = case_when(
      response == "Completely" ~ 3,
      response == "Mostly" ~ 2,
      response == "Somewhat" ~ 1,
      response == "Not very"~ 0,
      response == "Doesn't apply to me" ~ 0
    )) %>%
    mutate(response_num = case_when(
      response_num == 0 & clarify == "no" ~ NA_real_,
      response_num == 0 & clarify == "yes" ~ 0,
      TRUE ~ response_num
    )) %>%
    select(response, response_num, clarify)
  
}

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)

