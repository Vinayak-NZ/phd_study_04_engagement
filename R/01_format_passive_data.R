## ---- rename-app-id-passive
names(app_v1_passive_data)[names(app_v1_passive_data) == "UUID"] <- "id"

names(app_v2_passive_data)[names(app_v2_passive_data) == "UUID"] <- "id"

## ---- create-version-indicator
app_v1_passive_data$version <- "Version 1"

app_v2_passive_data$version <- "Version 2"

## ---- filter-pregnant-women
app_v1_active_data <- app_v1_active_data[app_v1_active_data$Audience == "A1", ]

app_v2_active_data <- app_v2_active_data[app_v2_active_data$Audience == "A1", ]

app_v1_passive_data <- app_v1_passive_data[app_v1_passive_data$Audience == "A1", ]

app_v2_passive_data <- app_v2_passive_data[app_v2_passive_data$Audience == "A1", ]

## ---- rename-codename-variable
names(app_v1_passive_data)[names(app_v1_passive_data) == "Codename"] <- "codename_id"

names(app_v2_passive_data)[names(app_v2_passive_data) == "Codename"] <- "codename_id"

## ---- rename-value-variable
names(app_v1_passive_data)[names(app_v1_passive_data) == "Value"] <- "item_response"

names(app_v2_passive_data)[names(app_v2_passive_data) == "Value"] <- "item_response"

## ---- format-merge-variables
app_v1_passive_data$codename_id <- iconv(app_v1_passive_data$codename_id, 
                                         "latin1", "ASCII", sub="")

app_v1_item_page_match$codename_id <- iconv(app_v1_item_page_match$codename_id, 
                                            "latin1", "ASCII", sub="")

app_v1_item_page_match$page_id <- iconv(app_v1_item_page_match$page_id, 
                                            "latin1", "ASCII", sub="")

app_v1_page_count$page_id <- iconv(app_v1_page_count$page_id, 
                                        "latin1", "ASCII", sub="")

app_v2_passive_data$codename_id <- iconv(app_v2_passive_data$codename_id, 
                                         "latin1", "ASCII", sub="")

app_v2_item_page_match$codename_id <- iconv(app_v2_item_page_match$codename_id, 
                                            "latin1", "ASCII", sub="")

app_v2_item_page_match$page_id <- iconv(app_v2_item_page_match$page_id, 
                                        "latin1", "ASCII", sub="")

app_v2_page_count$page_id <- iconv(app_v2_page_count$page_id, 
                                   "latin1", "ASCII", sub="")

