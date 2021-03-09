library(tidyverse)
library(kableExtra)
library(jsonlite)

data_path <- "./Data/"
turo_raw <- fromJSON(paste0(data_path, "database.json"))

# Get a copy of turo
turo <- turo_raw

# Get number of images, and remove image variable
turo$num_images <- sapply(turo$images, function(x) nrow(x))
turo$images <- NULL

# Add owner listings count, placeholder, and remove owner variable
turo <- turo %>%
  add_count(owner$id, name = "owner_listings_count") %>% 
  mutate(owner_image_place_holder = owner$image$placeholder) %>% 
  select(!c("owner", "owner$id"))

turo$deliveryLabel <- NULL                       # redundant

turo$rate$averageDailyPriceWithCurrency <- NULL  # redundant
turo$rate$averageDailyPrice <- NULL              # redundant

turo$distanceWithUnit <- NULL                    # redundant

turo$location$country <- NULL                    # All entries are US, not useful
turo$location$locationSource <- NULL             # All entries are Google, not useful
turo$location$precision <- NULL                  # All entries are the same, not useful
turo$location$addressLines <- NULL               # redundant

# Response time
turo$responseTime <- within(
                       data = turo$responseTime,
                       expr = {
                         value[!is.na(unit) & unit == "HOUR"] <-
                           value[!is.na(unit) & unit == "HOUR"]*60
                         value[is.na(unit)] <- mean(value[!is.na(unit)])  # impute mean
                       }
                     )
turo$responseTimeMin <- turo$responseTime$value
turo$responseTime <- NULL

# Remove unwanted variables within vehicle variable
turo$vehicle <- turo$vehicle %>% 
  select(
    !c("image", "trim", "name", "marketCountry",
       "url", "registration", "marketCurrency", "id")
  )


turo <- turo %>% 
  mutate(
    rating = replace(rating, is.na(rating), mean(rating, na.rm = TRUE)),
    responseRate = replace(
                     x      = responseRate,
                     list   = is.na(responseRate),
                     values = mean(responseRate, na.rm = TRUE)
                   ),
    distanceLabel = as.numeric(
                      replace(
                        x      = distanceLabel,
                        list   = !is.na(distanceLabel),
                        values = gsub(
                                   pattern     = "(Within | mi)",
                                   replacement = "",
                                   x           = distanceLabel[!is.na(distanceLabel)]
                                 )
                      )
                    ),
    distanceLabelMi = replace(
                        x      = distanceLabel,
                        list   = is.na(distanceLabel),
                        values = mean(as.numeric(distanceLabel), na.rm = TRUE)
                      )
  ) %>% 
  select(!c("distanceLabel"))

# We flatten the list so we get the dataframe we want
turo <- flatten(turo)

turo <- turo %>%
  mutate_if(is.character, as.factor)


#write_csv(turo, paste0(data_path, "turo.csv"))
#########################################################################################

turo <- read_csv(paste0(data_path, "turo.csv"), col_types = cols()) %>% 
  mutate_if(is.character, as.factor)


head(turo) %>% 
  kable() %>% 
  kable_styling()


glimpse(turo)



## Plot

ggplot(turo, aes(rate.daily)) +
  geom_histogram(aes(y = after_stat(density)), bins = 100) +
  scale_x_log10() +
  geom_vline(xintercept = mean(turo$rate.daily))

length(levels(turo$businessClass))

names(turo)

model_1 <- lm(
             formula = log(rate.daily) ~ distance + renterTripsTaken +
               freeDeliveryPromotion + instantBookDisplayed + owner_image_place_holder +
               location.state + vehicle.automaticTransmission + vehicle.year +
               reviewCount + rating + newListing + num_images + responseTimeMin +
               vehicle.make + businessClass + rentableFromSearchedAirport +
               location.latitude + location.longitude + owner_listings_count,
             data    = turo
           )
summary(model_1)

plot(model_1$)