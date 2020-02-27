library("hexbin")
library("tidyr")

library("jsonlite")

library("httr")

library("dplyr")

library("ggplot2")

library("shiny")

words <- c("this", "what", "is", "not", "making", "hello", "sense")
phrase <- paste(words[c(T, F)], collapse = " ")

name <- c("Ada", "Bob", "Chris", "Diya", "Emma")
height <- c(64, 74, 69, 69, 71)
weight <- c(135, 156, 139, 144, 152)
people <- data.frame(name, height, weight, stringsAsFactors = FALSE)

select(people, column)
select(people, name, height)
select(people, -name)

filter(people, height > 70, weight < 155)

people <- mutate(people, age = height / 3)

print(people)

summarise(people, max_height = max(height), avg_weight = mean(weight), count = n(), total_weight = sum(weight))


c(2.1, 2.98, 3.0999) %>% round(1)

par(mfrow=c(3, 2))

for (i in 1:nrow(outputhw4_4)) {
  num <- unlist(outputhw4_4[i, ], use.names = FALSE)
  hist(num, breaks= 200)
}

query_para <- list(q = "informatics")
GET("http://www.google.com/search", query = query_para)


mt_key <- TieWrcIAGGvYhYaPtXDuBEf5xkppzHTj

                        #function parameter(argument)
do_analysis <- function(search_term) {
  #path parameter
  endpoint <- paste0("/person/", person_id)
  #query parameter
para_list = list("api_key" = tmdb_key, "q" = search_term)
print(para_list)
response <- GET(uri, query = para_list)
body <- content(response, "text")
data <- body$results
}


colnames(midwest)

ggplot(data = midwest) + 
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = state))

ggplot(data = midwest) + 
  geom_col(mapping = aes(x = state, y = poptotal))

ggplot(data = midwest) +
  geom_hex(mapping = aes(x = percollege, y = percadultpoverty))


ggplot(data = midwest, mapping = aes(x = percollege, y = percadultpoverty)) +
  geom_point() + # uses the default x and y mappings
  geom_smooth() + # uses the default x and y mappings
  geom_point(mapping = aes(y = percchildbelowpovert)) # uses own y mapping



ggplot(data = midwest) + 
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = state)) +
  scale_x_reverse() + 
  scale_y_continuous(limits = c(0, 40)) + 
  scale_color_brewer(palette = "Set3") + 
  scale_color_hue(l = 20, c = 30)

top_10 <- midwest %>%
  top_n(10, wt = poptotal) %>%
  unite(county_state, county, state, sep = ", ") %>% # combine state + county
  arrange(poptotal) %>% # sort the data by population
  mutate(location = factor(county_state, county_state)) # set the row order


ggplot(top_10) +
  geom_col(mapping = aes(x = location, y = poptotal)) +
  coord_flip() # switch the orientation of the x- and y-axes

ggplot(mpg, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")

midwest


ordered_midwest <- midwest %>% 
  arrange(area)

ggplot(data = midwest) + 
  #                         order state byt area
  geom_col(mapping = aes(x = reorder(state, area), y = area))

my_plot <- ggplot(data = midwest) + 
  #                         order state byt area
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = state)) +
  labs(color = "STATE") +
  scale_color_discrete(labels = c("a", "b", "c", "d", "e")) +
  theme_void()
  

View(midwest)

nrow(midwest)


result <- midwest %>% 
  gather(key = "race", value = "count", c("popwhite", "popblack"))

View(result)

ggsave("my_beautiful_plot.png", my_plot)

rect <- data.frame(x_coords = c(2, 6, 5, 3), y_coords = c(1))

us_states <- map_data("state")
View(us_states)

ggplot(data = us_states) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap()

state_nums <- us_states %>% 
  select(region) %>% 
  distinct() %>% 
  mutate(value = 1:49)

map_with_nums <- left_join(us_states, state_nums, by = "region")

ggplot(data = map_with_nums) + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = value)) + 
  coord_quickmap()





my_ui <- fluidPage(
  h1("Hello Shiny"),
  #controls
  sidebarLayout(
    sidebarPanel(
    textInput(inputId = 'user_name', label = "What is your name?"),
    textInput(inputId = 'food', label = "What is your favorite food?"),
    sliderInput(inputId = 'slider_value', label = 'What is your favorite number?',
    min = 0, max = 10, value = c(0, 10)),
    radioButtons(inputId = "Radio", label = "I'm a radio", choices = c("Coffee", "Tea,", "Beer"), selected = "Beer"),
    ),
    #content
    mainPanel(
      textOutput(outputId = "message"),
      p("Look at that ", strong("DIRECTION"), "Wooohoooo!")
    )
  ) 
)

my_server <- function(input_list, output_list) {
  
  output_list$message <- renderText({
    greeting <- paste("Hello", input_list$user_name, "Have some", input_list$food)
    return (greeting)
  })
}

shinyApp(ui = my_ui, server = my_server)
