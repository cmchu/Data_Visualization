if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {install.packages("RColorBrewer")}
if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("ggvis" %in% rownames(installed.packages()) == FALSE) {install.packages("ggvis")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")}

library(shiny)
library(ggplot2)
library(ggvis)
library(dplyr)
library(RColorBrewer)
library(scales)
library(reshape2)

data <- read.table('dataset_Facebook.csv', sep = ';', header = TRUE)
data <- na.omit(data)
data$Paid <- factor(data$Paid, levels=c(0,1), labels=c("Not Paid", "Paid"))
data$Post.Month <- factor(data$Post.Month, levels=c(1:12), labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec'))
data$Post.Weekday <- factor(data$Post.Weekday)
data$Post.Hour <- factor(data$Post.Hour)
data$Type <- factor(data$Type)
data$Category <- factor(data$Category)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(title="Heatmap",
             mainPanel(uiOutput("ggvis_ui"), ggvisOutput("ggvis")),
             sidebarPanel(
               selectInput("groupby_x", "X-axis", c("Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post")),
               selectInput("groupby_y", "Y-axis", c("Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post"), selected="Post Weekday"),
               selectInput("metric", "Metric (defaults to mean)", c("Mean", "Median", "Quantile", "Minimum", "Maximum")),
               uiOutput('quantile_num'),
               selectInput("aggregate_col", "Value of Interest", c("Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post")),
               actionButton(inputId = 'apply_changes_button', label = 'Apply Changes', style="color: #fff; background-color: #737373; border-color: #737373; margin-left: 55px")
             )),
    tabPanel(title="Small Multiples",
             mainPanel(plotOutput("scatter_plot",
                                  click = "scatter_click"),
                       HTML("<br><br><br>"),
                       plotOutput("zoomed_scatter")),
             sidebarPanel(
               selectInput("scatter_x", "X-axis", c("Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post")),
               selectInput("scatter_y", "Y-axis", c("Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post"), selected = "Number of Likes"),
               selectInput("scatter_color", "Color By:", c("None", "Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post", "Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post")),
               selectInput("scatter_size", "Size By:", c("None", "Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post")),
               selectInput("scatter_facet", "Facet By:", c("Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post"), selected="Post Type"),
               tags$div(class="header", checked=NA,
                        tags$i("Click on any plot for a zoomed view"))
             )),
    tabPanel(title="Parallel Coordinates",
             mainPanel(plotOutput("parallel_coord"),
                       htmlOutput("parallel_coord_text")),
             sidebarPanel(
               selectInput("parallel_variables", "Variables", c("Number of Comments" = "Number.of_Comments", 
                                                                "Number of Likes" = "Number.of_Likes", 
                                                                "Number of Shares" = "Number.of_Shares", 
                                                                "Total Number of Interactions" = "Total.Number_of.Interactions", 
                                                                "Lifetime Post Total Reach" = "Lifetime.Post_Total.Reach", 
                                                                "Lifetime Post Total Impressions" = "Lifetime.Post_Total.Impressions", 
                                                                "Lifetime Engaged Users" = "Lifetime_Engaged.Users", 
                                                                "Lifetime Post Consumers" = "Lifetime.Post_Consumers", 
                                                                "Lifetime Post Consumptions" = "Lifetime.Post_Consumptions", 
                                                                "Lifetime Post Impressions by people who like your page" = "Lifetime.Post_Impressions.by_people.who_like.your.page", 
                                                                "Lifetime Post Reach by people who like your page" = "Lifetime.Post_Reach.by_people.who_like.your.page", 
                                                                "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.Number_of.People_who.like_your.page_and.engaged_with.your.post"), 
                           multiple=TRUE, selected=c("Number.of_Comments", "Number.of_Likes", "Number.of_Shares", "Lifetime_Engaged.Users")),
               selectInput("parallel_color", "Color By:", c("None", "Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post", "Number of Comments", "Number of Likes", "Number of Shares", "Total Number of Interactions", "Lifetime Post Total Reach", "Lifetime Post Total Impressions", "Lifetime Engaged Users", "Lifetime Post Consumers", "Lifetime Post Consumptions", "Lifetime Post Impressions by people who like your page", "Lifetime Post Reach by people who like your page", "Lifetime Number of People who like your page and engaged with your post")),
               uiOutput("color_discrete"),
               uiOutput("color_continuous"),
               actionButton(inputId = 'parallel_apply_changes_button', label = 'Apply Changes', style="color: #fff; background-color: #737373; border-color: #737373; margin-left: 55px")
             ))
  )
)

server <- function(input, output, session) {
  
  #################
  #### Heatmap ####
  #################
  
  # to make select input reactive so the x- and y-axis cannot be the same variable
  groupby_y_choices <- reactive({
    groupby_y_vector <- c("Post Month", "Post Weekday", "Post Hour", "Post Type", "Post Category", "Paid Post")
    groupby_y_vector <- groupby_y_vector[groupby_y_vector != input$groupby_x]
  })
  observe({
    updateSelectInput(session, "groupby_y", choices = groupby_y_choices())
  })
  
  # to add numeric input only if the metric "Quantile" is selected
  output$quantile_num <- renderUI({
    if (input$metric != "Quantile") {
      return(NULL)
    } else {
      return(numericInput('quantile_num', 'Quantile (0 to 100)', 50, min=0, max=100))
    }
  })
  
  groupby_x <- reactive({
    switch(input$groupby_x, 
           "Post Month" = "Post.Month",
           "Post Weekday" = "Post.Weekday",
           "Post Hour" = "Post.Hour",
           "Post Type" = "Type",
           "Post Category" = "Category",
           "Paid Post" = "Paid")
  })
  
  groupby_y <- reactive({
    switch(input$groupby_y, 
           "Post Month" = "Post.Month",
           "Post Weekday" = "Post.Weekday",
           "Post Hour" = "Post.Hour",
           "Post Type" = "Type",
           "Post Category" = "Category",
           "Paid Post" = "Paid")
  })
  
  metric <- reactive({
    switch(input$metric,
           "Mean" = mean,
           "Median" = median,
           "Quantile" = function(x) quantile(x, probs=(input$quantile_num/100)),
           "Minimum" = min,
           "Maximum" = max)
  })
  
  aggregate_col <- reactive({
    switch(input$aggregate_col,
           "Number of Comments" = "comment", 
           "Number of Likes" = "like",
           "Number of Shares" = "share",
           "Total Number of Interactions" = "Total.Interactions",
           "Lifetime Post Total Reach" = "Lifetime.Post.Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post.Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime.Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post.Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post.Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post.Impressions.by.people.who.have.liked.your.Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post.reach.by.people.who.like.your.Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post")
  })

  # group data by 2 columns and use aggregate function for column of interest
  data_groupby <- reactive({
    input$apply_changes_button
    isolate({
      agg_formula <- formula(paste0(aggregate_col(), '~', groupby_x(), '+', groupby_y()))
      temp <- aggregate(agg_formula, data=data, FUN=metric())
      colnames(temp) <- c("x", "y", "value")
      temp$id <- 1:nrow(temp)
      temp
    })
  })
  
  hover_value <- function(x) {
    if(is.null(x)) return(NULL)
    row <- data_groupby()[data_groupby()$id == x$id,]
    paste0(round(row$value, 1))
  }
  
  reactive({
    input$apply_changes_button # to make this take dependency on the action button
    isolate({ # to make this NOT take dependency reactively
      data_groupby() %>%
        ggvis(~x, ~y, fill=~value, key:=~id) %>%
        layer_rects(width=band(), height=band(), strokeWidth:=0) %>%
        add_tooltip(hover_value, "hover") %>%
        scale_nominal("x", padding = 0) %>%
        scale_nominal("y", padding = 0) %>%
        scale_numeric("fill", range = c("#c6dbef", "#08306b")) %>%
        add_axis("x", title=input$groupby_x) %>%
        add_axis("y", title=input$groupby_y) %>%
        add_legend("fill", title=paste('Value of Interest', '-', input$metric))
    })
  }) %>% bind_shiny("ggvis", "ggvis_ui")
  
  #########################
  #### Small Multiples ####
  #########################
  
  scatter_x <- reactive({
    switch(input$scatter_x,
           "Number of Comments" = "comment", 
           "Number of Likes" = "like",
           "Number of Shares" = "share",
           "Total Number of Interactions" = "Total.Interactions",
           "Lifetime Post Total Reach" = "Lifetime.Post.Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post.Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime.Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post.Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post.Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post.Impressions.by.people.who.have.liked.your.Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post.reach.by.people.who.like.your.Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post")
  })
  
  scatter_y <- reactive({
    switch(input$scatter_y,
           "Number of Comments" = "comment", 
           "Number of Likes" = "like",
           "Number of Shares" = "share",
           "Total Number of Interactions" = "Total.Interactions",
           "Lifetime Post Total Reach" = "Lifetime.Post.Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post.Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime.Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post.Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post.Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post.Impressions.by.people.who.have.liked.your.Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post.reach.by.people.who.like.your.Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post")
  })
  
  scatter_color <- reactive({
    switch(input$scatter_color,
           "None" = NULL,
           "Post Month" = "Post.Month",
           "Post Weekday" = "Post.Weekday",
           "Post Hour" = "Post.Hour",
           "Post Type" = "Type",
           "Post Category" = "Category",
           "Paid Post" = "Paid",
           "Number of Comments" = "comment", 
           "Number of Likes" = "like",
           "Number of Shares" = "share",
           "Total Number of Interactions" = "Total.Interactions",
           "Lifetime Post Total Reach" = "Lifetime.Post.Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post.Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime.Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post.Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post.Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post.Impressions.by.people.who.have.liked.your.Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post.reach.by.people.who.like.your.Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post")
  })
  
  scatter_color_legend_title <- reactive({
    switch(input$scatter_color,
           "None" = NULL,
           "Post Month" = "Post Month",
           "Post Weekday" = "Post Weekday",
           "Post Hour" = "Post Hour",
           "Post Type" = "Post Type",
           "Post Category" = "Post Category",
           "Paid Post" = "Paid Post",
           "Number of Comments" = "# of Comments", 
           "Number of Likes" = "# of Likes",
           "Number of Shares" = "# of Shares",
           "Total Number of Interactions" = "Total # of\nInteractions",
           "Lifetime Post Total Reach" = "Lifetime Post\nTotal Reach", 
           "Lifetime Post Total Impressions" = "Lifetime Post\nTotal Impressions", 
           "Lifetime Engaged Users" = "Lifetime\nEngaged Users", 
           "Lifetime Post Consumers" = "Lifetime Post\nConsumers", 
           "Lifetime Post Consumptions" = "Lifetime Post\nConsumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime Post\nImpressions by\npeople who\nhave liked\nyour Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime Post\nreach by\npeople who\nlike your Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime People\nwho have\nliked your\nPage and\nengaged with\nyour post")
  })
  
  
  scatter_size <- reactive({
    switch(input$scatter_size,
           "None" = NULL,
           "Number of Comments" = "comment", 
           "Number of Likes" = "like",
           "Number of Shares" = "share",
           "Total Number of Interactions" = "Total.Interactions",
           "Lifetime Post Total Reach" = "Lifetime.Post.Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post.Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime.Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post.Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post.Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post.Impressions.by.people.who.have.liked.your.Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post.reach.by.people.who.like.your.Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post")
  })
  
  scatter_size_legend_title <- reactive({
    switch(input$scatter_size,
           "None" = NULL,
           "Number of Comments" = "# of Comments", 
           "Number of Likes" = "# of Likes",
           "Number of Shares" = "# of Shares",
           "Total Number of Interactions" = "Total # of\nInteractions",
           "Lifetime Post Total Reach" = "Lifetime Post\nTotal Reach", 
           "Lifetime Post Total Impressions" = "Lifetime Post\nTotal Impressions", 
           "Lifetime Engaged Users" = "Lifetime\nEngaged Users", 
           "Lifetime Post Consumers" = "Lifetime Post\nConsumers", 
           "Lifetime Post Consumptions" = "Lifetime Post\nConsumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime Post\nImpressions by\npeople who\nhave liked\nyour Page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime Post\nreach by\npeople who\nlike your Page" , 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime People\nwho have\nliked your\nPage and\nengaged with\nyour post")
  })
  
  scatter_facet <- reactive({
    switch(input$scatter_facet,
           "Post Month" = "Post.Month",
           "Post Weekday" = "Post.Weekday",
           "Post Hour" = "Post.Hour",
           "Post Type" = "Type",
           "Post Category" = "Category",
           "Paid Post" = "Paid")
  })
  
  output$scatter_plot <- renderPlot({
    if (is.null(scatter_color()) & is.null(scatter_size())) {
      data_subset <- data[,c(scatter_x(), scatter_y(), scatter_facet())]
      colnames(data_subset) <- c("x", "y", "facet")
      
      ggplot(data_subset) + 
        facet_wrap(~facet) + 
        geom_point(aes(x=x, y=y), color="#3182bd", alpha=.6) + 
        xlab(input$scatter_x) + ylab(input$scatter_y) + 
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 14)) + 
        theme(strip.background=element_rect(fill="#636363")) +
        theme(strip.text=element_text(colour='white', face='bold', size=11))
    } else if (is.null(scatter_color())) {
      data_subset <- data[,c(scatter_x(), scatter_y(), scatter_size(), scatter_facet())]
      colnames(data_subset) <- c("x", "y", "size", "facet")
      
      ggplot(data_subset) + 
        facet_wrap(~facet) + 
        geom_point(aes(x=x, y=y, size=size), color="#3182bd", alpha=.6) + 
        xlab(input$scatter_x) + ylab(input$scatter_y) + 
        guides(size=guide_legend(title=scatter_size_legend_title())) + 
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
        scale_size_continuous(labels=comma) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 14)) + 
        theme(strip.background=element_rect(fill="#636363")) +
        theme(strip.text=element_text(colour='white', face='bold', size=11))
    } else if (is.null(scatter_size())) {
      if (is.factor(data[[scatter_color()]])) {
        data_subset <- data[,c(scatter_x(), scatter_y(), scatter_color(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "color", "facet")
        
        ggplot(data_subset) + 
          facet_wrap(~facet) + 
          geom_point(aes(x=x, y=y, color=color), alpha=.6) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          scale_colour_manual(values=colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(data_subset[["color"]]))), labels=comma) + 
          guides(colour = guide_legend(override.aes = list(size=4), title=scatter_color_legend_title())) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 14)) + 
          theme(strip.background=element_rect(fill="#636363")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      } else {
        data_subset <- data[,c(scatter_x(), scatter_y(), scatter_color(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "color", "facet")
        
        ggplot(data_subset) + 
          facet_wrap(~facet) + 
          geom_point(aes(x=x, y=y, color=color), alpha=.8) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          scale_colour_continuous(low="#c6dbef", high="#08306b", labels=comma) + 
          guides(color=guide_colorbar(title=scatter_color_legend_title())) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 14)) + 
          theme(strip.background=element_rect(fill="#636363")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      }
    } else {
      if (is.factor(data[[scatter_color()]])) {
        data_subset <- data[,c(scatter_x(), scatter_y(), scatter_color(), scatter_size(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "color", "size", "facet")
        
        ggplot(data_subset) + 
          facet_wrap(~facet) + 
          geom_point(aes(x=x, y=y, color=color, size=size), alpha=.6) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          scale_colour_manual(values=colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(data_subset[["color"]]))), labels=comma) + 
          guides(colour=guide_legend(override.aes = list(size=4), title=scatter_color_legend_title()), size=guide_legend(title=scatter_size_legend_title())) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          scale_size_continuous(labels=comma) +
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 14)) + 
          theme(strip.background=element_rect(fill="#636363")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      } else {
        data_subset <- data[,c(scatter_x(), scatter_y(), scatter_color(), scatter_size(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "color", "size", "facet")
        
        ggplot(data_subset) + 
          facet_wrap(~facet) + 
          geom_point(aes(x=x, y=y, color=color, size=size), alpha=.8) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          scale_colour_continuous(low="#c6dbef", high="#08306b", labels=comma) + 
          guides(color=guide_colorbar(title=scatter_color_legend_title()), size=guide_legend(title=scatter_size_legend_title())) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          scale_size_continuous(labels=comma) +
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 14)) + 
          theme(strip.background=element_rect(fill="#636363")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      }
    }
  })
  
  # to display single facet plot if user clicks on one of the facets
  output$zoomed_scatter <- renderPlot({
    if (is.null(input$scatter_click)) {
      return(NULL)
    } else {
      facet <- input$scatter_click$panelvar1
      
      if (is.null(scatter_color()) & is.null(scatter_size())) {
        data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "facet")
        
        ggplot(data_subset) + 
          facet_grid(.~facet) + 
          geom_point(aes(x=x, y=y), color="#3182bd", alpha=.6, size=4) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_blank()) + 
          theme(strip.background=element_rect(fill="#bdbdbd")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      } else if (is.null(scatter_color())) {
        data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_size(), scatter_facet())]
        colnames(data_subset) <- c("x", "y", "size", "facet")
        
        ggplot(data_subset) + 
          facet_grid(.~facet) + 
          geom_point(aes(x=x, y=y, size=size), color="#3182bd", alpha=.6) + 
          xlab(input$scatter_x) + ylab(input$scatter_y) + 
          guides(size=FALSE) + 
          scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
          scale_size_continuous(labels=comma, limits=c(floor(min(data[[scatter_size()]])), ceiling(max(data[[scatter_size()]])))) +
          theme_bw() + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_blank()) + 
          theme(strip.background=element_rect(fill="#bdbdbd")) +
          theme(strip.text=element_text(colour='white', face='bold', size=11))
      } else if (is.null(scatter_size())) {
        if (is.factor(data[[scatter_color()]])) {
          data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_color(), scatter_facet())]
          colnames(data_subset) <- c("x", "y", "color", "facet")
          
          ggplot(data_subset) + 
            facet_grid(.~facet) + 
            geom_point(aes(x=x, y=y, color=color), alpha=.6, size=4) + 
            xlab(input$scatter_x) + ylab(input$scatter_y) + 
            scale_colour_manual(values=colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(data_subset[["color"]]))), labels=comma) + 
            guides(colour = FALSE) + 
            scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title = element_blank()) + 
            theme(strip.background=element_rect(fill="#bdbdbd")) +
            theme(strip.text=element_text(colour='white', face='bold', size=11))
        } else {
          data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_color(), scatter_facet())]
          colnames(data_subset) <- c("x", "y", "color", "facet")
          
          ggplot(data_subset) + 
            facet_grid(.~facet) + 
            geom_point(aes(x=x, y=y, color=color), alpha=.8, size=4) + 
            xlab(input$scatter_x) + ylab(input$scatter_y) + 
            scale_colour_continuous(low="#c6dbef", high="#08306b", limits=c(floor(min(data[[scatter_color()]])), ceiling(max(data[[scatter_color()]]))), labels=comma) + 
            guides(color=FALSE) + 
            scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title = element_blank()) + 
            theme(strip.background=element_rect(fill="#bdbdbd")) +
            theme(strip.text=element_text(colour='white', face='bold', size=11))
        }
      } else {
        if (is.factor(data[[scatter_color()]])) {
          data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_color(), scatter_size(), scatter_facet())]
          colnames(data_subset) <- c("x", "y", "color", "size", "facet")
          
          ggplot(data_subset) + 
            facet_grid(.~facet) + 
            geom_point(aes(x=x, y=y, color=color, size=size), alpha=.6) + 
            xlab(input$scatter_x) + ylab(input$scatter_y) + 
            scale_colour_manual(values=colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(data_subset[["color"]]))), labels=comma) + 
            guides(colour=FALSE, size=FALSE) + 
            scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
            scale_size_continuous(labels=comma, limits=c(floor(min(data[[scatter_size()]])), ceiling(max(data[[scatter_size()]])))) +
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title = element_blank()) + 
            theme(strip.background=element_rect(fill="#bdbdbd")) +
            theme(strip.text=element_text(colour='white', face='bold', size=11))
        } else {
          data_subset <- data[(data[[scatter_facet()]]==facet),c(scatter_x(), scatter_y(), scatter_color(), scatter_size(), scatter_facet())]
          colnames(data_subset) <- c("x", "y", "color", "size", "facet")
          
          ggplot(data_subset) + 
            facet_grid(.~facet) + 
            geom_point(aes(x=x, y=y, color=color, size=size), alpha=.8) + 
            xlab(input$scatter_x) + ylab(input$scatter_y) + 
            scale_colour_continuous(low="#c6dbef", high="#08306b", limits=c(floor(min(data[[scatter_color()]])), ceiling(max(data[[scatter_color()]]))), labels=comma) + 
            guides(color=FALSE, size=FALSE) + 
            scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) + 
            scale_size_continuous(labels=comma, limits=c(floor(min(data[[scatter_size()]])), ceiling(max(data[[scatter_size()]])))) +
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.title = element_blank()) + 
            theme(strip.background=element_rect(fill="#bdbdbd")) +
            theme(strip.text=element_text(colour='white', face='bold', size=11))
        }
      }
    }
  })
  
  ##############################
  #### Parallel Coordinates ####
  ##############################
  
  parallel_color <- reactive({
    switch(input$parallel_color,
           "None" = NULL,
           "Post Month" = "Post.Month",
           "Post Weekday" = "Post.Weekday",
           "Post Hour" = "Post.Hour",
           "Post Type" = "Post.Type",
           "Post Category" = "Post.Category",
           "Paid Post" = "Paid.Post",
           "Number of Comments" = "Number.of_Comments", 
           "Number of Likes" = "Number.of_Likes", 
           "Number of Shares" = "Number.of_Shares", 
           "Total Number of Interactions" = "Total.Number_of.Interactions", 
           "Lifetime Post Total Reach" = "Lifetime.Post_Total.Reach", 
           "Lifetime Post Total Impressions" = "Lifetime.Post_Total.Impressions", 
           "Lifetime Engaged Users" = "Lifetime_Engaged.Users", 
           "Lifetime Post Consumers" = "Lifetime.Post_Consumers", 
           "Lifetime Post Consumptions" = "Lifetime.Post_Consumptions", 
           "Lifetime Post Impressions by people who like your page" = "Lifetime.Post_Impressions.by_people.who_like.your.page", 
           "Lifetime Post Reach by people who like your page" = "Lifetime.Post_Reach.by_people.who_like.your.page", 
           "Lifetime Number of People who like your page and engaged with your post" = "Lifetime.Number_of.People_who.like_your.page_and.engaged_with.your.post")
  })
  
  data_copy <- data
  names(data_copy) <- c("Page.Total.Likes", "Post.Type", "Post.Category", "Post.Month", "Post.Weekday", 
                        "Post.Hour", "Paid.Post", "Lifetime.Post_Total.Reach", "Lifetime.Post_Total.Impressions", 
                        "Lifetime_Engaged.Users", "Lifetime.Post_Consumers", "Lifetime.Post_Consumptions", 
                        "Lifetime.Post_Impressions.by_people.who_like.your.page", 
                        "Lifetime.Post_Reach.by_people.who_like.your.page", 
                        "Lifetime.Number_of.People_who.like_your.page_and.engaged_with.your.post", 
                        "Number.of_Comments", "Number.of_Likes", "Number.of_Shares", "Total.Number_of.Interactions")
  
  # to add another select input only if a discrete color column is selected
  output$color_discrete <- renderUI({
    if (!is.null(parallel_color())) {
      if (is.factor(data_copy[[parallel_color()]])) {
        selectInput('parallel_color_discrete', 'Highlight By (depends on Color):', 
                    c("None", levels(data_copy[[parallel_color()]])))
      }
    }
  })
  
  # to add slider range input only if a continuous color column is selected
  output$color_continuous <- renderUI({
    if (!is.null(parallel_color())) {
      if (!is.factor(data_copy[[parallel_color()]])) {
        sliderInput('parallel_color_continuous', 'Highlight By (depends on Color):', 
                    min = min(data_copy[[parallel_color()]]), max = max(data_copy[[parallel_color()]]), 
                    value = c(min(data_copy[[parallel_color()]]), max(data_copy[[parallel_color()]])))
      }
    }
  })
  
  output$parallel_coord <- renderPlot({
    input$parallel_apply_changes_button
    isolate({
    if (is.null(input$parallel_variables) | (length(input$parallel_variables) == 1)) {
      return(NULL)
    } else {
      if (is.null(parallel_color())) {
        # altering dataset for plotting
        data_subset <- data_copy[,input$parallel_variables]
        # adding columns to dataset that have same range for each variable for plotting purposes
        n_variables <- length(input$parallel_variables)
        min_var1 <- min(data_subset[[input$parallel_variables[1]]])
        max_var1 <- max(data_subset[[input$parallel_variables[1]]])
        for (i in 1:n_variables) {
          data_subset[[paste0(input$parallel_variables[i],"_edit")]] <- rescale(data_subset[[input$parallel_variables[i]]], c(min_var1, max_var1))
        }
        
        # plot visualization
        p <- ggplot(data_subset)
        
        x <- 0
        interval <- 20/(n_variables-1)
        y_dist <- (max_var1 - min_var1)*0.025
        y_dist2 <- (max_var1 - min_var1)*0.07
        for (i in 1:(n_variables-1)) {
          p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), size=.2, color="#08519c")
          p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
          p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
          p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
          p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
          x <- x + interval
        }
        p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
        p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i+1]]])),x=x, y=max_var1+y_dist,size=3.5)
        p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i+1]]])), x=x, y=min_var1-y_dist, size=3.5)
        p <- p + annotate("text", label=LETTERS[i+1], x=x, y=min_var1-y_dist2, size=4.5)
        
        p + theme(panel.background = element_blank()) + 
          theme(panel.grid=element_blank()) + 
          theme(axis.ticks=element_blank()) + 
          theme(axis.text=element_blank()) + 
          xlab("") + ylab("") + 
          ylim((min_var1-y_dist2),(max_var1+y_dist))
      } else if (length(input$parallel_color_discrete) != 0) {
        if (input$parallel_color_discrete != "None") {
          # altering dataset for plotting
          data_subset <- data_copy[,c(input$parallel_variables, parallel_color())]
          # adding columns to dataset that have same range for each variable for plotting purposes
          n_variables <- length(input$parallel_variables)
          min_var1 <- min(data_subset[[input$parallel_variables[1]]])
          max_var1 <- max(data_subset[[input$parallel_variables[1]]])
          for (i in 1:n_variables) {
            data_subset[[paste0(input$parallel_variables[i],"_edit")]] <- rescale(data_subset[[input$parallel_variables[i]]], c(min_var1, max_var1))
          }
          data_subset_highlight <- data_subset[data_subset[[parallel_color()]] == input$parallel_color_discrete,]
          
          # plot visualization
          p <- ggplot(data_subset)
          
          x <- 0
          interval <- 20/(n_variables-1)
          y_dist <- (max_var1 - min_var1)*0.025
          y_dist2 <- (max_var1 - min_var1)*0.07
          for (i in 1:(n_variables-1)) {
            p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), color="#c6dbef", size=.2)
            p <- p + geom_segment(data=data_subset_highlight, aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), color="#08519c", size=.35)
            p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
            p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
            p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
            p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
            x <- x + interval
          }
          p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
          p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i+1]]])),x=x, y=max_var1+y_dist,size=3.5)
          p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i+1]]])), x=x, y=min_var1-y_dist, size=3.5)
          p <- p + annotate("text", label=LETTERS[i+1], x=x, y=min_var1-y_dist2, size=4.5)
          
          p + theme(panel.background = element_blank()) + 
            theme(panel.grid=element_blank()) + 
            theme(axis.ticks=element_blank()) + 
            theme(axis.text=element_blank()) + 
            xlab("") + ylab("") + 
            ylim((min_var1-y_dist2),(max_var1+y_dist))
        } else {
          # altering dataset for plotting
          data_subset <- data_copy[,c(input$parallel_variables, parallel_color())]
          # adding columns to dataset that have same range for each variable for plotting purposes
          n_variables <- length(input$parallel_variables)
          min_var1 <- min(data_subset[[input$parallel_variables[1]]])
          max_var1 <- max(data_subset[[input$parallel_variables[1]]])
          for (i in 1:n_variables) {
            data_subset[[paste0(input$parallel_variables[i],"_edit")]] <- rescale(data_subset[[input$parallel_variables[i]]], c(min_var1, max_var1))
          }
          
          # plot visualization
          p <- ggplot(data_subset)
          
          x <- 0
          interval <- 20/(n_variables-1)
          y_dist <- (max_var1 - min_var1)*0.025
          y_dist2 <- (max_var1 - min_var1)*0.07
          for (i in 1:(n_variables-1)) {
            p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit"), color=parallel_color()), size=.2)
            p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
            p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
            p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
            p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
            x <- x + interval
          }
          p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
          p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i+1]]])),x=x, y=max_var1+y_dist,size=3.5)
          p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i+1]]])), x=x, y=min_var1-y_dist, size=3.5)
          p <- p + annotate("text", label=LETTERS[i+1], x=x, y=min_var1-y_dist2, size=4.5)
          
          p + theme(panel.background = element_blank()) + 
            theme(panel.grid=element_blank()) + 
            theme(axis.ticks=element_blank()) + 
            theme(axis.text=element_blank()) + 
            xlab("") + ylab("") + 
            ylim((min_var1-y_dist2),(max_var1+y_dist)) + 
            scale_colour_manual(values=colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(data_subset[[parallel_color()]]))), labels=comma) + 
            guides(colour=guide_legend(override.aes = list(size=4), title=gsub('_', '\n', gsub('\\.', ' ', parallel_color()))))
        }
      } else {
      if ((input$parallel_color_continuous[1] == min(data_copy[[parallel_color()]])) & (input$parallel_color_continuous[2] == max(data_copy[[parallel_color()]]))) {
        # altering dataset for plotting
        data_subset <- data_copy[,c(input$parallel_variables, parallel_color())]
        # adding columns to dataset that have same range for each variable for plotting purposes
        n_variables <- length(input$parallel_variables)
        min_var1 <- min(data_subset[[input$parallel_variables[1]]])
        max_var1 <- max(data_subset[[input$parallel_variables[1]]])
        for (i in 1:n_variables) {
          data_subset[[paste0(input$parallel_variables[i],"_edit")]] <- rescale(data_subset[[input$parallel_variables[i]]], c(min_var1, max_var1))
        }

        # plot visualization
        p <- ggplot(data_subset)

        x <- 0
        interval <- 20/(n_variables-1)
        y_dist <- (max_var1 - min_var1)*0.025
        y_dist2 <- (max_var1 - min_var1)*0.07
        for (i in 1:(n_variables-1)) {
          p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit"), color=parallel_color()), size=.2)
          p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
          p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
          p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
          p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
          x <- x + interval
        }
        p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
        p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i+1]]])),x=x, y=max_var1+y_dist,size=3.5)
        p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i+1]]])), x=x, y=min_var1-y_dist, size=3.5)
        p <- p + annotate("text", label=LETTERS[i+1], x=x, y=min_var1-y_dist2, size=4.5)

        p + theme(panel.background = element_blank()) +
          theme(panel.grid=element_blank()) +
          theme(axis.ticks=element_blank()) +
          theme(axis.text=element_blank()) +
          xlab("") + ylab("") +
          ylim((min_var1-y_dist2),(max_var1+y_dist)) +
          scale_colour_continuous(low="#c6dbef", high="#08306b", labels=comma) +
          guides(color=guide_colorbar(title=gsub('_', '\n', gsub('\\.', ' ', parallel_color()))))
        } else {
          # altering dataset for plotting
          data_subset <- data_copy[,c(input$parallel_variables, parallel_color())]
          # adding columns to dataset that have same range for each variable for plotting purposes
          n_variables <- length(input$parallel_variables)
          min_var1 <- min(data_subset[[input$parallel_variables[1]]])
          max_var1 <- max(data_subset[[input$parallel_variables[1]]])
          for (i in 1:n_variables) {
            data_subset[[paste0(input$parallel_variables[i],"_edit")]] <- rescale(data_subset[[input$parallel_variables[i]]], c(min_var1, max_var1))
          }
          data_subset_highlight <- data_subset[(data_subset[[parallel_color()]] >= input$parallel_color_continuous[1]) & (data_subset[[parallel_color()]] <= input$parallel_color_continuous[2]),]

          # plot visualization
          p <- ggplot(data_subset)
          
          x <- 0
          interval <- 20/(n_variables-1)
          y_dist <- (max_var1 - min_var1)*0.025
          y_dist2 <- (max_var1 - min_var1)*0.07
          
          if (nrow(data_subset_highlight) != 0) { # to deal with the case when no rows are selected in user determined range
            for (i in 1:(n_variables-1)) {
              p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), color="#c6dbef", size=.2)
              p <- p + geom_segment(data=data_subset_highlight, aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), color="#08519c", size=.35)
              p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
              p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
              p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
              p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
              x <- x + interval
            }
          } else {
            for (i in 1:(n_variables-1)) {
              p <- p + geom_segment(aes_string(x=x, xend=(x+interval), y=paste0(input$parallel_variables[i],"_edit"), yend=paste0(input$parallel_variables[i+1],"_edit")), color="#c6dbef", size=.2)
              p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
              p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i]]])),x=x, y=max_var1+y_dist,size=3.5)
              p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i]]])), x=x, y=min_var1-y_dist, size=3.5)
              p <- p + annotate("text", label=LETTERS[i], x=x, y=min_var1-y_dist2, size=4.5)
              x <- x + interval
            }
          }
          
          p <- p + geom_segment(x=x, xend=x, y=min_var1, yend=max_var1, size=1.5)
          p <- p + annotate("text", label=paste0(max(data_subset[[input$parallel_variables[i+1]]])),x=x, y=max_var1+y_dist,size=3.5)
          p <- p + annotate("text", label=paste0(min(data_subset[[input$parallel_variables[i+1]]])), x=x, y=min_var1-y_dist, size=3.5)
          p <- p + annotate("text", label=LETTERS[i+1], x=x, y=min_var1-y_dist2, size=4.5)

          p + theme(panel.background = element_blank()) +
            theme(panel.grid=element_blank()) +
            theme(axis.ticks=element_blank()) +
            theme(axis.text=element_blank()) +
            xlab("") + ylab("") +
            ylim((min_var1-y_dist2),(max_var1+y_dist))
        }
      }
    }
    })
  })
  
  output$parallel_coord_text <- renderUI({
    if (is.null(input$parallel_variables) | (length(input$parallel_variables) == 1)) {
      return(NULL)
    } else {
      n_variables <- length(input$parallel_variables)
      var_letters <- LETTERS[1:n_variables]
      HTML(paste0(LETTERS[1:n_variables], " = ", gsub("_", " ", gsub("\\.", " ", input$parallel_variables)), "<br/>"))
    }
  })
}

shinyApp(ui = ui, server = server)


