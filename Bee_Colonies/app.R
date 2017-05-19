library(shiny)
library(ggvis)
library(dplyr)
library(plyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(networkD3)
library(plotly)
library(shinythemes)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

######################
#### Prepare Data ####
######################

colonies <- read.csv('clean_data/colonies.csv', stringsAsFactors = FALSE)
honey <- read.csv('clean_data/honey.csv', stringsAsFactors = FALSE)
pollination <- read.csv('clean_data/pollination.csv', stringsAsFactors = FALSE)
stressors <- read.csv('clean_data/stressors.csv', stringsAsFactors = FALSE)

# get map data
map <- ggplot2::map_data("state") %>% select(long, lat, group, order, region)

# cleaning
# make all first letters of state names capitalized for merging
map$region <- sapply(map$region, simpleCap)
# remove all rows with year = 2016 in colonies and stressors data
colonies <- colonies[colonies$year != 2016,]
stressors <- stressors[stressors$year != 2016,]

# merge colonies with stressors data
colonies_stressors <- merge(colonies, stressors, by=c("state", "time_period", "year"))

###################
#### Shiny App ####
###################

ui <- fluidPage(theme = shinytheme("paper"),
                tabsetPanel(
                  tabPanel(title="Data Description",
                           tags$h3(tags$b("A Study of the Pollination, Honey Production, and Risk Factors for Bees in the US")),
                           "Bee populations are declining at unusually high rates. Much of this is due to climate change, parasites, 
                           diseases, and industrial agriculture. This is an alarming phenomenon as bees are essential for food production. 
                           In these visualizations, I provide insight into this phenomenon in order to help users understand this issue 
                           better.", tags$hr(), tags$h4(tags$b(tags$u("Dataset"))), "The public data set I worked with is from the United States Department of Agriculture (USDA). 
                           The USDA collects data on stressors to bee colonies, changes in numbers of bee colonies, honey production, and 
                           pollination costs in order to help track risk factors and mortality for our nation’s main pollinators. 
                           I specifically looked at 3 csv files spanning 2015. The files analyzed consisted of numerical, categorical, 
                           as well as temporal data.", tags$br(), tags$br(), 
                           "I gathered data from the following sources:", 
                           tags$ol(
                             tags$li(tags$a(href="https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Bee_and_Honey/", 
                                            "https://www.nass.usda.gov/Surveys/Guide_to_NASS_Surveys/Bee_and_Honey/")),
                             tags$li(tags$a(href="http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1191",
                                            "http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=1191")),
                             tags$li(tags$a(href="http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=2008",
                                            "http://usda.mannlib.cornell.edu/MannUsda/viewDocumentInfo.do?documentID=2008"))), tags$hr(), 
                           tags$h4(tags$b(tags$u("Description of Visualizations"))), tags$h5("Overview"), "This visualization provides a general 
                           overview of the datasets. It provides a high level of interactivity to the user to understand and 
                           explore all different aspects of the data.", tags$h5("Bee Colony Numbers & Stressors"), 
                           "This visualization provides the user with a geographical interface, as well as with more detailed 
                           information about bee colony numbers and stressors to bee colonies in each state."),
                  tabPanel(title="Overview",
                           sidebarPanel(selectInput("overview_dataset", "Step 1. Choose a Dataset", c("Bee Colony Numbers", "Stressors to Bee Colonies", "Pollination", "Honey Production")),
                                        uiOutput("overview_step2"),
                                        uiOutput("overview_step3"),
                                        uiOutput("overview_step4"),
                                        uiOutput("overview_step5"),
                                        tags$div(class="header", style="font-size:80%", checked=NA,
                                                 "All of the data in this visualization is for the year 2015."),
                                        width=3),
                           mainPanel(tags$style(type="text/css", # to suppress error messages that pop up very shortly when reacting to user input
                                                ".shiny-output-error { visibility: hidden; }",
                                                ".shiny-output-error:before { visibility: hidden; }"),
                                     conditionalPanel(condition = "input.overview_dataset == 'Bee Colony Numbers'",
                                                      plotOutput("overview1", height=600, width=900)),
                                     conditionalPanel(condition = "input.overview_dataset == 'Stressors to Bee Colonies'",
                                                      plotOutput("overview2", height=600, width=900)),
                                     conditionalPanel(condition = "input.overview_dataset == 'Pollination'",
                                                      diagonalNetworkOutput("overview3")),
                                     conditionalPanel(condition = "input.overview_dataset == 'Honey Production'",
                                                      plotOutput("overview4", height=600, width=900)))),
                  tabPanel(title="Bee Colony Numbers & Stressors",
                           mainPanel(fluidRow(column(width=6, offset=0,
                                                     selectInput("map_choropleth", "Color By:", c("Maximum Number of Colonies" = "max_colonies", 
                                                                                                  "Number of Colonies Lost" = "lost_colonies",
                                                                                                  "Percentage of Colonies Lost" = "percent_lost_colonies",
                                                                                                  "Number of Colonies Added" = "added_colonies",
                                                                                                  "Number of Renovated Colonies" = "renovated_colonies",
                                                                                                  "Percentage of Colonies Renovated" = "percent_renovated_colonies",
                                                                                                  "Percentage of Colonies Affected by Varroa Mites" = "varroa_mites",
                                                                                                  "Percentage of Colonies Affected by Other Pests or Parasites" = "other_pests_parasites",
                                                                                                  "Percentage of Colonies Affected by Disease" = "diseases",
                                                                                                  "Percentage of Colonies Affected by Pesticides" = "pesticides",
                                                                                                  "Percentage of Colonies Affected by Other Factors" = "other",
                                                                                                  "Percentage of Colonies Affected by Unknown Factors" = "unknown"),
                                                                 selected="percent_lost_colonies")),
                                              column(width=6, offset=0,
                                                     selectInput("map_time_period", "Time Period:", c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")))),
                                     plotlyOutput("map_bar"), width=7
                           ),
                           sidebarPanel(tags$style(".well {background-color:white;}"), 
                                        tags$b("Plot Summary:"), tags$br(),
                                        tags$div(class="header", style="font-size:90%", checked=NA,
                                                 tags$i("Bee populations are declining at unusually high rates 
                                                        due to climate change, parasites, 
                                                        diseases, and industrial agriculture. All data in 
                                                        this visualization is for the year 2015. 
                                                        White states have no data.")), 
                                        # tags$div(class="header", style="font-size:80%", checked=NA,
                                        #          "All of the data in these visualizations are for the year 2015. 
                                        #          White states have no data."),
                                        tags$div(class="header", checked=NA, style="text-align:center",
                                                 tags$i("Click on a state for more detailed info about that specific state.")),
                                        plotOutput("map_click_plot", click = "map_click_vals"), width=5),
                           plotOutput("map_bar_plot", height="200px")
                                        )
                  )
  )

server <- function(input, output) {
  
  #######################
  #### Overview Plot ####
  #######################
  
  output$overview_step2 <- renderUI({
    if (is.null(input$overview_dataset)) {
      return(NULL)
    } else if (input$overview_dataset == "Bee Colony Numbers") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Bee Colony Numbers Fields", "Time Period")))
    } else if (input$overview_dataset == "Stressors to Bee Colonies") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Stressors Fields", "Time Period")))
    } else if (input$overview_dataset == "Pollination") {
      return(selectInput("overview_colonies_facet", "Step 2: Choose a Grouping", c("Crop Category", "Region")))
    } else if (input$overview_dataset == "Honey Production") {
      return(checkboxGroupInput("overview_colonies_facet", "Step 2: Choose Groups to Visualize", c("# of Honey Producing Colonies (1,000 Colonies)",
                                                                                                   "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                                                                                                   "Stocks Held by Honey Producers (1,000 lbs)", 
                                                                                                   "Average Price Per Pound of Honey (cents)", 
                                                                                                   "Value of Honey Production (1,000 USD)")))
    }
  })
  
  output$overview_step3 <- renderUI({
    if (input$overview_dataset == "Honey Production") {
      state_names <- unique(honey$state)
      len_state_names <- length(state_names)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_states", "Step 3: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    }
    
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if ((input$overview_colonies_facet == "Bee Colony Numbers Fields") & (input$overview_dataset == "Bee Colony Numbers")) {
      return(checkboxGroupInput("overview_colonies_fields", "Step 3: Choose Groups to Visualize", c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                                                    "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                                                    "Number of Colonies Renovated", "Percent of Colonies Renovated")))
    } else if ((input$overview_colonies_facet == "Time Period") & (input$overview_dataset == "Bee Colony Numbers")) {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Category", c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                                    "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                                    "Number of Colonies Renovated", "Percent of Colonies Renovated")))
    } else if ((input$overview_colonies_facet == "Stressors Fields") & (input$overview_dataset == "Stressors to Bee Colonies")) {
      return(checkboxGroupInput("overview_colonies_fields", "Step 3: Choose Groups to Visualize", c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                                                    "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                                                    "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors")))
    } else if ((input$overview_colonies_facet == "Time Period") & (input$overview_dataset == "Stressors to Bee Colonies")) {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Category", c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                                    "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                                    "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors")))
    } else if (input$overview_colonies_facet == "Crop Category") {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Region", c("1", "2", "3", "4", "5", "6 & 7")))
    } else if (input$overview_colonies_facet == "Region") {
      return(selectInput("overview_colonies_fields", "Step 3: Choose a Crop Category", c("Tree fruit", "Melons", "Berries",
                                                                                         "Vegetables", "Other",
                                                                                         "Tree nuts", "Other fruit")))
    }
  })
  
  output$overview_step4 <- renderUI({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Bee Colony Numbers Fields", "Stressors Fields")) {
      return(selectInput("overview_colonies_time_period", "Step 4: Choose a Time Period", c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")))
    } else if (input$overview_colonies_facet == "Time Period") {
      state_names <- unique(honey$state)
      len_state_names <- length(state_names)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_states", "Step 4: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    } else if (input$overview_colonies_facet %in% c("Crop Category", "Region")) {
      return(selectInput("overview_colonies_category", "Step 4: Choose a Category", c("Number of Paid Pollinated Acres", "Pollination Price Per Acre (USD)", "Number of Colonies Used for Pollination",
                                                                                      "Pollination Price Per Colony (USD)", "Total Value of Pollination (1,000 USD)")))
    }
  })
  
  output$overview_step5 <- renderUI({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Bee Colony Numbers Fields", "Stressors Fields")) {
      state_names <- unique(honey$state)
      state_names <- state_names[c(-(length(state_names)-1), -length(state_names))]
      return(selectInput("overview_colonies_state", "Step 5: Choose States to Highlight", state_names, multiple=TRUE, selected=NULL))
    } else if (input$overview_colonies_facet == "Time Period") {
      return(NULL)
    } else if (input$overview_colonies_facet %in% c("Crop Category", "Region")) {
      return(tags$div(class="header", style="font-size:70%", checked=NA, tags$b("Region 1"), ": Connecticut, Illinois, Indiana, Iowa, Kansas, Massachusetts, Maine,
                      Michigan, Nebraska, New Hampshire, New Jersey, New York, Ohio, Pennsylvania, Rhode Island, Vermont, Wisconsin", tags$br(),
                      tags$b("Region 2"), ": Alabama, Delaware, Georgia, Kentucky, Maryland, North Carolina, South Carolina, Tennessee, Virginia, West Virginia", tags$br(),
                      tags$b("Region 3"), ": Arkansas, Florida, Louisiana, Missouri, Mississippi, New Mexico, Oklahoma, Texas", tags$br(),
                      tags$b("Region 4"), ": Colorado, Minnesota, Montana, Nevada, North Dakota, South Dakota, Utah, Wyoming", tags$br(),
                      tags$b("Region 5"), ": Alaska, Idaho, Oregon, Washington", tags$br(),
                      tags$b("Region 6 & 7"), ": Arizona, California, Hawaii"))
    }
  })
  
  #### Bee Colony Number Plot ####
  output$overview1 <- renderPlot({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet == "Bee Colony Numbers Fields") {
      df <- colonies
      names(df) <- c("State", "Number of Colonies", "Maximum Number of Colonies", "Number of Colonies Lost", "Percent of Colonies Lost", 
                     "Number of Colonies Added", "Number of Colonies Renovated", "Percent of Colonies Renovated", "Time Period", "Year")
      df <- melt(df, id.vars = c("State", "Time Period"), measure.vars = c("Maximum Number of Colonies", "Number of Colonies Lost", 
                                                                           "Percent of Colonies Lost", "Number of Colonies Added", 
                                                                           "Number of Colonies Renovated", "Percent of Colonies Renovated"))
      df["state_abb"] <- state.abb[match(df$State,state.name)]
      df <- df %>% drop_na
      
      if (length(input$overview_colonies_fields) == 0) {
        df_subset <- df[df[["Time Period"]]==input$overview_colonies_time_period,]
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
        
      } else {
        df_subset <- df[(df[["Time Period"]]==input$overview_colonies_time_period) & (df$variable %in% input$overview_colonies_fields),]
        df_subset$variable <- factor(df_subset$variable)
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
      
    } else { # if facet is by time period
      if (is.null(input$overview_colonies_fields)) {
        return(NULL)
      } else {
        df <- colonies
        names(df) <- c("State", "Number of Colonies", "Maximum Number of Colonies", "Number of Colonies Lost", "Percent of Colonies Lost", 
                       "Number of Colonies Added", "Number of Colonies Renovated", "Percent of Colonies Renovated", "Time Period", "Year")
        df["state_abb"] <- state.abb[match(df$State,state.name)]
        df <- df %>% drop_na
        
        df_subset <- df[,c(input$overview_colonies_fields, "Time Period", "state_abb", "State")]
        df_subset[["Time Period"]] <- factor(df_subset[["Time Period"]], levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
        names(col_palette) <- input$overview_colonies_states
        colonies_facet_plots <- lapply(split(df_subset,df_subset[["Time Period"]]), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x[[input$overview_colonies_fields]],decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`')), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`'), fill="State"), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x[["Time Period"]]))
          ggplotGrob(p)
        })
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
    }
  })
  
  #### Stressors to Bee Colonies Plot ####
  output$overview2 <- renderPlot({
    if (is.null(input$overview_colonies_facet)) {
      return(NULL)
    } else if (input$overview_colonies_facet == "Stressors Fields") {
      df <- stressors
      names(df) <- c("State", "% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                     "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                     "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors",
                     "Time Period", "Year")
      df <- melt(df, id.vars = c("State", "Time Period"), measure.vars = c("% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                                                                           "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                                                                           "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors"))
      df["state_abb"] <- state.abb[match(df$State,state.name)]
      df <- df %>% drop_na
      
      if (length(input$overview_colonies_fields) == 0) {
        df_subset <- df[df[["Time Period"]]==input$overview_colonies_time_period,]
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
        
      } else {
        df_subset <- df[(df[["Time Period"]]==input$overview_colonies_time_period) & (df$variable %in% input$overview_colonies_fields),]
        df_subset$variable <- factor(df_subset$variable)
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_state))
        names(col_palette) <- input$overview_colonies_state
        colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_state,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x$variable))
          ggplotGrob(p)
        })
        
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
      
    } else { # if facet is by time period
      if (is.null(input$overview_colonies_fields)) {
        return(NULL)
      } else {
        df <- stressors
        names(df) <- c("State", "% Colonies Affected by Varroa Mites", "% Colonies Affected by Other Pests/Parasites",
                       "% Colonies Affected by Diseases", "% Colonies Affected by Pesticides",
                       "% Colonies Affected by Other Factors", "% Colonies Affected by Unknown Factors",
                       "Time Period", "Year")
        df["state_abb"] <- state.abb[match(df$State,state.name)]
        df <- df %>% drop_na
        
        df_subset <- df[,c(input$overview_colonies_fields, "Time Period", "state_abb", "State")]
        df_subset[["Time Period"]] <- factor(df_subset[["Time Period"]], levels = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec"))
        col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
        names(col_palette) <- input$overview_colonies_states
        colonies_facet_plots <- lapply(split(df_subset,df_subset[["Time Period"]]), function(x){
          x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x[[input$overview_colonies_fields]],decreasing=T)])
          p <- ggplot(x) +
            geom_bar(aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`')), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
            geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes_string(x="state_abb", y=paste0('`', input$overview_colonies_fields, '`'), fill="State"), stat="identity") + 
            scale_fill_manual(values=col_palette) + 
            theme_bw() + 
            theme(legend.position="none", axis.title = element_blank(),
                  plot.title = element_text(hjust=0.5, face="bold"),
                  panel.grid = element_blank()) + 
            scale_y_continuous(labels = comma) + 
            ggtitle(unique(x[["Time Period"]]))
          ggplotGrob(p)
        })
        grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      }
    }
  })
  
  #### Pollination Plot ####
  output$overview3 <- renderDiagonalNetwork({
    
    df <- pollination[(pollination$year==2015),]
    names(df) <- c("Crop", "Number of Paid Pollinated Acres", "Pollination Price Per Acre (USD)", "Number of Colonies Used for Pollination",
                   "Pollination Price Per Colony (USD)", "Total Value of Pollination (1,000 USD)", "Crop Category", "Region", "Year")
    
    # function to transform data frame into list format needed for radialNetwork function
    maketreelist <- function(df, root = df[1, 1]) { # http://stackoverflow.com/questions/23839142/transform-a-dataframe-into-a-tree-structure-list-of-lists
      if(is.factor(root)) root <- as.character(root)
      r <- list(name = root)
      children = df[df[, 1] == root, 2]
      if(is.factor(children)) children <- as.character(children)
      if(length(children) > 0) {
        r$children <- lapply(children, maketreelist, df = df)
      }
      r
    }
    if ((is.null(input$overview_colonies_facet)) | (is.null(input$overview_colonies_fields) | (is.null(input$overview_colonies_category)))) {
      return(NULL)
    } else if (input$overview_colonies_facet=="Crop Category") {
      df_subset <- df[df[["Region"]]==input$overview_colonies_fields,]
      df_subset$crop_val <- paste(df_subset$Crop, df_subset[[input$overview_colonies_category]], sep=" - ")
      df_subset <- df_subset[order(-df_subset[[input$overview_colonies_category]]),]
      df_subset <- df_subset[,c("Crop Category", "crop_val")]
      temp <- lapply(split(df_subset, df_subset[["Crop Category"]]), maketreelist)
      temp <- list(name="Crop", children=temp)
      names(temp$children) <- NULL
      diagonalNetwork(temp, fontSize = 20, linkColour = "#d4b9da", nodeColour = "#88419d", nodeStroke = "#88419d", textColour = "#081d58")
      
    } else if (input$overview_colonies_facet=="Region") {
      df_subset <- df[df[["Crop Category"]]==input$overview_colonies_fields,]
      df_subset$crop_val <- paste(df_subset$Crop, df_subset[[input$overview_colonies_category]], sep=" - ")
      df_subset$Region <- paste("Region", df_subset$Region)
      df_subset <- df_subset[order(-df_subset[[input$overview_colonies_category]]),]
      df_subset <- df_subset[,c("Region", "crop_val")]
      temp <- lapply(split(df_subset, df_subset[["Region"]]), maketreelist)
      temp <- list(name="Crop", children=temp)
      names(temp$children) <- NULL
      diagonalNetwork(temp, fontSize = 20, linkColour = "#d4b9da", nodeColour = "#88419d", nodeStroke = "#88419d", textColour = "#081d58")
    }
  })
  
  #### Honey Production Plot ####
  output$overview4 <- renderPlot({
    df <- honey
    names(df) <- c("State", "# of Honey Producing Colonies (1,000 Colonies)", "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                   "Stocks Held by Honey Producers (1,000 lbs)", "Average Price Per Pound of Honey (cents)", 
                   "Value of Honey Production (1,000 USD)", "Year")
    df <- melt(df, id.vars = c("State"), measure.vars = c("# of Honey Producing Colonies (1,000 Colonies)", "Honey Yield per Colony (lbs)", "Honey Production (1,000 lbs)",
                                                          "Stocks Held by Honey Producers (1,000 lbs)", "Average Price Per Pound of Honey (cents)", 
                                                          "Value of Honey Production (1,000 USD)"))
    df["state_abb"] <- state.abb[match(df$State,state.name)]
    df <- df %>% drop_na
    
    if (length(input$overview_colonies_facet) == 0) {
      col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
      names(col_palette) <- input$overview_colonies_states
      colonies_facet_plots <- lapply(split(df,df$variable), function(x){
        x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
        p <- ggplot(x) +
          geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
          geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
          scale_fill_manual(values=col_palette) + 
          theme_bw() + 
          theme(legend.position="none", axis.title = element_blank(),
                plot.title = element_text(hjust=0.5, face="bold"),
                panel.grid = element_blank()) + 
          scale_y_continuous(labels = comma) + 
          ggtitle(unique(x$variable))
        ggplotGrob(p)
      })
      
      grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
      
    } else {
      df_subset <- df[(df$variable %in% input$overview_colonies_facet),]
      df_subset$variable <- factor(df_subset$variable)
      col_palette <- colorRampPalette(brewer.pal(8,"Paired"))(length(input$overview_colonies_states))
      names(col_palette) <- input$overview_colonies_states
      colonies_facet_plots <- lapply(split(df_subset,df_subset$variable), function(x){
        x$state_abb <- factor(x$state_abb, levels=x$state_abb[order(x$value,decreasing=T)])
        p <- ggplot(x) +
          geom_bar(aes(x=state_abb, y=value), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
          geom_bar(data=x[x$State %in% input$overview_colonies_states,], aes(x=state_abb, y=value, fill=State), stat="identity") + 
          scale_fill_manual(values=col_palette) + 
          theme_bw() + 
          theme(legend.position="none", axis.title = element_blank(),
                plot.title = element_text(hjust=0.5, face="bold"),
                panel.grid = element_blank()) + 
          scale_y_continuous(labels = comma) + 
          ggtitle(unique(x$variable))
        ggplotGrob(p)
      })
      
      grid.arrange(do.call(rbind, colonies_facet_plots), ncol=1)
    }
  })
  
  #############################
  #### Map with Bar Charts ####
  #############################
  
  map_choropleth <- reactive({
    switch(input$map_choropleth, 
           "max_colonies" = "Maximum Number\nof Colonies", 
           "lost_colonies" = "Number of\nColonies Lost",
           "percent_lost_colonies" = "Percentage of\nColonies Lost (%)",
           "added_colonies" = "Number of\nColonies Added",
           "renovated_colonies" = "Number of\nRenovated Colonies",
           "percent_renovated_colonies" = "Percentage of\nColonies Renovated (%)",
           "varroa_mites" = "Percentage of\nColonies Affected\nby Varroa Mites (%)",
           "other_pests_parasites" = "Percentage of Colonies\nAffected by Other\nPests or Parasites (%)",
           "diseases" = "Percentage of Colonies\nAffected by Disease (%)",
           "pesticides" = "Percentage of Colonies\nAffected by Pesticides (%)",
           "other" = "Percentage of\nColonies Affected\nby Other Factors (%)",
           "unknown" = "Percentage of\nColonies Affected by\nUnknown Factors (%)")
  })
  
  colonies_stressors_subset <- reactive({
    temp <- colonies_stressors %>% filter(time_period == input$map_time_period)
    temp$hover <- with(temp, paste(state))
    names(temp)[names(temp)==input$map_choropleth] <- 'color_col'
    temp["state_abb"] <- state.abb[match(temp$state,state.name)]
    temp <- temp %>% drop_na
    temp["id"] <- 0:(nrow(temp)-1)
    temp
  })
  
  output$map_bar <- renderPlotly({
    
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(colonies_stressors_subset(), locationmode = 'USA-states') %>%
      add_trace(z=~color_col, text=~hover, locations=~state_abb, color=~color_col, colors='Reds') %>%
      colorbar(title="") %>%
      layout(geo=g)
  })
  
  output$map_click_plot <- renderPlot({
    map_click_vals <- event_data("plotly_click")
    map_click_vals$state <- colonies_stressors_subset()[colonies_stressors_subset()$id == map_click_vals[["pointNumber"]],"state"]
    
    if (is.null(map_click_vals[["pointNumber"]])) {
      return(NULL)
    } else {
      df1 <- colonies_stressors[colonies_stressors$state == map_click_vals$state, ] %>%
        select_("time_period", "state", input$map_choropleth) %>% drop_na
      p1 <- ggplot(df1) +
        geom_bar(aes_string("time_period", input$map_choropleth), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("time_period", input$map_choropleth, label=input$map_choropleth), vjust=-.25) +
        theme(axis.title.x = element_blank(), panel.background = element_blank(),
              panel.grid = element_blank(), axis.line = element_line(colour = "black"),
              plot.title=element_text(hjust = 0.5, face="bold", size=18)) +
        ylab(map_choropleth()) +
        ggtitle(unique(df1$state)[1]) +
        scale_y_continuous(labels = comma, expand = c(0.2,0))
      
      df2 <- colonies_stressors[(colonies_stressors$state == map_click_vals$state) & (colonies_stressors$time_period == input$map_time_period), ] %>% drop_na
      colnames(df2) <- c("state", "time_period", "year", "num_colonies", "Max #\nColonies", "# Colonies\nLost",
                         "percent_lost_colonies", "# Colonies\nAdded", "# Colonies\nRenovated", "percent_renovated_colonies",
                         "Varroa Mites", "Other Pests/\nParasites", "Diseases", "Pesticides", "Other", "Unknown")
      p2 <- ggplot(melt(df2, measure.vars = c("Max #\nColonies", "# Colonies\nLost", "# Colonies\nAdded", "# Colonies\nRenovated"))) +
        geom_bar(aes_string("variable", "value"), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("variable", "value", label="value"), vjust=-.25) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title.x = element_blank(), panel.background = element_blank(),
              axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5),
              panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
        ggtitle("Changes in Number of Colonies") +
        scale_y_continuous(labels = comma, expand = c(0.28,0))
      p3 <- ggplot(melt(df2, measure.vars = c("Varroa Mites", "Other Pests/\nParasites", "Diseases", "Pesticides", "Other", "Unknown"))) +
        geom_bar(aes_string("variable", "value"), stat="identity", fill="#969696", color="#969696") +
        geom_text(aes_string("variable", "value", label="value"), vjust=-.25) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.background = element_blank(), panel.grid = element_blank(),
              axis.line = element_line(colour = "black"),
              plot.title = element_text(hjust = 0.5), axis.title.x = element_blank()) +
        ggtitle("Percent of Colonies Affected by Indicated Stressors") + ylab("(%)") +
        scale_y_continuous(labels = comma, expand = c(0.28,0))
      
      grid.arrange(do.call(rbind, list(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3))), ncol=1)
    }
  })
  
  output$map_bar_plot <- renderPlot({
    map_click_vals <- event_data("plotly_click")
    map_click_vals$state <- colonies_stressors_subset()[colonies_stressors_subset()$id == map_click_vals[["pointNumber"]],"state"]
    
    df1 <- colonies_stressors[colonies_stressors$time_period == input$map_time_period,]
    df1 <- df1[order(-df1[[input$map_choropleth]]),]
    df2 <- colonies_stressors[(colonies_stressors$state == map_click_vals$state) & (colonies_stressors$time_period == input$map_time_period), ] %>% drop_na
    
    ggplot(df1) +
      geom_bar(aes_string("state", input$map_choropleth), stat="identity", fill="#d9d9d9", color="#d9d9d9") +
      scale_x_discrete(limits = df1$state) +
      geom_bar(data=df2, aes_string("state", input$map_choropleth), stat="identity", fill="#990000", color="#990000") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank(), panel.background = element_blank(),
            panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
      ylab(map_choropleth()) +
      scale_y_continuous(labels = comma)
  })
}
  
shinyApp(ui = ui, server = server)
