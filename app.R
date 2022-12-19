## app.R ##
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(psych)
library(tidyverse)
library(spsComps)
library(kableExtra)
library(fontawesome)

dt <- readRDS("app_data/dt.RDS") 
line_means <- readRDS("app_data/line_means.rds") 
corr_p <- readRDS("app_data/corr_p.rds")
gwas_hits <- readRDS("app_data/gwas_hits.rds") 
gwas_summary <- readRDS("app_data/gwas_summary.rds")
herit_info <- readRDS("app_data/heritability.rds")
meta_info <- readRDS("app_data/meta_info.rds")
pub_info <- readRDS("app_data/pub_info.rds") 
trait_guilds <- as.list(sort(unique(dt$`Trait guild`)))
refs <- as.list(unique(dt$Reference))
sex <- as.list(unique(dt$Sex))
no_trait_selected_table <- tibble(` `= "Select a trait to view data about it.")
no_data_table <- tibble(` `= "No data available for this trait.")
all_data <- readRDS("app_data/all_data.rds")

infoBtn <- function(id) {
  actionButton(id,
               label = "",
               icon = icon("question"),
               style = "info",
               size = "extra-small",
               class='btn action-button btn-info btn-xs shiny-bound-input'
  )
}

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "DGRP resource",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Project in Github",
                                 message = "Documentation, Source, Citation",
                                 href = "https://github.com/becca-belmonte/DGRP_resource",
                                 icon = icon("github")
                               ),
                               messageItem(
                                 from = "Link to Paper",
                                 message = "Here is the associated paper",
                                 href = "https://onlinelibrary.wiley.com/",
                                 icon = icon("file")
                               ),
                               icon = icon("link"),
                               headerText = "External links")),
  dashboardSidebar(
    sidebarMenu(
      sidebarMenuOutput("menu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "traits",
    fluidRow(
        box(infoBtn('workingPop') %>%
              bsPopover(title = "Filtering traits",
                        content = "Use the selection tools on the left sidebar to limit your results based on the general category they belong to or the originial paper they came
                from. Or, if you have a particular trait in mind, select that from the middle selection tool.",
                        placement = "bottom",
                        trigger = "hover"
              ),h3(textOutput("instruct"), align = "center"), DTOutput("table"), width = 6),
    box(h3(textOutput("selection"), align = "center"), width = 6), 
    tabBox(width = 6,
           tabPanel("Trait value graph", plotlyOutput("barchart")),
           tabPanel("Manhattan plot", imageOutput("manhattan"))),
        tabBox(width = 6,
          tabPanel("Heritability Data",  DTOutput("herinfo")), 
          tabPanel("Strongest Correlations", DTOutput("corrinfo")),
          tabPanel("Publication Data", DTOutput("pubinfo")),
          tabPanel("Experimental Conditions", DTOutput("metainfo")),
          tabPanel("GWAS Hits", DTOutput("gwashits")),
          tabPanel("GWAS Summary", DTOutput("gwassummary"))
          ))),
    tabItem(
      tabName = "correlations",
      fluidRow(box(plotlyOutput("corr"), width = 6), box(DTOutput("corrtable"), width = 6))
      ),
    tabItem(
      tabName = "raw",
      fluidRow(box(DTOutput("rawdata"), width = 12))
    ),
    tabItem(
      tabName = "howto",
      tabBox(
        tabPanel("Info about traits",
          h4("This tab displays a wealth of information about a trait that you have selected."), 
          h4("The interactive table in the centre shows a list of over 2000 traits in the database."), 
          h4("Use the 'Select' bars to the left, or the search function above the central table, to explore the list of measured traits in this database, then click a trait that interests you."), 
          h4("The bar chart shows the estimated mean trait value for each DGRP line. The plot will show males and females separately, if line means were estimated for both sexes in the same study. Hover over this graph to see information about each bar."),
          h4("In the tabs in the lower right, you can view various tables. The heritability table shows COMPLETE THIS INFO. The 'Strongest correlations' table shows COMPLETE THIS INFO. The 'Publication data' gives a link to the source for this set of measurements, and a complete citation. The 'Experimental conditions' table shows COMPLETE THIS INFO. Finally, the 'GWAS hits' table shows a list of all the variants with a p-value below 1e-5 for the selected trait (see our publication for information on the GWAS methods), alongside information on each of these variants such as the site class of the variant, any genes it is inside/near, information on its major and minor alleles, and the GWAS summary statistics from PLINK. Note that some variants are associated with multiple genes and site classes, and so there may be more rows in table than there are significant GWAS hits."),

          h4("You can use the buttons marked Copy/CSV/Excel to save or export any of these data")),
      tabPanel("Correlations",
          h4("On this tab you can learn more about how different traits relate to each other."),
          h4("- To begin with you will see a correlation matrix of all traits."),
          h4("- Hover over this graph or zoom in to see specific correlation values."),
          h4("- Select trait categories or specific traits to view correlations of the applicable traits (Note: more than one trait must be selected)."),
          h4("- Below the graph, you will see a table of correlations between each two traits selected, along with the raw p-value.")),
      tabPanel("Raw Data",
          h4("If you are interested in the raw data that we used to build this resource, you can download it here."), 
          h4("- You can filter the data in the same manner as previous tabs."),
          h4("- Once you have selected the data you are interested in, click copy, csv, or excel to export in your desired format.")), width = 12)
    ),
    tabItem(
      tabName = "info",
      box(h2("Correlation calculations"),
          h4("To calculate the correlation between each trait, we used a spearman correlation test with the psych package for R. The code used was"),
          code("corr.test(corr_dt, adjust = 'holm', ci = FALSE, method = 'spearman')"), width = 12)
    )
    )
  )
)
  



server <- function(input, output, session) {
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Info about Traits", tabName = "traits"),
      menuItem("Correlations", tabName = "correlations"),
      menuItem("Raw Data", tabName = "raw"),
      menuItem("About", tabName = "about",
               menuSubItem("How to use", tabName = "howto"),
               menuSubItem("Info about calculations", tabName = "info")),
      menuItem(selectizeInput("trait",
                              "Select trait category", 
                              choices = trait_guilds,
                              multiple = TRUE)),
      menuItem(renderUI({
        if(is.null(input$trait)){spec_trait <- as.list(unique(dt$Trait))} else
        {spec_trait <- dt %>% 
          filter(`Trait guild` %in% input$trait)
        spec_trait <- as.list(unique(spec_trait$Trait))}
        selectizeInput("trait_spec", 
                       "Select specific trait", 
                       choices = spec_trait,
                       multiple = TRUE)
      })   ),
      menuItem(renderUI({
        if(is.null(input$trait)){study <- as.list(unique(dt$Reference))} else
        {study <- dt %>% 
          filter(`Trait guild` %in% input$trait)
        study <- as.list(unique(study$Reference))}
        selectizeInput("study", 
                       "Select study", 
                       choices = study,
                       multiple = TRUE)
      })   )
    )
  })
 
  output$instruct <- renderText({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    row <- input$table_cell_clicked$row
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait        
    if(length(selected_trait)>0) print("") else
      print("Click on a trait in the table below to view more information about it")
  }
  
  )
  
  
  output$table <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    select(data_table, `Trait guild`, Trait, Sex, Description, Reference)
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  selection = 'single',
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  
  output$selection <- renderText({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    row <- input$table_cell_clicked$row
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait
    selected_sex <- selected_row$Sex
    if(length(selected_trait) > 0) print(paste("You have selected: ", selected_trait, " (", selected_sex, ")",  sep="")) else
      print("")
  })
  
  
  
  output$barchart <- renderPlotly({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    row <- input$table_cell_clicked$row
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait_ID
    selected_sex <- selected_row$Sex
    selected_study <- selected_row$Reference
    
    if(length(selected_trait) > 0) {
      
      filter_data_trait <- line_means %>% 
        filter(Trait_ID == selected_trait) %>% 
        left_join(dt %>% select(Trait_ID, Trait, Sex, Reference) %>%
                    filter(Trait_ID == selected_trait), by = "Trait_ID") %>% 
        arrange(Sex) %>% 
        mutate(x_variable = paste(Sex, line, sep = "_"),
               x_variable = factor(x_variable, unique(x_variable)),
               facet_label = paste(Trait, " (", Sex, ")",  sep=""),
               line = factor(line, unique(line)))
      
      min_value <- min(filter_data_trait$trait_value)
      max_value <- max(filter_data_trait$trait_value)
      range <- max_value - min_value
      y_min <- min_value - 0.07 * range
      y_max <- max_value + 0.07 * range
      if(y_min < 0 & all(filter_data_trait$trait_value > 0)) y_min <- 0
      filter_data_trait <- rename(filter_data_trait, `Trait value` = trait_value)

      ggplotly(
        ggplot(filter_data_trait,
               aes(x = reorder(line, `Trait value`), 
                   y = `Trait value`, 
                   text = paste("Line ", line, "\n", Reference, sep = ""))) + 
          geom_bar(stat = "identity", aes(fill = line)) +
          facet_wrap(~ facet_label, 
                     scales = "free_x") + 
          scale_y_continuous(expand = c(0, 0)) +
          coord_cartesian(ylim = c(y_min, y_max)) +
          xlab("DGRP line")  +
          ylab("Trait value") +
          scale_fill_viridis_d() + 
          theme_bw() +
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()) +
          theme(legend.position = "none"), 
        
        tooltip = c("Trait value", "text"))} else
          ggplotly( 
            ggplot(line_means[1,], aes(x = line, y = trait_value)) +
              theme_bw() +
              theme(axis.text = element_blank(), axis.ticks = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
              xlab("DGRP line")  +
              ylab("Trait value")
          )
  })
  
  output$manhattan <- renderImage({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    selected_trait <- data_table[input$table_cell_clicked$row,]$Trait_ID 
    
    if(length(selected_trait) != 0) {
      filename <- normalizePath(file.path('./app_data/Manhattan plots/', paste(selected_trait, '.jpeg', sep = "")))  
    } else filename <- normalizePath(file.path('./app_data/blank_manhattan.jpeg'))
    
    if(!file.exists(filename)) filename <-  normalizePath(file.path('./app_data/blank_manhattan.jpeg'))
    
    width  <- (session$clientData$output_manhattan_width)
    height <- (session$clientData$output_manhattan_height)
    
    list(src = filename,
         width = width,
         height = height,
         alt = paste("Trait:", selected_trait))

  }, deleteFile = FALSE)
  
  
  output$pubinfo <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)
    
    pub_info <- pub_info %>% 
      filter(Reference %in% data_table[row, ]$Reference) %>% 
      select(-Reference)
    
    if(nrow(pub_info) == 0) pub_info <- no_data_table
    pub_info 

  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  
  output$metainfo <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)
    
    meta_info <- meta_info %>% 
      filter(Trait_ID %in% data_table[row, ]$Trait_ID) %>% 
      select(-Trait_ID, -Reference) %>% 
      select(
        where(
          ~!all(is.na(.x))
        )
      )
    
    if(nrow(meta_info) == 0) meta_info <- no_data_table
    meta_info 

  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  
  output$herinfo <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)
    
    herit_info <- herit_info %>% 
      filter(Trait_ID %in% data_table[row,]$Trait_ID)  %>% 
      select(-Trait_ID)

    if(nrow(herit_info) == 0) herit_info <- no_data_table
    herit_info 
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  output$corrinfo <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)
    selected_trait <- data_table[row, ]$Trait_ID
    
    spec_corr <- corr_p %>% 
      filter(trait_ID_1 == selected_trait | trait_ID_2 == selected_trait) %>% 
      filter(p_val < 0.05) %>% 
      arrange(desc(abs(Correlation))) %>% 
      rename(focal_trait = trait_ID_1, nonfocal_trait = trait_ID_2) 
    
    to_swap <- which(spec_corr$focal_trait != selected_trait)
    
    spec_corr$nonfocal_trait[to_swap] <- spec_corr$focal_trait[to_swap]
    spec_corr$focal_trait[to_swap] <- selected_trait
    
    spec_corr <- 
      left_join(spec_corr, select(dt, Trait, Trait_ID, Reference, Sex), 
                by = c("nonfocal_trait" = "Trait_ID")) %>% 
      mutate(Trait = paste(Trait, " (", Sex, ")",  sep = "")) %>% 
      select(`Correlated trait` = Trait, 
             `Reference for that trait` = Reference, 
             Correlation, `p-value` = p_val)
    
    
    if(nrow(spec_corr) == 0) spec_corr <- no_data_table
    spec_corr    
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  output$gwashits <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)

    gwas_hits <- gwas_hits %>% 
      filter(Trait_ID == data_table[row,]$Trait_ID) %>% 
      select(-Trait_ID)
    
    if(nrow(gwas_hits) == 0) gwas_hits <- no_data_table
    return(gwas_hits)
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  
  
  output$gwassummary <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- dt  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- dt %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- dt %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- dt %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- dt %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- dt %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- dt %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- dt %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    row <- input$table_cell_clicked$row
    if(length(row) == 0) return(no_trait_selected_table)
    
    gwas_summary <- gwas_summary %>% 
      filter(Trait_ID %in% data_table[row, ]$Trait_ID) %>% 
      select(-Trait_ID)
    
    if(nrow(gwas_summary) == 0) gwas_summary <- no_data_table
    gwas_summary 
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  
  
  
  output$corrtable <- renderDT({
    if(is.null(input$trait) & is.null(input$trait_spec)){filter_corr <- dt %>% 
      select(Trait, Trait_ID) %>% 
      distinct() }else
        if(is.null(input$trait_spec)){
          filter_corr <- dt %>%
            filter(`Trait guild` %in% input$trait)%>% 
            select(Trait, Trait_ID) %>% 
            distinct() }else
              if(is.null(input$trait))
              {filter_corr <- dt %>%  
                filter(Trait %in% input$trait_spec)%>% 
                select(Trait) %>% 
                distinct() }else
                {filter_corr <- dt %>% 
                  filter(`Trait guild` %in% input$trait) %>% 
                  filter(Trait %in% input$trait_spec)%>% 
                  select(Trait, Trait_ID) %>% 
                  distinct() }
    
    
    corr_gg <- corr_p %>% 
      filter(trait_ID_1 %in% filter_corr$Trait_ID & trait_ID_2 %in% filter_corr$Trait_ID) %>% 
      select(trait, sec_trait, Correlation, p_val) %>% 
      rename(`Trait #1` = trait,`Trait #2` = sec_trait) %>% 
      arrange(desc(abs(Correlation)))
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  
  output$corr <- renderPlotly({
    if(is.null(input$trait) & is.null(input$trait_spec)){filter_corr <- dt %>% 
      select(Trait, Trait_ID) %>% 
      distinct() }else
        if(is.null(input$trait_spec)){
          filter_corr <- dt %>%
            filter(`Trait guild` %in% input$trait)%>% 
            select(Trait, Trait_ID) %>% 
            distinct() }else
              if(is.null(input$trait))
              {filter_corr <- dt %>%  
                filter(Trait %in% input$trait_spec)%>% 
                select(Trait, Trait_ID) %>% 
                distinct() }else
                {filter_corr <- dt %>% 
                  filter(`Trait guild` %in% input$trait) %>% 
                  filter(Trait %in% input$trait_spec)%>% 
                  select(Trait) %>% 
                  distinct() }
    
    
    corr_gg <- corr_p %>% 
      filter(trait_ID_1 %in% filter_corr$Trait_ID & trait_ID_2 %in% filter_corr$Trait_ID)
    
    gg_corr <- ggplot(corr_gg, aes(x = trait_ID_1, y = trait_ID_2, fill = Correlation, text = paste("Trait #1: ", trait, "\n", "Trait #2: ", sec_trait, sep = ""))) +
        geom_tile()  +
        scale_fill_viridis_c(limits = c(-1, 1)) +
        scale_color_manual(values = "#FCFDBF") +
        theme_classic() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              legend.position = c(0.75,0.3)) +
        xlab("") +
        ylab("") +
        labs(fill = "Correlation") +
        scale_x_discrete(position = "top") +
        guides(color = "none")
    
    ggplotly(gg_corr, tooltip = c("text","Correlation"))
  } 
  )
  
  
  output$rawdata <- renderDT({
    {if(is.null(input$trait) & is.null(input$study) & is.null(input$trait_spec)) data_table <- all_data  else
      if(is.null(input$trait) & is.null(input$study)) data_table <- all_data %>% 
          filter(Trait %in% input$trait_spec) else
            if(is.null(input$study) & is.null(input$trait_spec)) data_table <- all_data %>% 
                filter(`Trait guild` %in% input$trait) else
                  if(is.null(input$trait) & is.null(input$trait_spec)) data_table <- all_data %>% 
                      filter(Reference %in% input$study) else
                        if(is.null(input$study) & is.null(input$trait_spec)) data_table <- all_data %>% 
                            filter(`Trait guild` %in% input$trait) else
                              if(is.null(input$trait_spec)) data_table <- all_data %>% 
                                  filter(Reference %in% input$study) %>% 
                                  filter(`Trait guild` %in% input$trait) else
                                    if(is.null(input$trait)) data_table <- all_data %>% 
                                        filter(Reference %in% input$study) %>% 
                                        filter(Trait %in% input$trait_spec) else
                                          if(is.null(input$study)) data_table <- all_data %>% 
                                              filter(`Trait guild` %in% input$trait) %>% 
                                              filter(Trait %in% input$trait_spec) else
                                                data_table <- all_data %>% 
                                                  filter(Reference %in% input$study) %>% 
                                                  filter(`Trait guild` %in% input$trait) %>% 
                                                  filter(Trait %in% trait_spec)}
    
    data_table %>% 
      rename(`Trait value` = trait_value,
             `DGRP line` = line,
             `Trait ID` = Trait_ID) 
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
  )
  

}

shinyApp(ui, server)