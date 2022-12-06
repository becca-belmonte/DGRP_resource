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

dt <- readRDS("dt.RDS") 
all_data <- readRDS("all_data.rds")
corr_p <- readRDS("corr_p.rds")
gwas_hits <- readRDS("new_gwas_hits.rds")
trait_guilds <- as.list(sort(unique(all_data$`Trait guild`)))

refs <- as.list(unique(all_data$Reference))

sex <- as.list(unique(all_data$Sex))

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
                                 message = "here is our paper",
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
                from. Or if you have one trait in mind, select that from the middle selection tool.",
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
          tabPanel("GWAS Data", DTOutput("gwasdata"))))),
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
          h4("In this tab you can go into depth to learn more about one specific trait."), 
          h4("- Use the selection tools at the top to limit your results based on the general category they belong to or the originial paper they came
                from. Or if you have one trait in mind, select that from the middle selection tool."), 
          h4("- You will then see the general information associated with the traits you selected, such as its associated group
                and its original source."),
          h4("- If you then click on one of those traits, you can scroll down to see the DGRP lines ranked according to that trait."),
          h4("- Hover over this graph to find specific lines in the ranking."),
          h4("- Scrolling down further, you can see information about the original publication, the experimental conditions, and calculated heritibality."),
          h4("- At the bottom you will see other traits that most strongly positively
                or negatively correlate with your selected trait."),
          h4("- If you are interested in exporting any of these data, click copy, csv, or excel, above the table you would like to export.")),
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
        if(is.null(input$trait)){spec_trait <- as.list(unique(all_data$Trait))} else
        {spec_trait <- all_data %>% 
          filter(`Trait guild` %in% input$trait)
        spec_trait <- as.list(unique(spec_trait$Trait))}
        selectizeInput("trait_spec", 
                       "Select specific trait", 
                       choices = spec_trait,
                       multiple = TRUE)
      })   ),
      menuItem(renderUI({
        if(is.null(input$trait)){study <- as.list(unique(all_data$Reference))} else
        {study <- all_data %>% 
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
      print("To learn more about a specific trait, please click on it in the below table")
  }
  
  )
  
  
  output$table <- renderDT({
    dt <- dt %>% 
      select(-Title, -`Full Text URL`)
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
    return(data_table)
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
    if(length(selected_trait)>0) print(c("You have selected ", selected_trait)) else
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
    selected_trait <- selected_row$Trait
    if(length(selected_trait)>0) {filter_data_trait <- all_data %>% 
      filter(Trait %in% selected_trait)
    roles <- function(x) sub("[^_]*_[^_]*_","",x )  
    ggplotly(ggplot(cbind(filter_data_trait, V4=paste(filter_data_trait$Reference,filter_data_trait$Sex,filter_data_trait$line,sep="_")), aes(x=reorder(V4,trait_value), y=trait_value, text = paste("DGRP line:", line)) ) + 
               geom_bar(stat = "identity", aes(fill = line)) +
               facet_wrap(~paste(filter_data_trait$Sex, filter_data_trait$Reference, sep=": "), scales = "free_x") + 
               xlab("DGRP line")  +
               ylab("Trait value") +
               scale_x_discrete(labels=roles)  +
               scale_fill_viridis_c() +
               theme_bw() +
               theme(axis.text.x=element_blank()) +
               theme(legend.position='none'), tooltip = c("trait_value", "text"))} else
                 ggplotly(ggplot(all_data, aes(x = line, y = trait_value)), tooltip = c("trait_value", "text"))
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
    row <- input$table_cell_clicked$row
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait
    selected_sex <- selected_row$Sex
    selected_study <- selected_row$Reference
    
    filter_data <- all_data %>% 
      filter(Trait %in% selected_trait & Sex %in% selected_sex & Reference %in% selected_study)
    
    old_trait_name <- unique(filter_data$Trait_old)
    
    width  <- (session$clientData$output_manhattan_width)
    height <- (session$clientData$output_manhattan_height)
    
    filename <- normalizePath(file.path('./', paste(old_trait_name, '.jpeg', sep = "")))
    
    list(src = filename,
         width = (height),
         height = (height),
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
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait 
    selected_sex <- selected_row$Sex
    pub_info <- all_data %>% 
      filter(Trait %in% selected_trait)  %>% 
      ungroup() %>% 
      select(Reference, Authors, Title, Year, `Full Text URL`)%>% 
      distinct(Reference, .keep_all = TRUE)
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
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait 
    meta_info <- all_data %>% 
      filter(Trait %in% selected_trait)  %>% 
      ungroup() %>% 
      select(Reference, Description, `Trait guild`, No_lines_used, Age, Housing, Diet, Temperature, `Wolbachia adjusted`, 
             `Baseline reference`)%>% 
      distinct(Description, .keep_all = TRUE)
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
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait 
    her_info <- all_data %>% 
      filter(Trait %in% selected_trait)  %>% 
      ungroup() %>% 
      select(Reference, Sex, `V(G)`, `V(E)`, `V(P)`, `V(G)/V(P)`, `SE V(G)`, `SE V(E)`, `SE V(P)`, `SE V(G)/V(P)`)%>% 
      distinct(`V(G)`, .keep_all = TRUE)
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
    selected_row <- data_table[row,]
    selected_trait <- selected_row$Trait 
    filter_corr <- all_data %>% 
      filter(Trait %in% selected_trait)
    
    spec_corr <- corr_p %>% 
      filter(trait %in% filter_corr$Trait_old | sec_trait %in% filter_corr$Trait_old) %>% 
      filter(p_val < 0.05) %>% 
      arrange(desc(abs(Correlation))) %>% 
      select(trait, sec_trait, p_val, Correlation)
    
    old_to_new <- all_data %>% 
      select(Trait, Trait_old, Reference, Sex) %>% 
      distinct(Trait_old, Trait, Reference, Sex)
    
    
    spec_corr$first_reference <- old_to_new$Reference[match(spec_corr$trait, old_to_new$Trait_old)]
    spec_corr$sec_reference <- old_to_new$Reference[match(spec_corr$sec_trait, old_to_new$Trait_old)]
    spec_corr$first_sex <- old_to_new$Sex[match(spec_corr$trait, old_to_new$Trait_old)]
    spec_corr$sec_sex <- old_to_new$Sex[match(spec_corr$sec_trait, old_to_new$Trait_old)]
    spec_corr$trait <- old_to_new$Trait[match(spec_corr$trait, old_to_new$Trait_old)]
    spec_corr$sec_trait <- old_to_new$Trait[match(spec_corr$sec_trait, old_to_new$Trait_old)]
    
    spec_corr <- spec_corr %>% 
      select(trait, first_reference, first_sex, sec_trait, sec_reference, sec_sex, p_val, Correlation)
    
    colnames(spec_corr) <- c("Trait #1", "Reference #1", "Sex #1", "Trait #2", "Reference #2", "Sex #2", "p-value", "Correlation")
    return(spec_corr)
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  output$gwasdata <- renderDT({
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
    
    filter_data <- all_data %>% 
      filter(Trait %in% selected_trait)
    
    gwas <- gwas_hits %>% 
      filter(trait %in% filter_data$Trait_old)
    
    old_to_new <- all_data %>% 
      select(Trait, Trait_old, Reference, Sex) %>% 
      distinct(Trait_old, Trait, Reference, Sex)
    
    
    gwas$Reference <- old_to_new$Reference[match(gwas$trait, old_to_new$Trait_old)]
    gwas$Sex <- old_to_new$Sex[match(gwas$trait, old_to_new$Trait_old)]
    gwas$Trait <- old_to_new$Trait[match(gwas$trait, old_to_new$Trait_old)]
    
    gwas <- gwas %>% 
      select(Trait, Reference, Sex, SNP, FBID, gene_name, site_class, distance_to_gene, MAF, minor_allele, major_allele, BETA, SE, P, log10_P)
    return(gwas)
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  
  
  output$corrtable <- renderDT({
    if(is.null(input$trait) & is.null(input$trait_spec)){filter_corr <- all_data%>% 
      select(Trait, Trait_old) %>% 
      distinct() }else
        if(is.null(input$trait_spec)){
          filter_corr <- all_data %>%
            filter(`Trait guild` %in% input$trait)%>% 
            select(Trait, Trait_old) %>% 
            distinct() }else
              if(is.null(input$trait))
              {filter_corr <- all_data %>%  
                filter(Trait %in% input$trait_spec)%>% 
                select(Trait) %>% 
                distinct() }else
                {filter_corr <- all_data %>% 
                  filter(`Trait guild` %in% input$trait) %>% 
                  filter(Trait %in% input$trait_spec)%>% 
                  select(Trait, Trait_old) %>% 
                  distinct() }
    
    
    corr_gg <- corr_p %>% 
      filter(trait %in% filter_corr$Trait_old & sec_trait %in% filter_corr$Trait_old) %>% 
      select(trait, sec_trait, Correlation, p_val) %>% 
      arrange(desc(abs(Correlation)))
    
  }, class = 'cell-border stripe', rownames = FALSE, 
  
  extensions = 'Buttons',
  
  options = list(
    scrollX = TRUE,
    dom = 'lBfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))
  
  
  output$corr <- renderPlotly({
    if(is.null(input$trait) & is.null(input$trait_spec)){filter_corr <- all_data%>% 
      select(Trait, Trait_old) %>% 
      distinct() }else
        if(is.null(input$trait_spec)){
          filter_corr <- all_data %>%
            filter(`Trait guild` %in% input$trait)%>% 
            select(Trait, Trait_old) %>% 
            distinct() }else
              if(is.null(input$trait))
              {filter_corr <- all_data %>%  
                filter(Trait %in% input$trait_spec)%>% 
                select(Trait, Trait_old) %>% 
                distinct() }else
                {filter_corr <- all_data %>% 
                  filter(`Trait guild` %in% input$trait) %>% 
                  filter(Trait %in% input$trait_spec)%>% 
                  select(Trait) %>% 
                  distinct() }
    
    
    corr_gg <- corr_p %>% 
      filter(trait %in% filter_corr$Trait_old & sec_trait %in% filter_corr$Trait_old)
    
    gg_corr <- ggplot(corr_gg, aes(x = trait, y = sec_trait, fill = Correlation)) +
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
    return(ggplotly(gg_corr))} 
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
    return(data_table)
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