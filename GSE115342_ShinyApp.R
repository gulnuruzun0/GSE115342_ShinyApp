# Homework 5: Shiny
# Gülnur UZUN
# Student No: 235009063007

# Load necessary libraries
library(dplyr)
library(shiny)
library(ggplot2)
library(plotly)

# Load the processed data
load("diet.RData")

# Define user interface phase for shiny application
ui = fluidPage(
  
  # Add custom styles for head and backgrounds with html
  tags$head( #set bold type for heads
    tags$style(HTML("
      body {
        font-weight: bold;
      }
      .blue-panel {
        background-color: #d1ecf1; #set background color as blue
        padding: 15px; #Add 15px padding inside the panel
        border-radius: 5px; #Round the corners of the panel
      }
      .green-panel {
        background-color: #d4edda;#set background color as green
        padding: 15px;
        border-radius: 5px;
      }
      .yellow-panel{
        background-color: #ffff99;#set background color as yellow
        padding: 15px;
        border-radius: 5px;
      }
    "))
  ),
  
  titlePanel("Transcriptome Analysis of Regular-fed and LCKD-fed Mice on Shiny Application"), #set of the title in the page
  
  # create a fluid layout for the application
  fluidRow(
    column(3,
           h4("An interactive analysis according to the Diet Type and Related Tissue in mice (regular-LCKD diet and brain-liver tissues)."), # for insert the header 4th level
           p("This Shiny application provides an interactive analysis of transcriptome data from mice on regular or LCKD diets. # insert the description relevant panel
             Users can explore scatter plots, data tables, and box plots for different tissues and diet categories."),
           p("dplyr, ggplot2, and plotly libraries were used in the application for effective data manipulation, analysis, and visualization."),
           class = "yellow-panel" # set of background class
    ), 
    # 2nd column: main panel with tabs for different visualizations
    column(9,
           tabsetPanel(
             # First tab: Scatter plot
             tabPanel("Tab1: Scatter Plot",
                      sidebarLayout(
                        sidebarPanel(
                          p("In this tab, you can choose between brain and liver tissue. 
                          Then, you will select an 'n' value to display in the graph 
                          (n is set to 100 by default, with a minimum of 1 and a maximum of 
                          59,305, which is the number of genes in our data). 
                          Based on the selected information and values, a scatter plot will be displayed. 
                          The scatter plot will include the top 'n' genes with the highest expression values."),
                          class = "blue-panel", # set the background
                          radioButtons("tissue", "Choose Tissue Type:",
                                       choices = list("Brain" = "brain",
                                                      "Liver" = "liver")),
                          numericInput("numGenes", "Number of Top Genes:",
                                       value = 100, 
                                       min = 1, 
                                       max = nrow(diet)) # maximum number of genes from the dataset
                        ),
                        mainPanel(
                          class = "green-panel",  # set the background color
                          plotlyOutput("scatterplot") # output for the scatter plot
                        )
                      )
             ),
             
             # Second tab: Data table
             tabPanel("Tab2: Data Table",
                      sidebarLayout(
                        sidebarPanel(
                          p("In this tab, the expression data for the genes selected in the first tab 
                          (based on the 'n' input) will be displayed as a data table. 
                          The table will contain 'n' genes (in the rows) and their expression 
                          levels across 4 categories (in the columns), sorted by the maximum expression level. 
                            NA values will be displayed as blanks in the table."),
                          class = "blue-panel", # set the background color
                          numericInput("numGenes", "Number of Top Genes:",
                                       value = 100, 
                                       min = 1, 
                                       max = nrow(diet))
                        ),
                        mainPanel(
                          dataTableOutput("dataTable_tab2") # output for the datatable
                        )
                      ),
                      width = 10 # width of the tab panel
             ),
             
             # Third tab: Top genes and box plot
             tabPanel("Tab3: Top Genes & Box Plot",
                      sidebarLayout(
                        sidebarPanel(
                          p("In this tab, a drop-down menu will allow users to select one of the four categories from the dataset. 
                            For each selected category, the output will provide the top 5 genes with the highest expression levels
                            in that category. Additionally, within the same tab, an interactive box plot will display the expression 
                            values of all genes in the selected category."),
                          class = "blue-panel",
                          selectInput("category", "Select Category:",
                                      choices = list("Brain Regular Diet" = "brain_chow",
                                                     "Liver Regular Diet" = "liver_chow",
                                                     "Brain LCKD" = "brain_lckd",
                                                     "Liver LCKD" = "liver_lckd"))
                        ),
                        mainPanel(
                          class = "green-panel", # set the background color
                          verbatimTextOutput("topGenes"), # output for displaying top genes
                          plotlyOutput("boxPlot") #output for the boxplot
                        )
                      )
             )
           )
    )
  )
)

# Define server phase for shiny application
server = function(input, output) {
  
  # Reactive expression to filter and prepare data for plotting
  filteredData = reactive({
    diet %>%
      # select the relevant columns for analysis
      select(gene_symbols, brain_chow, brain_lckd, liver_chow, liver_lckd) %>%
      # calculate the mean expression across the selected tissues and diets, with ignoring NA values
      mutate(mean_expression = rowMeans(select(., brain_chow, brain_lckd, liver_chow, liver_lckd), na.rm = TRUE)) %>%
      # arrange the data by descending order of mean expression
      arrange(desc(mean_expression)) %>%
      # slice the top "n" genes as specified by the users input
      slice(1:input$numGenes)
  })
  
  # Render scatter plot
  output$scatterplot = renderPlotly({
    df = filteredData() # get the filtered data
    
    if (input$tissue == "brain") {
      x_col = "brain_chow"
      y_col = "brain_lckd"
    } else {
      x_col = "liver_chow"
      y_col = "liver_lckd"
    }
    
    # create the scatter plot using ggplot2 and plotly
    p = ggplot(df, aes_string(x = x_col, y = y_col, text = "gene_symbols")) +
      geom_point(color = "darkgreen") +
      labs(
        x = paste(input$tissue, "Chow Expression Level"), # x-label as the user input
        y = paste(input$tissue, "LCKD Expression Level"), # y-label as the user input
        title = paste("Scatter Plot of Top", input$numGenes, input$tissue, "Genes")
      ) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal() # apply minimal theme to the plot
    
    ggplotly(p, tooltip = "text") # convert ggplot object to a plotly object for interactivity
  })
  
  
  # Render data table
  output$dataTable_tab2 = renderDataTable({
    data = filteredData() # get filtered data
    data %>%
      # select relevant columns and rename them for better readability
      select(gene_symbols, brain_chow, liver_chow, brain_lckd, liver_lckd) %>%
      rename("Gene Symbol" = gene_symbols,
             "Brain Regular Diet Expression" = brain_chow,
             "Liver Regular Diet Expression" = liver_chow,
             "Brain LCKD Expression" = brain_lckd,
             "Liver LCKD Expression" = liver_lckd)
  })
  
  # Render top genes text output
  output$topGenes = renderPrint({
    # switch the selected category to the corresponding column name
    category_col = switch(input$category,
                          "brain_chow" = "brain_chow",
                          "brain_lckd" = "brain_lckd",
                          "liver_chow" = "liver_chow",
                          "liver_lckd" = "liver_lckd")
    
    # get the top 5 genes with the highest expression in the selected category
    top_genes = diet %>%
      arrange(desc(.data[[category_col]])) %>%
      slice(1:5) %>%
      pull(gene_symbols)
    
    # print the selected category and the top 5 genes
    cat("Selected category:", 
        input$category, 
        "\nTop 5 genes with highest expression:", 
        paste(top_genes, 
              collapse = ", "))
  })
  
  # Render box plot
  output$boxPlot = renderPlotly({
    # switch the selected category to the corresponding column name
    category_col = switch(input$category,
                          "brain_chow" = "brain_chow",
                          "brain_lckd" = "brain_lckd",
                          "liver_chow" = "liver_chow",
                          "liver_lckd" = "liver_lckd")
    
    df = diet
    
    # create the box plot using ggplot2 and plotly
    p = ggplot(df, aes_string(x = "factor(1)", y = category_col)) +
      geom_boxplot() + # add a boxplot layer
      # add a jitter layer for individual data points, for transparency and size; also gene symbols
      geom_jitter(aes(text = gene_symbols), alpha = 0.1, color = "navyblue") + 
      geom_text(aes(label = gene_symbols), vjust = -0.5, size = 2.5, color = "black") +
      labs(x = paste(input$category, "Gene Symbols"), 
           y = " Gene Expression Level", 
           title = paste("Box Plot of", input$category)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) # customize the plot theme to hide x axis text
    
    ggplotly(p, tooltip = "text") #convert ggplot object toa plotly object for interactivity
  })
}

# Complete Shiny app with both UI and server
shinyApp(ui = ui, server = server)
