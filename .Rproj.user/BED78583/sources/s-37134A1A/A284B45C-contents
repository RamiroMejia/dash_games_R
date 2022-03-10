library(dash)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyr)
library(lubridate)



data <- utils::read.csv('data/vgsales.csv')

# pivoting data

pivoted_data <- data %>%  tidyr::pivot_longer('NA_Sales':'Global_Sales',
                                             names_to='region' , values_to='sales')


pivoted_data <- 
  pivoted_data %>%  dplyr::mutate(region = dplyr::case_when(region=='NA_Sales' ~ 'North America',
                                                           region=='EU_Sales' ~ 'Europe',
                                                           region=='JP_Sales' ~ 'Japan',
                                                           region=='Other_Sales' ~ 'Rest of the World',
                                                           region=='Global_Sales' ~ 'Global'),
                                 Year = lubridate::year(lubridate::as_date(Year, format="%Y" ))
  )


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      dccDropdown(
        id='col-select',
        options = pivoted_data %>% 
          dplyr::select(Genre) %>% 
          unique() %>%
          dplyr::pull() %>% 
          purrr::map(function(col) list(label = col, value = col)), 
        value='Sports')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    p <- ggplot2::ggplot(pivoted_data %>% dplyr::filter(Genre==xcol)) +
      ggplot2::aes(x = Year,
                   y = sales,
                   color = region) +
      ggplot2::geom_line(stat = 'summary', fun = mean, size=1) + 
      ggplot2::ggtitle('Mean Sales') +
      ggplot2::ggtitle('Mean Sales in Millions USD') +
      ggthemes::scale_color_tableau()
    plotly::ggplotly(p)
  }
)

app$run_server(host = '0.0.0.0')
