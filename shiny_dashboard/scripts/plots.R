library(ggplot2)
library(ggformula)
library(ggeasy)
library(dplyr)
library(plotly)

# UNUSED
get_death_from_cancer_with_avg_age <- function(clinical_data) {
  dfc_yes <- clinical_data %>%
    filter(death_from_cancer == "yes")
  dfc_no <- clinical_data %>%
    filter(death_from_cancer == "no")
  return(
    plot_ly(
      data = clinical_data,
      x =  ~ death_from_cancer ,
      y =  ~ mean(age_at_diagnosis),
      type = "scatter",
      mode = "lines",
      name = "Avg. Age at Diagnosis"
    ) %>%
      add_boxplot(
        data = dfc_yes,
        x = ~ death_from_cancer,
        y =  ~ overall_survival_months,
        inherit  = F,
        name = "Death From Cancer"
      ) %>%
      add_boxplot(
        data = dfc_no,
        x = ~ death_from_cancer,
        y =  ~ overall_survival_months,
        inherit  = F,
        name = "Not Death From Cancer"
      ) %>%
      layout(
        title = "Death from Cancer vs. no Death from cancer",
        yaxis = list(title = "Overall survival Months"),
        xaxis = list(title = "Death from cancer"),
        legend = list(orientation = "h",
                      y = -0.2)
      )
  )
}

get_generic_boxplot <-
  function(clinical_data,
           x,
           y,
           title,
           xaxis,
           yaxis,
           colors) {
    fig <-
      plot_ly(
        data = clinical_data,
        x = ~ x,
        y = ~ y,
        type = 'box',
        color =  ~ x,
        colors = "Set2"
      ) %>% layout(
        title = title,
        xaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff',
          showticklabels = FALSE,
          title = list(text = xaxis)
        ),
        yaxis = list(title = yaxis),
        legend = list(orientation = "h",
                      y = -0.3)
      )
    
    return(fig)
  }

get_generic_piechart <-
  function(clinical_data, labels, title, labelType = "label+percent") {
    colors <-
      c(
        'rgb(102, 194, 165)',
        'rgb(252, 141, 98)',
        'rgb(141, 160, 203)',
        'rgb(231, 138, 195)',
        'rgb(166, 216, 84)',
        'rgb(255, 217, 47)',
        'rgb(229, 196, 148)',
        'rgb(179, 179, 179)'
      )
    fig <-
      plot_ly(
        data = clinical_data,
        labels = ~ labels,
        type = 'pie',
        textposition = 'inside',
        textinfo = labelType,
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(
        title = title,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    return(fig)
  }

get_generic_linechart <- function(clinical_data, x, y, title, xlabel, ylabel) {
  fig <-
    plot_ly(data = clinical_data, type = 'scatter', mode = 'markers',
            x = ~x,
            y = ~y) %>% layout(
              title = title,
              xaxis = list(title = xlabel),
              yaxis = list(title = ylabel)
            )
  
  return(fig)
}

get_generic_qqplot <- function(clinical_data, x, title, histogram = TRUE) {
  
  if(histogram) {
    p <-
      plot_ly(
        x = ~ x,
        type = "histogram",
        marker = list(
          color = "#fec6b1",
          line = list(color = "darkgray",
                      width = 2)
        )
      ) %>%
      layout(title = title)
    return(p)
  }
  else {
    p <- ggplot(clinical_data, aes(sample=x)) +
      geom_qq(color="#fec6b1") + geom_qq_line() +
      labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles")
    return(ggplotly(p))
    }
}