
#+ datasetup1, echo=FALSE, include=FALSE

var1='state'
var2='urbanization'

df_population %>% 
  filter( supply_year==2013)  %>%
  filter( lga != '.' & lga != 99399 ) %>%
  group_by( lga ) %>%
  summarise( population = sum( population )) %>% 
  { . } -> df_lga_population



# singleFacetPlot_boxplot   --------------------------------------------------------------

singleFacetPlot_boxplot = function( df1, var1, var2 ) {

  df1 %>%
    select_and_standardise_ddd( standardise_over = c("lga", var1, var2 )) %>% 
    ggplot() +
    geom_boxplot( mapping=aes_string(x = var1, y= "ddd" , color=var1, fill=var1)) + 
    ggtitle( paste("The range of LGA total DDD for each", var1, "facetted by", var2)) +
    facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}


# singleFacetPlot_violin   --------------------------------------------------------------

singleFacetPlot_violin = function( df1, var1, var2 ) {

  df1 %>%
    select_and_standardise_ddd( standardise_over = c("lga", var1, var2 )) %>% 
    inner_join( df_lga_population %>% group_by( lga ) %>% summarise( population=sum(population)), by='lga') %>%
    ggplot(mapping=aes_string(x = var1, y= "ddd", color=var1, fill=var1, weight="population")) +
    geom_violin( )  +
    ggtitle( paste("The range of LGA total DDD for each", var1, "facetted by", var2)) +
    facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')

}


# generate_multiplots --------------------------------------------------------------

generate_multiplots <- function () {

  df %>%
    inner_join( df_patient, by='pin' ) %>%
    inner_join( df_population %>% distinct( lga, seifa, urbanization), by='lga' ) %>%
    filter( lga != '.' & lga != 99399 ) %>%
    { . } -> df1


  variables = qw("state urbanization sex age type_name seifa drug_type doc_benzo doc_opioid supply_month")
  # variables = qw("age sex")
  to_plot = combn( variables, 2 , simplify=FALSE)
  to_plot=c(to_plot, lapply(to_plot, rev))
  plot_list = list()
  i=1

  for (i in 1:length(to_plot)) {
    cat( to_plot[[ i ]][1], to_plot[[ i ]][2], i, "\n")
    plot_list[[i]] <- NA
    if (  intersect( to_plot[[ i ]], qc( age, sex ) ) %>% length() < 2 ) {
      plot_list[[i]] <- singleFacetPlot_violin( df1, to_plot[[ i ]][1], to_plot[[ i ]][2] )
    }

    file_name = paste("graphics/violin_", to_plot[[ i ]][1], "_", to_plot[[ i ]][2], i, ".tiff", sep="")
    tiff(file_name)
    print(plot_list[[i]])
    dev.off()
  }

  pdf("graphics/boxplot_combinations.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()

}
generate_multiplots()


# generate_multiplots --------------------------------------------------------------

compareQLD <- function () {

  df %>%
    inner_join( df_patient, by='pin' ) %>%
    inner_join( df_population %>% distinct( lga, seifa, urbanization), by='lga' ) %>%
    filter( lga != '.' & !endsWith( lga,'99') ) %>%
    mutate( region = ifelse( state=='QLD', 'QLD', 'OTHER')) %>%
    { . } -> df1


  variables = qw("state urbanization sex age type_name seifa drug_type doc_benzo doc_opioid supply_month")
  # variables = qw("age sex")
  to_plot = combn( variables, 2 , simplify=FALSE)
  to_plot=c(to_plot, lapply(to_plot, rev))
  plot_list = list()
  i=1

  for (i in 1:length(to_plot)) {
    cat( to_plot[[ i ]][1], to_plot[[ i ]][2], i, "\n")
    plot_list[[i]] <- NA
    # don't plot anything with aage or sex
    if (  intersect( to_plot[[ i ]], qc( age, sex ) ) %>% length() < 2 ) {
      plot_list[[i]] <- singleFacetPlot_violin( df1, to_plot[[ i ]][1], to_plot[[ i ]][2] )
    }

    file_name = paste("graphics/violin_", to_plot[[ i ]][1], "_", to_plot[[ i ]][2], i, ".tiff", sep="")
    tiff(file_name)
    print(plot_list[[i]])
    dev.off()
  }

  pdf("graphics/boxplot_combinations.pdf")
  for (i in 1:length(plot_list)) {
    print(plot_list[[i]])
  }
  dev.off()

}


var1='urbanization'
var2='seifa'

df1 %>%
  select_and_standardise_ddd( standardise_over = c("lga", 'region', var1, var2)) %>% 
  inner_join( df_lga_population , by='lga') %>%
  ggplot(mapping=aes_string(x = var1, y= "ddd", color=var1, fill=var1, weight="population")) +
  geom_violin( )  +
  ggtitle( paste("The range of LGA total DDD for each", var1, "facetted by", var2)) +
  facet_wrap( as.formula(paste(region, "~", var2)), ncol=2, scales='fixed')


