#!/usr/bin/env Rscript

source( "lib/functions_preload.R")
detach_all_packages()

safe_load( 'workflowr')
safe_load("seas")
safe_load("magrittr")
safe_load("stringr")
safe_load("knitr")
#safe_load("kableExtra")
#safe_load("pander")
safe_load("lubridate")

safe_load("readstata13" )
#safe_load("foreign" )
safe_load("wrapr" )   # for the qc function

#safe_load("ordinal" )
safe_load("DataCache", dev='jbryer/DataCache' )
safe_load( 'fuzzyjoin')
library('IRanges')
safe_load("multidplyr", dev = "hadley/multidplyr")
safe_load("tidyverse")
safe_load("tictoc")



# one of these need the newest version of libgdal

# sudo add-apt-repository -y ppa:ubuntugis/ppa

# agi libudunits2-dev libgdal-dev libudunits2-dev libgdal-dev libgeos-dev libproj-dev


#################################################################################
# usage - add '-sadgg' to the end of ggplot string to store the plot in a directory
# sadgg
#################################################################################

`-.gg` <- function(e1, e2) e2(e1)
sadgg <- function ( plot ) {
  base_dir='~/ggplots/'
  filetype='svg'
  if( !dir.exists( base_dir ) ) {
    dir.create( base_dir )
  }

  list.files( base_dir ) %>%
    as.tibble() %>%
    separate(value, into='n', sep="_", extra='drop') %>% 
    mutate( n=as.numeric(n) ) %>%
    summarise( maxn=max(n)) %$% maxn %>% 
    { . } -> maxn
  if (maxn==-Inf) { maxn=0}    

  filename = paste0( sprintf( "%03d_", maxn+1), 
                    paste( plot$mapping, collapse='_'),
                    '.', filetype)

  ggsave( filename, plot, device=filetype, path=base_dir)
  print(plot)
  #svgPanZoom( plot )

  #    if( system2('pidof', args='geeqie', stdout='/dev/null') ) {
  system2( 'geeqie', args=c( '-r', '-t',  paste0( 'file:', base_dir, filename)), wait=FALSE)
  #system2( 'firefox', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #system2( 'display', args=c( paste0(  base_dir, filename)), wait=FALSE)
  #    }
}

#################################################################################
is_theme_complete = function (x)  {TRUE}  # fix up bug in current tricolore library

# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
  sapply( year, function(year) { year.length(as.character(year )) })
}

#################################################################################
really_is_numeric <- function(x) inherits(x, c("numeric","integer"))

#################################################################################
keep <- function(x, name) {assign(as.character(substitute(name)), x, pos = 1)}
#################################################################################
qw <- function(x) unlist(strsplit(x, "[[:space:]]+"))

#################################################################################
destring <- function(x,keep="0-9.-") {
  return( as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)) )
}

#################################################################################
save_data_single <- function( item ) {
	fwrite( eval(parse(text = item)), paste0(item, ".csv"))
}

#################################################################################
save_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, save_data_single )
	} else {
		save_data_single( item ) 
	}
}
#################################################################################
read_data_single <- function( item ) {
	 do.call("<<-",list(item, fread( paste0(item, ".csv")) %>% as.tibble()))
}
read_data<- function( item ) {
	if( is.vector(item) ) {
		lapply( item, read_data_single )
	} else {
		read_data_single( item ) 
	}
}
# my.year.length  ------------------------------------------------------------------
#
my_year_length <- function( year ) {
	sapply( year, function(year) { year.length(as.character(year )) })
}


# seqNext ------------------------------------------------------------------
seqNext <- function(x1, y1) {
  dat <- data.frame( x= x1
  			 , y=y1) 
  unname(
    predict(lm(y ~ x, data=dat), newdata=list(x=c(2016)))
  )
}

# bothDiff  ------------------------------------------------------------------
both_diff <- function ( set1, set2, by=intersect(  names( set1 ), names( set2))) {

  print("first - second")
  set1 %>%
    anti_join( set2, by=by ) %>%
    arrange_( by ) %>%
    print()

  print("second - first")
  set2 %>%
    anti_join( set1, by=by ) %>%
    arrange_( by ) %>%
    print()

}
# ------------------------------------------------------------------
slurpTable <- function( name ) {

	state_geo_query = paste( "
			SELECT * FROM ", name
			, sep=""
			)
	dbGetQuery(con, state_geo_query ) %>% 
		as.tibble() %>%
	return( . )
}

# -------------------------------------------------

broken_get_state_code_restriction <- function( state_id) { 
#
state = c( "NSW" , "VIC" , "QLD" , "WA" , "SA" , "TAS" , "NT" , "ACT" , "UNK")
sapply( state_id, function( x )  {
                         paste(
                                "'"
                                , state[ as.numeric( x ) ]
                                , "'"
                                , sep=""
                                )
                  }
    )
}

# -------------------------------------------------
get_state_code_from_lga <- function( lga ) { 
#

lga %>%
  substr( 1,1 ) %>%
  plyr::mapvalues( from=c(as.character(1:9), '.'),
            to=c( "NSW" , "VIC" , "QLD" , "SA" , 
                 "WA" , "TAS" , "NT" , "ACT" , "UNK", "UNK"),
            warn_missing=FALSE
            )

}


# -------------------------------------------------
get_significant_trend <- function( df, ddd_direction_labels)	{

	df  %>% 
		mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
		filter( supply_year != 2012 & supply_year != 2016 ) %>%
		group_by(lga, supply_year) %>% 
		summarise( no_doses=sum(no_doses)) %>%
		inner_join( 
			df_population %>%
				mutate( supply_year = as.numeric(as.character( supply_year ))) %>%
				filter( supply_year != 2012 | supply_year != 2016 ) %>%
				group_by( lga, supply_year) %>%
				summarize( person_days = sum(population * my_year_length(supply_year)))
			, by=c("lga", "supply_year") 
		) %>%
		mutate( ddd = (no_doses * 1000) / (person_days)) %>%
		as_tibble() %>%
		select( lga, supply_year, ddd) %>%
		split(.$lga)   %>%
		map(~  # p value for each lga
			lm(ddd ~ as.integer(supply_year), data=.)   %>% 
			tidy() %>% 
			filter(term != "(Intercept)") %>% 
			select(estimate, p.value) 
		) %>%
		do.call("rbind", . ) %>%
		mutate( lga = as.factor(rownames(.))) %>%
		mutate( direction=ifelse( round(p.value, 2) >.05 , "None" , ifelse( estimate>0, "Rising", "Falling"))
			, direction=factor(direction, levels=ddd_direction_labels)) 
}
# -------------------------------------------------


singleFacetPlot_boxplot = function( df_raw_input, var1, var2 ) {

		gb = c("lga", var1, var2 )
		df_raw_input %>%
			group_by_at( vars(gb)) %>%
			summarise( no_users = n()) %>%
			ggplot() +
			geom_boxplot( mapping=aes_string(x = var1, y= "no_users" , color=var1, fill=var1)) + 
			ggtitle( paste("The range of LGA number of users for each", var1, "facetted by", var2)) +
			facet_wrap( as.formula(paste("~", var2)), ncol=2, scales='fixed')
}
#-------------------------------------------------------------------------------------------

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#-------------------------------------------------------------------------------------------

urbanisation_rollup_single <- function( code, desc ) {
	rv = as.character(desc)
	if (substr(as.character( code ), 1, 2) %in% qw("RA RT UD UF UR")) {
			rv =  sub("^([^ ]* [^ ]*) .*","\\1", rv) 
	}
	rv = sub( "Agriculture", "Agricultural", rv )
	rv
}
#-------------------------------------------------------------------------------------------
urbanisation_rollup <- function(code, desc) {
	mapply( urbanisation_rollup_single, code, desc )
}

#-------------------------------------------------------------------------------------------
is_geographic_LGA  <- function( lga ) {
  !endsWith( as.character(lga), '99') && 
    !startsWith( as.character( lga), 'UNK') && 
    (lga != '.')
}

#-------------------------------------------------------------------------------------------
clipboard <- function(x, sep="\t", row.names=FALSE, col.names=TRUE){
     con <- pipe("xclip -selection clipboard -i", open="w")
     write.table(x, con, sep=sep, row.names=row.names, col.names=col.names)
     close(con)
}

#-------------------------------------------------------------------------------------------
ws <- function( ... ) {
  wideScreen( ... )
}

#-------------------------------------------------------------------------------------------
wideScreen <- function(howWide=Sys.getenv("COLUMNS")) {
  options(width=as.integer(howWide))
}

#-------------------------------------------------------------------------------------------
sourcep <- function(file){
  coms <- parse(file)
  for (i in seq_along(coms)){
    print(coms[[i]])
    eval(coms[[i]],envir=.GlobalEnv)
    mess <- paste("Expression",i,"of",length(coms),"parsed. Press <return> to continue.")
    cat(mess)
    readLines(n=1)
  }
}

#-----------------------------------------------------------------------------
age_grouping_mofi <- function( age) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
  cut( age, 
      breaks=c(-1,19,44,64, Inf),
      labels=c("0-19", "20-44", "45-64", "65+"),
      ordered_result=TRUE)
}


#-----------------------------------------------------------------------------
age_grouping <- function( age, n=25 ) {
  if( typeof(age) == 'character') {
    age = replace( age, age=="100+", "101")
    age = as.integer(age)
  } 
      paste0(sprintf("%02d", 
                     floor(as.numeric(age)/n)*n), 
             "-", 
             sprintf( "%02d", 
                     (floor(as.numeric(age) / n) + 1 ) * n - 1)
             )
  }



#-----------------------------------------------------------------------------

rows = function(x) lapply(seq_len(nrow(x)), function(i) lapply(x,"[",i))



#-----------------------------------------------------------------------------
# send formattded to clipboard

xc = function (df ) {
  df %>% 
    mutate_if( really_is_numeric, round, 2 ) %>%
    tableHTML::tableHTML() %>% clipr::write_clip()
} 


#-----------------------------------------------------------------------------
# multi assignment functions

# Generic form
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)

  if (length(r) > length(l))
    warning("RHS has more args than LHS. Only first", length(l), "used.")

  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }

  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)

  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin

  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}


# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}


################################################################################
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
                                       fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
                             format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
                      as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()


################################################################################
