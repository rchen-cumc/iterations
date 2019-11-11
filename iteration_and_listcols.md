Iteration and List Cols
================
RuiJun Chen
10/29/2019

## This is gonna be so great

``` r
l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l$vec_numeric #can use variable names from within list
```

    ## [1] 5 6 7 8

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l[[2]] #can use index to pull out things from list; supposed to use 2 sq brackets
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
mean(l$vec_numeric)
```

    ## [1] 6.5

``` r
df = list(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

df$a
```

    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840
    ##  [8] 1.087654 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036
    ## [15] 3.350910 2.825453 2.408572 1.665973 1.902701 5.036104

``` r
df[[2]]
```

    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380
    ##  [6] -6.26177962 -5.04751876  3.75695597 -6.54176756  2.63770049
    ## [11] -2.66769787 -1.99188007 -3.94784725 -1.15070568  4.38592421
    ## [16]  2.26866589 -1.16232074  4.35002762  8.28001867 -0.03184464

``` r
is.list(df)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
mean_and_sd(df[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

``` r
mean_and_sd(df[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
output = vector("list", length = 4) 
```

Write our first for loop\!

``` r
for (i in 1:4) {
  output[[i]] = mean_and_sd(df[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
output = map(df, mean_and_sd) #for each element within df, runs through mean_and_sd
output
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
output_median = map(df, median)
output_median
```

    ## $a
    ## [1] 2.621376
    ## 
    ## $b
    ## [1] 0.7210996
    ## 
    ## $c
    ## [1] 10.05016
    ## 
    ## $d
    ## [1] -3.521665

``` r
output_summary = map(df, summary)
output_summary
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.088   1.731   2.621   2.700   3.466   5.036 
    ## 
    ## $b
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -6.5418 -2.1608  0.7211  0.4164  3.8297  8.2800 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   9.798   9.943  10.050  10.073  10.164  10.480 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -5.321  -4.459  -3.522  -3.431  -2.670  -1.281

``` r
output_median = map_dbl(df, median) #makes output a single list of doubles
output = map_dfr(df, mean_and_sd) #joins output into dataframe

output = map(df, ~mean_and_sd(.x)) #being very explicit about what is being mapped
```

## Napoleon\!\!

``` r
read_page_reviews = function(url) {
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_urls = str_c(url_base, 1:5) #Adds 1-5 to end of url_base

vec_urls
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
    ## [2] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"
    ## [3] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"
    ## [4] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=4"
    ## [5] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

``` r
read_page_reviews(vec_urls[[1]])
```

    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "Classic an funny!\n      …     5 "Classic movie and hilarious!!!\n    …
    ##  2 "I can watch this again an…     5 "Great movie...classic\n            " 
    ##  3 "Great quality!\n         …     5 "Great quality.\n            "        
    ##  4 "Good comedy\n            "     4 "Not as funny as I remember years ago…
    ##  5 "Awesome\n            "         5 "Favorite movie of all time\n        …
    ##  6 "yes\n            "             5 "good\n            "                  
    ##  7 "Gotta watch it!\n        …     5 "Super fun cult film. A must-see! Fun…
    ##  8 "Great movie\n            "     5 "Love this movie.\n            "      
    ##  9 "Duh\n            "             5 "Best movie ever\n            "       
    ## 10 "Great video\n            "     5 "Product as described.  Great transac…

``` r
output = vector("list", length = 5) #if don't explicitly creates output list, R will try to figure out what it needs. However, R also creates new output for each iteration, copies it over and adds to it, can be slow

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "Classic an funny!\n      …     5 "Classic movie and hilarious!!!\n    …
    ##  2 "I can watch this again an…     5 "Great movie...classic\n            " 
    ##  3 "Great quality!\n         …     5 "Great quality.\n            "        
    ##  4 "Good comedy\n            "     4 "Not as funny as I remember years ago…
    ##  5 "Awesome\n            "         5 "Favorite movie of all time\n        …
    ##  6 "yes\n            "             5 "good\n            "                  
    ##  7 "Gotta watch it!\n        …     5 "Super fun cult film. A must-see! Fun…
    ##  8 "Great movie\n            "     5 "Love this movie.\n            "      
    ##  9 "Duh\n            "             5 "Best movie ever\n            "       
    ## 10 "Great video\n            "     5 "Product as described.  Great transac…
    ## 
    ## [[2]]
    ## # A tibble: 10 x 3
    ##    title                        stars text                                 
    ##    <chr>                        <dbl> <chr>                                
    ##  1 "Give me some of your tots\…     5 "This movie will always be my favori…
    ##  2 "Nostalgic\n            "        5 "One of the best nostalgic movies of…
    ##  3 "Make you giggle type movie…     5 "I love, love, love this movie.  It …
    ##  4 "This movie is so stupid.\n…     5 "No, really.  It's so stupid.  Your …
    ##  5 "Hilarious\n            "        5 "Hilarious\n            "            
    ##  6 "Waste of money\n          …     1 "Terrible movie! Please don’t waste …
    ##  7 "Good movie\n            "       5 "Funny\n            "                
    ##  8 "A classic\n            "        5 "I like your sleeves. They're real b…
    ##  9 "FRIKKEN SWEET MOVIE, GAWSH…     5 "It’s Napolean Dynamite. It’s charmi…
    ## 10 "You gonna eat the rest of …     5 "One of my favorite movies ever.  Yo…
    ## 
    ## [[3]]
    ## # A tibble: 10 x 3
    ##    title                         stars text                                
    ##    <chr>                         <dbl> <chr>                               
    ##  1 "Tina you fat lard come get …     5 "It's a great movie\n            "  
    ##  2 "Great family movie\n       …     5 "My kids as well as the adults love…
    ##  3 "Teens love it\n            "     5 "Original and funny\n            "  
    ##  4 "Great\n            "             5 "Funny\n            "               
    ##  5 "Great Movie, Bad Packaging\…     4 "First off, the stick-on label on t…
    ##  6 "jeez napoleon\n            "     5 "gosh\n            "                
    ##  7 "👍\n            "                5 "👍\n            "                  
    ##  8 "A classic!\n            "        5 "A classic movie.  Hilarious!\n    …
    ##  9 "A must own\n            "        5 "Great movie\n            "         
    ## 10 "If you like 80s ...you must…     5 "My all time favorite movie. I have…
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    title                      stars text                                   
    ##    <chr>                      <dbl> <chr>                                  
    ##  1 "🤘\n            "             5 "🤘\n            "                     
    ##  2 "Super Slow Mooovie...\n …     1 "Too slow and too damn quiet... My gir…
    ##  3 "Awesome!\n            "       5 "Love this movie !\n            "      
    ##  4 "Very funny\n            "     4 "Very funny\n            "             
    ##  5 "Eat your food tina\n    …     5 "Cant go wrong\n            "          
    ##  6 "Dumb funny\n            "     5 "Dumb funny\n            "             
    ##  7 "Annoying! Not in a good …     1 "I know that I am one of the very few …
    ##  8 "Fun\n            "            5 "Fun\n            "                    
    ##  9 "such a great movie\n    …     5 "a true comedy classic\n            "  
    ## 10 "Napoleon Dud\n          …     3 "Not impressed w/movie.\n            " 
    ## 
    ## [[5]]
    ## # A tibble: 10 x 3
    ##    title                           stars text                              
    ##    <chr>                           <dbl> <chr>                             
    ##  1 "Five stars\n            "          5 "Such a weird, awesome movie\n   …
    ##  2 "Fun!\n            "                5 "Great movie\n            "       
    ##  3 "Funny movie- bravo for Amazon…     5 "My son loves this movie, so I wa…
    ##  4 "Movie\n            "               5 "Movie\n            "             
    ##  5 "Funny movie, quotable lines\n…     5 "My kids quote this movie all the…
    ##  6 "Great for teenagers!\n       …     5 "My students loved this movie.\n …
    ##  7 "can't believe we fell for thi…     1 "a pretty lame movie--can't belie…
    ##  8 "shut up tina you fat lard.\n …     5 "i LOVE napoleon.\n            "  
    ##  9 "Laughter is the Best Medicine…     5 "FAST SHIPPING! Love this Movie! …
    ## 10 "New condition\n            "       5 "Classic for the kids to watch.\n…

``` r
output = map(vec_urls, read_page_reviews)
output
```

    ## [[1]]
    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "Classic an funny!\n      …     5 "Classic movie and hilarious!!!\n    …
    ##  2 "I can watch this again an…     5 "Great movie...classic\n            " 
    ##  3 "Great quality!\n         …     5 "Great quality.\n            "        
    ##  4 "Good comedy\n            "     4 "Not as funny as I remember years ago…
    ##  5 "Awesome\n            "         5 "Favorite movie of all time\n        …
    ##  6 "yes\n            "             5 "good\n            "                  
    ##  7 "Gotta watch it!\n        …     5 "Super fun cult film. A must-see! Fun…
    ##  8 "Great movie\n            "     5 "Love this movie.\n            "      
    ##  9 "Duh\n            "             5 "Best movie ever\n            "       
    ## 10 "Great video\n            "     5 "Product as described.  Great transac…
    ## 
    ## [[2]]
    ## # A tibble: 10 x 3
    ##    title                        stars text                                 
    ##    <chr>                        <dbl> <chr>                                
    ##  1 "Give me some of your tots\…     5 "This movie will always be my favori…
    ##  2 "Nostalgic\n            "        5 "One of the best nostalgic movies of…
    ##  3 "Make you giggle type movie…     5 "I love, love, love this movie.  It …
    ##  4 "This movie is so stupid.\n…     5 "No, really.  It's so stupid.  Your …
    ##  5 "Hilarious\n            "        5 "Hilarious\n            "            
    ##  6 "Waste of money\n          …     1 "Terrible movie! Please don’t waste …
    ##  7 "Good movie\n            "       5 "Funny\n            "                
    ##  8 "A classic\n            "        5 "I like your sleeves. They're real b…
    ##  9 "FRIKKEN SWEET MOVIE, GAWSH…     5 "It’s Napolean Dynamite. It’s charmi…
    ## 10 "You gonna eat the rest of …     5 "One of my favorite movies ever.  Yo…
    ## 
    ## [[3]]
    ## # A tibble: 10 x 3
    ##    title                         stars text                                
    ##    <chr>                         <dbl> <chr>                               
    ##  1 "Tina you fat lard come get …     5 "It's a great movie\n            "  
    ##  2 "Great family movie\n       …     5 "My kids as well as the adults love…
    ##  3 "Teens love it\n            "     5 "Original and funny\n            "  
    ##  4 "Great\n            "             5 "Funny\n            "               
    ##  5 "Great Movie, Bad Packaging\…     4 "First off, the stick-on label on t…
    ##  6 "jeez napoleon\n            "     5 "gosh\n            "                
    ##  7 "👍\n            "                5 "👍\n            "                  
    ##  8 "A classic!\n            "        5 "A classic movie.  Hilarious!\n    …
    ##  9 "A must own\n            "        5 "Great movie\n            "         
    ## 10 "If you like 80s ...you must…     5 "My all time favorite movie. I have…
    ## 
    ## [[4]]
    ## # A tibble: 10 x 3
    ##    title                      stars text                                   
    ##    <chr>                      <dbl> <chr>                                  
    ##  1 "🤘\n            "             5 "🤘\n            "                     
    ##  2 "Super Slow Mooovie...\n …     1 "Too slow and too damn quiet... My gir…
    ##  3 "Awesome!\n            "       5 "Love this movie !\n            "      
    ##  4 "Very funny\n            "     4 "Very funny\n            "             
    ##  5 "Eat your food tina\n    …     5 "Cant go wrong\n            "          
    ##  6 "Dumb funny\n            "     5 "Dumb funny\n            "             
    ##  7 "Annoying! Not in a good …     1 "I know that I am one of the very few …
    ##  8 "Fun\n            "            5 "Fun\n            "                    
    ##  9 "such a great movie\n    …     5 "a true comedy classic\n            "  
    ## 10 "Napoleon Dud\n          …     3 "Not impressed w/movie.\n            " 
    ## 
    ## [[5]]
    ## # A tibble: 10 x 3
    ##    title                           stars text                              
    ##    <chr>                           <dbl> <chr>                             
    ##  1 "Five stars\n            "          5 "Such a weird, awesome movie\n   …
    ##  2 "Fun!\n            "                5 "Great movie\n            "       
    ##  3 "Funny movie- bravo for Amazon…     5 "My son loves this movie, so I wa…
    ##  4 "Movie\n            "               5 "Movie\n            "             
    ##  5 "Funny movie, quotable lines\n…     5 "My kids quote this movie all the…
    ##  6 "Great for teenagers!\n       …     5 "My students loved this movie.\n …
    ##  7 "can't believe we fell for thi…     1 "a pretty lame movie--can't belie…
    ##  8 "shut up tina you fat lard.\n …     5 "i LOVE napoleon.\n            "  
    ##  9 "Laughter is the Best Medicine…     5 "FAST SHIPPING! Love this Movie! …
    ## 10 "New condition\n            "       5 "Classic for the kids to watch.\n…

## list columns / weather df

``` r
weather = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2016-01-01",
    date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'crul':
    ##   method                 from
    ##   as.character.form_file httr

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## file path:          /Users/RayChen/Library/Caches/rnoaa/ghcnd/USW00094728.dly

    ## file last updated:  2019-10-14 01:14:10

    ## file min/max dates: 1869-01-01 / 2019-10-31

    ## file path:          /Users/RayChen/Library/Caches/rnoaa/ghcnd/USC00519397.dly

    ## file last updated:  2019-10-14 01:14:30

    ## file min/max dates: 1965-01-01 / 2019-10-31

    ## file path:          /Users/RayChen/Library/Caches/rnoaa/ghcnd/USS0023B17S.dly

    ## file last updated:  2019-10-14 01:14:37

    ## file min/max dates: 1999-09-01 / 2019-10-31

nest within stations

``` r
weather_nest = 
  weather %>% 
  nest(data = date:tmin) #data has 3:prcp, tmin, tmax for each date (366 x 4)

weather_nest
```

    ## # A tibble: 3 x 3
    ##   name           id                    data
    ##   <chr>          <chr>       <list<df[,4]>>
    ## 1 CentralPark_NY USW00094728      [366 × 4]
    ## 2 Waikiki_HA     USC00519397      [366 × 4]
    ## 3 Waterhole_WA   USS0023B17S      [366 × 4]

is the list column really a list??

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## <list_of<
    ##   tbl_df<
    ##     date: date
    ##     prcp: double
    ##     tmax: double
    ##     tmin: double
    ##   >
    ## >[3]>
    ## [[1]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows
    ## 
    ## [[2]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0  29.4  16.7
    ##  2 2016-01-02     0  28.3  16.7
    ##  3 2016-01-03     0  28.3  16.7
    ##  4 2016-01-04     0  28.3  16.1
    ##  5 2016-01-05     0  27.2  16.7
    ##  6 2016-01-06     0  27.2  20  
    ##  7 2016-01-07    46  27.8  18.3
    ##  8 2016-01-08     3  28.3  17.8
    ##  9 2016-01-09     8  27.8  19.4
    ## 10 2016-01-10     3  28.3  18.3
    ## # … with 356 more rows
    ## 
    ## [[3]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   1.7  -5.9
    ##  2 2016-01-02    25  -0.1  -6  
    ##  3 2016-01-03     0  -5   -10  
    ##  4 2016-01-04    25   0.3  -9.8
    ##  5 2016-01-05    25   1.9  -1.8
    ##  6 2016-01-06    25   1.4  -2.6
    ##  7 2016-01-07     0   1.4  -3.9
    ##  8 2016-01-08     0   1.1  -4  
    ##  9 2016-01-09     0   1.4  -4.5
    ## 10 2016-01-10     0   2.3  -3.8
    ## # … with 356 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows

``` r
weather_nest %>% 
  unnest()
```

    ## # A tibble: 1,098 x 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2016-01-01     0   5.6   1.1
    ##  2 CentralPark_NY USW00094728 2016-01-02     0   4.4   0  
    ##  3 CentralPark_NY USW00094728 2016-01-03     0   7.2   1.7
    ##  4 CentralPark_NY USW00094728 2016-01-04     0   2.2  -9.9
    ##  5 CentralPark_NY USW00094728 2016-01-05     0  -1.6 -11.6
    ##  6 CentralPark_NY USW00094728 2016-01-06     0   5    -3.8
    ##  7 CentralPark_NY USW00094728 2016-01-07     0   7.8  -0.5
    ##  8 CentralPark_NY USW00094728 2016-01-08     0   7.8  -0.5
    ##  9 CentralPark_NY USW00094728 2016-01-09     0   8.3   4.4
    ## 10 CentralPark_NY USW00094728 2016-01-10   457  15     4.4
    ## # … with 1,088 more rows

## Operations on list columns

can I do useful things with a list column…?

``` r
central_park_df = weather_nest$data[[1]]

lm(tmax ~ tmin, data = central_park_df)
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = central_park_df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045

``` r
lm(tmax ~ tmin, data = weather_nest$data[[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326

``` r
lm(tmax ~ tmin, data = weather_nest$data[[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

try a loop…

``` r
output = vector("list", length = 3)

for (i in 1:3) {
  output[[i]] = lm(tmax ~ tmin, weather_nest$data[[i]])
}

output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[i]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
```

``` r
for (i in 1:3) {
  output[[i]] = weather_lm(weather_nest$data[[i]])
}
output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
output = map(weather_nest$data, weather_lm)
output
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.779        1.045  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##      22.489        0.326  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       6.851        1.245

``` r
weather_nest %>% 
  mutate(lin_models = map(data, weather_lm))
```

    ## # A tibble: 3 x 4
    ##   name           id                    data lin_models
    ##   <chr>          <chr>       <list<df[,4]>> <list>    
    ## 1 CentralPark_NY USW00094728      [366 × 4] <lm>      
    ## 2 Waikiki_HA     USC00519397      [366 × 4] <lm>      
    ## 3 Waterhole_WA   USS0023B17S      [366 × 4] <lm>

``` r
weather_nest %>% 
  mutate(lin_models = map(data, weather_lm)) %>% 
  select(-data) %>% 
  filter(name != "CentralPark_NY")
```

    ## # A tibble: 2 x 3
    ##   name         id          lin_models
    ##   <chr>        <chr>       <list>    
    ## 1 Waikiki_HA   USC00519397 <lm>      
    ## 2 Waterhole_WA USS0023B17S <lm>

## Revisit Napoleon.. again

``` r
napoleon = 
  tibble(
    page = 1:5,
    urls = str_c(url_base, page)
  ) %>% 
  mutate(
    reviews = map(urls, read_page_reviews)
  )
napoleon
```

    ## # A tibble: 5 x 3
    ##    page urls                                                  reviews      
    ##   <int> <chr>                                                 <list>       
    ## 1     1 https://www.amazon.com/product-reviews/B00005JNBQ/re… <tibble [10 …
    ## 2     2 https://www.amazon.com/product-reviews/B00005JNBQ/re… <tibble [10 …
    ## 3     3 https://www.amazon.com/product-reviews/B00005JNBQ/re… <tibble [10 …
    ## 4     4 https://www.amazon.com/product-reviews/B00005JNBQ/re… <tibble [10 …
    ## 5     5 https://www.amazon.com/product-reviews/B00005JNBQ/re… <tibble [10 …

``` r
napoleon = 
  tibble(
    page = 1:5,
    urls = str_c(url_base, page)
  ) %>% 
  mutate(
    reviews = map(urls, read_page_reviews)
  ) %>% 
  unnest(reviews) %>% 
  select(-urls)

napoleon
```

    ## # A tibble: 50 x 4
    ##     page title                     stars text                              
    ##    <int> <chr>                     <dbl> <chr>                             
    ##  1     1 "Classic an funny!\n    …     5 "Classic movie and hilarious!!!\n…
    ##  2     1 "I can watch this again …     5 "Great movie...classic\n         …
    ##  3     1 "Great quality!\n       …     5 "Great quality.\n            "    
    ##  4     1 "Good comedy\n          …     4 "Not as funny as I remember years…
    ##  5     1 "Awesome\n            "       5 "Favorite movie of all time\n    …
    ##  6     1 "yes\n            "           5 "good\n            "              
    ##  7     1 "Gotta watch it!\n      …     5 "Super fun cult film. A must-see!…
    ##  8     1 "Great movie\n          …     5 "Love this movie.\n            "  
    ##  9     1 "Duh\n            "           5 "Best movie ever\n            "   
    ## 10     1 "Great video\n          …     5 "Product as described.  Great tra…
    ## # … with 40 more rows
