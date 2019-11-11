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
