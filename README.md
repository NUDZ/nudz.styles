nudz.styles
================
Lukáš ‘hejtmy’ Hejtmánek
15/08/2020

R styles for easy NUDZ like coloring of your presentations and posters
using ggplot.

## Description

The main functions you will need to style your ggplots are
`scale_color_nudz` and `scale_fill_nudz`. The main arguments allow you
to specify what type of palette you want (see below), if you need
discrete or continuous scaling and should the order of the colors be
reversed. If you opt in for base plot package, you may use the
`nudz_palette` command to produce your own palettes.

I have a slight issue with the yellow color, as it is literaly invisible
on white backgroud. So I recommend using some more reflective background
fill if you opt for thhe main palette, or just use a different one :)

Beware that the main palette only contains 5 values and the other ones
even less. So not the best option for exploratory analyses of XY groups
:)

## Installation

Installation can be easilly done with `devtools`.

``` r
devtools::install_github("nudz/nudz.styles")
```

## Usage

### Discrete scales

``` r
plt <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
    geom_point(size = 4)
```

``` r
plt + scale_color_nudz() + ggtitle("Main NUDZ palette")
```

![](figures/unnamed-chunk-4-1.png)<!-- -->

``` r
plt + scale_color_nudz(reversed = TRUE) + ggtitle("Main reversed NUDZ palette")
```

![](figures/unnamed-chunk-5-1.png)<!-- -->

``` r
plt + scale_color_nudz(n_colors = c(1,3,4)) + ggtitle("Main NUDZ palette with selected colors")
```

![](figures/unnamed-chunk-6-1.png)<!-- -->

### Continuous scales

``` r
plt <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Width)) + 
  geom_point()
```

``` r
plt + scale_color_nudz(discrete = FALSE) + ggtitle("Main continuous NUDZ palette")
```

![](figures/unnamed-chunk-8-1.png)<!-- -->

``` r
plt + scale_color_nudz(discrete = FALSE, n_colors = c(1,4)) + 
  ggtitle("Main continuous NUDZ palette with selected colors")
```

![](figures/unnamed-chunk-8-2.png)<!-- -->

### Base plots

Base plots are a little more troublesome as you need to do a biut of
pallete preprocessing, especially for the continous values, but it can
be done if you are more versed in base plotting than in ggplot.

``` r
pal <- nudz_palette()
pal <- pal(length(unique(iris$Species)))
plot(iris$Sepal.Width, iris$Sepal.Length, pch=19, col = pal[iris$Species])
legend("topright", col=pal, pch=19, 
       title = "Petal Length", legend=levels(iris$Species))
```

![](figures/unnamed-chunk-9-1.png)<!-- -->

``` r
pal <- nudz_palette(n_colors = c(3,4), discrete=FALSE)
petal_cols <- (iris$Petal.Length-min(iris$Petal.Length))*10 # standardizing to whole numbers 
pal <- pal(max(petal_cols))
plot(iris$Sepal.Width, iris$Sepal.Length, pch=19, col = pal[petal_cols])
legend("topright", col=c(pal[1], pal[length(pal)]), pch=19, 
       title = "Petal Length", legend=range(iris$Petal.Length))
```

![](figures/unnamed-chunk-10-1.png)<!-- -->

## All palettes

``` r
show_palette(nudz_palette(palette = "main"))
```

![](figures/unnamed-chunk-11-1.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "main", discrete = FALSE))
```

![](figures/unnamed-chunk-11-2.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "grey_and_blue"))
```

![](figures/unnamed-chunk-11-3.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "grey_and_blue", discrete = FALSE))
```

![](figures/unnamed-chunk-11-4.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "red_and_blue"))
```

![](figures/unnamed-chunk-11-5.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "red_and_blue", discrete = FALSE))
```

![](figures/unnamed-chunk-11-6.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "black_and_white"))
```

![](figures/unnamed-chunk-11-7.png)<!-- -->

``` r
show_palette(nudz_palette(palette = "black_and_white", discrete = FALSE))
```

![](figures/unnamed-chunk-11-8.png)<!-- -->
