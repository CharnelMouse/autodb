# Nested data

``` r

library(autodb)
```

    ## 
    ## Attaching package: 'autodb'

    ## The following object is masked from 'package:stats':
    ## 
    ##     decompose

``` r

if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x, ...) DiagrammeR::grViz(gv(x, ...), width = "100%")
}else{
  show <- function(x, ...) cat(d2(x), sep = "\n")
}
```

While `autodb` takes a single data frame for input, its data might not
be “flat”. As an example, the data might be imported from JSON, using
the `jsonlite` package.

Such nested data frames can be a pain to work with in base R, because
many functions aren’t designed to handle them. For example, consider the
JSON data below.

``` json
[
  {
    "a": 1,
    "b": [1],
    "c": [1, 2],
    "d": [
      [1, 2],
      [3, 4]
    ],
    "e": [1, 2],
    "f": [
      [1, 2],
      [3, 4]
    ],
    "g": [1, 2],
    "h": [[1], [1]],
    "i": {
      "i.1": 1,
      "i.2": "a"
    },
    "k": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "l": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "m": [
      {
        "m.1": 1,
        "m.2": 4
      },
      {
        "m.1": 2,
        "m.2": 5
      },
      {
        "m.1": 3,
        "m.2": 6
      }
    ],
    "n": [
      {
        "n.1": 1,
        "n.2": 4
      },
      {
        "n.1": 2,
        "n.2": 5
      },
      {
        "n.1": 3,
        "n.2": 6
      }
    ]
  },
  {
    "a": 2,
    "b": [2, 3],
    "c": [3, 4],
    "d": [
      [5, 6],
      [7, 8]
    ],
    "e": [1, 2],
    "f": [
      [5, 6],
      [7, 8]
    ],
    "g": [2, 1],
    "h": [[2], [2]],
    "i": {
      "i.1": 2,
      "i.2": "b"
    },
    "k": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "l": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "m": [
      {
        "m.1": 1,
        "m.2": 4
      },
      {
        "m.1": 2,
        "m.2": 5
      },
      {
        "m.1": 3,
        "m.2": 6
      }
    ],
    "n": [
      {
        "n.1": 1,
        "n.2": 4
      },
      {
        "n.1": 2,
        "n.2": 5
      },
      {
        "n.1": 3,
        "n.2": 6
      }
    ]
  },
  {
    "a": 3,
    "b": [4, 5, 6],
    "c": [5, 6],
    "d": [
      [9, 10],
      [11, 12]
    ],
    "e": ["a", "b"],
    "f": [
      [9, 10],
      [11, 12]
    ],
    "g": [1, 2],
    "h": [[1], [1]],
    "i": {
      "i.1": 3,
      "i.2": "c"
    },
    "k": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "l": [
      [1, 4],
      [2, 5],
      [3, 6]
    ],
    "m": [
      {
        "m.1": 1,
        "m.2": 4
      },
      {
        "m.1": 2,
        "m.2": 5
      },
      {
        "m.1": 3,
        "m.2": 6
      }
    ],
    "n": [
      {
        "n.1": 1,
        "n.2": 4
      },
      {
        "n.1": 2,
        "n.2": 5
      },
      {
        "n.1": 3,
        "n.2": 6
      }
    ]
  },
  {
    "a": 4,
    "b": [1],
    "c": [3, 4],
    "d": [
      [9, 10],
      [11, 12]
    ],
    "e": [
      ["a"],
      ["b", "c"]
    ],
    "f": [
      [9, 10]
    ],
    "g": [2, 1],
    "h": [[3], [3]],
    "i": {
      "i.1": 4,
      "i.2": "d"
    },
    "k": [
      [1, 6],
      [2, 7],
      [3, 8],
      [4, 9],
      [5, 10]
    ],
    "l": [
      [7, 10],
      [8, 11],
      [9, 12]
    ],
    "m": [
      {
        "m.1": 1,
        "m.2": 6
      },
      {
        "m.1": 2,
        "m.2": 7
      },
      {
        "m.1": 3,
        "m.2": 8
      },
      {
        "m.1": 4,
        "m.2": 9
      },
      {
        "m.1": 5,
        "m.2": 10
      }
    ],
    "n": [
      {
        "n.1": 7,
        "n.2": 10
      },
      {
        "n.1": 8,
        "n.2": 11
      },
      {
        "n.1": 9,
        "n.2": 12
      }
    ]
  },
  {
    "a": 5,
    "b": [2, 3],
    "c": [3, 4],
    "d": [
      [9, 10],
      [11, 12]
    ],
    "e": [
      ["a"],
      ["b", "c"]
    ],
    "f": [
      [9, 10]
    ],
    "g": [1, 2],
    "h": [[2], [2]],
    "i": {
      "i.1": 5,
      "i.2": "e"
    },
    "k": [
      [1, 6],
      [2, 7],
      [3, 8],
      [4, 9],
      [5, 10]
    ],
    "l": [
      [7, 10],
      [8, 11],
      [9, 12]
    ],
    "m": [
      {
        "m.1": 1,
        "m.2": 6
      },
      {
        "m.1": 2,
        "m.2": 7
      },
      {
        "m.1": 3,
        "m.2": 8
      },
      {
        "m.1": 4,
        "m.2": 9
      },
      {
        "m.1": 5,
        "m.2": 10
      }
    ],
    "n": [
      {
        "n.1": 7,
        "n.2": 10
      },
      {
        "n.1": 8,
        "n.2": 11
      },
      {
        "n.1": 9,
        "n.2": 12
      }
    ]
  }
]
```

R hides a lot of class details when printing; printing as a tibble hides
less.

``` r

x <- jsonlite::fromJSON(js)
x
```

    ##   a       b    c             d       e             f    g    h i.i.1 i.i.2
    ## 1 1       1 1, 2    1, 3, 2, 4    1, 2    1, 3, 2, 4 1, 2 1, 1     1     a
    ## 2 2    2, 3 3, 4    5, 7, 6, 8    1, 2    5, 7, 6, 8 2, 1 2, 2     2     b
    ## 3 3 4, 5, 6 5, 6 9, 11, 10, 12    a, b 9, 11, 10, 12 1, 2 1, 1     3     c
    ## 4 4       1 3, 4 9, 11, 10, 12 a, b, c         9, 10 2, 1 3, 3     4     d
    ## 5 5    2, 3 3, 4 9, 11, 10, 12 a, b, c         9, 10 1, 2 2, 2     5     e
    ##                               k                   l
    ## 1              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 2              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 3              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 4 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ## 5 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ##                               m                   n
    ## 1              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 2              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 3              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 4 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ## 5 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12

``` r

print(tibble::as_tibble(x), width = Inf)
```

    ## # A tibble: 5 × 13
    ##       a b         c         d             e          f             g        
    ##   <int> <list>    <list>    <list>        <list>     <list>        <list>   
    ## 1     1 <int [1]> <int [2]> <int [2 × 2]> <int [2]>  <int [2 × 2]> <int [2]>
    ## 2     2 <int [2]> <int [2]> <int [2 × 2]> <int [2]>  <int [2 × 2]> <int [2]>
    ## 3     3 <int [3]> <int [2]> <int [2 × 2]> <chr [2]>  <int [2 × 2]> <int [2]>
    ## 4     4 <int [1]> <int [2]> <int [2 × 2]> <list [2]> <int [1 × 2]> <int [2]>
    ## 5     5 <int [2]> <int [2]> <int [2 × 2]> <list [2]> <int [1 × 2]> <int [2]>
    ##   h             i$i.1 $i.2  k             l             m           
    ##   <list>        <int> <chr> <list>        <list>        <list>      
    ## 1 <int [2 × 1]>     1 a     <int [3 × 2]> <int [3 × 2]> <df [3 × 2]>
    ## 2 <int [2 × 1]>     2 b     <int [3 × 2]> <int [3 × 2]> <df [3 × 2]>
    ## 3 <int [2 × 1]>     3 c     <int [3 × 2]> <int [3 × 2]> <df [3 × 2]>
    ## 4 <int [2 × 1]>     4 d     <int [5 × 2]> <int [3 × 2]> <df [5 × 2]>
    ## 5 <int [2 × 1]>     5 e     <int [5 × 2]> <int [3 × 2]> <df [5 × 2]>
    ##   n           
    ##   <list>      
    ## 1 <df [3 × 2]>
    ## 2 <df [3 × 2]>
    ## 3 <df [3 × 2]>
    ## 4 <df [3 × 2]>
    ## 5 <df [3 × 2]>

`autodb` has some ability to deal with nested data: specifically, where
columns contain lists, data frames, or matrices. When plotting, column
classes are described with any common nested class information within
angle brackets (`<>`), and common dimensions are given within square
brackets (`[]`):

``` r

show(x)
```

The first dimension isn’t at the top level, since the number of rows is
already given in the table’s header. For example, column `i` is a data
frame, so has the same number of rows as the main data frame: the size
information only gives its column count.

`nest_level` can be set to limit how deeply nested the given class
information can be:

``` r

show(x, nest_level = 0)
```

Dependency discovery and decomposition work as expected:

``` r

db <- autodb(x)
show(db)
```

Also as expected, rejoining the resulting database into a single data
frame gives back the original:

``` r

y <- rejoin(db)
df_equiv(y, x)
```

    ## [1] TRUE

``` r

y
```

    ##   a       b    c             d       e             f    g    h i.i.1 i.i.2
    ## 1 1       1 1, 2    1, 3, 2, 4    1, 2    1, 3, 2, 4 1, 2 1, 1     1     a
    ## 2 2    2, 3 3, 4    5, 7, 6, 8    1, 2    5, 7, 6, 8 2, 1 2, 2     2     b
    ## 3 3 4, 5, 6 5, 6 9, 11, 10, 12    a, b 9, 11, 10, 12 1, 2 1, 1     3     c
    ## 4 4       1 3, 4 9, 11, 10, 12 a, b, c         9, 10 2, 1 3, 3     4     d
    ## 5 5    2, 3 3, 4 9, 11, 10, 12 a, b, c         9, 10 1, 2 2, 2     5     e
    ##                               k                   l
    ## 1              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 2              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 3              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 4 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ## 5 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ##                               m                   n
    ## 1              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 2              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 3              1, 2, 3, 4, 5, 6    1, 2, 3, 4, 5, 6
    ## 4 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
    ## 5 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 7, 8, 9, 10, 11, 12
