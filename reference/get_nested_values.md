# Helper function to allow a nested list to be traversed using a key string supporting nesting

Helper function to allow a nested list to be traversed using a key
string supporting nesting

## Usage

``` r
get_nested_values(lst, key_strings, sep = "/", simplify = TRUE)
```

## Arguments

- lst:

  a list to be traversed

- key_strings:

  a character vector of keys to traverse the list. Each key string
  should be a single string with the keys separated by a separator
  (default is "/"). For example, "parent/child/grandchild" would
  correspond to my_list\$parent\$child\$grandchild

- sep:

  a character string to separate the keys in the key strings. Default is
  "/"

- simplify:

  Logical; whether to simplify the output to a vector (if possible).
  Default is `TRUE`.

## Value

a named list of values corresponding to the keys in the key strings
