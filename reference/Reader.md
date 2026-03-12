# Reader

Construct objects for reading data.

## Usage

``` r
Reader(...)

CSVReader(...)

JSONReader(...)

TXTReader(...)

RReader(...)

NULLReader(...)
```

## Arguments

- ...:

  Character vectors giving path components to the file to be read.

## Functions

- `CSVReader()`: Read CSV files.

- `JSONReader()`: Read JSON files.

- `TXTReader()`: Read TXT files.

- `RReader()`: Read R files.

- `NULLReader()`: Placeholder reader that always returns `NULL`.
