# clquestions
clquestions is an R package for downloading data from the UK Parliament's [Written Questions API](https://writtenquestions-api.parliament.uk/index.html). This package has been developed principally to support work on Parliamentary data in the House of Commons Library but it may be useful to other researchers working with this data. This package is still in active development and the API may evolve over time.

## Overview
The package provides sets of functions for retrieving data from different endpoints of the written questions API and returning the data as a tibble. The package does not aim to exhaustively expose every possible parameter of the API, but is focussed on downloading key datasets than can be further explored, transformed and combined with other data in R. To help with using parts of the API that are not explicitly covered, the package also provides some lower level functions that allow you to retrieve data from the API with different parameters as native R data structures.

## Installation
Install from Github with using remotes.

```r
install.packages("remotes")
remotes::install_github("houseofcommonslibrary/clquestions")
```

---

## Written questions and answers
All functions have four optional arguments: ```from_date```, ```to_date```, ```on_date``` and ```house``` which can be used to filter the data returned from the API based on the date of a question or answer and the house it was tabled.

The ```on_date``` argument is a convenience that sets the ```from_date``` and ```to_date``` to the same given date. The ```on_date``` has priority: if the ```on_date``` is set, the ```from_date``` and ```to_date``` are ignored. The values for these arguments can be either a Date or a string specifying a date in ISO 8601 format ("YYYY-MM-DD"). By default the date arguments are set to ```NULL``` meaning that data form the API is not filtered by question or answer date.

The date arguments filter data retrieved by the API based on whether the function is aimed at ```_questions``` or ```_answers```. For example, in  ```fetch_written_questions``` the date filtering will be against the date of when the question was tabled, whereas ```fetch_written_answers``` will be against the date of when a question was answered. Functions ending in ```_questions``` are more inclusive and will return questions regardless if they have been answered or not. 

The ```house``` argument accepts "c", "C", "Commons", "l", "L", "Lords" to indicate which house the written question was tabled in. By default it is set to `NULL` and returns data from both houses. 

There are two additional arguments which must be supplied for each function: ```take``` and ```summary```. 

```take``` is an integer used to tell the API how many records should be retrieved. Records are taken newest to oldest, e.g. if ```take = 1000``` (the default) then up to 1,000 of the most recent records are retrieved from the API. If there are fewer records available than the value of ```take``` all records will be retrieved. A warning appears if there are more records available than the value of ```take```. 

```summary``` is a boolean indicating whether to exclude nested and empty columns in the results. The default is TRUE.

---

```r
clquestions::fetch_written_questions(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)

clquestions::fetch_written_answers(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written question and answer, with one row per question and answer.

This dataframe contains summary details for each written question and answer, such as the question/answer date, question/answer text and details on the member who asked/answered the question.

---

```r
clquestions::fetch_written_questions_body(
    body_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)

clquestions::fetch_written_answers_body(
    body_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written question and answer by the body responsible for responding, with one row per question and answer.

```body_id``` is an integer representing the ID of the body responsible for answering the written question. Only one integer can be supplied.

This dataframe contains summary details for each written question and answer, such as the question/answer date, question/answer text and details on the member who asked/answered the question.

Use ```get_answer_bodies()``` for a list of all available answer bodies and their corresponding ID numbers.

---

```r
clquestions::fetch_written_questions_member(
    member_mnis_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)

clquestions::fetch_written_answers_member(
    member_mnis_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written question and answer by the member asking or answering the question, with one row per question and answer.

```member_mnis_id``` is an integer representing the MNIS ID for Commons and Lords members.

This dataframe contains summary details for each written question and answer, such as the question/answer date, question/answer text and details on the member who asked/answered the question.

---

```r
clquestions::fetch_written_questions_search(
    search_term = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)

clquestions::fetch_written_answers_search(
    search_term = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written question and answer which contain the specified search term, with one row per question and answer.

```search_term``` is a string containing a single search term, e.g. "veterans". It does not use boolean logic.

This dataframe contains summary details for each written question and answer, such as the question/answer date, question/answer text and details on the member who asked/answered the question.

---

## Written statements
All functions have four optional arguments: ```from_date```, ```to_date```, ```on_date``` and ```house``` which can be used to filter the data returned from the API based on the date of a statement and the house it was made.

The ```on_date``` argument is a convenience that sets the ```from_date``` and ```to_date``` to the same given date. The ```on_date``` has priority: if the ```on_date``` is set, the ```from_date``` and ```to_date``` are ignored. The values for these arguments can be either a Date or a string specifying a date in ISO 8601 format ("YYYY-MM-DD"). By default the date arguments are set to ```NULL``` meaning that data form the API is not filtered by statement date.

The ```house``` argument accepts "c", "C", "Commons", "l", "L", "Lords" to indicate which house the written statement was made in. By default it is set to `NULL` and returns data from both houses. 

There are two additional arguments which must be supplied for each function: ```take``` and ```summary```. 

```take``` is an integer used to tell the API how many records should be retrieved. Records are taken newest to oldest, e.g. if ```take = 1000``` (the default) then up to 1,000 of the most recent records are retrieved from the API. If there are fewer records available than the value of ```take``` all records will be retrieved. A warning appears if there are more records available than the value of ```take```. 

```summary``` is a boolean indicating whether to exclude nested and empty columns in the results. The default is TRUE.

---

```r
clquestions::fetch_written_statements(
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written statement, with one row per statement.

This dataframe contains summary details for each written statement, such as the statement date, statement text and details on the member who made the statement.

---

```r
clquestions::fetch_written_statements_body(
    body_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written statement by the body responsible for the statement, with one row per statement.

```body_id``` is an integer representing the ID of the body responsible for the written statement. Only one integer can be supplied.

This dataframe contains summary details for each written statement, such as the statement date, statement text and details on the member who made the statement.

Use ```get_answer_bodies()``` for a list of all available answer bodies and their corresponding ID numbers.

---

```r
clquestions::fetch_written_statements_member(
    member_mnis_id = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)
```

Fetch a dataframe of key details about each written statement by the member who made the statement, with one row per statement.

```member_mnis_id``` is an integer representing the MNIS ID for Commons and Lords members.

This dataframe contains summary details for each written statement, such as the statement date, statement text and details on the member who made the statement.

---

```r
clquestions::fetch_written_statements_search(
    search_term = NULL,
    from_date = NULL,
    to_date = NULL,
    on_date = NULL,
    house = NULL,
    take = 1000,
    summary = TRUE)

```

Fetch a dataframe of key details about each written statement which contain the specified search term, with one row per statement.

```search_term``` is a string containing a single search term, e.g. "veterans". It does not use boolean logic.

This dataframe contains summary details for each written statement, such as the statement date, statement text and details on the member who made the statement.
