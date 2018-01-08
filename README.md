# Course Data

This project cleans and analyzes enrollment and evaluation data from my past courses.

## Dependencies

- `R` and several `R` packages
- `make`

## Usage

- Make a directory `data/enrollments` and drop course enrollment CSV files in there.
- Make a directory `data/evaluations` and drop evaluation PDF files (the "Responses" ones, not the "Summary" ones) in there.
- Run `make` from the top of the directory.  Alternatively, you can manually run `data-cleaning.r` and then `course-analysis.Rmd`.
- Your report should show up in the `output` directory as an HTML file.