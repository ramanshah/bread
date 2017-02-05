# Scale bread recipes at the command line

This is just a chance for me to practice some Haskell and learn Haskell's
dependency management tooling while solving a common issue in my hobby of
baking bread.

## Problem statement

In baking bread, one often has to scale recipes up and down to optimize them
for a given pan. This gets annoying to do manually.  Given a file with
ingredients and amounts in an ergonomic (TBD) format and a positive real
scaling factor at the command line, pretty-print a scaled recipe that shows
quantities to a limited number of significant digits.
