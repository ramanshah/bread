# Scale recipes at the command line

`bread` is a program written in Haskell that scales recipes at the command line
and prints them nicely to the terminal:

```{bash}
$ bread ciabatta.yml 1.2
------------------------------------
Night before
------------------------------------
yeast                        0.6 tsp
whole wheat flour             60 g
unbleached all purpose flour 120 g
water                        360 g
------------------------------------
Day of
------------------------------------
water                        156 g
unbleached all purpose flour 480 g
extra virgin olive oil        18 g
salt                         1.8 tsp
------------------------------------
```

In baking bread (a hobby I enjoy), one often has to scale recipes up and down
to optimize them for a given pan. Baking is exacting, so one has to be
quantitative in this scaling work. All of the multiplication gets annoying to
do manually. `bread` allows you to enter your recipes as a collection of
sections with ingredients and amounts in an ergonomic YAML format (example
[`ciabatta.yml`](./doc/examples/ciabatta.yml) here). Given a positive real
scaling factor at the command line, it renders a scaled recipe that shows
quantities to a limited number of significant digits in a readable table
format.

Despite the name, this program is serviceable for all recipes; in particular,
it may be of use in non-bread baking, candy making, mixology, soap making,
routine chemical synthesis---anywhere you make stuff with measured ingredients
and are willing to have a computer handy.

## Installation

Installation from source using the Haskell Tool Stack proceeds as follows:

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/). As
   recommended in the Stack setup instructions, if you want `bread` to be
   globally available, ensure that you've added `~/.local/bin` to your path.
   In my `.zshrc`, I have `export PATH="${HOME}/.local/bin:${PATH}"`.
1. Clone this repo in a convenient spot, then `cd bread`.
1. `stack build`. This takes a long time the first time around.
1. `stack install`.
1. `cd doc/examples`, and try the above example!
