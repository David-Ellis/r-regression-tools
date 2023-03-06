# r-regression-tools

This is a collection of functions I’ve built to make my life easier when
performing regressions in r

## `all_pairs()`

When investigating marginal effects, we’re often only interested in the
first-order effects, i.e. those between two variables only.

That means that if we have three variables A, B and C, we’re not
interested in A*B*C. However, when we’ve got lots of variables, it can
take a long time to write out our function with each pair e.g. output ~
A + B + C + A*B + B+C + C*B and quickly gets worse as we increase the
number of variables.

`all_pairs()` writes the formula for us automatically:

      variables <- c("A", "B", "C", "D")
      my_output = "Output"
      all_pairs(my_output, variables)

    ## Output ~ A + B + C + D + A * B + A * C + A * D + B * C + B * 
    ##     D + C * D
    ## <environment: 0x000001f21918ddb0>
