# Compensate FCS operator

##### Description

`read_fcs` operator transforms FCS files to Tercen datasets.

##### Usage

Input projection|.
---|---
`row`      | Variable, channel
`column`   | Observation, cells
`y`        | Measurement
`labels`   | "compensation_matrices" factor output during FCS import

Output relations|.
---|---
`filename`          | character, the name of the FCS file
`compensated`          | numeric, compensated values, per channel and observation

##### Details

The operator uses the `compensate()` function from the `flowCore` R package.
