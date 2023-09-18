# Compensate FCS operator

##### Description

`read_fcs` operator transforms FCS files to Tercen datasets.

##### Usage

Multi data step - Left|.
---|---
`row`      | Channel
`column`   | Event ID + filename
`y`        | Measurement

Multi data step - Right|.
---|---
`row`      | "comp_1" factor as output by FCS importer
`column`   | "comp_2" factor as output by FCS importer + Filename
`y`        | Compensation value

Output relations|.
---|---
`compensated`          | numeric, compensated values, per channel and observation

##### Details

The operator uses the `compensate()` function from the `flowCore` R package.
