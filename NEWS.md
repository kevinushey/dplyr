dplyr 0.1.0.99
--------------

* new `location()` and `changes()` functions which provide more information
  about how data frames are stored in memory so that you can see what
  gets copied

* renamed `explain_tbl()` to `explain()` (#182)

* `tally()` gains `sort` argument to sort output so highest counts
  come first. (#173)

* `ungroup.grouped_df()`, `tbl_df()`, `as.data.frame()` no longer make deep
  copies of their input. (#191)

* `summarise` correctly propagate attributes. (#194)

* `group_by` correctly handles grouping by a factor that has NA. (#183)

