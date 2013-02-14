r2sstat
=======

Simple tool for reading data from R2S UMB device and displaying it in iostat
style:

```
        timestamp a.temp a.ltr/m ltr/m prec.type s/s sum of snow-flakes sum of goodcounts sum of badcounts 
13/02/14 17:44:54  -9.18    0.01  0.00      none   0                 10                 0              546 
13/02/14 17:44:58  -9.18    0.01  0.00      none   0                 10                 0              607 
13/02/14 17:45:01  -9.18    0.01  0.00      none   0                 11                 1              672 
13/02/14 17:45:05  -9.18    0.01  0.00      none   0                 11                 1              754 
13/02/14 17:45:09  -9.18    0.01  0.00      none   0                 11                 0               39 
13/02/14 17:45:12  -9.18    0.01  0.00      none   0                 11                 0               92 
13/02/14 17:45:16  -9.18    0.01  0.00      none   0                 12                 1              164
```

Installation
============

Just clone repository and run `rebar co es`. Binary named `r2sstat` should
appear in working directory.
