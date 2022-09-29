# SPDMatrices method cholesky_factor() works

    Code
      spdm$cholesky_factor(A)
    Output
      [1] 1.2247449 0.4082483 0.0000000 1.1547005 0.0000000 1.7320508

# SPDMatrices method differential_cholesky_factor() works

    Code
      spdm$differential_cholesky_factor(diag(1, 3), A)
    Output
                 [,1]      [,2]      [,3]
      [1,]  0.4082483 0.0000000 0.0000000
      [2,] -0.1360828 0.4811252 0.0000000
      [3,]  0.0000000 0.0000000 0.2886751

# SPDMatrices method differential_exp() works

    Code
      spdm$differential_exp(diag(1, 3), A)
    Output
               [,1]     [,2]     [,3]
      [1,] 5.053669 2.335387  0.00000
      [2,] 2.335387 5.053669  0.00000
      [3,] 0.000000 0.000000 20.08554

# SPDMatrices method differential_log() works

    Code
      spdm$differential_log(diag(1, 3), A)
    Output
            [,1]  [,2]      [,3]
      [1,]  0.75 -0.25 0.0000000
      [2,] -0.25  0.75 0.0000000
      [3,]  0.00  0.00 0.3333333

# SPDMatrices method differential_power() works

    Code
      spdm$differential_power(2, diag(1, 3), A)
    Output
           [,1] [,2] [,3]
      [1,]    3    1    0
      [2,]    1    3    0
      [3,]    0    0    6

