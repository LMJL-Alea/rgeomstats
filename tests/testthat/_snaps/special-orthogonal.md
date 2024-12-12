# SpecialOrthogonalMatrices method intrinsic_to_extrinsic_coords() works

    Code
      so3$intrinsic_to_extrinsic_coords(Id)
    Condition
      Error in `py_call_impl()`:
      ! NotImplementedError: intrinsic_to_extrinsic_coords is not implemented.
      Run `reticulate::py_last_error()` for details.

# SpecialOrthogonalMatrices method extrinsic_to_intrinsic_coords() works

    Code
      so3$extrinsic_to_intrinsic_coords(Id)
    Condition
      Error in `py_call_impl()`:
      ! NotImplementedError: extrinsic_to_intrinsic_coords is not implemented.
      Run `reticulate::py_last_error()` for details.

# SpecialOrthogonal2Vectors method random_uniform() works

    Code
      so2$random_uniform()
    Output
      [1] -1.93824

# SpecialOrthogonal3Vectors method random_uniform() works

    Code
      so3$random_uniform()
    Output
      [1]  0.2442175 -0.1245445  0.5707172

