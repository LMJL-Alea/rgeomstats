## Test environments

- local macOS R installation, R 4.2.2
- continuous integration via GH actions:
    - macOS latest release (Python 3.9)
    - windows latest release (Python 3.9)
    - linux ubuntu release (Python 3.7/3.8/3.9/3.10)
    - linux ubuntu devel (Python 3.9)
- win-builder (release and devel)
- macOS-builder
- R-hub
    - Windows Server 2022, R-devel, 64 bit
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There was no ERROR and no WARNINGs.

There were 3 NOTEs:

    * checking for detritus in the temp directory ... NOTE
    Found the following files/directories:
      'lastMiKTeXException'

This NOTE appears only on Windows.

    * checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    Skipping checking math rendering: package 'V8' unavailable

This NOTE appears only on Fedora.

    * checking examples ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
                               user system elapsed
    SpecialOrthogonal3Vectors 2.373  0.670  10.201
    LieGroup                  2.235  0.628   9.481
    SPDMatrices               2.173  0.607   9.312
    RiemannianMetric          1.842  0.476   7.802
    MatrixLieGroup            1.253  0.384   5.317
    Manifold                  1.264  0.353   5.305

This NOTE appears only on Ubuntu.
