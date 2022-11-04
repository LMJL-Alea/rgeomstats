## Resubmission

There is a resubmission. In this version, I have:

- Updated the `DESCRIPTION` file according to CRAN request: I have omitted the
redundant "An R" from title and description and single quoted Geomstats in the
title.

I was also asked to add a few more small executable examples in Rd-files to
illustrate the use of the exported function but also enable automatic testing.

All R6 class methods are actually documented and have unit tests implemented.
However they all rely on the **geomstats** Python package to be found on the
system or installed by the user, as the submitted R package is a wrapper around
the python package. As such I always need to check if **geomstats** can be found
before running any example or test. This results in examples and tests being all
disabled upon CRAN checks.

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
