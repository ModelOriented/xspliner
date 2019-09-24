## Test environments
* local check
  Ubuntu 18.10, R 3.6.1
* win-builder
  R version 3.5.3 (2019-03-11)
  R version 3.6.1 (2019-07-05)
* rhub check
  Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  Fedora Linux, R-devel, clang, gfortran

## `R CMD check xspliner_0.0.4.tar.gz --as-cran` results

```
Status: OK
```

## win-builder result

```
* using log directory 'd:/RCompile/CRANguest/R-oldrelease/xspliner.Rcheck'
* using R version 3.5.3 (2019-03-11)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* DONE
Status: OK
```

```
* using log directory 'd:/RCompile/CRANguest/R-release/xspliner.Rcheck'
* using R version 3.6.1 (2019-07-05)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* DONE
Status: OK
```

## rhub check result
```
+R-HUB-R-HUB-R-HUBdocker run --user docker --rm rhub/fedora-clang-devel bash -c echo $RHUB_PLATFORM
+R-HUB-R-HUB-R-HUBexport RHUB_PLATFORM=linux-x86_64-fedora-clang
...
{"status":"ok"}
Finished: SUCCESS
```

```
Started by user r-hub Jenkins admin
Building remotely on windows-server3-cdf9107c
...
{"status":"ok"}
Finished: SUCCESS
```
