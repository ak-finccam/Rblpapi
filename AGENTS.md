# Rblpapi, finccam fork

The branch `feature/bql-finccam` is finccam's fork of Rblpapi. It adds `bql()`.

## Raise the version with every change to the package

Every push to `feature/bql-finccam` starts the workflow `.github/workflows/build-binaries.yml`.
The workflow publishes the package to `ak-finccam/r-packages`. It publishes each version only
once. So a change to the package reaches users only when it also raises `Version` in
`DESCRIPTION`. Raise the last part of the version, such as `0.3.16.9004` to `0.3.16.9005`. A
change to only the workflow or to this file needs no new version.

Never replace a published tarball in `ak-finccam/r-packages`. The nix setup of `finccam-engine`
stores the hash of each source tarball. A rebuilt tarball has a new `Packaged` date in its
`DESCRIPTION`, so its hash changes and the engine's dev shell no longer builds.
