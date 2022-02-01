# REST-in-C

## HTTP Specification

1. <https://datatracker.ietf.org/doc/html/rfc7230>

## Haskell Pass gcc flags

1. <https://wiki.haskell.org/GHC/FAQ#How_can_I_make_GHC_always_use_some_extra_gcc_or_linker_option.3F>

## Virtual Memory 1TB?

1. https://wiki.haskell.org/GHC/Using_the_FFI

```shell
ghc-pkg describe rts > rts.package.conf
echo "cc-options: \"-g\"" >> rts.package.conf
sudo ghc-pkg update rts.package.conf
```
