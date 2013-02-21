democrify
=========

A Mac application for democratic handling of music at parties, using libspotify.

## Building

Building this project should be pretty straightforward.

1. Make sure you have CocoaLibSpotify.framework sitting in /Library/Frameworks

2. Get an appkey.c file from Spotify and place it in the folder with the other Democrify source files (don't add it to the Xcode project!)

3. Install the Haskell Platform (64 Bit, tested GHC version is 7.4.2), then `cd` into the democrify/Democrify directory and run `cabal install --only-dependencies`.
Do not try to build and install using `cabal`, it is only used for managing the Haskell dependencies!

4. You should then just be able to build & run in Xcode!

More detailed instructions will follow :)

## Important

Please note that this project contains a lot of Haskell code that is written in a way in which **no Haskell code should ever be written**. Please do **not** use this project as a guide on how to use Haskell! :D