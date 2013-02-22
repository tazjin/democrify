democrify
=========

A Mac application for democratic handling of music at parties, using libspotify. This is **not** an official Spotify project!

## Download

The current release is available at http://klaud.tazj.in/N6UA

Please file all issues you find here in Github or - if you don't have a Github account - send them to vincent@spotify.com

Have fun!

## Building

Building this project should be pretty straightforward.

1. Make sure you have CocoaLibSpotify.framework sitting in /Library/Frameworks

2. Get an appkey.c file from Spotify and place it in the folder with the other Democrify source files (don't add it to the Xcode project!)

3. Install the Haskell Platform (64 Bit, tested GHC version is 7.4.2), then `cd` into the democrify/Democrify directory and run `cabal install --only-dependencies`.
Do not try to build and install using `cabal`, it is only used for managing the Haskell dependencies!

4. You should then just be able to build & run in Xcode!

More detailed instructions will follow :)

## User guide

When you start Democrify it is initialized with an empty play queue. You can load new tracks into the queue either through the webinterface (host:8686) or through the playlist loading function (in the menubar on the host machine).

Everybody who is connected to the same network can then access the web interface at host:8686, an admin interface is available at /admin but it is only accessible from the host computer itself.

There are other functions like Skip, Pause and Empty Queue which are integrated as options in the menu bar icon's dropdown list.

## Important

Please note that this project contains a lot of Haskell code that is written in a way in which **no Haskell code should ever be written**. Please do **not** use this project as a guide on how to use Haskell! :D