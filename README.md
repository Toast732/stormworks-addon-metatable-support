# stormworks-addon-metatable-support
Allows you to use metatables within stormworks, made specifically for use with addons.

## WIP: VERY UNSTABLE
This script is still in heavy development, and fails in alot of cases
This will very likely break your outputted scripts
This will very likely not work as intended
This will very likely error

As of current, this script is not in a spot for use yet.

## How to use

### Users
If you are a user, then you don't need to do anything for this to work, this all is setup when the script is compiled, so it will not require you to do anything

### Developers
To use this script
- Copy LICENSE and Metatables.lua file into your addon's directory, under a subdirectory of SWAMS
- Require this script in ``_buildacions.lua`` and call it in ``onLBBuildFileComplete()``
