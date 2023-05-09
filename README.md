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

Currently incomplete, due to the script not being easily flexible yet, once it's easier I'll update this.
To use this script
- Copy LICENSE and Metatables.lua file into your addon's directory, under a subdirectory of SWAMS
- Require this script in ``_buildacions.lua`` and call it in ``onLBBuildFileComplete()``

## FAQ

### Horrible Code?
This is still in heavy development, and is the first time I've been doing something like this, so sorry that the code is pretty bad. I've been pushing aside well written/performing code as alot of times I'm just trying out something to see if it works.

### Performance?
This will have a performance impact due to the fact that it's adding function calls, however it shouldn't be too much of a performance difference from normal metatables.

### Memory Usage Benifets?
This should give you the same memory usage Benifets as normal metatables. It assigns an id to each metatable and when metamethods are called, it will try to index a table with that id for the metamethod, so the metamethods are not duplicated for each metatable.

### Accuracy to Real Metatables
The behaviour of the metatables I test in normal lua, and then I replicate it's behaviour for this, so it should behave the exact same, however, if you notice any inaccuracies, please let me know.

### Differences from Real Metatables
The main difference from real Metatables is that it will add a value to your table which you make a metatable, __TEA_metatable_id, this is to ensure the metatable it uses will always stay with the table, otherwise there would be issues with reloading the script, and also with tracking the table.

## Known Issues
- Struggles with nested metatables
- Difficulty with metatables stored in tables with dynamic indexes (eg: arrays)
- adds __index metamethod to the function definitions
- treats negative numbers as __sub metamethod
- sometimes will insert improperly

## Features

### Current
- Ability to use metatables in SW
- Functions
-- setmetatable
- Metamethods
     - __add
     - __sub
     - __mul
     - __div
     - __mod
     - __index

