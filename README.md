# Brian's Brain Battle

A multiplayer battle game based on the cellular automata, [Brian's Brain](http://en.wikipedia.org/wiki/Brian%27s_Brain)

## Compiling

    ghc -threaded -rtsopts -with-rtsopts="-N -qg0 -H64m" -O2 --make brain-battle

At the moment, both GD and the Haskell module, `Graphics.GD`, as well as SDL and `Graphics.UI.SDL` are required. In the future, there will be compile-time options to enable or disable these.

## Running

    ./brain-battle png    <source-file> <output-dir>
                   sdl    <source-file>
                   bbbout <source-file> <output-file>
                   null   <source-file>

Note that `sdl` does not do any kind of syncing, so it may run too quickly to see.

To run the 500x500 example with SDL output:

    ./brain-battle sdl Examples/500x500.bbbattle

To run 200x200 with PNG:

    mkdir out200
    ./brain-battle png Examples/200x200.bbbattle out200

PNG files will be stored as `out200/%04d.png`, that is, four digit generation numbers. These can be made into a video with `ffmpeg`, for example:

    ffmpeg -i "out200/%04d.png" -c:v libx264 -preset slow out200.mp4

## The bbbattle file format

Here is a dissection of the `Examples/Ship.bbbattle` file:

    100 100

This specifies the width and height of the field.

    0 128 255:a5,10 5,11

This specifes the color of the first team, `rgb(0,128,255)`, which is a mid-blue-cyan color. After the colon, the `a` designates the beginning of the list of "alive" cells.

               8,10 8,11
               10,9 10,12
               11,8 11,13
               12,7 12,14
               13,9 13,12
               14,8 14,13.

More "alive" cells. Cells are described as `x,y` and are separated by a non-zero amount of whitespace. A period (`.`) ends the list of alive cells.

              d6,10 6,11

The beginning of the "dying" cells section. This is formatted identically to the "alive" section, but with a `d` instead of an `a`.

               9,10 9,11
               11,9 11,12
               12,8 12,13
               13,7 13,14
               14,9 14,12.

The rest of the "dying" cells. The section is ended with a period as before.

    255 160 0:a30,30 30,31.
              d31,30 31,31.

Another team, with color `rgb(255,160,0)`, a shade of orange. Alive cells and dying cells are specified.

It is worth noting that the alive or dying sections may be empty, but are required to both be present. An example of this is `Examples/200x200.bbattle`, which consists of only two alive cells for each team. The dying section is still specified, as `d.`.

## The bbbout file format

Work in progress.
