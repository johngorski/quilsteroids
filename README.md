# quilsteroids

An Asteroids clone written in Clojure with Quil.

## Usage

Far from a perfect clone, but a fun excuse to play around a bit.

Controls:
- Left/Right arrow keys rotate the ship
- Up arrow key thrusts forwards
- Shift shoots
- Escape exits

Expectations: Definitely not a full clone! In particular,
- No sound
- No score-keeping
- Infinite lives
- No UFOs
- No asteroid field refresh when the field is cleared
- No teleportation
- Asteroid art doesn't match

The list is not meant to be exhaustive.

Hope you have fun playing and fiddling with the code!

### Play!

Download, build, run!

1. Download: `git clone https://github.com/johngorski/quilsteroids.git`
2. Build (requires clojure):
   `cd quilsteroids`
   `clj T:build uber`
   Move `target/quilsteroids-*.jar` as-desired
3. Run: `java -jar target/quilsteroids-*.jar`

### Develop

Start nREPL: `clj -M:repl`

### Test

Run test suite: `clj -M:test`

## License

Copyright © 2024 John Gorski

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
