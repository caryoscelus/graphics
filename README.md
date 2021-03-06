[![Build Status](https://travis-ci.org/haskell-game/graphics.png)](https://travis-ci.org/haskell-game/graphics)

This is a 2D graphics API using a similar denotation to that of
[graphics-drawingcombinators](http://hackage.haskell.org/package/graphics-drawingcombinators),
just without the extra value. Colors are actually alpha blending
functions. Images are just colors over 2D space.

    [[ RGB     ]] = R * R * R
    [[ Color   ]] = {f : RGB -> RGB | exists (x : RGB) (a : [0,1]). forall c. f(c) = ax + (1-a)c}
    [[ Image c ]] = c*c -> Color

Most of the API falls out of these denotations. `Color` and `Image`
are both instances of `Monoid`. Affine transformations can be applied
to `Image`.
