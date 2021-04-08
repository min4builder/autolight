# autolight

This is a basic comonadic implementation of an automatic lighting algorithm for
line art in Haskell.

To run:

    cabal run autolight <image> <output>

## How it works

There are four parameters that can be set for the algorithm, besides the input
image:

- The gaussian blur radius (`blurr`): how "blobby" the shadows look.
- The distance radius (`distr`): how much the shadows connect to lines.
- The distance weight (`distw`): adjustment. Values too high will create fake
  shadows around lines, values too low will make the shadows ignore them.
- The direction of the light (`lx`, `ly`)

First the image is converted to grayscale, for ease of computation. Then, the
image goes through a gaussian blur of `blurr` radius. The result is used as an
approximate heightmap of the image, on the assumption that, in line art, lines
usually indicate an inward fold.

Then, the directional derivative in the direction of the light is calculated.
The result is an approximate mapping of where the lights and shadows should go
in the image.

In order to convert these to hard shadows, a distance field from the lines is
calculated and clamped to `distr`. Then, it goes through the following
smoothing formula:

    m = (1 / (1 + e^(-6v/distr))) - 1

This formula has the property of mapping the regions close to the lines (so v
-> 0) to negative numbers, and the regions further away smoothly approach 0.
Then it is weighted by `distw`.

The approximate lightmap and the weighted distance function are then added
together. The effect is making the light slightly darker around the lines,
which, if the weight is set correctly, will give the effect of connecting the
shadows to the lines.

Finally, positive values are mapped to light while negative values are mapped
to shadow. The result is blurred slightly to avoid aliasing and for artistic
effect.

## When it doesn't work

If the background isn't black, it will be shaded as well.

If there are lines which represent outward protrusions, they will be
misinterpreted as holes. Noses and the outside of buildings are common examples.

Overlapping (or deep) geometry isn't handled very well. It might work for
clothing and hair, but for anything that's not relatively flat it will probably
fail.

The shading might still require some human intervention to look correct.

## Future improvement

Use a better heuristic to approximate the heightmap. Neural networks should be
a better tool for this job.

Use a smarter distance function that can consider thinner lines as well.

