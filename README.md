# ST-Me

## Introduction

The ST-Me is a meteorological processor for surface and upper air stations data.

The ST-Me differs from other meteorological processors in the following aspects:

- It is not limited to "readily available meteorological data": in case data from an ultrasonic anemometer or SODAR(/RASS) is available, it is used directly.
- It is model-agnostic, in the sense it can write meteorological inputs suitable for use by some diffused dispersion models.
- It performs meteorological processing in "transparent mode", by documenting extensively the results from all gap-filling and estimation steps, and their impact on the statistical distribution of data.

## Deep vs Shallow Estimation

Conventional, van Ulden - Holtslag style meteorological processors proceed according to a _deep estimation_ method.

This scheme starts from a minimal set of "non-estimable data" including temperature, relative humidity and wind, and an elementary-level knowledge of terrain characteristics and position.

Then global solar radiation is estimated, if not measured.

After this, cloud cover and net radiation are estimated.

Then, turbulence parameters are estimated.

And last, mixing height is estimated.

Estimates are performed in chain mode, that is, at step 'n' the estimation is performed using step 'n-1' estimates instead of direct measurements.

Deep estimates are notoriously difficult to validate. Each estimation step adds its error contribution to the already accumulated.

The shallow estimation approach is, to some extent, the opposite of deep estimation: measure all what you can. Ideally, the only estimate really necessary in the shallow approach is mixing height, all other quantities being either directly measured or expressed through some simple, algebraic model (Monin-Obukhov Surface Layer similarity for example).

The shallow estimation approach differs in another way from the deep: data accuracy, especially as wind is concerned.As a by-product of deep estimation approach, measurements (of wind and other quantities) is performed through low-resolution, relatively innaccurate electro-mechanical sensors.

In the shallow approach an ultrasonic anemometer is used instead, with much higher resolution and accuracy.

So, the same quantity may contain quite different information (or, if you prefer, _entropy_) in the two approaches.

Generally speaking, so, the shallow estimation approach is preferable.

Most (let's say _all_) conventional meteorolgoical processors employ a deep estimation approach.

With ST-Me, the deep or shallow approach may be selected, depending on operational constraints (availability of a 3D ultrasonic anemomoeter) and the actual data quality.
