# A very simple Least-Squares-Regression library

Takes input in the form of

```haskell

lsr :: Int -> [(Double,Double)] -> [Double]

```

With the types (T -> U -> V) representing

* T: The order of the polynomial desired
* U: The data points
* V: The polynomial constants [A,B,C...] in the form of (A + Bx + Cx^2...)

## Example usage

```haskell

data = [(1.0, 1.0),
        (1.8, 4.3469161482595915),
        (2.6, 10.90017247569964),
        (3.4000000000000004, 21.31558678526116),
        (4.200000000000001, 36.151242302305484),
        (5.000000000000002, 55.90169943749479),
        (5.8000000000000025, 81.01584832611465),
        (6.600000000000003, 111.90766622533074),
        (7.400000000000004, 148.96330501167077),
        (8.200000000000005, 192.54605765894067),
        (9.000000000000005, 243.00000000000037),
        (9.800000000000006, 300.65275598271216)]

order = 3

result = lsr order data

```

## Enjoy!
