# sparse_numeric class

An S4 sparse vector storing only non-zero entries.

## Slots

- `value`:

  numeric non-zero values (may include NA/NaN from operations)

- `pos`:

  integer positions of values (1-based, strictly increasing)

- `length`:

  integer total vector length
