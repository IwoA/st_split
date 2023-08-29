# st_split
New version of the lwgeom::st_split() function

## The problem

There is a need to split road network with the grid.
The only function for this I'm aware of is lwgeom::st_split
This function is quite slow for dense grid (thousands of lines) and many roads (milions)

The challange is how to optimize it or make more 'smarter'