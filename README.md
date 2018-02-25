# smoothfractal
Smoothed Mandelbrot in Haskell

Inspired by Inigo Quilez: http://iquilezles.org/www/articles/mset_smooth/mset_smooth.htm

## Installation and Usage
Install haskell stack, cd into directory ```stack install```

This builds two binaries, ```smoothfractal-exe``` and ```smoothfractal-child-exe```

When running ```smoothfractal-exe``` you may pass in command line functions of variables z and c with the ```-F``` flag like: ```smoothfractal-exe -F "z^2+c"```. This executable writes a file ```$REPO_DIR/app/auxiliary/Func.hs``` with the function ```z^2+c```, compiles and runs ```smoothfractal-child-exe``` which using Repa for multicore, renders the function to a file.

To do smoothing requires the degree of the polynomial used, ```smoothfractal-child-exe``` uses overloading to determine the polynomial degree of the passed in function. Due to this, if you use constants in the function, you have to add them in with the ```-C``` flag like: ```smoothfractal-exe -F "z^2 + c + c1" -C "[('c1','(-0.2):+(-0.2))]"```

See ```smoothfractal-exe -h``` for flags

## Example 1

![alt text](https://github.com/fabricatedmath/smoothfractal/blob/master/examples/example.jpg "Example")

Example generated with

```smoothfractal-exe -A 2 -R 1024 -f example2.bmp -F "z^7 + 4*z^2 + 4*c^8 + 2*c1" -C "[('c1','(-2.7829999999999966e-2):+(-0.1)')]"```

## Example 2

![alt text](https://github.com/fabricatedmath/smoothfractal/blob/master/examples/example2.jpg "Example2")

Example generated with 

```smoothfractal-exe -A 2 -R 1024 -f example.bmp -F "c2*(z^7 + (3-c)*z^3 + (c + c1)*z + c)" -C "[('c1','(0.01):+1'),('c2','0.75:+0.75']"```