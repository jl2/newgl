#version 400 core

uniform int maxIterations;

in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     int iter;
//      int maxIterations = maxIters;
     float tempzx, tempzy, Creal, Cimag;
     float r2 = 0.0;
     vec2 pos = complexCoordinate;

     float zx = pos.x;
     float zy = pos.y;

     Creal = zx;
     Cimag = zy;
     float ox = zx;
     float oy = zy;
     for (iter = 0; iter < maxIterations; iter++)
     {
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += Creal;
          zy += Cimag;
          r2 = r2 * r2;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     vec4 color;
     if (r2 < 4) {
          color = vec4 (0.0, 0.0, 0.0, 1.0); // black
     }
     else
     {
          float tmpval, tmpval2, red, green, blue, pi, fi;
          pi = 3.141592654;

          tmpval = fract(iter / 8422.0);
          tmpval2 = fract(iter / 11133.0);
          red = tmpval2;
          green = (1.0 - tmpval);
          blue = tmpval;

          color = vec4(red, green, blue, 1.0);
     }
     Color = color;
}
