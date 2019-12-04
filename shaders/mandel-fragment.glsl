#version 400 core

// uniform int maxIterations;
in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     float tmpval;
     float tmpval2;
     int iter;
     int maxIterations = 2400;
     float tempreal, tempimag, Creal, Cimag;
     float r2;
     vec2 pos = complexCoordinate;
     float real = pos.x;
     float imag = pos.y;
     Creal = real;
     Cimag = imag;
     for (iter = 0; iter < maxIterations; iter++)
     {
// z = z^2 + c
          tempreal = real;
          tempimag = imag;
          real = (tempreal * tempreal) - (tempimag * tempimag);
          imag = 2 * tempreal * tempimag;
          real += Creal;
          imag += Cimag;
          r2 = (real * real) + (imag * imag);
          if (r2 >= 4)
               break;
     }
// Base the color on the number of iterations
     vec4 color;
     if (r2 < 4) {
          color = vec4 (0.0, 0.0, 0.0, 1.0); // black
     }
     else
     {
          // tmpval = fract(iter / 8422.0);
          // tmpval2 = fract(iter / 11133.0);
          // color = vec4 (tmpval2, sin(3.1415 * (1.0 - tmpval)), tmpval, 1.0);
          float red = clamp(imag * real * (iter % 5)/4.0, 0.0, 1.0);
          float green = clamp(imag * (iter % 3)/2.0, 0.0, 1.0);
          float blue = clamp(real * real * (iter % 4)/3.0, 0.0, 1.0);
          color = vec4(red, green, blue, 1.0);
     }
     Color = color;
     // Color = vec4(complexCoordinate.y, complexCoordinate.x, 0.0, 1.0);
}
