#version 400 core

uniform int maxIterations;
uniform float cReal;
uniform float cImag;
uniform float aspectRatio;

in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     int iter;
     // int maxIterations = 3200;
     float tempzx, tempzy, Creal, Cimag;
     float r2 = 0.0;
     vec2 pos = complexCoordinate;
     float zx;
     float zy;

     if (aspectRatio > 0) {
          zx = pos.x * aspectRatio;
          zy = pos.y;
     } else {
          zx = pos.x ;
          zy = -1 * pos.y * aspectRatio;
     }

     Creal = zx;
     Cimag = zy;
     for (iter = 0; iter < maxIterations; iter++)
     {
// z = z^2 + c
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += cReal;
          zy += cImag;
          r2 = (zx * zx) + (zy * zy);
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
          float tmpval, tmpval2, red, green, blue, pi, fi;
          pi = 3.141592654;

          tmpval = fract(iter / 8422.0);
          tmpval2 = fract(iter / 11133.0);
          red = tmpval2;
          green = sin(3 * (1.0 - tmpval));
          blue = tmpval;

          // red = clamp(zy * zx * (iter % 67)/66.0, 0.0, 1.0);
          // green = clamp(zy * (iter % 47)/46.0, 0.0, 1.0);
          // blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);

          red = clamp(zy * zx * zx * (iter % 67)/66.0, 0.0, 1.0);
          green = clamp(zy * zy * (iter % 47)/46.0, 0.0, 1.0);
          blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);

          // red = clamp(zy * sin(3.1415*zx) * (iter % 67)/66.0, 0.0, 1.0);
          // green = clamp(zx * cos(3.1415*zy) * (iter % 47)/46.0, 0.0, 1.0);
          // blue = clamp(zx * zy * (iter % 24)/23.0, 0.0, 1.0);

          // red = clamp(cos(0.3*zy) * sin(0.7*zx) + (iter % 7000)/6999.0, 0.0, 1.0);
          // green = clamp(abs(sin(6*zx) * cos(0.5*zy)) * (1.0 - (iter % 1000)/999.0), 0.0, 1.0);
          // blue = clamp(abs(cos(4*zx + zy)) * (1.0 - (iter % 240)/239.0), 0.0, 1.0);

          // red = fract(cos(0.25*zy) * sin(0.2*zx) * (iter/299.0));
          // green = fract(sin((iter/999.0)* zy * zx) + (iter/999.0));
          blue = fract(cos(zx * iter));

          // pi = 3.141592654;
          // fi = sin(0.5*(iter/24.0));
          // red =   clamp(fi * abs(sin(pow(fi, pos.x))), 0.0, 1.0);
          // green = clamp(fi * abs(sin(pow(fi, zx))), 0.0, 1.0);
          // blue =  clamp(fi * abs(cos(pow(fi, fi)) * sin(pow(zx, fi))), 0.0, 1.0);

          // pi = 3.141592654;
          // fi = (0.5 + sin(pi * (iter/2400.0))) / 2.0;
          // red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
          // green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
          // blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);

          // pi = 3.141592654;
          // fi = (0.5 + sin(pi * (iter/2400.0))) / 2.0;
          // red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
          // green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
          // blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);

          // pi = 3.141592654;
          // fi = (0.85 + sin(pi * (iter/maxIterations))) / 2.0;
          // // red =   clamp(abs(/(1.0+4*fi)), 0.0, 1.0);
          // // green = clamp(, 0.0, 1.0);
          // // blue =  clamp(, 0.0, 1.0);

          // red = clamp(zx * fi, 0.0, 1.0);
          // green = clamp(abs(sin(zy * fi)), 0.0, 1.0) ;
          // blue = clamp(abs(sin(zx * zy * fi)), 0.0, 1.0);

          color = vec4(red, green, blue, 1.0);
     }
     Color = color;
     // Color = vec4(complexCoordinate.y, complexCoordinate.x, 0.0, 1.0);
}
