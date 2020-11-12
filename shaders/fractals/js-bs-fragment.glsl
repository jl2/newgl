#version 400 core

uniform int maxIterations;
uniform float cReal;
uniform float cImag;

in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     int iter;
     // int maxIterations = 3200;
     float tempreal, tempimag, zx
     float r2 = 0.0;
     vec2 pos = complexCoordinate;
     float zx = pos.x;
     float zy = pos.y;
     for (iter = 0; iter < maxIterations; iter++)
     {
// z = z^2 + c
          
          xtemp = zx * zx - zy * zy + pos.x;
          zy = abs(2 * zx * zy + pos.y);
          zx = abs(xtemp);
          imag += cImag;
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
          // float tmpval = fract(iter / 8422.0);
          // float tmpval2 = fract(iter / 11133.0);
          // float red = tmpval2;
          // float green = sin(3 * (1.0 - tmpval));
          // float blue = tmpval;

          // float red = clamp(imag * real * (iter % 67)/66.0, 0.0, 1.0);
          // float green = clamp(imag * (iter % 47)/46.0, 0.0, 1.0);
          // float blue = clamp(real * real * (iter % 24)/23.0, 0.0, 1.0);

          // float red = clamp(imag * real * real * (iter % 67)/66.0, 0.0, 1.0);
          // float green = clamp(imag * imag * (iter % 47)/46.0, 0.0, 1.0);
          // float blue = clamp(real * real * (iter % 24)/23.0, 0.0, 1.0);

          // float red = clamp(imag * sin(3.1415*real) * (iter % 67)/66.0, 0.0, 1.0);
          // float green = clamp(real * cos(3.1415*imag) * (iter % 47)/46.0, 0.0, 1.0);
          // float blue = clamp(real * imag * (iter % 24)/23.0, 0.0, 1.0);

          // float red = clamp(cos(0.3*imag) * sin(0.7*real) + (iter % 7000)/6999.0, 0.0, 1.0);
          // float green = clamp(abs(sin(6*real) * cos(0.5*imag)) * (1.0 - (iter % 1000)/999.0), 0.0, 1.0);
          // float blue = clamp(abs(cos(4*real + imag)) * (1.0 - (iter % 240)/239.0), 0.0, 1.0);

          float red = fract(cos(0.25*imag) * sin(0.2*real) * (iter/299.0));
          float green = fract(sin((iter/999.0)* imag * real) + (iter/999.0));
          float blue = fract(cos(real * iter));

          // float pi = 3.141592654;
          // float fi = sin(0.5*(iter/24.0));
          // float red =   clamp(fi * abs(sin(pow(fi, pos.x))), 0.0, 1.0);
          // float green = clamp(fi * abs(sin(pow(fi, real))), 0.0, 1.0);
          // float blue =  clamp(fi * abs(cos(pow(fi, fi)) * sin(pow(real, fi))), 0.0, 1.0);

          // float pi = 3.141592654;
          // float fi = (0.5 + sin(pi * (iter/2400.0))) / 2.0;
          // float red =   clamp(pow((1.0 - fi), (real*imag)), 0.0, 1.0);
          // float green = clamp(pow(fi, abs(sin(fi+imag))), 0.0, 1.0);
          // float blue =  clamp(abs(tan(fi - sin(fi + real))), 0.0, 1.0);

          // float pi = 3.141592654;
          // float fi = (0.5 + sin(pi * (iter/2400.0))) / 2.0;
          // float red =   clamp(pow((1.0 - fi), (real*imag)), 0.0, 1.0);
          // float green = clamp(pow(fi, abs(sin(fi+imag))), 0.0, 1.0);
          // float blue =  clamp(abs(tan(fi - sin(fi + real))), 0.0, 1.0);

          // float pi = 3.141592654;
          // float fi = (0.85 + sin(pi * (iter/maxIterations))) / 2.0;
          // // float red =   clamp(abs(/(1.0+4*fi)), 0.0, 1.0);
          // // float green = clamp(, 0.0, 1.0);
          // // float blue =  clamp(, 0.0, 1.0);

          // float red = clamp(real * fi, 0.0, 1.0);
          // float green = clamp(abs(sin(imag * fi)), 0.0, 1.0) ;
          // float blue = clamp(abs(sin(real * imag * fi)), 0.0, 1.0);

          color = vec4(red, green, blue, 1.0);
     }
     Color = color;
     // Color = vec4(complexCoordinate.y, complexCoordinate.x, 0.0, 1.0);
}
