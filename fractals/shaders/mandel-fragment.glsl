#version 400 core

in vec3 position;
in vec2 complexCoordinate;

uniform int maxIterations;

out vec4 outColor;

vec4 mandelbrotColor(int maxIter, vec2 pos) {
     int iter;
     float tempzx, tempzy;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

     float ox = zx;
     float oy = zy;
     for (iter = 0; iter < maxIter; iter++)
     {
          tempzx = zx;
          tempzy = zy;
          zx = (tempzx * tempzx) - (tempzy * tempzy);
          zy = 2 * tempzx * tempzy;
          zx += ox;
          zy += oy;
          r2 = r2 * r2;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 1.0); // black
     }
     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     fi = (0.5 + sin(pi * (iter/200.0))) / 2.0;
     red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
     green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
     blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);

     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = mandelbrotColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
