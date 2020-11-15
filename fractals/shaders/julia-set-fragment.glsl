#version 400 core

in vec3 normal;
in vec3 position;

in vec2 complexCoordinate;
uniform float cReal;
uniform float cImag;

uniform mat4 transform;
uniform mat4 normalTransform;

out vec4 outColor;

const vec3 lightPos = vec3(vec4(0.0, -8.0, -8.0, 1.0));
const vec3 lightColor = vec3(1.0, 1.0, 1.0);
const float lightPower = 80.0;
const vec3 ambientColor = vec3(0.00, 0.00, 0.000);
const vec3 specColor = vec3(1.0, 1.0, 1.0);
const float shininess = 38.0;
const float screenGamma = 1.3; // Assume the monitor is calibrated to the sRGB color space

uniform int maxIterations;

vec4 juliaSetColor(int maxIter, vec2 pos) {
     int iter;
     float tempzx, tempzy;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

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
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 1.0);
     }

     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     tmpval = fract(iter / 8422.0);
     tmpval2 = fract(iter / 11133.0);

     red = clamp(zy * zx * zx * (iter % 67)/66.0, 0.0, 1.0);
     green = clamp(zy * zy * (iter % 47)/46.0, 0.0, 1.0);
     blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);

     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = juliaSetColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
