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

vec4 bsJsColor(int maxIter, vec2 pos) {



     int iter;
     float tempreal, tempimag;
     float r2 = 0.0;

     float zx = pos.x;
     float ox = zx;

     float zy = -pos.y;
     float oy = zy;

     for (iter = 0; iter < maxIterations; iter++)
     {
          float xtemp = zx * zx - zy * zy + ox;
          zy = abs(2 * zx * zy + oy);
          zx = abs(xtemp);
          zx += cReal;
          zy += cImag;
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4(0.0, 0.0, 0.0, 1.0); // black
     }
     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     red = clamp(zy * zx * zx * (iter % 67)/66.0, 0.0, 1.0);
     green = clamp(zy * zy * (iter % 47)/46.0, 0.0, 1.0);
     blue = clamp(zx * zx * (iter % 24)/23.0, 0.0, 1.0);
     return vec4(red, green, blue, 1.0);
}

void main() {
     vec4 diffuseColor = bsJsColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
}
