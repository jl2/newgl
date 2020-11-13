#version 400 core

in vec3 normal;
in vec3 position;

in vec2 complexCoordinate;

uniform mat4 transform;
uniform mat4 normalTransform;

uniform int mode = 2;

out vec4 outColor;

const vec3 lightPos = vec3(vec4(0.0, -8.0, -8.0, 1.0));
const vec3 lightColor = vec3(1.0, 1.0, 1.0);
const float lightPower = 80.0;
const vec3 ambientColor = vec3(0.00, 0.00, 0.000);
const vec3 specColor = vec3(1.0, 1.0, 1.0);
const float shininess = 38.0;
const float screenGamma = 1.3; // ssume the monitor is calibrated to the sRGB color space

uniform int maxIterations;

vec4 burningShipColor(int maxIter, vec2 pos) {
     int iter;
     float tempreal, tempimag;
     float r2 = 0.0;

     float zx = pos.x;
     float zy = pos.y;

     float ox = zx;
     float oy = zy;
     
     for (iter = 0; iter < maxIterations; iter++)
     {
          float xtemp = zx * zx - zy * zy + ox;
          zy = abs(2 * zx * zy - oy);
          zx = abs(xtemp);
          r2 = (zx * zx) + (zy * zy);
          if (r2 >= 4)
               break;
     }
     if (r2 < 4) {
          return vec4 (0.0, 0.0, 0.0, 1.0); // black
     }
     float tmpval, tmpval2, red, green, blue, pi, fi;
     pi = 3.141592654;

     // tmpval = fract(iter / 8422.0);
     // tmpval2 = fract(iter / 11133.0);
     // red = tmpval2;
     // green = sin(3 * (1.0 - tmpval));
     // blue = tmpval;

     fi = (0.5 + sin(pi * (iter/200.0))) / 2.0;
     red =   clamp(pow((1.0 - fi), (zx*zy)), 0.0, 1.0);
     green = clamp(pow(fi, abs(sin(fi+zy))), 0.0, 1.0);
     blue =  clamp(abs(tan(fi - sin(fi + zx))), 0.0, 1.0);

          return vec4(red, green, blue, 1.0);
}
void main() {
     vec4 diffuseColor = burningShipColor(maxIterations, complexCoordinate);
     outColor = diffuseColor;
     //      outColor = diffuseColor;
     // } else {
     //      vec3 lightDir = lightPos - position;
     //      float distance = length(lightDir);
     //      distance = distance * distance;
     //      lightDir = normalize(lightDir);

     //      float lambertian = max(dot(lightDir, normal), 0.0);
     //      float specular = 0.0;

     //      if (lambertian > 0.0) {

     //           vec3 viewDir = normalize(-position);

     //           // this is blinn phong
     //           vec3 halfDir = normalize(lightDir - viewDir);
     //           float specAngle = max(dot(halfDir, normal), 0.0);
     //           specular = pow(specAngle, shininess);

     //           // this is phong (for comparison)
     //           if (mode == 3) {
     //                vec3 reflectDir = reflect(-lightDir, normal);
     //                specAngle = max(dot(reflectDir, viewDir), 0.0);
     //                // note that the exponent is different here
     //                specular = pow(specAngle, shininess/4.0);
     //           }
     //      }
     //      vec3 colorLinear = ambientColor +
     //           diffuseColor.rgb * lambertian * lightColor * lightPower / distance +
     //           specColor * specular * lightColor * lightPower / distance;
     //      // apply gamma correction (assume ambientColor, diffuseColor and specColor
     //      // have been linearized, i.e. have no gamma correction in them)
     //      vec3 colorGammaCorrected = pow(colorLinear, vec3(1.0 / screenGamma));
     //      // use the gamma corrected color in the fragment
     //      outColor = vec4(colorGammaCorrected, 1.0);
     // }
}
