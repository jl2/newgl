#version 400 core

in vec3 normal;
in vec3 position;

out vec4 outColor;

uniform mat4 transform;
uniform mat4 normalTransform;

uniform int mode = 1;
in vec4 diffuseColor;

const vec3 lightColor = vec3(1.0, 1.0, 1.0);
const float lightPower = 40.0;
const vec3 ambientColor = vec3(0.00, 0.01, 0.000);
const vec3 specColor = vec3(1.0, 1.0, 1.0);
const float shininess = 65.0;
const float screenGamma = 1.3; // ssume the monitor is calibrated to the sRGB color space

void main() {

     // vec3 real_normal = normalize(normal);
     vec3 lightPos = vec3(transform * vec4(0.0, 10.0, -10.0, 1.0));
     vec3 lightDir = lightPos - position;
     float distance = length(lightDir);
     distance = distance * distance;
     lightDir = normalize(lightDir);

     float lambertian = max(dot(lightDir, normal), 0.0);
     float specular = 0.0;

     if (lambertian > 0.0) {

          vec3 viewDir = normalize(-position);

          // this is blinn phong
          vec3 halfDir = normalize(lightDir + viewDir);
          float specAngle = max(dot(halfDir, normal), 0.0);
          specular = pow(specAngle, shininess);

          // this is phong (for comparison)
          if (mode == 2) {
               vec3 reflectDir = reflect(-lightDir, normal);
               specAngle = max(dot(reflectDir, viewDir), 0.0);
               // note that the exponent is different here
               specular = pow(specAngle, shininess/4.0);
          }
     }
     vec3 colorLinear = ambientColor +
          diffuseColor.rgb * lambertian * lightColor * lightPower / distance +
          specColor * specular * lightColor * lightPower / distance;
     // apply gamma correction (assume ambientColor, diffuseColor and specColor
     // have been linearized, i.e. have no gamma correction in them)
     vec3 colorGammaCorrected = pow(colorLinear, vec3(1.0 / screenGamma));
     // use the gamma corrected color in the fragment
     outColor = vec4(colorGammaCorrected, 1.0);
}
