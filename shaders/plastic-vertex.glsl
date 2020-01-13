#version 400 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 transform;
uniform mat3 normalTransform;
uniform int mode;


out vec4 Color;

const vec3 lightPos = vec3(1.0, 1.0, 1.0);
const vec3 specColor = vec3(1.0, 1.0, 1.0);



void main()
{
    // Modified from
//https://www.mathematik.uni-marburg.de/~thormae/lectures/graphics1/code/WebGLShaderLightMat/ShaderLightMat.html
     vec4 vertPos4 = transform * vec4(position, 1.0);
     gl_Position = vertPos4;

     // all following gemetric computations are performed in the
     // camera coordinate system (aka eye coordinates)
     vec3 normal = normalTransform * normal;
     vec3 vertPos = vec3(vertPos4) / vertPos4.w;
     vec3 lightDir = normalize(lightPos - vertPos);
     vec3 reflectDir = reflect(-lightDir, normal);
     vec3 viewDir = normalize(-vertPos);

     float lambertian = max(dot(lightDir,normal), 0.0);
     float specular = 0.0;
     if (lambertian > 0.0) {
          float specAngle = max(dot(reflectDir, viewDir), 0.0);
          specular = pow(specAngle, 4.0);

          // the exponent controls the shininess (try mode 2)
          if(mode == 2) {
               specular = pow(specAngle, 16.0);
          }

          // according to the rendering equation we would need to multiply
          // with the the "lambertian", but this has little visual effect
          if (mode == 3) {
               specular *= lambertian;
          }
          // switch to mode 4 to turn off the specular component
          if (mode == 4) {
               specular *= 0.0;
          }
     }

     Color = lambertian*color + vec4(specular*specColor, 1.0);
}
