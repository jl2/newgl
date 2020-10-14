#version 400 core

in vec3 normal;
in vec3 position;
in vec2 uv;

const vec3 lightPos = vec3(5.0, 5.0, -5.0);

const vec3 ambientColor = vec3(0.1, 0.1, 0.1);
const vec3 specColor = vec3(1.0, 1.0, 1.0);

uniform sampler2D image;

out vec4 outColor;

void main()
{

     vec4 diffuseColor = texture(image, uv);

     vec3 lightDir = normalize(lightPos - position);
     vec3 reflectDir = reflect(-lightDir, normal);
     vec3 viewDir = normalize(-position);

     float lambertian = max(dot(lightDir,normal), 0.0);
     float specular = 0.0;
     if (lambertian > 0.0) {
          float specAngle = max(dot(reflectDir, viewDir), 0.0);
          specular = pow(specAngle, 8.0);
     }
   outColor = vec4(ambientColor +
                   lambertian*vec3(diffuseColor) +
                   specular*specColor,
                   diffuseColor.w);
}
