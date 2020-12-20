#version 400 core

uniform mat4 view_transform;
uniform mat4 obj_transform;

in vec3 normal;
in vec3 position;
in vec4 diffuse_color;

out vec4 out_color;

const vec3 light_pos = vec3(vec4(0.0, 8.0, 0.0, 1.0));
const vec3 light_color = vec3(1.0, 1.0, 1.0);
const float light_power = 40.0;
const vec3 ambient_color = vec3(0.00, 0.01, 0.000);
const vec3 spec_color = vec3(1.0, 1.0, 1.0);
const float shininess = 128.0;
const float screen_gamma = 1.3; // ssume the monitor is calibrated to the sRGB color space

void main() {

     vec3 light_dir = light_pos - position;
     float distance = length(light_dir);
     distance = distance * distance;
     light_dir = normalize(light_dir);

     float lambertian = max(dot(light_dir,
                                normal),
                            0.0);
     float specular = 0.0;

     if (lambertian > 0.0) {

          vec3 view_dir = normalize(-position);

          // Blinn-Phong
          vec3 half_dir = normalize(light_dir - view_dir);
          float spec_angle = max(dot(half_dir, normal), 0.0);
          specular = pow(spec_angle, shininess);

     }
     vec3 color_linear = ambient_color +
          diffuse_color.rgb * lambertian * light_color * light_power / distance +
          spec_color * specular * light_color * light_power / distance;
     // apply gamma correction (assume ambient_color, diffuseColor and spec_color
     // have been linearized, i.e. have no gamma correction in them)
     vec3 color_gamma_corrected = pow(color_linear, vec3(1.0 / screen_gamma));
     // use the gamma corrected color in the fragment
     out_color = vec4(color_gamma_corrected, diffuse_color.a);
}
