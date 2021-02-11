#version 400 core

uniform mat4 view_transform;
uniform mat4 obj_transform;

uniform sampler2D image;

uniform float screen_gamma = 1.4;

in vec3 position;
in vec2 uv;

out vec4 out_color;

void main() {
     vec4 diffuse_color = texture(image, uv);

     // apply gamma correction (assume ambient_color, diffuseColor and spec_color
     // have been linearized, i.e. have no gamma correction in them)
     vec3 color_gamma_corrected = pow(diffuse_color.rgb, vec3(1.0 / screen_gamma));

     // use the gamma corrected color in the fragment
     out_color = vec4(color_gamma_corrected, diffuse_color.a);
}
