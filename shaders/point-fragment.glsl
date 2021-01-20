#version 400 core

uniform mat4 view_transform;
uniform mat4 obj_transform;

in vec3 normal;
in vec3 position;
in vec4 diffuse_color;

out vec4 out_color;

void main()
{
    out_color = diffuse_color;
}
