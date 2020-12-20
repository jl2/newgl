#version 400 core

in vec3 diffuse_color;

out vec4 out_color;

void main()
{
    out_color = diffuse_color;
}
