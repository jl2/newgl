#version 400 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec3 normal;
layout(location = 2) in vec4 color;

uniform mat4 transform;

out vec4 Color;

void main()
{
     // gl_Position = vec4(position, 1.0);
     gl_Position = transform * vec4(position, 1.0);
     Color = color;
}
