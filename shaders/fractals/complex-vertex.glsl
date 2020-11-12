#version 400 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;

out vec2 complexCoordinate;

void main(void)
{
     gl_Position = vec4(position.x, position.y, position.z, 1.0);
     complexCoordinate = uv;
}
