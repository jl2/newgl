#version 400 core

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inUV;

uniform mat4 transform;
uniform mat4 normalTransform;

out vec3 position;
out vec2 complexCoordinate;

void main(void)
{
     vec4 pos4 = transform * vec4(inPosition, 1.0);
     gl_Position = pos4;

     position = vec3(pos4);
     complexCoordinate = inUV;
}
