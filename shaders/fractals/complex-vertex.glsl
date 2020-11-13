#version 400 core

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;

uniform mat4 transform;
uniform mat4 normalTransform;
uniform int mode=1;
out vec3 normal;
out vec3 position;
out vec2 complexCoordinate;

void main(void)
{
     vec4 pos4 = transform * vec4(inPosition, 1.0);
     gl_Position = pos4;

     position = vec3(pos4);
     normal = normalize(normalTransform * vec4(inNormal, 0.0)).xyz;
     complexCoordinate = inUV;
}
