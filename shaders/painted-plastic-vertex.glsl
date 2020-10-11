#version 400 core

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inUV;

uniform mat4 transform;
uniform mat4 normalTransform;

out vec3 normal;
out vec3 position;
out vec2 uv;

void main()
{
    // Modified from
//https://www.mathematik.uni-marburg.de/~thormae/lectures/graphics1/code/WebGLShaderLightMat/ShaderLightMat.html
     vec4 pos4 = transform * vec4(inPosition, 1.0);
     gl_Position = pos4;

     uv = inUV;
     position = vec3(pos4);
     normal = normalize(mat3(normalTransform) * inNormal);
}
