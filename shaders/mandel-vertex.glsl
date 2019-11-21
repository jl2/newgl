#version 400 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec2 uv;

out vec2 complexCoordinate;

void main()
{
     gl_Position = vec4(position.x, position.y, position.z, 1.0);
     complexCoordinate = uv;
     // // gl_position = vec4 (position, 1.0);
    // vec4 transformedposition = transformationmatrix * vec4(position.xyz, 0.0);
    // vec4 l = vec4(normalize(vec3(transformedposition.xyz - vec3(1.0,1.0,-10.0))), 1.0);

    // color = color * max(0.5, dot(l, transpose(inverse(transformationmatrix)) * vec4(normal, 1.0)));
    // gl_PointSize = 1.0;
    // gl_Position = transformedPosition;
}
