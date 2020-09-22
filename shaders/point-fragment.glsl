#version 400 core

in vec4 Color;
out vec4 outColor;

void main()
{
    outColor = vec4(Color.brg, 0.25);
}
