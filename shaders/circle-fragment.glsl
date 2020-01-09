#version 400 core

in vec2 complexCoordinate;

out vec4 Color;

void main (void)
{
     vec2 pos = complexCoordinate;
     float dsquared = pos.x*pos.x + pos.y*pos.y;

     float radius = 1.0;
     float rsquared = radius*radius;

     vec4 circleColor = vec4(0.0, 1.0, 0.0, 1.0);
     vec4 outColor = vec4(0.25, 0.25, 0.25, 1.0);

     if (dsquared < rsquared) {
          Color = vec4(abs(pos.x), abs(pos.y), 0.0, 1.0);
     } else {
          Color = outColor;
     }
}
