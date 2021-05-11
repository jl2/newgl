#version 410 core

layout(location = 0) in vec2 in_uv;
layout(location = 1) in vec4 in_color;
layout(location = 2) in vec2 in_radi;
layout(location = 3) in mat4 obj_transform;

uniform mat4 view_transform;

out vec2 uv;
out vec3 normal;
out vec3 position;
out vec4 diffuse_color;

void main()
{
     float inner = in_radi.x;
     float outer = in_radi.y;
     float sv = in_uv.s;
     float tv = in_uv.t;

     mat4 final_transform = view_transform * obj_transform;
     mat3 norm_view_transform = transpose(inverse(mat3(obj_transform)));
     float rcost = outer + inner * cos(sv);
     vec4 pos = final_transform * vec4(rcost * cos(tv),
                                       rcost * sin(tv),
                                       inner * cos(sv), 1.0);
     diffuse_color = in_color;
     position = pos.xyz;
     gl_Position = pos;
     normal = normalize(norm_view_transform * -pos.xyz);
     uv = in_uv;
}
