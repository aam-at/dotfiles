// Grayscale screen for wellbeing's bedtime on Hyprland:
//   hyprctl keyword decoration:screen_shader <this file>
#version 300 es
precision highp float;

in vec2 v_texcoord;
uniform sampler2D tex;
out vec4 fragColor;

void main() {
    vec4 color = texture(tex, v_texcoord);
    float luminance = dot(color.rgb, vec3(0.3, 0.6, 0.1));
    fragColor = vec4(vec3(luminance), color.a);
}
