#pragma once
#include "common.h"

class fractal_t
{
public:
    fractal_t();
    ~fractal_t();

    void init_buffer();
    void init_vertex_array();
    void draw_frame(float time_from_start);

    void move(vec2 directions);
    void scale(int power);
private:
    vec2 pos_;
    float scale_factor_;
    GLuint vs_, fs_, program_;
    GLuint vx_buf_;
    GLuint vao_;

    vec2 const delta = vec2(0.1, 0.1);
    float const scale_step = 0.5;
};