#pragma once
#include "common.h"

struct face_t {
    int vtx_idx[3];
    int uv_idx[3];
    int norm_idx[3];
};

class model_t
{
public:
    model_t();
    ~model_t();

    void init_buffer();
    void init_vertex_array();
    void draw_frame(float time_from_start);

private:
    void load_model(std::string const & filename);
    void draw_model(float time_from_start, bool wired = false);
    void calc_parameters();

    bool show_wireframe;
    bool show_waves_;
    GLuint vs_norm_, fs_norm_, program_norm_;
    GLuint vs_wave_, fs_wave_, program_wave_;
    GLuint vx_buf_, norm_buf_;
    GLuint vao_norm_, vao_wave_;
    quat   rotation_by_control_;
    float v_, k_;

    std::vector<vec3> vertices;
    std::vector<vec3> normals;
    std::vector<vec2> texture_uv;
    std::vector<face_t> faces;
    vec3 center_;
    float max_dist_;
};

