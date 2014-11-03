#include "model.h"
#include "shader.h"

#include <algorithm>
#include <sstream>

static void TW_CALL toggle_fullscreen_callback( void * )
{
   glutFullScreenToggle();
}

model_t::model_t()
    : show_wireframe(false), show_waves_(false), v_(-1), k_(20)
{
#ifdef USE_CORE_OPENGL
   TwInit(TW_OPENGL_CORE, NULL);
#else
   TwInit(TW_OPENGL, NULL);
#endif
   // Определение "контролов" GUI
   TwBar *bar = TwNewBar("Parameters");
   TwDefine(" Parameters size='400 200' color='70 100 120' valueswidth=200 iconpos=topleft");

   TwAddButton(bar, "Fullscreen toggle", toggle_fullscreen_callback, NULL,
               " label='Toggle fullscreen mode' key=f");
   
   TwAddVarRW(bar, "Show wireframe", TW_TYPE_BOOLCPP, &show_wireframe, " true='ON' false='OFF' key=w");
   TwAddVarRW(bar, "Show waves", TW_TYPE_BOOLCPP, &show_waves_, " true='ON' false='OFF' key=s");

   TwAddVarRW(bar, "v", TW_TYPE_FLOAT, &v_, " label='v coef'");
   TwAddVarRW(bar, "k", TW_TYPE_FLOAT, &k_, " label='k coef'");

   TwAddVarRW(bar, "ObjRotation", TW_TYPE_QUAT4F, &rotation_by_control_,
              " label='Object orientation' opened=true help='Change the object orientation.' ");

   // Создание шейдеров
   vs_norm_ = create_shader(GL_VERTEX_SHADER  , "shaders//model.glslvs");
   fs_norm_ = create_shader(GL_FRAGMENT_SHADER, "shaders//model.glslfs");
   vs_wave_ = create_shader(GL_VERTEX_SHADER, "shaders//model_wave_color.glslvs");
   fs_wave_ = create_shader(GL_FRAGMENT_SHADER, "shaders//model_wave_color.glslfs");
   // Создание программы путём линковки шейдерова
   program_norm_ = create_program(vs_norm_, fs_norm_);
   program_wave_ = create_program(vs_wave_, fs_wave_);

   load_model("model.obj");
   calc_parameters();
   // Создание буфера с вершинными данными
   init_buffer();
   // Создание VAO
   init_vertex_array();
}

model_t::~model_t()
{
   // Удаление ресурсов OpenGL
   glDeleteProgram(program_norm_);
   glDeleteProgram(program_wave_);
   glDeleteVertexArrays(1, &vao_norm_);
   glDeleteVertexArrays(1, &vao_wave_);
   glDeleteBuffers(1, &vx_buf_);
   glDeleteBuffers(1, &norm_buf_);

   for (GLuint shader : {vs_norm_, vs_wave_, fs_norm_, fs_wave_}) {
       glDeleteShader(shader);
   }

   TwDeleteAllBars();
   TwTerminate();
}

void model_t::calc_parameters() {
    center_ = vec3(0, 0, 0);
    for (vec3 const & vtx : vertices) {
        center_ += vtx;
    }
    center_ /= vertices.size();
    
    max_dist_ = -1;
    for (vec3 const & vtx : vertices) {
        max_dist_ = std::max(max_dist_, glm::length(vtx - center_));
    }
}

void model_t::init_buffer()
{
    std::vector<vec3> triangles;
    std::vector<vec3> norms;
    for (face_t face : faces) {
        for (int i = 0; i < 3; ++i) {
            triangles.push_back(vertices[face.vtx_idx[i] - 1]);
            norms.push_back(normals[face.norm_idx[i] - 1]);
        }
    }

    glGenBuffers(1, &vx_buf_);
    glGenBuffers(1, &norm_buf_);

    glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vec3) * triangles.size(), &triangles[0], GL_STATIC_DRAW);
    
    glBindBuffer(GL_ARRAY_BUFFER, norm_buf_);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vec3) * norms.size(), &norms[0], GL_STATIC_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void model_t::init_vertex_array()
{
    glGenVertexArrays(1, &vao_norm_);
    glBindVertexArray(vao_norm_);
        glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);
        GLuint const pos_location = glGetAttribLocation(program_norm_, "in_pos");
        glVertexAttribPointer(pos_location, 3, GL_FLOAT, GL_FALSE, sizeof(vec3), 0);
        glEnableVertexAttribArray(pos_location);


        glBindBuffer(GL_ARRAY_BUFFER, norm_buf_);
        GLuint const norm_location = glGetAttribLocation(program_norm_, "in_norm");
        glVertexAttribPointer(norm_location, 3, GL_FLOAT, GL_FALSE, sizeof(vec3), 0);
        glEnableVertexAttribArray(norm_location);
    
    glGenVertexArrays(1, &vao_wave_);
    glBindVertexArray(vao_wave_);
        glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);
        GLuint const pos_wave_location = glGetAttribLocation(program_wave_, "in_pos");
        glVertexAttribPointer(pos_wave_location, 3, GL_FLOAT, GL_FALSE, sizeof(vec3), 0);
        glEnableVertexAttribArray(pos_wave_location);

    glBindVertexArray(0);
};

void model_t::draw_frame(float time_from_start) {
    draw_model(time_from_start, false);

    if (show_wireframe) {
        draw_model(time_from_start, true);
    }
}

void model_t::draw_model(float time_from_start, bool wired) {
    float const w = (float)glutGet(GLUT_WINDOW_WIDTH);
    float const h = (float)glutGet(GLUT_WINDOW_HEIGHT);

    mat4  const proj = perspective(45.0f, w / h, 0.1f, 100.0f);
    mat4  const view = lookAt(vec3(0, 0, 15), vec3(0, 0, 0), vec3(0, 1, 0));
    mat4 const rotation = mat4_cast(rotation_by_control_);
    mat4  const model = rotation;
    mat4  const mvp = proj * view * model;

    if (wired) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
        glEnable(GL_POLYGON_OFFSET_LINE);
        glPolygonOffset(-2, -2);
    }
    else {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        // очистка буфера кадра
        glClearColor(0.2f, 0.2f, 0.2f, 1);
        glClearDepth(1);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

    
    GLuint program = show_waves_ ? program_wave_ : program_norm_;

    glUseProgram(program);

    GLuint const mvp_location = glGetUniformLocation(program, "mvp");
    glUniformMatrix4fv(mvp_location, 1, GL_FALSE, &mvp[0][0]);

    GLuint const is_wireframe_location = glGetUniformLocation(program, "is_wireframe");
    glUniform1ui(is_wireframe_location, wired);

    
    if (show_waves_) {
        float add = time_from_start * v_ * 2 * glm::pi<float>();
        float mult = k_ / max_dist_;

        GLuint const add_location = glGetUniformLocation(program, "add_coef");
        glUniform1f(add_location, add);
        GLuint const mult_location = glGetUniformLocation(program, "mult_coef");
        glUniform1f(mult_location, mult);
        GLuint const center_location = glGetUniformLocation(program, "center");
        glUniform3fv(center_location, 1, glm::value_ptr(center_));

        glBindVertexArray(vao_wave_);
    }
    else {
        GLuint const model_location = glGetUniformLocation(program_norm_, "model");
        glUniformMatrix4fv(model_location, 1, GL_FALSE, &model[0][0]);

        glBindVertexArray(vao_norm_);
    }

    // отрисовка
    glDrawArrays(GL_TRIANGLES, 0, 3 * faces.size());

    if (wired) {
        glDisable(GL_POLYGON_OFFSET_LINE);
    }
}

namespace {
    std::istream& operator>>(std::istream& is, vec3 & vec) {
        float x, y, z;
        is >> x >> y >> z;
        vec = vec3(x, y, z);
        //is >> vec.x >> vec.y >> vec.z;
        return is;
    }

    std::istream& operator>>(std::istream& is, vec2 & vec) {
        is >> vec.x >> vec.y;
        return is;
    }

    std::istream& operator>>(std::istream& is, face_t & f) {
        std::string str;
        std::getline(is, str);

        std::string tmp = str;
        std::replace(tmp.begin(), tmp.end(), '/', ' ');
        std::stringstream ss(tmp);
        
        bool failed = false;
        for (int i = 0; i < 3; ++i) {
            if (!(ss >> f.vtx_idx[i] >> f.uv_idx[i] >> f.norm_idx[i])) {
                failed = true;
                break;
            }
        }
        
        if (failed) {
            throw std::logic_error("Unsupported format of face: " + str);
        }

        return is;
    }
    
    template<class T>
    void read_and_push(std::istream& is, std::vector<T> & buf) {
        T tmp;
        is >> tmp;
        buf.push_back(tmp);
    }
}

void model_t::load_model(std::string const & filename) {
    std::ifstream ifs(filename);

    std::string cmd;
    while (ifs >> cmd) {
        if (cmd[0] == '#') {
            std::getline(ifs, cmd);
            continue;
        }
        if (cmd == "v") {
            read_and_push(ifs, vertices);
            continue;
        }
        if (cmd == "vt") {
            read_and_push(ifs, texture_uv);
            continue;
        }
        if (cmd == "vn") {
            read_and_push(ifs, normals);
            continue;
        }
        if (cmd == "f") {
            read_and_push(ifs, faces);
            continue;
        }
        
        throw std::logic_error("Unsupported format of obj file: " + cmd);
    }
}

