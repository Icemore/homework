#include "fractal.h"
#include "shader.h"

static void TW_CALL toggle_fullscreen_callback( void * )
{
   glutFullScreenToggle();
}

fractal_t::fractal_t()
    : pos_(0.5, 0), scale_factor_(1)
{
#ifdef USE_CORE_OPENGL
   TwInit(TW_OPENGL_CORE, NULL);
#else
   TwInit(TW_OPENGL, NULL);
#endif
   // ����������� "���������" GUI
   TwBar *bar = TwNewBar("Parameters");
   TwDefine(" Parameters size='300 50' color='70 100 120' valueswidth=220 iconpos=topleft");

   TwAddButton(bar, "Fullscreen toggle", toggle_fullscreen_callback, NULL,
               " label='Toggle fullscreen mode' key=f");

   // �������� ��������
   vs_ = create_shader(GL_VERTEX_SHADER  , "shaders//fractal.glslvs");
   fs_ = create_shader(GL_FRAGMENT_SHADER, "shaders//fractal.glslfs");
   // �������� ��������� ���� �������� ���������
   program_ = create_program(vs_, fs_);
   // �������� ������ � ���������� �������
   init_buffer();
   // �������� VAO
   init_vertex_array();
}

void fractal_t::move(vec2 directions) {
    pos_ += 1/scale_factor_ * directions * delta;
}

void fractal_t::scale(int power) {
    if (power < 0) {
        scale_factor_ /= scale_step;
    }
    else {
        scale_factor_ *= scale_step;
    }
}

fractal_t::~fractal_t()
{
   // �������� �������� OpenGL
   glDeleteProgram(program_);
   glDeleteShader(vs_);
   glDeleteShader(fs_);
   glDeleteVertexArrays(1, &vao_);
   glDeleteBuffers(1, &vx_buf_);

   TwDeleteAllBars();
   TwTerminate();
}

void fractal_t::init_buffer()
{
   // �������� ������� ������
   glGenBuffers(1, &vx_buf_);
   // ������ ����� ��������
   glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);

   // ������ ��� ������������
   vec2 const data[4] =
   {
       vec2(-3, -2),
       vec2(1, -2),
       vec2(-3, 2),
       vec2(1, 2)
   };

   // �������� ������ ��� �������� ������ �� GPU
   glBufferData(GL_ARRAY_BUFFER, sizeof(vec2) * 4, data, GL_STATIC_DRAW);

   // ���������� ������� �������� �����
   glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void fractal_t::init_vertex_array()
{
   glGenVertexArrays(1, &vao_);
   glBindVertexArray(vao_);
      // ������������ ����� vx_buf_ � vao
      glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);

      // ����������� ����� ��������� � ���������, ��������� �� ������� ��������
      GLuint const pos_location = glGetAttribLocation(program_, "in_pos");
      // ������������� ������ ������ ��� ��������� "pos_location"
      // 2 float'� �����������������, ��� ����� ��������� ����� sizeof(vec2), �������� �� ������ ������ ����� 0
      glVertexAttribPointer(pos_location, 2, GL_FLOAT, GL_FALSE, sizeof(vec2), 0);
      // "��������" �������� "pos_location"
      glEnableVertexAttribArray(pos_location);
   glBindVertexArray(0);
};

void fractal_t::draw_frame( float time_from_start )
{
   float const w                = (float)glutGet(GLUT_WINDOW_WIDTH);
   float const h                = (float)glutGet(GLUT_WINDOW_HEIGHT);
   // ������ ������� �������� � aspect ratio (���������� ������) ����� ��, ��� � ����
   mat4  const proj             = perspective(45.0f, w / h, 0.1f, 100.0f);
   // �������������� �� �� ���� � �� ������
   mat4  const view             = lookAt(vec3(0, 0, 4), vec3(0, 0, 0), vec3(0, 1, 0));
   //mat4  const mvp = proj * view;
   mat4 const model = glm::scale(vec3(scale_factor_, scale_factor_, 1))* translate(vec3(pos_, 0));
   mat4 const mvp = proj * view * model;

   // ��������� ��������� ��������� ������������
   glDisable(GL_CULL_FACE);
   // ��������� ���� �������
   glDisable(GL_DEPTH_TEST);
   // ������� ������ �����
   glClearColor(0, 0, 0, 1);
   glClearDepth(1);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   // ��������� �������� ��� ���������
   glUseProgram(program_);

   // ��������� uniform'��
   GLuint const mvp_location = glGetUniformLocation(program_, "mvp");
   glUniformMatrix4fv(mvp_location, 1, GL_FALSE, &mvp[0][0]);

   // ��������� vao (����� � ������� + ������)
   glBindVertexArray(vao_);

   // ���������
   glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}