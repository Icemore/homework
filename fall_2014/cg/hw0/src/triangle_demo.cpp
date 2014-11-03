#include "triangle_demo.h"
#include "shader.h"

static void TW_CALL toggle_fullscreen_callback( void * )
{
   glutFullScreenToggle();
}

triangle_demo_t::triangle_demo_t()
   : screen_chess_(false)
{
#ifdef USE_CORE_OPENGL
   TwInit(TW_OPENGL_CORE, NULL);
#else
   TwInit(TW_OPENGL, NULL);
#endif
   // ����������� "���������" GUI
   TwBar *bar = TwNewBar("Parameters");
   TwDefine(" Parameters size='500 150' color='70 100 120' valueswidth=220 iconpos=topleft");

   TwAddVarRW(bar, "Texture in screen", TW_TYPE_BOOLCPP, &screen_chess_, " true='ON' false='OFF' key=w");

   TwAddButton(bar, "Fullscreen toggle", toggle_fullscreen_callback, NULL,
               " label='Toggle fullscreen mode' key=f");

   TwAddVarRW(bar, "ObjRotation", TW_TYPE_QUAT4F, &rotation_by_control_,
              " label='Object orientation' opened=true help='Change the object orientation.' ");

   // �������� ��������
   vs_ = create_shader(GL_VERTEX_SHADER  , "shaders//triangle.glslvs");
   fs_ = create_shader(GL_FRAGMENT_SHADER, "shaders//triangle.glslfs");
   // �������� ��������� ���� �������� ���������
   program_ = create_program(vs_, fs_);
   // �������� ������ � ���������� �������
   init_buffer();
   // �������� VAO
   init_vertex_array();
}

triangle_demo_t::~triangle_demo_t()
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

void triangle_demo_t::init_buffer()
{
   // �������� ������� ������
   glGenBuffers(1, &vx_buf_);
   // ������ ����� ��������
   glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);

   // ������ ��� ������������
   vec2 const data[3] =
   {
        vec2(0, 2)
      , vec2(-1.73205, -1)
      , vec2(1.73205, -1)
   };

   // �������� ������ ��� �������� ������ �� GPU
   glBufferData(GL_ARRAY_BUFFER, sizeof(vec2) * 3, data, GL_STATIC_DRAW);

   // ���������� ������� �������� �����
   glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void triangle_demo_t::init_vertex_array()
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

void triangle_demo_t::draw_frame( float time_from_start )
{
   float const rotation_angle = time_from_start * 90;

   float const w                = (float)glutGet(GLUT_WINDOW_WIDTH);
   float const h                = (float)glutGet(GLUT_WINDOW_HEIGHT);
   // ������ ������� �������� � aspect ratio (���������� ������) ����� ��, ��� � ����
   mat4  const proj             = perspective(45.0f, w / h, 0.1f, 100.0f);
   // �������������� �� �� ���� � �� ������
   mat4  const view             = lookAt(vec3(0, 0, 8), vec3(0, 0, 0), vec3(0, 1, 0));
   // �������� �� �������
   quat  const rotation_by_time = quat(vec3(0, 0, radians(rotation_angle)));
   mat4 const rotation = mat4_cast(rotation_by_control_ * rotation_by_time);
   mat4  const model_view = view * rotation;
   mat4  const mvp = proj * model_view;

   vec4 t  = mvp * vec4(0, 2, 0, 1);

   // ��������� ��������� ��������� ������������
   glDisable(GL_CULL_FACE);
   // ��������� ���� �������
   glDisable(GL_DEPTH_TEST);
   // ������� ������ �����
   glClearColor(0.2f, 0.2f, 0.2f, 1);
   glClearDepth(1);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   // ��������� �������� ��� ���������
   glUseProgram(program_);

   // ��������� uniform'��
   GLuint const mvp_location = glGetUniformLocation(program_, "mvp");
   glUniformMatrix4fv(mvp_location, 1, GL_FALSE, &mvp[0][0]);

   GLuint const model_view_location = glGetUniformLocation(program_, "model_view");
   glUniformMatrix4fv(model_view_location, 1, GL_FALSE, &model_view[0][0]);

   GLuint const time_location = glGetUniformLocation(program_, "time");
   glUniform1f(time_location, time_from_start);

   GLuint const screen_chess_location = glGetUniformLocation(program_, "screen_chess");
   glUniform1ui(screen_chess_location, screen_chess_);

   // ��������� vao (����� � ������� + ������)
   glBindVertexArray(vao_);

   // ���������
   glDrawArrays(GL_TRIANGLES, 0, 3);
}