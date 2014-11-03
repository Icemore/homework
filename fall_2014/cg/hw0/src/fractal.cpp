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
   // Определение "контролов" GUI
   TwBar *bar = TwNewBar("Parameters");
   TwDefine(" Parameters size='300 50' color='70 100 120' valueswidth=220 iconpos=topleft");

   TwAddButton(bar, "Fullscreen toggle", toggle_fullscreen_callback, NULL,
               " label='Toggle fullscreen mode' key=f");

   // Создание шейдеров
   vs_ = create_shader(GL_VERTEX_SHADER  , "shaders//fractal.glslvs");
   fs_ = create_shader(GL_FRAGMENT_SHADER, "shaders//fractal.glslfs");
   // Создание программы путём линковки шейдерова
   program_ = create_program(vs_, fs_);
   // Создание буфера с вершинными данными
   init_buffer();
   // Создание VAO
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
   // Удаление русурсов OpenGL
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
   // Создание пустого буфера
   glGenBuffers(1, &vx_buf_);
   // Делаем буфер активным
   glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);

   // Данные для визуализации
   vec2 const data[4] =
   {
       vec2(-3, -2),
       vec2(1, -2),
       vec2(-3, 2),
       vec2(1, 2)
   };

   // Копируем данные для текущего буфера на GPU
   glBufferData(GL_ARRAY_BUFFER, sizeof(vec2) * 4, data, GL_STATIC_DRAW);

   // Сбрасываем текущий активный буфер
   glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void fractal_t::init_vertex_array()
{
   glGenVertexArrays(1, &vao_);
   glBindVertexArray(vao_);
      // присоединяем буфер vx_buf_ в vao
      glBindBuffer(GL_ARRAY_BUFFER, vx_buf_);

      // запрашиваем индек аттрибута у программы, созданные по входным шейдерам
      GLuint const pos_location = glGetAttribLocation(program_, "in_pos");
      // устанавливаем формам данных для аттрибута "pos_location"
      // 2 float'а ненормализованных, шаг между вершиными равен sizeof(vec2), смещение от начала буфера равно 0
      glVertexAttribPointer(pos_location, 2, GL_FLOAT, GL_FALSE, sizeof(vec2), 0);
      // "включаем" аттрибут "pos_location"
      glEnableVertexAttribArray(pos_location);
   glBindVertexArray(0);
};

void fractal_t::draw_frame( float time_from_start )
{
   float const w                = (float)glutGet(GLUT_WINDOW_WIDTH);
   float const h                = (float)glutGet(GLUT_WINDOW_HEIGHT);
   // строим матрицу проекции с aspect ratio (отношением сторон) таким же, как у окна
   mat4  const proj             = perspective(45.0f, w / h, 0.1f, 100.0f);
   // преобразование из СК мира в СК камеры
   mat4  const view             = lookAt(vec3(0, 0, 4), vec3(0, 0, 0), vec3(0, 1, 0));
   //mat4  const mvp = proj * view;
   mat4 const model = glm::scale(vec3(scale_factor_, scale_factor_, 1))* translate(vec3(pos_, 0));
   mat4 const mvp = proj * view * model;

   // выключаем отсечение невидимых поверхностей
   glDisable(GL_CULL_FACE);
   // выключаем тест глубины
   glDisable(GL_DEPTH_TEST);
   // очистка буфера кадра
   glClearColor(0, 0, 0, 1);
   glClearDepth(1);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

   // установка шейдеров для рисования
   glUseProgram(program_);

   // установка uniform'ов
   GLuint const mvp_location = glGetUniformLocation(program_, "mvp");
   glUniformMatrix4fv(mvp_location, 1, GL_FALSE, &mvp[0][0]);

   // установка vao (буфер с данными + формат)
   glBindVertexArray(vao_);

   // отрисовка
   glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}