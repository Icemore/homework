//#define USE_CORE_OPENGL

#include "common.h"
#include "model.h"

#ifndef APIENTRY
   #define APIENTRY
#endif

unique_ptr<model_t> g_model;

// отрисовка кадра
void display_func()
{
   static chrono::system_clock::time_point const start = chrono::system_clock::now();

   // вызов функции отрисовки с передачей ей времени от первого вызова
   g_model->draw_frame(chrono::duration<float>(chrono::system_clock::now() - start).count());

   // отрисовка GUI
   TwDraw();

   // смена front и back buffer'а (напоминаю, что у нас используется режим двойной буферизации)
   glutSwapBuffers();
}

// Переисовка кадра в отсутствии других сообщений
void idle_func()
{
   glutPostRedisplay();
}

void keyboard_func( unsigned char button, int x, int y )
{
   if (TwEventKeyboardGLUT(button, x, y))
      return;

   switch(button)
   {
   case 27:
     // g_sample.reset();
      exit(0);
   }
}

// Отработка изменения размеров окна
void reshape_func( int width, int height )
{
   if (width <= 0 || height <= 0)
      return;
   glViewport(0, 0, width, height);
   TwWindowSize(width, height);
}

// Очищаем все ресурсы, пока контекст ещё не удалён
void close_func()
{
   g_model.reset();
}

// callback на различные сообщения от OpenGL
void APIENTRY gl_debug_proc(  GLenum         //source
                              , GLenum         type
                              , GLuint         //id
                              , GLenum         //severity
                              , GLsizei        //length
                              , GLchar const * message
                              
                              , GLvoid * //user_param
                                )
{
   if (type == GL_DEBUG_TYPE_ERROR_ARB)
   {
      cerr << message << endl;
      exit(1);
   }
}

int main( int argc, char ** argv )
{
   // Размеры окна по-умолчанию
   size_t const default_width  = 800;
   size_t const default_height = 800;

   glutInit               (&argc, argv);
   glutInitWindowSize     (default_width, default_height);
   // Указание формата буфера экрана:
   // - GLUT_DOUBLE - двойная буферизация
   // - GLUT_RGB - 3-ёх компонентный цвет
   // - GLUT_DEPTH - будет использоваться буфер глубины
   glutInitDisplayMode    (GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
   // Создаем контекст версии 3.3
   glutInitContextVersion (3, 3);
   // Контекст будет поддерживать отладку и "устаревшую" функциональность, которой, например, может пользоваться библиотека AntTweakBar
   glutInitContextFlags   (GLUT_FORWARD_COMPATIBLE | GLUT_DEBUG);
   // Указание либо на core либо на compatibility профил
   glutInitContextProfile (GLUT_COMPATIBILITY_PROFILE );
   int window_handle = glutCreateWindow("OpenGL basic sample");

   // Инициализация указателей на функции OpenGL
   if (glewInit() != GLEW_OK)
   {
      cerr << "GLEW init failed" << endl;
      return 1;
   }

   // Проверка созданности контекста той версии, какой мы запрашивали
   if (!GLEW_VERSION_3_3)
   {
      cerr << "OpenGL 3.3 not supported" << endl;
      return 1;
   }

   glEnable(GL_DEPTH_TEST);
   glDepthFunc(GL_LEQUAL);

#ifdef USE_CORE_OPENGL
   glutDestroyWindow(window_handle);
   glutInitContextProfile(GLUT_CORE_PROFILE);
   window_handle = glutCreateWindow("OpenGL basic sample");
#endif

   // Трассировка ошибок по callback'у
   glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
   glDebugMessageCallbackARB(gl_debug_proc, NULL);
   // выключить все трассировки
   glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE           , GL_DONT_CARE, 0, NULL, false);
   // включить сообщения только об ошибках
   glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_ERROR_ARB, GL_DONT_CARE, 0, NULL, true );

   // подписываемся на оконные события
   glutReshapeFunc(reshape_func);
   glutDisplayFunc(display_func);
   glutIdleFunc   (idle_func   );
   glutCloseFunc  (close_func  );
   glutKeyboardFunc(keyboard_func);

   // подписываемся на события для AntTweakBar'а
   glutMouseFunc        ((GLUTmousebuttonfun)TwEventMouseButtonGLUT);
   glutMotionFunc       ((GLUTmousemotionfun)TwEventMouseMotionGLUT);
   glutPassiveMotionFunc((GLUTmousemotionfun)TwEventMouseMotionGLUT);
   glutSpecialFunc      ((GLUTspecialfun    )TwEventSpecialGLUT    );
   TwGLUTModifiersFunc  (glutGetModifiers);

   try
   {
      g_model.reset(new model_t());
      // Вход в главный цикл приложения
      glutMainLoop();
   }
   catch( std::exception const & except )
   {
      std::cout << except.what() << endl;
      return 1;
   }

   return 0;
}
