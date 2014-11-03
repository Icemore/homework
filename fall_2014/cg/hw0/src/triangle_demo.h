#pragma once
#include "common.h"

class triangle_demo_t
{
public:
   triangle_demo_t();
   ~triangle_demo_t();

   void init_buffer();
   void init_vertex_array();
   void draw_frame( float time_from_start );

private:
   bool screen_chess_;
   GLuint vs_, fs_, program_;
   GLuint vx_buf_;
   GLuint vao_;
   quat   rotation_by_control_;
};