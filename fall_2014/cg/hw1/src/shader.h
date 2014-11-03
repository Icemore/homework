#pragma once

#include "common.h"

GLuint create_shader( GLenum shader_type, char const * file_name );
GLuint create_program( GLuint vs, GLuint fs );
