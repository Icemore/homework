#pragma once

#include <cstddef>
#include <vector>
using std::vector;
#include <string>
using std::string;
#include <fstream>
using std::ifstream;
#include <iostream>
using std::cerr;
using std::endl;
#include <chrono>
namespace chrono = std::chrono;
#include <memory>
using std::unique_ptr;

// GLEW - 
#include <GL/glew.h>
// freeglut - �������� �����, ��������� ���������
#include <GL/freeglut.h>

// AntTweakBar - GUI
#include <AntTweakBar.h>

// GLM - ������ � ���������, ���������������� ������ � ��������
#include <glm/glm.hpp>
#include <glm/ext.hpp>
using namespace glm;
