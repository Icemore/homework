cmake_minimum_required(VERSION 2.8)

project(hw0)

set(cpps_triangle src/main_triangle.cpp src/shader.cpp src/triangle_demo.cpp)
set(headers_triangle src/shader.h src/common.h src/triangle_demo.h)
set(shaders_triangle shaders/triangle.glslfs shaders/triangle.glslvs)

set(cpps_frac src/main_fractal.cpp src/shader.cpp src/fractal.cpp)
set(headers_frac src/shader.h src/common.h src/fractal.h)
set(shaders_frac shaders/fractal.glslvs shaders/fractal.glslfs)

IF (WIN32)
   set(EXTERNAL_LIBS ${PROJECT_SOURCE_DIR}/../../ext CACHE STRING "external libraries location")

   include_directories(${EXTERNAL_LIBS}/AntTweakBar/include)
   include_directories(${EXTERNAL_LIBS}/freeglut/include)
   include_directories(${EXTERNAL_LIBS}/glew-1.10.0/include)
   include_directories(${EXTERNAL_LIBS}/glm)

   link_directories(${EXTERNAL_LIBS}/AntTweakBar/lib)
   link_directories(${EXTERNAL_LIBS}/freeglut/lib)
   link_directories(${EXTERNAL_LIBS}/glew-1.10.0/lib/Release/Win32)

   add_executable(triangle ${cpps_triangle} ${headers_triangle} ${shaders_triangle})
   source_group(shaders FILES ${shaders_triangle})
   
   add_executable(fractal ${cpps_frac} ${headers_frac} ${shaders_frac})
   source_group(shaders FILES ${shaders_frac})
    
   foreach(main triangle fractal)
       target_link_libraries(${main} freeglut glew32 AntTweakBar)
       add_custom_command(TARGET ${main} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            "${EXTERNAL_LIBS}/AntTweakBar/lib"
            $<TARGET_FILE_DIR:${main}>)
       add_custom_command(TARGET ${main} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            "${EXTERNAL_LIBS}/glew-1.10.0/bin/Release/Win32"
            $<TARGET_FILE_DIR:${main}>)
       add_custom_command(TARGET ${main} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            "${EXTERNAL_LIBS}/freeglut/bin"
            $<TARGET_FILE_DIR:${main}>)
        
       add_custom_command(TARGET ${main} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_directory
            "${PROJECT_SOURCE_DIR}/shaders"
            $<TARGET_FILE_DIR:${main}>/shaders)    
    endforeach()
    
ELSE (WIN32)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -std=c++11")

    add_executable(triangle ${cpps_triangle} ${headers_triangle})
    add_executable(fractal ${cpps_frac} ${headers_frac})

    find_package(OpenGL REQUIRED)
    find_package(GLUT REQUIRED)
    find_package(GLEW REQUIRED)
    
    include_directories(${GLEW_INCLUDE_DIRS})
    link_directories(${GLEW_LIBRARY_DIRS})
    add_definitions(${GLEW_DEFINITIONS})
    if(NOT GLEW_FOUND)
       message(Error "GLEW not found")
    endif(NOT GLEW_FOUND)

   include_directories(${OPENGL_INCLUDE_DIRS}  ${GLUT_INCLUDE_DIRS} ${GLEW_INCLUDE_DIRS})
   
   target_link_libraries(triangle AntTweakBar X11 GL glut GLEW)
   target_link_libraries(fractal AntTweakBar X11 GL glut GLEW)
ENDIF (WIN32)
