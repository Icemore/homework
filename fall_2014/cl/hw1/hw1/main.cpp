#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.h>
#include "cl.hpp"

#include <vector>
#include <fstream>
#include <iostream>
#include <iterator>
#include <iomanip>

void read_matrix(std::istream &is, std::vector<float> & mat, size_t n) {
	mat.resize(n * n);
	for (size_t i = 0; i < n; ++i) {
		for (size_t j = 0; j < n; ++j) {
			is >> mat[i * n + j];
		}
	}
}

void print_matrix(std::ostream &os, std::vector<float> const & mat, size_t n) {
	os << std::fixed << std::setprecision(3);

	for (size_t i = 0; i < n; ++i) {
		for (size_t j = 0; j < n; ++j) {
			os << mat[i * n + j] << " ";
		}
		os << std::endl;
	}
}

int main()
{
   std::vector<cl::Platform> platforms;
   std::vector<cl::Device> devices;
   std::vector<cl::Kernel> kernels;

   try {
	  std::ifstream is("input.txt");
	  std::ofstream os("output.txt");

      // create platform
      cl::Platform::get(&platforms);
      platforms[0].getDevices(CL_DEVICE_TYPE_GPU, &devices);

      // create context
      cl::Context context(devices);

      // create command queue
      cl::CommandQueue queue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

      // load opencl source
      std::ifstream cl_file("convolution2d.cl");
      std::string cl_string(std::istreambuf_iterator<char>(cl_file), (std::istreambuf_iterator<char>()));
      cl::Program::Sources source(1, std::make_pair(cl_string.c_str(),
         cl_string.length() + 1));

      // create program
      cl::Program program(context, source);

      // compile opencl source
      program.build(devices);

	  //read data
	  size_t input_size = 0;
	  size_t mask_size = 0;
	  std::vector<float> input;
	  std::vector<float> mask;
		
	  is >> input_size >> mask_size;
	  read_matrix(is, input, input_size);
	  read_matrix(is, mask, mask_size);
		
	  size_t input_len = input_size * input_size;
	  size_t mask_len = mask_size * mask_size;

      // create a message to send to kernel
      size_t const block_size = 16;

      // allocate device buffer to hold message
      cl::Buffer dev_input (context, CL_MEM_READ_ONLY, sizeof(float) * input_len);
      cl::Buffer dev_output(context, CL_MEM_WRITE_ONLY, sizeof(float) * input_len);
      cl::Buffer dev_mask  (context, CL_MEM_READ_ONLY, sizeof(float) * mask_len);

      // copy from cpu to gpu
      queue.enqueueWriteBuffer(dev_input, CL_TRUE, 0, sizeof(float) * input_len, &input[0]);
      queue.enqueueWriteBuffer(dev_mask, CL_TRUE, 0, sizeof(float)* mask_len, &mask[0]);

      // load named kernel from opencl source
	  int global_range_size = input_size;
	  if (global_range_size % block_size != 0) {
		  global_range_size += block_size - global_range_size % block_size;
	  }

      queue.finish();
      cl::Kernel kernel_gmem(program, "gpu_convolution2d_gmem");
	  cl::KernelFunctor convolution2d_gmem(kernel_gmem, queue, cl::NullRange, cl::NDRange(global_range_size, global_range_size),
		  cl::NDRange(block_size, block_size));
      cl::Event event = convolution2d_gmem(dev_input, dev_mask, dev_output, (int)mask_size, (int)input_size);

      event.wait();
      cl_ulong start_time = event.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      cl_ulong end_time   = event.getProfilingInfo<CL_PROFILING_COMMAND_END>();
      cl_ulong elapsed_time = end_time - start_time;
		
	  //print output
	  std::vector<float> output(input_len);
      queue.enqueueReadBuffer(dev_output, CL_TRUE, 0, sizeof(float) * input_len, &output[0]);
	  print_matrix(os, output, input_size);

      std::cerr << std::setprecision(2) << "Total time: " << elapsed_time / 1000000.0 << " ms" << std::endl;

   }
   catch (cl::Error const & e)
   {
      std::cerr << std::endl << e.what() << " : " << (e.err()) << std::endl;
   }

   return 0;
}