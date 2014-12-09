#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.h>
#include "cl.hpp"

#include <vector>
#include <fstream>
#include <iostream>
#include <iterator>
#include <iomanip>
#include <assert.h>


class solver {
public:
    solver() {
        std::vector<cl::Platform> platforms;
        std::vector<cl::Device> devices;
        std::vector<cl::Kernel> kernels;

        // create platform
        cl::Platform::get(&platforms);
        platforms[0].getDevices(CL_DEVICE_TYPE_GPU, &devices);

        // create context
        context = cl::Context(devices);

        // create command queue
        queue = cl::CommandQueue(context, devices[0], CL_QUEUE_PROFILING_ENABLE);

        // load opencl source
        std::ifstream cl_file("scan.cl");
        std::string cl_string(std::istreambuf_iterator<char>(cl_file), (std::istreambuf_iterator<char>()));
        cl::Program::Sources source(1, std::make_pair(cl_string.c_str(),
            cl_string.length() + 1));

        // create program
        program = cl::Program(context, source);

        // compile opencl source
        program.build(devices);

        kernel_scan = cl::Kernel(program, "scan_blelloch");
        kernel_add  = cl::Kernel(program, "add_in_blocks");
    }

    void read_input(std::istream& is, std::vector<float>& vec) {
        size_t n;
        is >> n;

        vec.resize(n);
        for (size_t i = 0; i < n; ++i) {
            is >> vec[i];
        }
    }

    void print_res(std::ostream& os, std::vector<float> const & vec) {
        os << std::fixed << std::setprecision(3);

        for (size_t i = 0; i < vec.size(); ++i) {
            os << vec[i] << " ";
        }
        os << std::endl;
    }

    void run(){
        std::ifstream is("input.txt");
        std::ofstream os("output.txt");

        std::vector<float> input;
        read_input(is, input);
        size_t const test_array_size = input.size();

        // allocate device buffer to hold message
        cl::Buffer dev_input(context, CL_MEM_READ_ONLY, sizeof(float) * test_array_size);
        cl::Buffer dev_output(context, CL_MEM_WRITE_ONLY, sizeof(float) * test_array_size);

        // copy from cpu to gpu
        queue.enqueueWriteBuffer(dev_input, CL_TRUE, 0, sizeof(float) * test_array_size, &input[0]);
        queue.finish();
        
        scan(dev_input, dev_output, test_array_size);

        // read result
        std::vector<float> output(input.size());
        queue.enqueueReadBuffer(dev_output, CL_TRUE, 0, sizeof(int) * input.size(), &output[0]);

        print_res(os, output);
    }

    void scan(cl::Buffer const & dev_input, 
              cl::Buffer const & dev_output,
              size_t const array_size) 
    {
        size_t range = round_for_block(array_size);
        size_t block_cnt = range / block_size;
        cl::Buffer dev_block_sums(context, CL_MEM_READ_WRITE, sizeof(float) * block_cnt);

        if (array_size <= block_size) {
            cl::KernelFunctor scan_b(kernel_scan, queue, cl::NullRange, cl::NDRange(range), cl::NDRange(block_size));
            cl::Event event = scan_b(dev_input, dev_output, dev_block_sums, cl::__local(sizeof(float) * block_size), array_size);
            event.wait();
        }
        else {
            cl::Buffer dev_scan_aux(context, CL_MEM_READ_WRITE, sizeof(float) * array_size);
            cl::Buffer dev_add_aux(context, CL_MEM_READ_WRITE, sizeof(float) * block_cnt);

            cl::KernelFunctor scan_f(kernel_scan, queue, cl::NullRange, cl::NDRange(range), cl::NDRange(block_size));
            cl::Event event = scan_f(dev_input, dev_scan_aux, dev_block_sums, cl::__local(sizeof(float) * block_size), array_size);
            event.wait();

            scan(dev_block_sums, dev_add_aux, block_cnt);
            
            cl::KernelFunctor add(kernel_add, queue, cl::NullRange, cl::NDRange(range), cl::NDRange(block_size));
            event = add(dev_scan_aux, dev_output, dev_add_aux);
            event.wait();
        }
    }

    size_t round_for_block(size_t val) {
        size_t res = val;

        if (val % block_size != 0) {
            res += block_size - val % block_size;
        }

        return res;
    }

private:
    cl::Context context;
    cl::Program program;
    cl::CommandQueue queue;
    cl::Kernel kernel_scan, kernel_add;

    size_t const block_size = 256;
};

int main() {

    try {
        solver s;
        s.run();
    }
    catch (cl::Error e) {
        std::cout << std::endl << e.what() << " : " << e.err() << std::endl;
    }

    return 0;
}