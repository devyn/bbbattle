#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#ifdef __APPLE__
  #include <OpenCL/cl.h>
#else
  #include <CL/cl.h>
#endif

#include "bbbattle.cl.h"
#include "fmt_bbbattle.h"
#include "fmt_bbbout.h"

cl_platform_id platform;
cl_device_id device;
cl_context context;
cl_command_queue queue;

cl_kernel step_bbbattle;

cl_mem alive_d;
cl_mem dying_d;
cl_mem new_alive_d;

size_t dimensions[2];

void step() {
  cl_int err;

  err = clEnqueueNDRangeKernel(queue, step_bbbattle, 2, NULL, dimensions, NULL, 0, NULL, NULL);
  assert(err == CL_SUCCESS);

  // swap buffers
  cl_mem temp = new_alive_d;
  new_alive_d = dying_d;
  dying_d = alive_d;
  alive_d = temp;

  err = clSetKernelArg(step_bbbattle, 0, sizeof(cl_mem),  &alive_d);     assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 1, sizeof(cl_mem),  &dying_d);     assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 2, sizeof(cl_mem),  &new_alive_d); assert(err == CL_SUCCESS);
}

int main(int argc, char **argv) {
  cl_int err;

  int generations = 0;

  if (argc < 4) {
    fprintf(stderr, "Usage: %s <generations> <bbbattle_file> <bbbout_file>\n", argv[0]);
    exit(1);
  }

  generations = strtol(argv[1], NULL, 10);

  /* create buffers and load bbbattle file */

  int width;
  int height;
  int teams;
  char *alive_h;
  char *dying_h;
  struct rgb24 team_colors[256];

  FILE *bbbf = fopen(argv[2], "r");

  if (bbbf == NULL) {
    perror(argv[2]);
    return 1;
  }

  int bbberr = read_bbbattle(&width, &height, &teams, &alive_h, &dying_h, team_colors, bbbf);
  fclose(bbbf);
  assert(bbberr == READ_BBBATTLE_SUCCESS);

  /* open bbbout stream */

  bbbout_stream *bbbo = bbbout_open(argv[3], width, height, teams, team_colors);
  
  if (bbbo == NULL) {
    perror(argv[3]);
    return 1;
  }

  bbbout_write_generation(bbbo, 0, alive_h, dying_h);

  /* create platform */

  cl_uint n_platforms = 0;

  err = clGetPlatformIDs(1, &platform, &n_platforms);

  if (n_platforms == 0) return 1;

  char platform_name[256];
  size_t platform_name_size;

  char platform_vendor[256];
  size_t platform_vendor_size;

  err = clGetPlatformInfo(platform, CL_PLATFORM_NAME, 256, platform_name, &platform_name_size);
  err = clGetPlatformInfo(platform, CL_PLATFORM_VENDOR, 256, platform_vendor, &platform_vendor_size);

  platform_name[platform_name_size] = '\0';
  platform_vendor[platform_vendor_size] = '\0';

  printf("Platform Name: %s, Vendor: %s\n", platform_name, platform_vendor);
  
  /* create device */

  cl_uint n_devices = 0;

  err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, &n_devices);

  if (n_devices == 0) return 1;

  char device_name[256];
  size_t device_name_size;

  char device_vendor[256];
  size_t device_vendor_size;

  err = clGetDeviceInfo(device, CL_DEVICE_NAME, 256, device_name, &device_name_size);
  err = clGetDeviceInfo(device, CL_DEVICE_VENDOR, 256, device_vendor, &device_vendor_size);

  device_name[device_name_size] = '\0';
  device_vendor[device_vendor_size] = '\0';

  printf("GPU Name: %s, Vendor: %s\n", device_name, device_vendor);

  /* create context */

  cl_context_properties cprops[3];

  cprops[0] = CL_CONTEXT_PLATFORM;
  cprops[1] = (cl_context_properties) platform;
  cprops[2] = 0;

  context = clCreateContext(cprops, 1, &device, NULL, NULL, &err);
  assert(err == CL_SUCCESS);

  const size_t program_source_len = strlen(program_source);

  cl_program program = clCreateProgramWithSource(context, 1, (const char **) &program_source, &program_source_len, &err);
  assert(err == CL_SUCCESS);

  err = clBuildProgram(program, 1, &device, NULL, NULL, NULL);
  if (err != CL_SUCCESS) {
    char log[65536];
    size_t log_size;

    clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 65536, log, &log_size);

    fwrite(log, 1, log_size, stderr);

    assert(err == CL_SUCCESS);
  }

  /* create command queue */

  queue = clCreateCommandQueue(context, device, 0, &err);

  /* create device buffers */

  const size_t mem_size = width * height * sizeof(char);

  dimensions[0] = width;
  dimensions[1] = height;

  alive_d = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, mem_size, alive_h, &err);
  assert(err == CL_SUCCESS);

  dying_d = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, mem_size, dying_h, &err);
  assert(err == CL_SUCCESS);

  new_alive_d = clCreateBuffer(context, CL_MEM_READ_WRITE, mem_size, NULL, &err);
  assert(err == CL_SUCCESS);

  /* get the kernel */

  step_bbbattle = clCreateKernel(program, "step_bbbattle", &err);
  assert(err == CL_SUCCESS);

  err = clSetKernelArg(step_bbbattle, 0, sizeof(cl_mem),  &alive_d);     assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 1, sizeof(cl_mem),  &dying_d);     assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 2, sizeof(cl_mem),  &new_alive_d); assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 3, sizeof(cl_uint), &width);       assert(err == CL_SUCCESS);
  err = clSetKernelArg(step_bbbattle, 4, sizeof(cl_uint), &height);      assert(err == CL_SUCCESS);

  /* run kernel and stream to bbbout */

  int gen;
  for (gen = 1; gen <= generations; gen++) {
    step();
    err = clEnqueueReadBuffer(queue, alive_d, CL_TRUE, 0, mem_size, alive_h, 0, NULL, NULL);
    assert(err == CL_SUCCESS);

    bbbout_write_generation(bbbo, gen, alive_h, NULL);
  }

  bbbout_close(bbbo);

  free(alive_h);
  free(dying_h);

  clReleaseCommandQueue(queue);
  clReleaseKernel(step_bbbattle);
  clReleaseProgram(program);
  clReleaseMemObject(alive_d);
  clReleaseMemObject(dying_d);
  clReleaseMemObject(new_alive_d);
  clReleaseContext(context);
  clReleaseDevice(device);

  return 0;
}
