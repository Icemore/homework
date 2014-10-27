__kernel void gpu_convolution2d_gmem(__global float * input, __global float * mask, 
                                   __global float * output, int mask_width, int width)
{
   int idx_x = get_global_id(0);
   int idx_y = get_global_id(1);

   if (idx_x >= width || idx_y >= width)
	   return;

   float res = 0.0;
   for (int i = 0; i < mask_width; ++i) {
	   for (int j = 0; j < mask_width; ++j) {
		   int input_idx_x = idx_x + i - mask_width / 2;
		   int input_idx_y = idx_y + j - mask_width / 2;

		   bool in_mat_x = (input_idx_x + width) / width == 1;
		   bool in_mat_y = (input_idx_y + width) / width == 1;

		   if (in_mat_x && in_mat_y)
			   res += input[input_idx_x * width + input_idx_y] * mask[i * mask_width + j];
	   }
   }

   output[idx_x * width + idx_y] = res;
}