__kernel void scan_blelloch(__global float * input, __global float * output, 
                            __global float * block_sum,
                            __local float * b, int n)
{
    uint gid = get_global_id(0);
    uint lid = get_local_id(0);
    uint block_idx  = get_group_id(0);
    uint block_size = get_local_size(0);
    uint dp = 1;
    
    if (gid >= n) return;

    b[lid] = input[gid];

    for(uint s = block_size>>1; s > 0; s >>= 1)
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        if(lid < s)
        {
            uint i = dp*(2*lid+1)-1;
            uint j = dp*(2*lid+2)-1;
            b[j] += b[i];
        }

        dp <<= 1;
    }

    if (lid == 0) {
        block_sum[block_idx] = b[block_size - 1];
        b[block_size - 1] = 0;
    }

    for(uint s = 1; s < block_size; s <<= 1)
    {
        dp >>= 1;
        barrier(CLK_LOCAL_MEM_FENCE);

        if(lid < s)
        {
            uint i = dp*(2*lid+1)-1;
            uint j = dp*(2*lid+2)-1;

            float t = b[j];
            b[j] += b[i];
            b[i] = t;
        }
    }

    barrier(CLK_LOCAL_MEM_FENCE);

    output[gid] = b[lid];
}


__kernel void add_in_blocks(__global float * input, __global float * output, __global float * sums)
{
    uint gid = get_global_id(0);
    uint block_idx  = get_group_id(0);

    output[gid] = input[gid] + sums[block_idx];
}