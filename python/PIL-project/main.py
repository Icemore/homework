from PIL import Image
from random import shuffle
import os
import PuzzleSolver
import argparse


def split_image(file, width_in_blocks, height_in_blocks):
    image = Image.open(file)
    name, ext = os.path.splitext(file)

    if not os.path.exists(name):
        os.makedirs(name)

    width, height = image.size

    block_size = min(width // width_in_blocks, height // height_in_blocks)

    blocks_cnt = width_in_blocks * height_in_blocks
    blocks_ids = list(range(blocks_cnt))
    shuffle(blocks_ids)
    blocks_ids_it = iter(blocks_ids)

    for i in range(0, height_in_blocks):
        for j in range(0, width_in_blocks):
            block = image.crop((j*block_size, i*block_size, (j+1)*block_size, (i+1)*block_size))

            id = next(blocks_ids_it)
            cur_name = os.path.join(name, str(id)+'.png')

            block.save(cur_name)


parser = argparse.ArgumentParser()
parser.add_argument("mode", choices=['d', 'a'], help="operating mode: d for disassemble, a for assemble")
parser.add_argument("name", help="path to image or folder with pieces")
parser.add_argument("width_in_blocks", type=int)
parser.add_argument("height_in_blocks", type=int)
args = parser.parse_args()

if args.mode == "d":
    split_image(args.name, args.width_in_blocks, args.height_in_blocks)
else:
    res = PuzzleSolver.assemble_picture(args.name)
    res.save(args.name+"_res.png")