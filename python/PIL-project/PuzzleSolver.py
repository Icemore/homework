from PIL import Image
from metrics import get_weight
import os

directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]
angles = [90, 0, -90, 180]


class Block:
    def __init__(self, block_id, image):
        self.id = block_id
        self.image = image
        self.pos = (0, 0)
        self.chunk_id = 0


class Edge:
    def __init__(self, block1, block2, direction):
        self.v = block1
        self.u = block2
        self.dir = direction
        self.weight = get_weight(block1.image, block2.image, angles[direction])


def load_blocks(folder, blocks_cnt):
    res = []

    for i in range(blocks_cnt):
        img = Image.open(os.path.join(folder, str(i)+'.png'))
        res.append(Block(i, img))

    return res


def generate_edges(blocks):
    edges = []

    for i in range(len(blocks)):
        for j in range(i+1, len(blocks)):
            for t in range(4):
                edges.append(Edge(blocks[i], blocks[j], t))

    return sorted(edges, key=lambda x: x.weight)


def check_chunks_intersections(c1, c2, shift):
    for pos in c2.keys():
        shifted = (pos[0] + shift[0], pos[1] + shift[1])

        if shifted in c1.keys():
            return True

    return False


def unite_chunks(chunks, id1, id2, shift):
    c1 = chunks[id1]
    c2 = chunks[id2]

    for block in c2.values():
        block.pos = (block.pos[0] + shift[0], block.pos[1] + shift[1])
        block.chunk_id = id1
        c1[block.pos] = block


def create_mst(blocks, edges):
    chunks = []
    for block in blocks:
        block.chunk_id = len(chunks)
        chunks.append({(0, 0): block})

    for edge in edges:
        if edge.v.chunk_id == edge.u.chunk_id:
            continue

        move = directions[edge.dir]
        shift = (-edge.u.pos[0] + edge.v.pos[0] + move[0], -edge.u.pos[1] + edge.v.pos[1] + move[1])

        if check_chunks_intersections(chunks[edge.v.chunk_id], chunks[edge.u.chunk_id], shift):
            continue

        unite_chunks(chunks, edge.v.chunk_id, edge.u.chunk_id, shift)


def get_picture(blocks):
    block_size = blocks[0].image.size[0]

    minx = min([b.pos[0] for b in blocks])
    miny = min([b.pos[1] for b in blocks])
    maxx = max([b.pos[0] for b in blocks])
    maxy = max([b.pos[1] for b in blocks])

    image = Image.new('RGB', ((maxx-minx+1)*block_size, (maxy-miny+1)*block_size))

    for block in blocks:
        x = (block.pos[0] - minx) * block_size
        y = (block.pos[1] - miny) * block_size

        image.paste(block.image, (x, y, x+block_size, y+block_size))

    return image


def assemble_picture(folder):
    blocks_cnt = len(os.listdir(folder))

    blocks = load_blocks(folder, blocks_cnt)
    edges = generate_edges(blocks)

    create_mst(blocks, edges)

    return get_picture(blocks)