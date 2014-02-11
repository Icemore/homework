class Node:
    def __init__(self, val):
        self.val = val
        self.left = None
        self.right = None

class BSTIter:
    def __init__(self, li, bst):
        self.li=li
        self.bst = bst
        self.counter = bst.counter
        self.cur=0

    def __next__(self):
        if self.counter < self.bst.counter:
            raise Exception()

        self.cur+=1
        if self.cur-1 == len(self.li):
            raise StopIteration()

        return self.li[self.cur-1]

class BST:
    def __init__(self):
        self.root = None
        self.counter = 0

    def addNode(self, curNode, newNode):
        if not curNode:
            return newNode

        self.counter += 1

        if newNode.val <= curNode.val:
            curNode.left = self.addNode(curNode.left, newNode)
        else:
            curNode.right = self.addNode(curNode.right, newNode)

        return curNode

    def getNodesInOrder(self, curNode, res):
        if not curNode:
            return

        self.getNodesInOrder(curNode.left, res)
        res.append(curNode)
        self.getNodesInOrder(curNode.right, res)

    def __iter__(self):
        res = []
        self.getNodesInOrder(self.root, res)
        return BSTIter(res, self)

    def __iadd__(self, other):
        if isinstance(other, Node):
            self.root = self.addNode(self.root, other)
        elif isinstance(other, BST):
            for node in other:
                self.root = self.addNode(self.root, node)
        else:
            raise TypeError()

        return self

b=BST()
b+=Node(3)
b+=Node(23)

for n in b:
    print(n.val)