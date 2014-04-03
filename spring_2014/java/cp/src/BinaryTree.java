public class BinaryTree<T extends Comparable<? super T>> {
    private BinaryTreeNode<T> root;

    public void add(T val) throws BinaryTreeInsertException {
        root = add(null, root, val);
    }

    private BinaryTreeNode<T> add(BinaryTreeNode<T> parent, BinaryTreeNode<T> node, T val) throws BinaryTreeInsertException {
        if (node == null) {
            return new BinaryTreeNode<T>(parent, val);
        }

        if (val.compareTo(node.getValue()) == 0) {
            throw new BinaryTreeInsertException("key " + val.toString() + " already in the tree");
        }

        if (val.compareTo(node.getValue()) < 0) {
            node.setLeft(add(node, node.getLeft(), val));
        } else {
            node.setRight(add(node, node.getRight(), val));
        }

        node.updateHeight();
        return node;
    }

    public int getHeight() {
        return root == null ? 0 : root.getSubtreeHeight();
    }

    public boolean has(T val) {
        return has(root, val);
    }

    private boolean has(BinaryTreeNode<T> node, T val) {
        if (node == null) return false;

        if (val.compareTo(node.getValue()) == 0) {
            return true;
        }

        if (val.compareTo(node.getValue()) < 0) {
            return has(node.getLeft(), val);
        } else {
            return has(node.getRight(), val);
        }
    }

    public AscendingBinaryTreeSelector<T> getAscendingSelector() {
        return new AscendingBinaryTreeSelector<>(root);
    }

    public DescendingBinaryTreeSelector<T> getDescendingSelector() {
        return new DescendingBinaryTreeSelector<>(root);
    }
}
