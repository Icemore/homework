public class BinaryTreeNode<T> {
    private BinaryTreeNode<T> left;
    private BinaryTreeNode<T> right;
    private BinaryTreeNode<T> parent;
    private T value;
    private int subtreeHeight;

    public BinaryTreeNode(BinaryTreeNode<T> parent, T val) {
        this.value = val;
        subtreeHeight = 1;
        left = right = null;
        this.parent = parent;
    }

    public void updateHeight() {
        int newH = 0;

        if (left != null) {
            newH = Math.max(newH, left.subtreeHeight);
        }

        if (right != null) {
            newH = Math.max(newH, right.subtreeHeight);
        }

        subtreeHeight = newH + 1;
    }

    public BinaryTreeNode<T> next() {
        BinaryTreeNode<T> cur = this;

        if (cur.right != null) {
            cur = cur.right;
            while (cur.left != null) {
                cur = cur.left;
            }
        } else {
            while (cur.parent != null && cur.parent.right == cur) {
                cur = cur.parent;
            }
            cur = cur.parent;
        }

        return cur;
    }

    public BinaryTreeNode<T> prev() {
        BinaryTreeNode<T> cur = this;

        if (cur.left != null) {
            cur = cur.left;
            while (cur.right != null) {
                cur = cur.right;
            }
        } else {
            while (cur.parent != null && cur.parent.left == cur) {
                cur = cur.parent;
            }
            cur = cur.parent;
        }

        return cur;
    }

    public BinaryTreeNode<T> getParent() {
        return parent;
    }

    public void setParent(BinaryTreeNode<T> parent) {
        this.parent = parent;
    }

    public BinaryTreeNode<T> getLeft() {
        return left;
    }

    public void setLeft(BinaryTreeNode<T> left) {
        this.left = left;
    }

    public BinaryTreeNode<T> getRight() {
        return right;
    }

    public void setRight(BinaryTreeNode<T> right) {
        this.right = right;
    }

    public T getValue() {
        return value;
    }

    public int getSubtreeHeight() {
        return subtreeHeight;
    }
}
