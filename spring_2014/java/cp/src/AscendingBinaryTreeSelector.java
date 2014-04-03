public class AscendingBinaryTreeSelector<T> implements Selector<T> {
    private BinaryTreeNode<T> currentNode;
    private boolean isInitialised;

    public AscendingBinaryTreeSelector(BinaryTreeNode<T> root) {
        currentNode = root;
        isInitialised = false;
    }

    private void init() {
        BinaryTreeNode<T> last = currentNode;

        while (last != null) {
            currentNode = last;
            last = last.getLeft();
        }

        isInitialised = true;
    }

    @Override
    public T current() {
        return currentNode.getValue();
    }

    @Override
    public boolean hasNext() {
        if (!isInitialised) {
            return currentNode != null;
        } else {
            return currentNode.next() != null;
        }
    }

    @Override
    public void next() {
        if (!isInitialised) {
            init();
        } else {
            currentNode = currentNode.next();
        }
    }
}
