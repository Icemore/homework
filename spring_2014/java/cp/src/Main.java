import java.io.File;
import java.io.IOException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        String fileName = args[0];

        try (Scanner scanner = new Scanner(new File(fileName))) {
            run(scanner);
        } catch (BinaryTreeInsertException e) {
            System.err.println(e.getMessage());
        } catch (IOException e) {
            System.err.println("IO exception happened: " + e.getMessage());
        }
    }

    private static void run(Scanner scanner) throws BinaryTreeInsertException, IOException {
        BinaryTree<Integer> tree = new BinaryTree<>();

        while (scanner.hasNextInt()) {
            int cur = scanner.nextInt();
            tree.add(cur);
        }

        if (scanner.ioException() != null) {
            throw scanner.ioException();
        }

        Printer.print(tree.getAscendingSelector());
        Printer.print(tree.getDescendingSelector());
    }
}
