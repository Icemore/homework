public class Printer {
    public static void print(Selector<?> selector) {
        while (selector.hasNext()) {
            selector.next();

            System.out.print(selector.current());
            System.out.print(" ");
        }
        System.out.println();
    }
}
