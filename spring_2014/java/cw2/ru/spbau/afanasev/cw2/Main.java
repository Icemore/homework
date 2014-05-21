package ru.spbau.afanasev.cw2;

public class Main {
    static class A {
        private int t;
        private String str;

        public A () {}

        public A(int t, String str) {
            this.t = t;
            this.str = str;
        }

        public int getT() {
            return t;
        }

        public void setT(int t) {
            this.t = t;
        }

        public String getStr() {
            return str;
        }

        @Override
        public String toString() {
            return "A{" +
                    "t=" + t +
                    ", str='" + str + '\'' +
                    '}';
        }

        public void setStr(String str) {
            this.str = str;
        }
    }

    static class B {
        private String b;
        private int i;

        public int getI() {
            return i;
        }

        public void setI(int i) {
            this.i = i;
        }

        public String getASD() {
            return "qweqwe";
        }

        B(String b) {
            this.b = b;
        }

        public String getB() {

            return b;
        }

        public void setB(String b) {
            this.b = b;
        }

        @Override
        public String toString() {
            return "B{" +
                    "b='" + b + '\'' +
                    ", i=" + i +
                    '}';
        }

        public B() {}
    }

    public static void main(String[] args) {
        DistributedSerializator<B> db = new DistributedSerializator<>(B.class);

        B b = new B("b inst");

        try {
            db.serialize(b, "b1");
            System.out.println(db.deserialize("b1").toString());
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        db.shutdown();
    }
}
