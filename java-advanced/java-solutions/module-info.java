/**
 * Java-advanced homeworks.
 */
module info.kgeorgiy.ja.sharipov {
    requires java.compiler;
    requires java.rmi;
    requires hamcrest.core;
    requires junit;
    requires jsoup;
    requires quickcheck;

    exports info.kgeorgiy.ja.sharipov.bank;

    opens info.kgeorgiy.ja.sharipov.bank to junit;
}