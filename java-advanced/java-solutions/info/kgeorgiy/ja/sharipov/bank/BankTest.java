package info.kgeorgiy.ja.sharipov.bank;

import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

import static org.junit.Assert.*;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class BankTest {

    private final static int DEFAULT_PORT = 8888;
    private static Bank bank;
    private final static String name = "Alexander";
    private final static String surname = "Sharipov";
    private final static String passport = "311632";
    private final static String id = "228";


    // :NOTE: naming
    @BeforeClass
    public static void before_all() {
        bank = new RemoteBank(DEFAULT_PORT);
        try {
            UnicastRemoteObject.exportObject(bank, DEFAULT_PORT);
            Naming.rebind("//localhost/bank", bank);
            System.out.println("Server started");
        } catch (final RemoteException | MalformedURLException ignored) {
            //
        }
    }

    @Test
    public void test1_account_test() throws RemoteException {
        bank.createPerson(name, surname, passport);
        Account account = bank.createAccount(passport, id);
        assertEquals(0, account.getAmount());
        account.setAmount(200);
        assertEquals(200, account.getAmount());
        account.setAmount(account.getAmount() + 100);
        assertEquals(300, account.getAmount());
        account.setAmount(-100);
        assertEquals(-100, account.getAmount());
    }

    @Test
    public void test2_account_test() throws RemoteException {
        Person remotePerson = bank.getPerson(passport, Bank.PersonType.REMOTE);
        Person remotePerson2 = bank.getPerson(passport, Bank.PersonType.REMOTE);
        Person localPerson = bank.getPerson(passport, Bank.PersonType.LOCAL);
        remotePerson.getAccount(id).setAmount(1000);
        Person localPerson2 = bank.getPerson(passport, Bank.PersonType.LOCAL);
        assertEquals(1000, bank.getAccount(passport, id).getAmount());
        assertEquals(remotePerson.getAccount(id).getAmount(), remotePerson2.getAccount(id).getAmount());
        assertEquals(remotePerson.getAccount(id).getAmount(), localPerson2.getAccount(id).getAmount());
        assertNotEquals(1000, localPerson.getAccount(id).getAmount());
        localPerson.getAccount(id).setAmount(500);
        assertNotEquals(500, bank.getAccount(passport, id).getAmount());
        assertNotEquals(500, remotePerson.getAccount(id).getAmount());
        assertNotEquals(500, remotePerson2.getAccount(id).getAmount());
        assertNotEquals(500, localPerson2.getAccount(id).getAmount());
    }

    @Test
    public void test3_account_test() throws RemoteException {
        assertNull(bank.getAccount(passport, "123"));
        assertNull(bank.getAccount("12345", "123"));
        assertNull(bank.createAccount("12345", "123"));
    }

    @Test
    public void test4_person_test() throws RemoteException {
        assertNull(bank.createPerson("Wrong", "Name", passport));
        assertNull(bank.getPerson("12345", Bank.PersonType.LOCAL));
        assertNull(bank.getPerson("12345", Bank.PersonType.REMOTE));
    }

    @Test
    public void test5_person_test() throws RemoteException {
        Person person = bank.createPerson("New", "Person", "111111");
        Person clone = bank.createPerson("New", "Person", "111111");
        bank.createAccount("111111", "123");
        assertEquals(person, clone);
        assertEquals(person.getAccount("123"), clone.getAccount("123"));
    }

    @Test
    public void test6_person_test() throws RemoteException {
        final String pass = "drive";
        final String idx = "123";
        bank.createPerson("Ryan", "Gosling", pass);
        bank.createAccount(pass, idx);
        Person remotePersonOne = bank.getPerson(pass, Bank.PersonType.REMOTE);
        Person remotePersonTwo = bank.getPerson(pass, Bank.PersonType.REMOTE);
        Person localPersonOne = bank.getPerson(pass, Bank.PersonType.LOCAL);
        bank.getAccount(pass, idx).addAmount(100);
        Person localPersonTwo = bank.getPerson(pass, Bank.PersonType.LOCAL);
        assertEquals(100, remotePersonOne.getAccount(idx).getAmount());
        assertEquals(100, remotePersonTwo.getAccount(idx).getAmount());
        assertEquals(100, localPersonTwo.getAccount(idx).getAmount());
        assertEquals(0, localPersonOne.getAccount(idx).getAmount());
    }

    @Test
    public void test7_threads_remote_test() throws RemoteException {
        ExecutorService workers = Executors.newFixedThreadPool(10);
        bank.createPerson("Multi", "Thread", "5318008");
        Account account = bank.createAccount("5318008", "A113");
        test_thread_account(workers, account, 100);
        assertEquals(1000, account.getAmount());
    }

    @Test
    public void test8_threads_local_test() throws RemoteException {
        ExecutorService workers = Executors.newFixedThreadPool(10);
        Account localAccount = bank.getPerson("5318008", Bank.PersonType.LOCAL).getAccount("A113");
        test_thread_account(workers, localAccount, -100);
        assertEquals(0, localAccount.getAmount());
    }

    @Test
    public void test9_threads_test() throws RemoteException {
        ExecutorService workers = Executors.newFixedThreadPool(10);
        CountDownLatch latch = new CountDownLatch(10);
        IntStream.range(0, 10).forEach(i ->
                workers.submit(() -> {
                    try {
                        bank.createPerson("thread_" + i, "hello", "10000" + i);
                    } catch (RemoteException ignored) {
                        //
                    }
                    latch.countDown();
                })
        );
        try {
            latch.await();
        } catch (InterruptedException ignored) {
            //
        }
        for (int i = 0; i < 10; i++) {
            Person person = bank.getPerson("10000" + i, Bank.PersonType.REMOTE);
            assertNotNull(person);
            assertEquals(person.getName(), "thread_" + i);
            assertEquals(person.getSurname(), "hello");
        }
    }

    @Test
    public void test10_thread_test() throws RemoteException {
        ExecutorService workers = Executors.newFixedThreadPool(10);
        CountDownLatch latch = new CountDownLatch(10);
        bank.createPerson("Several", "Accounts", "threads");
        IntStream.range(0, 10).forEach(i ->
                workers.submit(() -> {
                    try {
                        Account acc = bank.createAccount("threads", "10" + i);
                        for (int j = 0; j < 10; j++) {
                            acc.addAmount(j);
                        }
                    } catch (RemoteException ignored) {
                        //
                    }
                    latch.countDown();
                })
        );
        try {
            latch.await();
        } catch (InterruptedException ignored) {
            //
        }
        for (int i = 0; i < 10; i++) {
            Account account = bank.getAccount("threads", "10" + i);
            assertNotNull(account);
            assertEquals(account.getAmount(), 45);
        }
    }

    private static void test_thread_account(ExecutorService workers, Account localAccount, int diff) {
        CountDownLatch latch = new CountDownLatch(10);
        IntStream.range(0, 10).forEach(i ->
                workers.submit(() -> {
                    try {
                        localAccount.addAmount(diff);
                    } catch (RemoteException ignored) {
                        //
                    }
                    latch.countDown();
                }));
        try {
            latch.await();
        } catch (InterruptedException ignored) {
            //
        }
    }
}
