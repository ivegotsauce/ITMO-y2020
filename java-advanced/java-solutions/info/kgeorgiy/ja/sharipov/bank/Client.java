package info.kgeorgiy.ja.sharipov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public final class Client {
    /** Utility class. */
    private Client() {}

    public static void main(final String... args) throws RemoteException {
        final Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        }

        if (args.length < 5) {
            System.out.println("Five arguments required: <name> <surname> <passport id> <account id> <amount>");
            return;
        }

        final String name = args[0];
        final String surname = args[1];
        final String passport = args[2];
        final String id = args[3];
        final int amount;
        try {
            amount = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            System.out.println("Wrong number: " + e.getMessage());
            return;
        }

        Person person = bank.getPerson(passport, Bank.PersonType.REMOTE);
        if (person == null) {
            System.out.println("Creating person");
            bank.createPerson(name, surname, passport);
        } else {
            if (person.getName().equals(name) && person.getSurname().equals(surname)
                    && person.getPassport().equals(passport)) {
                System.out.println("Person already exists");
            } else {
                System.out.println("Wrong person's data");
                return;
            }
        }

        Account account = bank.getAccount(passport, id);
        if (account == null) {
            System.out.println("Creating account");
            account = bank.createAccount(passport, id);
        } else {
            System.out.println("Account already exists");
        }
        System.out.println("Account id: " + account.getId());
        System.out.println("Balance: " + account.getAmount());
        System.out.println("Adding money");
        account.setAmount(account.getAmount() + amount);
        System.out.println("New Balance: " + account.getAmount());
    }
}
