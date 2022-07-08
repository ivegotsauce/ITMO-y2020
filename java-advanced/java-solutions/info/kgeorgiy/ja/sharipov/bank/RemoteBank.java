package info.kgeorgiy.ja.sharipov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, Map<String, Account>> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Person> persons = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    @Override
    public Account createAccount(final String passport, String id) throws RemoteException {
        id = getAccountId(passport, id);
        final Person person = persons.get(passport);
        if (person == null) {
            System.out.println("No such person.");
            return null;
        }
        System.out.println("Creating account " + id);
        final Account account = new RemoteAccount(id);
        if (accounts.get(passport).putIfAbsent(id, account) == null) {
            UnicastRemoteObject.exportObject(account, port);
            return account;
        } else {
            return getAccount(passport, id);
        }
    }

    @Override
    public Account getAccount(final String passport, final String id) {
        System.out.println("Retrieving account " + id);
        Map<String, Account> map = accounts.get(passport);
        if (map == null) {
            return null;
        }
        return map.get(getAccountId(passport, id));
    }

    @Override
    public Person getPerson(String passport, PersonType type) throws RemoteException {
        System.out.println("Retrieving person " + passport);
        return switch (type) {
            case REMOTE -> persons.get(passport);
            case LOCAL -> {
                final Person person = persons.get(passport);
                if (person == null) {
                    yield null;
                }
                final Map<String, Account> accountMap = new ConcurrentHashMap<>();
                for (Map.Entry<String, Account> entry : accounts.get(passport).entrySet()) {
                    LocalAccount account = new LocalAccount(entry.getValue().getId(), entry.getValue().getAmount());
                    accountMap.put(account.getId(), account);
                 }
                yield new LocalPerson(person.getPassport(), person.getName(), person.getSurname(), accountMap);
            }
        };
    }


    @Override
    public Person createPerson(String name, String surname, String passport) throws RemoteException {
        final Person person = new RemotePerson(passport, name, surname, this);
        if (persons.putIfAbsent(passport, person) == null) {
            accounts.put(passport, new ConcurrentHashMap<>());
            UnicastRemoteObject.exportObject(person, port);
            return person;
        } else {
            Person remotePerson = getPerson(passport, PersonType.REMOTE);
            if (remotePerson != null && remotePerson.getName().equals(name)
                    && remotePerson.getSurname().equals(surname)) {
                return remotePerson;
            }
        }
        return null;
    }

    private static String getAccountId(String passport, String id) {
        return String.format("%s:%s", passport, id);
    }
}
