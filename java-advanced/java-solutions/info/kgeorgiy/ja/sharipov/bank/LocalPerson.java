package info.kgeorgiy.ja.sharipov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.Map;

public class LocalPerson implements Person, Serializable {
    private final String passport;
    private final String name;
    private final String surname;
    private final Map<String, Account> accounts;

    public LocalPerson(String passport, String name, String surname, Map<String, Account> accounts) {
        this.passport = passport;
        this.name = name;
        this.surname = surname;
        this.accounts = accounts;
    }

    // :NOTE: copypaste
    //------
    @Override
    public String getName() throws RemoteException {
        return name;
    }

    @Override
    public String getSurname() throws RemoteException {
        return surname;
    }

    @Override
    public String getPassport() throws RemoteException {
        return passport;
    }
    //------

    @Override
    public Account getAccount(String id) throws RemoteException {
        return getAccounts().get(String.format("%s:%s", passport, id));
    }

    public Map<String, Account> getAccounts() {
        return accounts;
    }

}
