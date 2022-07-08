package info.kgeorgiy.ja.sharipov.bank;

import java.rmi.RemoteException;
import java.util.Map;

public class RemotePerson implements Person {
    private final String passport;
    private final String name;
    private final String surname;
    private final Bank bank;

    public RemotePerson(String passport, String name, String surname, Bank bank) {
        this.passport = passport;
        this.name = name;
        this.surname = surname;
        this.bank = bank;
    }

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

    @Override
    public Account getAccount(String id) throws RemoteException {
        return bank.getAccount(getPassport(), id);
    }

}
