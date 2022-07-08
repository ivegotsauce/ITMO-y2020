package info.kgeorgiy.ja.sharipov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {

    enum PersonType {
        REMOTE,
        LOCAL
    }

    /**
     * Creates a new account with specified identifier if it is not already exists.
     * @param id account id
     * @return created or existing account.
     */
    Account createAccount(String passport, String id) throws RemoteException;

    /**
     * Returns account by identifier.
     * @param id account id
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    Account getAccount(String passport, String id) throws RemoteException;

    Person getPerson(String passport, PersonType type) throws RemoteException;

    Person createPerson(String name, String surname, String passport) throws RemoteException;
}
