package info.kgeorgiy.ja.sharipov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Map;

public interface Person extends Remote {
    /** Returns person's name. */
    String getName() throws RemoteException;

    /** Returns person's surname. */
    String getSurname() throws RemoteException;

    /** Returns person's passport identifier. */
    String getPassport() throws RemoteException;

    Account getAccount(String id) throws RemoteException;
}
