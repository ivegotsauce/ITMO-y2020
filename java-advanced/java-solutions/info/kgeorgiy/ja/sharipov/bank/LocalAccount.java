package info.kgeorgiy.ja.sharipov.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

public class LocalAccount implements Account, Serializable {
    private final String id;
    private int amount;

    public LocalAccount(String id, int amount) {
        this.id = id;
        this.amount = amount;
    }

    @Override
    public String getId() throws RemoteException {
        return id;
    }

    @Override
    public synchronized int getAmount() throws RemoteException {
        return amount;
    }

    @Override
    public synchronized void setAmount(int amount) throws RemoteException {
        this.amount = amount;
    }

    @Override
    public synchronized void addAmount(int diff) {
        this.amount += diff;
    }
}
