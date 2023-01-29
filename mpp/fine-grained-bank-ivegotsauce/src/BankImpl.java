import java.util.concurrent.locks.ReentrantLock;
/**
 * Bank implementation.
 *
 * <p>:TODO: This implementation has to be made thread-safe.
 *
 * @author : Sharipov Aleksandr
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long getAmount(int index) {
        Account account = accounts[index];
        try {
            account.lock();
            return account.amount;
        } finally {
            account.unlock();
        }
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long getTotalAmount() {
        long sum = 0;
        try {
            for (Account account : accounts) {
                account.lock();
            }
            for (Account account : accounts) {
                sum += account.amount;
            }
            return sum;
        } finally {
            for (int i = accounts.length - 1; i >= 0; i--) {
                accounts[i].unlock();
            }
        }
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        try {
            account.lock();
            if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT)
                throw new IllegalStateException("Overflow");
            account.amount += amount;
            return account.amount;
        } finally {
            account.unlock();
        }
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        try {
            account.lock();
            if (account.amount - amount < 0)
                throw new IllegalStateException("Underflow");
            account.amount -= amount;
            return account.amount;
        } finally {
            account.unlock();
        }
    }

    /**
     * <p>:TODO: This method has to be made thread-safe.
     */
    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        Account from = accounts[fromIndex];
        Account to = accounts[toIndex];
        try {
            if (fromIndex < toIndex) {
                from.lock();
                to.lock();
            } else {
                to.lock();
                from.lock();
            }
            if (amount > from.amount) {
                throw new IllegalStateException("Underflow");
            } else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT) {
                throw new IllegalStateException("Overflow");
            }
            from.amount -= amount;
            to.amount += amount;
        } finally {
            if (fromIndex < toIndex) {
                to.unlock();
                from.unlock();
            } else {
                from.unlock();
                to.unlock();
            }
        }
    }

    /**
     * Private account data structure.
     */
    static class Account {
        ReentrantLock lock = new ReentrantLock();

        void lock() {
            lock.lock();
        }

        void unlock() {
            lock.unlock();
        }
        /**
         * Amount of funds in this account.
         */
        long amount;
    }
}
