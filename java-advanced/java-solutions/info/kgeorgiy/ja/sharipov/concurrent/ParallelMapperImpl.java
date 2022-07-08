package info.kgeorgiy.ja.sharipov.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {

    private final int MAXSIZE = 1000;
    private final List<Thread> workers;
    private final Queue<Runnable> tasks;

    /**
     * Constructor, that creates new ParallelMapperImpl with given number of {@code threads}.
     *
     * @param threads number of threads
     */
    public ParallelMapperImpl(int threads) {
        workers = new ArrayList<>();
        tasks = new ArrayDeque<>();
        for (int i = 0; i < threads; i++) {
            Thread thread = new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        Runnable task;
                        synchronized (tasks) {
                            while (tasks.isEmpty()) {
                                tasks.wait();
                            }
                            task = tasks.poll();
                            tasks.notifyAll();
                        }
                        task.run();
                    }
                } catch (InterruptedException e) {
                    //
                }
            });
            thread.start();
            workers.add(thread);
        }
    }

    /**
     * A counter that can count some value using multiple threads.
     */
    private static class Counter {
        private int cnt;

        /**
         * Constructor, that creates new Counter with given {@code cnt} value.
         *
         * @param cnt Counter initial value.
         */
        public Counter(int cnt) {
            this.cnt = cnt;
        }

        /**
         * Decrements the counter.
         */
        public synchronized void decrement() {
            cnt--;
            if (cnt == 0) {
                notify();
            }
        }

        /**
         * Waits until the counter value is 0.
         *
         * @throws InterruptedException if the thread was interrupted.
         */
        public synchronized void waitAll() throws InterruptedException {
            while (cnt != 0) {
                wait();
            }
        }
    }

    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        ArrayList<R> results = new ArrayList<>(Collections.nCopies(args.size(), null));
        Counter counter = new Counter(args.size());
        for (int i = 0; i < args.size(); i++) {
            final int pos = i;
            Runnable task = () -> {
                results.set(pos, f.apply(args.get(pos)));
                counter.decrement();
            };
            synchronized (tasks) {
                while (tasks.size() == MAXSIZE) {
                    tasks.wait();
                }
                tasks.add(task);
                tasks.notifyAll();
            }
        }
        counter.waitAll();
        return results;
    }

    @Override
    public void close() {
        for (Thread thread : workers) {
            thread.interrupt();
        }
        for (Thread thread : workers) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                //
            }
        }
    }
}
