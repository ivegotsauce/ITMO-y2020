package info.kgeorgiy.ja.sharipov.concurrent;

import info.kgeorgiy.ja.sharipov.concurrent.ParallelMapperImpl;
import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class IterativeParallelism implements ScalarIP {

    private final ParallelMapper mapper;

    /**
     * Constructor, that creates new {@link IterativeParallelism} with given {@link ParallelMapper}.
     * @param mapper {@link ParallelMapper}.
     */
    public IterativeParallelism(ParallelMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Default constructor, that creates new {@link IterativeParallelism}.
     */
    public IterativeParallelism() {
        this.mapper = null;
    }

    private <T, E> E iterativeParallelism(int threads, List<? extends T> values,
                                          Function<Stream<? extends T>, E> func,
                                          Function<Stream<? extends E>, E> resultsJoiner) throws InterruptedException {
        final int size = values.size();
        threads = Math.max(1, Math.min(threads, size));
        final int step = size / threads;
        final int addOne = size % threads;
        int left = 0;
        final List<E> results;
        List<Thread> workers = new ArrayList<>();
        List<Stream<? extends T>> streams = new ArrayList<>();
        for (int i = 0; i < threads; i++) {
            final int right = left + step + (addOne > i ? 1 : 0);
            final Stream<? extends T> stream = values.subList(left, right).stream();
            streams.add(stream);
            left = right;
        }

        if (mapper != null) {
            results = mapper.map(func, streams);
        } else {
            results = new ArrayList<>(Collections.nCopies(threads, null));
            IntStream.range(0, threads).forEach(i -> {
                Stream<? extends T> stream = streams.get(i);
                Thread thread = new Thread(() -> results.set(i, func.apply(stream)));
                thread.start();
                workers.add(thread);
            });
            InterruptedException exception = null;
            for (Thread thread : workers) {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    exception = e;
                }
            }
            if (exception != null) {
                throw exception;
            }
        }
        return resultsJoiner.apply(results.stream());
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        Function<Stream<? extends T>, T> getMax = stream -> stream.max(comparator).orElse(null);
        return iterativeParallelism(threads, values, getMax, getMax);
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, Collections.reverseOrder(comparator));
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return iterativeParallelism(threads, values, stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(t -> t));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return iterativeParallelism(threads, values, stream -> stream.anyMatch(predicate),
                stream -> stream.anyMatch(t -> t));
    }
}
