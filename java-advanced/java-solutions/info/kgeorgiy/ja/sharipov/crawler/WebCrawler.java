package info.kgeorgiy.ja.sharipov.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;

public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final Map<String, IOException> errors = new ConcurrentHashMap<>();
    private final Phaser phaser = new Phaser(1);
    private final Set<String> downloaded = ConcurrentHashMap.newKeySet();
    private final Set<String> cached = ConcurrentHashMap.newKeySet();

    /**
     * Constructor, that creates new {@link WebCrawler} with given {@link Downloader}, number of downloaders,
     * number of extractors, perHost.
     * @param downloader {@link Downloader}
     * @param downloaders number of downloaders
     * @param extractors number of extractors
     * @param perHost max number of pages that can be simultaneously downloaded from one host.
     */
    @SuppressWarnings("unused")
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
    }

    private void downloadAndExtractLinks(String url, Set<String> extractedLinks, boolean notLast) {
        downloaders.submit(() -> {
            try {
                Document document = downloader.download(url);
                downloaded.add(url);
                if (notLast) {
                    phaser.register();
                    extractors.submit(() -> {
                        try {
                            for (String link : document.extractLinks()) {
                                if (!cached.contains(link)) {
                                    cached.add(link);
                                    extractedLinks.add(link);
                                }
                            }
                        } catch (IOException e) {
                            errors.put(url, e);
                        } finally {
                            phaser.arriveAndDeregister();
                        }
                    });
                }
            } catch (IOException e) {
                errors.put(url, e);
            } finally {
                phaser.arriveAndDeregister();
            }
        });
    }

    @Override
    public Result download(String url, int depth) {
        final Set<String> extractedLinks = ConcurrentHashMap.newKeySet();
        extractedLinks.add(url);
        for (int i = 0; i < depth; i++) {
            cached.addAll(extractedLinks);
            final Set<String> listLinks = Set.copyOf(extractedLinks);
            extractedLinks.clear();
            for (String link : listLinks) {
                phaser.register();
                downloadAndExtractLinks(link, extractedLinks,  i != depth - 1);
            }
            phaser.arriveAndAwaitAdvance();
        }
        return new Result(new ArrayList<>(downloaded), errors);
    }

    /**
     * The entry point of the {@link WebCrawler}.
     *
     * There should be 1-5 non-null arguments: url [depth [downloaders [extractors [perHost]]]]
     *
     * {@link WebCrawler#download(String, int)} will be run.
     *
     * @param args given arguments.
     */
    public static void main(String[] args) {
        String arguments = "url [depth [downloaders [extractors [perHost]]]]";
        if (args == null || args.length < 1) {
            System.err.println("There should be at least 1 argument: " + arguments);
            return;
        }
        for (String arg : args) {
            if (Objects.isNull(arg)) {
                System.err.println("Non-null arguments required: " + arguments);
                return;
            }
        }
        try {
            Crawler crawler = new WebCrawler(new CachingDownloader(), getArg(2, args),
                    // :NOTE: > 1 default
                    getArg(3, args), getArg(4, args));
            crawler.download(args[0], getArg(1, args));
        } catch (IOException e) {
            System.err.println("Crawler cannot be initialized: " + e.getMessage());
        } catch (NumberFormatException e) {
            System.err.println("Invalid arguments, expected numbers, but were: " + e.getMessage());
        }
    }

    private static int getArg(int i, String[] args) throws NumberFormatException {
        if (i < args.length) {
            return Integer.parseInt(args[i]);
        }
        return 1;
    }

    @Override
    public void close() {
        downloaders.shutdown();
        extractors.shutdown();
        boolean downloadersTerminated = false;
        boolean extractorsTerminated = false;
        try {
            downloadersTerminated = downloaders.awaitTermination(10, TimeUnit.MINUTES);
            extractorsTerminated = extractors.awaitTermination(10, TimeUnit.MINUTES);
        } catch (InterruptedException ignored) {
            //
        }
        if (!downloadersTerminated || !extractorsTerminated) {
            System.err.println("Executors weren't shutdown: 10 minutes timeout elapsed");
        }
    }
}
