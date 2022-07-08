package info.kgeorgiy.ja.sharipov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

public class HelloUDPServer implements HelloServer {
    private DatagramSocket socket;
    private ExecutorService workers;

    @Override
    public void start(int port, int threads) {
        try {
            socket = new DatagramSocket(port);
            workers = Executors.newFixedThreadPool(threads);
            IntStream.range(0, threads).forEach(thread ->
                    workers.submit(() -> {
                                try {
                                    while (!socket.isClosed()) {
                                        final DatagramPacket packet = UDPUtils.createPacket(socket);
                                        socket.receive(packet);
                                        final String request = new String(packet.getData(), packet.getOffset(),
                                                packet.getLength(), StandardCharsets.UTF_8);
                                        packet.setData(("Hello, " + request).getBytes(StandardCharsets.UTF_8));
                                        socket.send(packet);
                                    }
                                } catch (SocketException e) {
                                    System.err.println("No connection: " + e.getMessage());
                                } catch (IOException e) {
                                    //
                                }
                            }
                    ));
        } catch (SocketException e) {
            //
        }

    }

    @Override
    public void close() {
        socket.close();
        UDPUtils.terminateWorkers(workers);
    }

    /**
     * The entry point of the {@link HelloUDPServer}.
     * <p>
     * There should be two non-null arguments.
     * If there are two arguments {@code <port> <threads>}
     * {@link HelloUDPServer#start(int, int)} will be run.
     *
     * @param args given arguments.
     */
    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Expected two non-null arguments: <port> <threads>");
            return;
        }
        try {
            new HelloUDPServer().start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        } catch (NumberFormatException e) {
            System.err.println("All arguments should be numbers");
        }
    }
}
