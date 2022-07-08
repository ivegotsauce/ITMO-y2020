package info.kgeorgiy.ja.sharipov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

public class HelloUDPClient implements HelloClient {

    private void makeRequest(InetSocketAddress socketAddress, String prefix, int thread, int requests) {
        try (DatagramSocket datagramSocket = new DatagramSocket()) {
            datagramSocket.setSoTimeout(100);
            IntStream.range(0, requests).forEach(i ->
            {
                final String message = prefix + thread + '_' + i;
                System.out.println("sending: " + message);
                final byte[] data = message.getBytes(StandardCharsets.UTF_8);
                final DatagramPacket packet = new DatagramPacket(data, data.length, socketAddress);
                while (!datagramSocket.isClosed()) {
                    try {
                        datagramSocket.send(packet);
                        final DatagramPacket receivePacket = UDPUtils.createPacket(datagramSocket);
                        datagramSocket.receive(receivePacket);
                        String received = new String(receivePacket.getData(), receivePacket.getOffset(),
                                receivePacket.getLength(), StandardCharsets.UTF_8);
                        if (received.equals("Hello, " + message)) {
                            System.out.println("received: " + received);
                            break;
                        }
                    } catch (IOException e) {
                        //
                    }
                }
            });
        } catch (SocketException e) {
            //
        }
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        final ExecutorService workers = Executors.newFixedThreadPool(threads);
        try {
            final InetSocketAddress socketAddress = new InetSocketAddress(InetAddress.getByName(host), port);
            IntStream.range(0, threads).forEach(i ->
                    workers.submit(() -> makeRequest(socketAddress, prefix, i, requests)));

        } catch (UnknownHostException e) {
            //
        }
        UDPUtils.terminateWorkers(workers);
    }

    /**
     * The entry point of the {@link HelloUDPClient}.
     * <p>
     * There should be five non-null arguments.
     * If there are two arguments {@code <host> <port> <prefix> <threads> <requests>}
     * {@link HelloUDPClient#run(String, int, String, int, int)} will be run.
     *
     * @param args given arguments.
     */
    public static void main(String[] args) {
        if (args == null || args.length != 5) {
            System.err.println("Five arguments required: <host> <port> <prefix> <threads> <requests>");
            return;
        }
        for (String arg : args) {
            if (arg == null) {
                System.err.println("All arguments should be non-null");
                return;
            }
        }
        try {
            new HelloUDPClient().run(args[0], Integer.parseInt(args[1]), args[2],
                    Integer.parseInt(args[3]), Integer.parseInt(args[4]));
        } catch (NumberFormatException e) {
            System.err.println("Argument 2, 4 and 5 should be numbers");
        }
    }
}
