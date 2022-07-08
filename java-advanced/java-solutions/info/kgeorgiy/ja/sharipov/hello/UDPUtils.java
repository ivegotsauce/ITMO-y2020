package info.kgeorgiy.ja.sharipov.hello;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class UDPUtils {

    public static DatagramPacket createPacket(DatagramSocket socket) throws SocketException {
        return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
    }

    public static void terminateWorkers(ExecutorService workers) {
        workers.shutdown();
        boolean terminated = false;
        try {
            terminated = workers.awaitTermination(10, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            //
        }
        if (!terminated) {
            System.err.println("Executors weren't shutdown: 10 minutes timeout elapsed");
        }
    }
}
