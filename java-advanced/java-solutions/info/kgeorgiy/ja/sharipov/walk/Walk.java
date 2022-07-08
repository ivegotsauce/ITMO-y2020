package info.kgeorgiy.ja.sharipov.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

public class Walk {

    private static class WalkException extends Exception {
        public WalkException() {
            super();
        }
    }

    private static Path getPath(int ind, String[] args, String message) throws WalkException {
        try {
            return Paths.get(args[ind]);
        } catch (InvalidPathException e) {
            System.err.println(message + e.getMessage());
            throw new WalkException();
        }
    }

    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.err.println("Expected two arguments: <input> <output>.");
            return;
        }
        Path inputPath;
        Path outputPath;
        try {
            inputPath = getPath(0, args, "Invalid input path: ");
            outputPath = getPath(1, args, "Invalid output path: ");
        } catch (WalkException e) {
            return;
        }

        try {
            if (outputPath.getParent() != null) {
                Files.createDirectories(outputPath.getParent());
            }
        } catch (IOException e) {
            System.err.println("Cannot create directory: " + e.getMessage());
            return;
        }
        try (BufferedReader reader = Files.newBufferedReader(inputPath, StandardCharsets.UTF_8)
        ) {
            try (BufferedWriter writer = Files.newBufferedWriter(outputPath, StandardCharsets.UTF_8)) {
                String path;
                MessageDigest digest = MessageDigest.getInstance("SHA-1");
                byte[] buffer = new byte[1 << 16];
                final String zeros = "0".repeat(40);
                while ((path = reader.readLine()) != null) {
                    try (InputStream is = Files.newInputStream(Paths.get(path))) {
                        digest.reset();
                        int c;
                        while ((c = is.read(buffer)) >= 0) {
                            digest.update(buffer, 0, c);
                        }
                        writer.write(HexFormat.of().formatHex(digest.digest()));
                        writer.write(" " + path);
                        writer.newLine();
                    } catch (IOException | IllegalArgumentException e) {
                        writer.write(zeros + " " + path);
                        writer.newLine();
                    }
                }
            } catch (IOException e) {
                System.err.println("Output is invalid. " + e.getMessage());
            }
        } catch (InvalidPathException e) {
            System.err.println("Invalid path of the file: " + e.getMessage());
        } catch (NoSuchFileException e) {
            System.err.println("No such file. " + e.getMessage());
        } catch (AccessDeniedException e) {
            System.err.println("Access to the file is denied. " + e.getMessage());
        } catch (IOException e) {
            System.err.println("Input is invalid. " + e.getMessage());
        } catch (NoSuchAlgorithmException e) {
            System.err.println("No such algorithm. " + e.getMessage());
        }
    }
}
