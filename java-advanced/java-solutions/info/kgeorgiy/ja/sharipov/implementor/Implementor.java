package info.kgeorgiy.ja.sharipov.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.*;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.StringJoiner;
import java.util.function.Function;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;

/**
 * Interface implementor.
*/
public class Implementor implements JarImpler {

    /**
     * Separates lines in the generated class.
     */
    private static final String LN = System.lineSeparator();
    /**
     * Shows the end of a statement in the generated class.
     */
    private static final String SEMICOLON = ";";
    /**
     * Separates words in the generated class.
     */
    private static final String SPACE = "    ";
    /**
     * Aligns the code of the generated class.
     */
    private static final String TAB = "\t";
    /**
     * Shows the beginning of the body in the generated class.
     */
    private static final String OPENING_BRACE = "{";
    /**
     * Shows the end of the body in the generated class.
     */
    private static final String CLOSING_BRACE = "}";
    /**
     * Used to supply parameters to a method in a generated class.
     */
    private static final String OPENING_PARENTHESE = "(";
    /**
     * Used to supply parameters to a method in a generated class.
     */
    private static final String CLOSING_PARENTHESE = ")";


    /**
     * A simple visitor of files that deletes a file tree.
     */
    private static final SimpleFileVisitor<Path> DELETE_VISITOR = new SimpleFileVisitor<>() {
        /**
         * Removes the visited file and continues.
         * @param file a {@link Path} reference to the file.
         * @param attrs the file's basic attributes.
         * @return {@link FileVisitResult#CONTINUE} if the deletion was successful.
         * @throws IOException if the deletion failed.
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Removes visited directory and continues.
         * @param dir a {@link Path} reference to the file.
         * @param exc null if the iteration of the directory completes without an error;
         *            otherwise the I/O exception that caused the iteration of the directory to complete prematurely.
         * @return {@link FileVisitResult#CONTINUE} if the deletion was successful.
         * @throws IOException if the deletion failed.
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };

    /**
     * Generates a header with the package name of the class being produced.
     * @param token type token to create header for.
     * @return {@link String} text of the class header.
     */
    private static String getClassHeader(Class<?> token) {
        // :NOTE: introduce variable
        return !token.getPackageName().equals("") ? "package " + token.getPackageName() + SEMICOLON + LN.repeat(2) : "";
    }

    /**
     * Generates a body for the class being produced.
     * The body includes:
     * <ul>
     *     <li>Class name</li>
     *     <li>The name of interface being implemented</li>
     *     <li>Methods of the class</li>
     * </ul>
     * @param token type token to create body for.
     * @return {@link String} text of the class body.
     */
    private static String getClassBody(Class<?> token) {
        return "public class " + getClassName(token) + " implements " + token.getCanonicalName() +
                SPACE + OPENING_BRACE + LN +
                getClassMethods(token) +
                CLOSING_BRACE;
    }

    /**
     * Generates the methods for the class being produced.
     * @param token type token to create methods for.
     * @return {@link String} text of the class methods.
     */
    private static String getClassMethods(Class<?> token) {
        StringBuilder sb = new StringBuilder();
        for (Method method : Arrays.stream(token.getMethods()).filter(method
                -> Modifier.isAbstract(method.getModifiers())).toList()) {
            sb.append(getMethod(method)).append(LN);
        }
        return sb.toString();
    }

    /**
     * Generates a single method body for the class being produced.
     * @param method method to generate code for.
     * @return {@link String} text of the class method.
     */
    private static String getMethod(Method method) {
        Class<?> returnType = method.getReturnType();
        Class<?>[] exceptionTypes = method.getExceptionTypes();
        return TAB + "public " + returnType.getCanonicalName() + SPACE +
                method.getName() + OPENING_PARENTHESE + getMethodParameters(method) +
                CLOSING_PARENTHESE + SPACE +
                (exceptionTypes.length > 0 ? "throws " + getMethodExceptions(exceptionTypes) + SPACE : "") +
                OPENING_BRACE + LN + TAB.repeat(2) +
                "return " + getDefaultReturnValue(returnType) + SEMICOLON +
                LN + TAB + CLOSING_BRACE;
    }

    /**
     * Generates {@link String} with the names of the exceptions separated by a comma and whitespace.
     * @param exceptionTypes an array of exception types.
     * @return {@link String} text with exceptions names.
     */
    private static String getMethodExceptions(Class<?>[] exceptionTypes) {
        StringJoiner stringJoiner = new StringJoiner(", ");
        for (Class<?> exceptionType : exceptionTypes) {
            stringJoiner.add(exceptionType.getCanonicalName());
        }
        return stringJoiner.toString();
    }

    /**
     * Generates {@link String} with the types and names of the parameters of the method being produced.
     * Parameters are separated by comma and whitespace.
     * @param method method to generate parameters for.
     * @return {@link String} text with method parameters.
     */
    private static String getMethodParameters(Method method) {
        StringJoiner stringJoiner = new StringJoiner(", ");
        for (Parameter parameter : method.getParameters()) {
            stringJoiner.add(getParameter(parameter));
        }
        return stringJoiner.toString();
    }

    /**
     * Generates the {@link String} with the type and name of single parameter.
     * @param parameter method parameter to create code for.
     * @return {@link String} text with the type and name of the parameter.
     */
    private static String getParameter(Parameter parameter) {
        return parameter.getType().getCanonicalName() + SPACE + parameter.getName();
    }

    /**
     * Returns the {@link String} value of the default value of the given type:
     * <ul>
     *     <li>"" if the type is String</li>
     *     <li>empty {@link String} if the type is void</li>
     *     <li>"false" if the type is bool</li>
     *     <li>"0" if type is a number or char</li>
     * </ul>
     * @param returnType type to create default value {@link String} for.
     * @return {@link String} value of the default value.
     */
    private static String getDefaultReturnValue(Class<?> returnType) {
        if (returnType.equals(String.class)) {
            return "\"\"";
        } else if (returnType.equals(Void.TYPE)) {
            return "";
        } else if (returnType.equals(Boolean.TYPE)) {
            return "false";
        } else if (returnType.isPrimitive()) {
            return "0";
        }
        return "null";
    }

    /**
     * Generates the name of the class implementing interface specified by provided {@code token}.
     * Class file name contains class name generated by {@link #getClassName} and {@code .java} suffix.
     * @param token type token to create new file name for.
     * @return {@link String} name of the class file.
     */
    private static String getFileName(Class<?> token) {
        return getClassName(token) + ".java";
    }

    /**
     * Generates the name of the class implementing interface specified by provided {@code token}.
     * Class name is the same as type token class name with {@code Impl} suffix added.
     * @param token type token to create new class name for.
     * @return {@link String} name of the class being produced.
     */
    private static String getClassName(Class<?> token) {
        return token.getSimpleName() + "Impl";
    }

    /**
     * Returns the {@link Path} to the file or directory, depending on some function.
     * @param token the argument of the {@code function}.
     * @param root root directory.
     * @param function function applying to the {@code token}.
     * @return {@link Path} to the file or directory.
     */
    private static Path getRootImpl(Class<?> token, Path root, Function<Class<?>, String> function) {
        return root.resolve(token.getPackageName().replace(".", File.separator))
                .resolve(function.apply(token));
    }

    /**
     * Returns the {@link Path} to the file where the implementation of the interface specified by provided {@code token}
     * should go.
     * @param token type token to create implementation file for.
     * @param root root directory.
     * @return {@link Path} to the file for interface implementation.
     */
    private static Path getRoot(Class<?> token, Path root) {
        return getRootImpl(token, root, Implementor::getFileName);
    }


    /**
     * Produces code implementing interface specified by provided {@code token}.
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws ImplerException when implementation cannot be
     * generated.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        // :NOTE: check interface
        if (token.isPrimitive() || token.isArray() || token.equals(String.class) ||
                Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Cannot implement this class: " + token.getCanonicalName());
        }
        root = getRoot(token, root);
        try {
            if (root.getParent() != null) {
                Files.createDirectories(root.getParent());
            }
        } catch (IOException e) {
            throw new ImplerException("Cannot create directory: " + e.getMessage(), e);
        }
        try (BufferedWriter writer = Files.newBufferedWriter(root, StandardCharsets.UTF_8)) {
            writer.write(encode(getClassHeader(token) + getClassBody(token) + LN));
        } catch (IOException e) {
            throw new ImplerException("Cannot write in this file: " + e.getMessage(), e);
        }
    }

    /**
     * Compiles the class specified by provided {@code token} and {@code root}.
     * @param token type token which implementation to be compiled.
     * @param root root directory.
     * @throws ImplerException if there was an error while compiling.
     */
    private static void compile(Class<?> token, final Path root) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler, include tools.jar to classpath");
        }
        final String classpath = root + File.pathSeparator + getClassPath(token);
        final String filePath = getRoot(token, root).toString();
        final String[] args = Stream.of(filePath, "-cp", classpath).toArray(String[]::new);
        final int exitCode = compiler.run(null, null, null, args);
        if (exitCode != 0) {
            throw new ImplerException("Failing while compile: exit code is " + exitCode);
        }
    }

    /**
     * Returns the path to the class specified by provided {@code token}.
     * @param token token to get path for.
     * @return {@link String} path to the class.
     * @throws ImplerException if the class token has incorrect URL.
     */
    private static String getClassPath(Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException("Failing while compile: " + e.getMessage(), e);
        }
    }

    /**
     * Produces <var>.jar</var> file implementing class or interface specified by provided <var>token</var>.
     * <var>.jar</var> file contains the code, produced by {@link #implement(Class, Path)}
     * @param token type token to create implementation for.
     * @param jarFile target <var>.jar</var> file.
     * @throws ImplerException when implementation cannot be generated.
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        try {
            if (token == null || jarFile == null) {
                throw new ImplerException("Every argument should not be null.");
            }
            Path root = Files.createTempDirectory(jarFile.getParent().toAbsolutePath(), "Temp");
            implement(token, root);
            compile(token, root);
            Manifest manifest = new Manifest();
            manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
            Path path = getRootImpl(token, root, t -> getClassName(t) + ".class");
            try (JarOutputStream outputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
                String name = String.join("/", token.getPackageName().replace(".", "/"),
                        getClassName(token) + ".class");
                outputStream.putNextEntry(new JarEntry(name));
                Files.copy(path, outputStream);
            } catch (IOException e) {
                throw new ImplerException("Cannot write in this jar file: " + e.getMessage(), e);
            } finally {
                clean(root);
            }
        } catch (IOException e) {
            throw new ImplerException("Cannot create jar file directory: " + e.getMessage(), e);
        }
    }

    /**
     * Recursively deletes everything in the provided directory and then deleting this directory.
     * @param root root directory.
     * @throws IOException if the deleting failed.
     */
    private static void clean(final Path root) throws IOException {
        if (Files.exists(root)) {
            Files.walkFileTree(root, DELETE_VISITOR);
        }
    }

    /**
     * Converts String to UTF-8.
     * @param str {@link String} to encode.
     * @return encoded {@link String} {@code str}.
     */
    private static String encode(String str) {
        StringBuilder sb = new StringBuilder();
        for (char c : str.toCharArray()) {
            sb.append(String.format("\\u%04X",(int) c));
        }
        return sb.toString();
    }

    /**
     * The entry point of the {@link Implementor}.
     *
     * There should be two or three non-null arguments.
     * If there are two arguments {@code <class name> <output root>} {@link Implementor#implement(Class, Path)}
     * will be run.
     * If there are three arguments {@code -jar <class name> <file.jar>} {@link Implementor#implementJar(Class, Path)}
     * will be run.
     *
     * @param args given arguments.
     */
    public static void main(String[] args) {
        if (args == null || !(args.length == 2 || args.length == 3)) {
            System.err.println("Expected arguments: <class name> <output root> or -jar <class name> <jar file path>.");
            return;
        }
        for (String arg : args) {
            if (arg == null) {
                System.err.println("Every argument should not be null");
            }
        }
        JarImpler implementor = new Implementor();
        try {
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            } else {
                if (!args[0].equals("-jar") || !args[2].endsWith(".jar")) {
                    System.err.println("Expected arguments: -jar <class name> <jar file path>");
                    return;
                }
                implementor.implementJar(Class.forName(args[1]), Path.of(args[2]));
            }
        } catch (ClassNotFoundException e) {
            System.err.println("Class not found: " + e.getMessage());
        } catch (ImplerException e) {
            System.err.println("Failing while implementation: " + e.getMessage());
        }
    }
}
