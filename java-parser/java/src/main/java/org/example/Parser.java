package org.example;

import com.github.javaparser.StaticJavaParser;
import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Parser {

    // ============== JNI ==================

    static {
        String pwd = Paths.get("").toAbsolutePath().toString();
        System.load(pwd + "/target/debug/libreader.so"); // NOTE : to change to release
    }

    public static void main(String[] args) throws Exception {
        rustFunc();
    }

    public static native void rustFunc();

    // Used to get the exact header signature of the function
    // Isn't need to run
    public static native ClassOrInterfaceDeclaration getClassRust(String fileName, String className);


    // ============== JAVA PARSER ==================

    public static ClassOrInterfaceDeclaration getClass(String fileName, String className) throws FileNotFoundException, IOException {
        CompilationUnit compilationUnit = StaticJavaParser.parse(Paths.get(fileName));
        return compilationUnit
                .getClassByName(className)
                .orElseThrow(IllegalArgumentException::new);
    }

    public static List<MethodDeclaration> getMethod(ClassOrInterfaceDeclaration c) {
        return c.getMethods();
    }

    public static BlockStmt getMethodBody(MethodDeclaration method) {
        return method.getBody().orElseThrow(IllegalArgumentException::new);
    }

    public static List<FieldDeclaration> getFields(ClassOrInterfaceDeclaration c) {
        return c.getFields();
    }

    // ============== UTILITY FUNCTION ==================

    public static void callback() {
        System.out.println("Called From JNI");
    }

    public static String ls(String p) throws IOException {
        Stream<Path> files = Files.list(Paths.get(p));
        List<String> fileList = files
            .map(Path::getFileName)
            .map(Path::toString)
            .collect(Collectors.toList());
        files.close();
        return String.join(",", fileList);
    }
}
