package dsmt.turtle.ejbs;

import dsmt.turtle.interf.TestRunner;

import javax.ejb.Stateless;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;


@Stateless(name = "TestRunnerEJB")
public class TestRunnerBean implements TestRunner {
    private final String PATH = "/tmp/";
    private final ProcessBuilder pb = new ProcessBuilder().directory(Paths.get(PATH).toFile());

    public TestRunnerBean() {
    }

    @Override
    public String doTest(String classFileName, String classFileContent, String testClassFileName, String testClassContent) {
        try {
            saveFileLocally(classFileName, classFileContent);
            saveFileLocally(testClassFileName, testClassContent);

            pb.command("javac", "-cp", ".:junit-4.13.1.jar", classFileName, testClassFileName);
            Process compile = pb.start();

            if (compile.waitFor() != 0){
                return "Compile error";
            }

            pb.command("java", "-cp", ".:junit-4.13.1.jar:hamcrest-core-1.3.jar", "org.junit.runner.JUnitCore", testClassFileName.split("\\.")[0]);
            Process execute = pb.start();

            String message = new BufferedReader(new InputStreamReader(execute.getInputStream())).lines().collect(Collectors.joining("\n"));

            execute.waitFor();
            return message;

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
            return "Error";
        }
    }

    private void saveFileLocally(String fileName, String content) throws IOException {
        Path path = Paths.get(PATH + fileName);
        if (!Files.exists(path))
            path = Files.createFile(Paths.get(PATH + fileName));
        Files.write(path, content.getBytes());
    }
}
