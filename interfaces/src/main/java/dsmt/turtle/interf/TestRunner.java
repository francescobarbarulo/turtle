package dsmt.turtle.interf;

import javax.ejb.Remote;

@Remote
public interface TestRunner {
    String doTest(String classFileName, String classFileContent, String testClassFileName, String testClassContent);
}
