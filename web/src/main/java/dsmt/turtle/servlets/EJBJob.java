package dsmt.turtle.servlets;

import dsmt.turtle.interf.TestRunner;

import javax.ejb.EJB;
import javax.servlet.ServletException;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;
import java.io.*;
import java.nio.file.Paths;
import java.util.stream.Collectors;

@WebServlet(name = "EJBJob", urlPatterns = {"/"})
@MultipartConfig
public class EJBJob extends HttpServlet {
    @EJB
    private TestRunner testRunner;

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Part classFile = request.getPart("class");
        Part testClassFile = request.getPart("testclass");

        String classFileName = Paths.get(classFile.getSubmittedFileName()).getFileName().toString();
        String testClassFileName = Paths.get(testClassFile.getSubmittedFileName()).getFileName().toString();

        PrintWriter out = response.getWriter();

        String result = testRunner.doTest(classFileName, getContent(classFile), testClassFileName, getContent(testClassFile));

        response.setContentType("text/html");
        out.println("<h3>Uploaded</h3>" +
                "<ul>" +
                "<li>" + classFileName + "</li>" +
                "<li>" + testClassFileName + "</li>" +
                "</ul>" +
                "<h3>Test result</h3>" +
                "<pre>" + result + "</pre>");
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        request.getRequestDispatcher("/index.jsp").forward(request, response);
    }

    private String getContent(Part part) throws IOException {
        InputStream is = part.getInputStream();
        return new BufferedReader(new InputStreamReader(is))
                .lines()
                .collect(Collectors.joining("\n"));
    }
}
