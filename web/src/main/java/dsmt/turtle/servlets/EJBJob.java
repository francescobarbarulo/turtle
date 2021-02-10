package dsmt.turtle.servlets;

import dsmt.turtle.ejbs.ErlangProducerBean;
import dsmt.turtle.websockets.UpdateEndpoint;

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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

@WebServlet(name = "EJBJob", urlPatterns = {"/"})
@MultipartConfig
public class EJBJob extends HttpServlet {
    @EJB
    private ErlangProducerBean erlangProducer;

    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        response.setContentType("text/html");
        PrintWriter out = response.getWriter();

        String userAgent = request.getHeader("User-Agent");

        String sessionId = request.getSession().getId();
        Part classFile = request.getPart("class");
        Part testClassFile = request.getPart("testclass");

        if (classFile == null || testClassFile == null) {
            // An incorrect self-made post request has been issued
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return;
        }

        if (classFile.getSize() == 0 || testClassFile.getSize() == 0){
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            out.println("No file received");
            return;
        }

        String classFileName = Paths.get(classFile.getSubmittedFileName()).getFileName().toString();
        String testClassFileName = Paths.get(testClassFile.getSubmittedFileName()).getFileName().toString();

        /* submit request to the Java-embedded master node */
        Future<String> result = erlangProducer.send(sessionId, classFileName, getContent(classFile), testClassFileName, getContent(testClassFile));

        out.println("OK");
        out.flush();        // this terminates the reply to the browser

        try {
            if (userAgent.contains("curl")) {   /* if request comes form curl, then blocking get */
                out.println(result.get());
            } else {    /* else if request comes from browser, reply through websocket */
                System.out.println("Waiting for web async response");
                UpdateEndpoint.sendAsyncResponse(sessionId, result.get());
                System.out.println("Replying through web async response");
            }
        } catch (ExecutionException | InterruptedException e) {
            e.printStackTrace();
        }
    }

    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        /* reply with presentation page, properly formatted */
        response.setContentType("text/html");
        request.getRequestDispatcher("index.jsp").forward(request, response);
    }

    private String getContent(Part part) throws IOException {
        InputStream is = part.getInputStream();
        return new BufferedReader(new InputStreamReader(is))
                .lines()
                .collect(Collectors.joining("\n"));
    }
}
