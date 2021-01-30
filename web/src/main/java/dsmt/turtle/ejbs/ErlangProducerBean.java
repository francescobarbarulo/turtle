package dsmt.turtle.ejbs;

import javax.ejb.Stateless;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;

@Stateless(name = "ErlangProducerEJB")
public class ErlangProducerBean {
    private static final String nodeName = "java@master";
    private static final String cookie = "turtle";
    private static final String serverNodeName = "master_node@master";
    private static final String serverRegisteredName = "master";
    private static final AtomicInteger Id = new AtomicInteger(0);

    private final OtpNode node;

    public ErlangProducerBean() throws IOException {
        node = new OtpNode(nodeName, cookie);
    }

    public String send(String sessionId, String classFileName, String classFileContent, String testClassFileName, String testClassContent) {
        try {
            OtpMbox mbox = node.createMbox("default_mbox_" + Id.getAndIncrement());
            System.out.println("Created mailbox " + mbox.getName());

            OtpErlangTuple content = new OtpErlangTuple(new OtpErlangObject[]{
                    new OtpErlangString(classFileName),
                    new OtpErlangString(classFileContent),
                    new OtpErlangString(testClassFileName),
                    new OtpErlangString(testClassContent)
            });

            OtpErlangTuple payload = new OtpErlangTuple(new OtpErlangObject[]{
                    new OtpErlangString(sessionId),
                    content
            });

            OtpErlangTuple req = new OtpErlangTuple(new OtpErlangObject[]{mbox.self(), payload});

            mbox.send(serverRegisteredName, serverNodeName, req);
            System.out.println("Request sent: " + req.toString());

            OtpErlangObject res = mbox.receive(5000);
            mbox.close();
            
            if (res == null)
                return "Service not available";

            System.out.println("Response received on mailbox " + mbox.getName() + ": " + res.toString());
            return res.toString().replaceAll("\"", "");
        } catch (Exception e) {
            e.printStackTrace();
            return "Error";
        }
    }
}
