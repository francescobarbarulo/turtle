package dsmt.turtle.ejbs;

import javax.annotation.PreDestroy;
import javax.ejb.*;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.concurrent.Future;

// https://javaee.github.io/tutorial/ejb-basicexamples003.html#_managing_concurrent_access_in_a_singleton_session_bean
@ConcurrencyManagement(ConcurrencyManagementType.BEAN)
@Singleton(name = "ErlangProducerEJB")
public class ErlangProducerBean {
    private static final String thisNodeName = "java_node@master";
    private static final String cookie = "turtle";
    private static final String serverNodeName = "master_node@master";
    private static final String serverRegisteredName = "master";
    private static final long timeout = 15000;

    private final OtpNode node;

    public ErlangProducerBean() throws IOException {
        node = new OtpNode(thisNodeName, cookie);
    }

    @Asynchronous
    public Future<String> send(String sessionId, String classFileName, String classFileContent, String testClassFileName, String testClassContent) {
        OtpMbox mbox;
        /*
            If multiple requests arrive concurrently the only part
            on which the threads must synchronize is the creation
            of a new mailbox starting from the shared node instance
         */
        synchronized (node) {
            mbox = node.createMbox();
        }
        System.out.println("Created mailbox " + mbox.self());

        try {
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

            /* actually send the request to the Erlang Master Node using OTP */
            mbox.send(serverRegisteredName, serverNodeName, req);
            System.out.println("[" + mbox.self() + "] Request sent");

            String msg;
            OtpErlangObject res = mbox.receive(timeout);

            if (res == null) {
                msg = "Something went wrong. Please retry.";
                System.out.println("Timeout expired on mailbox " + mbox.self());
            } else {
                msg = res.toString().replaceAll("\"", "");
                System.out.println("Response received on mailbox " + mbox.self());
            }

            mbox.close();

            Thread.sleep(10000);

            /* async return to servlet */
            return new AsyncResult<>(msg);

        } catch (Exception e) {
            e.printStackTrace();
            mbox.close();
            return new AsyncResult<>("Error");
        }
    }

    @PreDestroy
    private void terminate(){
        node.close();
    }
}
