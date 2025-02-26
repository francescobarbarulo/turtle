package dsmt.turtle.websockets;

import javax.websocket.OnClose;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// https://javaee.github.io/tutorial/websocket011.html
@ServerEndpoint("/update/{httpSession}")
public class UpdateEndpoint {
    private final static Map<String, Session> sessions = new ConcurrentHashMap<>();

    @OnOpen
    public void onOpen(@PathParam("httpSession") String httpSession, Session session) {
        System.out.println("Opened socket with " + httpSession);
        /* associate http session with websocket session */
        sessions.put(httpSession, session);
    }

    @OnClose
    public void onClose(@PathParam("httpSession") String httpSession) {
        System.out.println("Closing socket " + httpSession);
        sessions.remove(httpSession);
    }

    public static void sendAsyncResponse(String httpSession, String text) throws IOException {
        /* retrieve websocket session from http session, and send reply */
        Session session = sessions.get(httpSession);
        if (session == null) {
            System.out.println("Session " + httpSession + " not found");
            return;
        }

        session.getBasicRemote().sendText(text);
    }
}
