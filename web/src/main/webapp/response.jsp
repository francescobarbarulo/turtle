<%--
  Created by IntelliJ IDEA.
  User: Francesco
  Date: 30/01/2021
  Time: 17:22
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Turtle</title>
</head>
<body>
    <h1>Files received at the server</h1>

    <ul>
    <%
        String[] files = (String[]) request.getAttribute("files");
        for (String name: files) {
            out.write("<li>" + name + "</li>");
        }
    %>
    </ul>

    <h3>Test result</h3>
    <pre>${result}</pre>

    <p><a href="${pageContext.request.contextPath}">Make a new test</a></p>
</body>
</html>
