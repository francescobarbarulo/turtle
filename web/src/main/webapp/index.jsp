<%--
  Created by IntelliJ IDEA.
  User: Francesco
  Date: 25/01/2021
  Time: 11:29
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Run JUnit Tests</title>
</head>
<body>
    <h1>Run JUnit Tests</h1>
    <h4>Upload the module class and its test class</h4>
    <form action="./" method="post" enctype="multipart/form-data">
        <p>
            <label for="class">Class:</label>
            <input type="file" id="class" name="class" />
        </p>
        <p>
            <label for="testclass">Test:</label>
            <input type="file" id="testclass" name="testclass" />
        </p>
        <button type="submit">Submit</button>
    </form>
</body>
</html>
