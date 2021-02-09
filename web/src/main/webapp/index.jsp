<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Turtle</title>
</head>
<body>
    <h1>Run JUnit Tests</h1>
    <h4>Upload the module class and its test class</h4>
    <form action="${pageContext.request.contextPath}" method="post" enctype="multipart/form-data">
        <p>
            <label for="class">Class:</label>
            <input type="file" id="class" name="class" required />
        </p>
        <p>
            <label for="testclass">Test:</label>
            <input type="file" id="testclass" name="testclass" required />
        </p>
        <button type="submit">Submit</button>
    </form>
</body>
</html>
