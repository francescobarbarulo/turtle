<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Turtle</title>
</head>
<body>
    <h1>Run JUnit Tests</h1>
    <h4>Upload the module class and its test class</h4>
    <form id="form">
        <p>
            <label for="class">Class:</label>
            <input type="file" id="class" name="class" required />
        </p>
        <p>
            <label for="testclass">Test:</label>
            <input type="file" id="testclass" name="testclass" required />
        </p>
        <button type="submit">Submit</button>
        <p>
            <pre id="result"></pre>
        </p>
    </form>
    <script>
        //
        document.getElementById("form").addEventListener("submit", function (e) {
            e.preventDefault();
            const formData = new FormData(document.getElementById("form"));
            document.getElementById("result").textContent = "Contacting the server...";

            fetch('http://localhost:8080/web', {
                method: 'POST',
                body: formData
            })
            .then(response => response)
            .then(result => {
                if (result.status !== 200) {
                    document.getElementById("result").textContent = result.statusText;
                } else {
                    document.getElementById("result").textContent = "Processing...";
                }
            })
            .catch(error => {
                console.error('Error:', error);
                document.getElementById("result").textContent = "Something went wrong in contacting the server. Please retry.";
            });
        })

        // Create WebSocket connection.
        const socket = new WebSocket('ws://localhost:8080/web/update/${pageContext.session.id}');

        // Listen for messages
        socket.addEventListener('message', function (event) {
            document.getElementById("result").textContent = event.data;
        });
    </script>
</body>
</html>
