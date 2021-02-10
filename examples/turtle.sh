#!/bin/sh

R=$(curl -X POST -s --connect-timeout 120 -F class=@MessageUtil.java -F testclass=@TestJunit.java http://localhost:8080/web)
echo "$R"