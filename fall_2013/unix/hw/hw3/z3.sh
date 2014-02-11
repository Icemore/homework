#!/bin/bash

# input: file.csv
# output: out.html

cat > out.html << EOF
<!DOCTYPE html>
<html>
<head>
	<title>Phone book</title>
</head>
<body>
<table border="1">
<tr>
	<td> Name </td>
	<td> Surname </td>
	<td> Phone </td>
</tr>
EOF

sed -r 's/(\w+),(\w+),(\w+)/<tr><td>\1<\/td><td>\2<\/td><td>\3<\/td><\/tr>/' file.csv >> out.html

cat >> out.html << EOF
</table>
</body>
</html>
EOF
