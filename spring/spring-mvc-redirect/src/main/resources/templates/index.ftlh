<html lang="en">
<head>
    <title>Start Page</title>
</head>
<body>
<h1>This is my view!</h1>
<p>${slogan}</p>
<form action="/calculate" method="post">
    <label>
        Number 1:
        <input type="number" name="number1">
    </label>
    <label>
        Number 2:
        <input type="number" name="number2">
    </label>
    <input type="submit" value="Calculate">
    <#if result??>
        Result:
        <output>${result}</output>
    </#if>
</form>
</body>
</html>