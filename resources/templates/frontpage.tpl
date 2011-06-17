<html>
  <head>
    <title>FH Suche</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
    <script type="text/javascript" src="ajax.js"></script>
  </head>
  <body>
    <div id="header"/>
    <div id="content">
      <div id="form">
        <form action="/querypage">
          <input id="query" name="query" type="text" autocomplete="off"
                 value=$(oldquery) onblur="hide()" onkeyup="keyUpHandler(event)"/>
          <input id="button" name="button" type="submit" value="Suchen"/>
          <div id="suggestion"/>
        </form>
      </div>
      <div id="result">
        <result/>
      </div>
      <div id="pager">
        <pager/>
      </div>
    </div>
    <div id="sider"/>
    <div id="footer">
      &copy;&nbsp;FH&nbsp;Wedel
    </div>
  </body>
</html>
