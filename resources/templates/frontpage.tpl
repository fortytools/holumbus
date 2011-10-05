<html>
  <head>
    <title>FH Suche</title>
    <link rel="stylesheet" type="text/css" href="screen.css" />
    <script type="text/javascript" src="ajax.js"></script>
  </head>
  <body>
    <div id="header"/>
    <table cellspacing="0" cellpadding="0" style="border-left: 1px solid #777777; border-right: 1px solid #777777;">
    <tr>
    <td style="vertical-align:top; background-color:#FFFFFF;">
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
    </td>
    <td style="vertical-align:top; background-color:#1E326F;">
    <div id="sider"/>
    </td>
    </tr>
    </table>
    <div id="footer">
      &copy;&nbsp;FH&nbsp;Wedel
    </div>
  </body>
</html>
