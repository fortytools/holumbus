<div id="query">
  <div id="logo">
    <a href="hayoo.html"><img src="/hayoo/hayoo.png" alt="Hayoo! logo" class="logo" /></a>
  </div>
  <form action="hayoo.html" id="queryform" onsubmit="return forceProcessQuery()" method="get">
    <div id="queryinterface">
      <input id="querytext" type="text" onkeyup="tryProcessQuery()" name="query" autocomplete="off" value="" />
      <input id="querybutton" type="submit" value="Search" />
      <img src="/hayoo/loader.gif" alt="Throbber" id="throbber" style="display:none;" />
      <div class="stats"><packages-searched /></div>
    </div>
  </form>
</div>
