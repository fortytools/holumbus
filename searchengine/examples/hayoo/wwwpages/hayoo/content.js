
help = ' \
<div id="result"> \
  <div id="status">Enter some search terms above to start a search.</div> \
\
  <div id="helptext" class="text"> \
\
    <h2>Basic Usage</h2> \
    <p> \
      By default, Hayoo! searches for function names, module names, signatures and function descriptions. \
      With every letter typed, Hayoo! will show the results it thinks are best matching the query as \
      well as some suggestions on how the words from the query could be completed. Clicking one of these \
      suggestions will replace the according word in the query. \
    </p> \
    <p> \
      Hayoo! displays results as a list of functions, including full qualified module name and the function \
      signature. Clicking the function name will lead directly to the corresponding documentation while \
      clicking the module name will lead to the documentation of the module. Additionally, Hayoo! shows \
      a little bit of the function description (if available) and provides a link leading directly to \
      the source of the function (if available). \
    </p> \
\
    <h2>Advanced Queries</h2> \
    <p> \
      If words are seperated by whitespace, Hayoo! will search for results containing both words. Instead of \
      using whitespace, the explicit <span class="query">AND</span> operator can be used. Hayoo! also  \
      supports <span class="query">OR</span> and <span class="query">NOT</span> operators, \
      although the <span class="query">NOT</span> operator may only be used together with \
      <span class="query">AND</span>, e.g. <span class="query">map NOT fold</span> or \
      <span class="query">map AND NOT fold</span>. Operator precedence can be influenced using round parentheses. \
      Phrases can be searched using double quotes, e.g. <span class="query">"this is a phrase"</span>. \
    </p> \
    <p> \
      It is possible to restrict a search to certain packages or modules. The most simple way would be to just include \
      the package name in the search, e.g. <span class="query">map base</span> will prefer hits from the base package. \
      But the restriction can also be more explicit, like <span class="query">map package:base</span> or like \
      <span class="query">map module:data.list</span>. It is also possible to specify several different modules or packages, \
      like this: <span class="query">fold module:(data.list OR data.map)</span>. This will return all hits for fold in \
      the module hierarchies below Data.List and Data.Map. \
    </p> \
    <p> \
      Hayoo! always performs fuzzy queries. This means, it tries to find something even if the query contains \
      spelling	errors. For example, Hayoo! will still find "fold" if "fodl" is searched. If Hayoo! \
      detects "-&gt;" in the query string, it will only search for signatures. A signature query may consist \
      of explicit type names as well as type variables. For example, searching for "a -&gt; b" will find \
      signatures like "Int -&gt; Bool". \
    <p> \
\
    <h2>Scope</h2> \
    <p> \
      Currently, Hayoo! searches all packages available on <a href="http://hackage.haskell.org">Hackage</a> \
      and <a href="http://www.haskell.org/gtk2hs/">Gtk2Hs</a>. Additionally, any Haskell documentation generated \
      by Haddock can be included in Hayoo!. \
      Just send a message including an URI where the documentation can be found to \
      <a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a>. \
    </p> \
  </div> \
</div> \
';

about = ' \
<div id="result"> \
  <div id="status">Enter some search terms above to start a search.</div> \
\
  <div id="abouttext" class="text"> \
\
    <h2>About Hayoo!</h2> \
    <p> \
      Hayoo! is a search engine specialized on <a href="http://www.haskell.org">Haskell</a> API documentation. The goal of \
      Hayoo! is to provide an interactive, easy-to-use search interface to the documenation of various Haskell packages and \
      libraries. Although the Hayoo! data is regularly updated, we might miss a package or library. If you think there \
      is some documentation for Haskell modules available on the Internet which should be added to Hayoo!, \
      just drop us a note at <a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a> and tell us the location where \
      we can find the documentation. \
    </p> \
\
    <h2>Background</h2> \
    <p> \
      Hayoo! is an example application of the <a href="http://holumbus.fh-wedel.de">Holumbus</a> framework and was  \
      heavily inspired by <a href="http://www.haskell.org/hoogle">Hoogle</a>. The Holumbus library \
      provides the search and indexing backend for Hayoo!. Holumbus and Hayoo! have been developed by  \
      Sebastian M. Schlatt and Timo B. Hübel at <a href="http://www.fh-wedel.de">FH Wedel University of Applied Sciences</a>.  \
      The Holumbus framework provides the basic building blocks for creating highly customizable search engines.  \
      To demonstrate the flexibility of the framework by a very special use case, the Hayoo! Haskell API search  \
      was implemented using Holumbus. \
    </p> \
    <p> \
      Currently, Hayoo! is still in beta stage. This means, it can become unavailable unexpectedly, as we \
      do some maintenance or add new features. Therefore you should not yet rely on Hayoo! as primary \
      search engine for Haskell documentation. \
    </p> \
\
    <h2>Technical Information</h2> \
    <p> \
      Hayoo! is entirely written in Haskell, including the underlying webserver. The web interface uses the \
      <a href="http://darcs2.fh-wedel.de/repos/janus">Janus</a> application server, written by Christian Uhlig. \
      This server allows easy and seamless integration of Haskell code in web applications. \
    </p> \
\
    <h2>Feedback</h2> \
    <p> \
      We would like to know what you think about Hayoo!, therefore you can reach us at \
      <a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a> and tell us about bugs, suggestions or \
      anything else related to Hayoo!.\
    </p> \
\
    <div id="hol"> \
      <a href="http://holumbus.fh-wedel.de"><img class="logo" src="hayoo/hol.png" alt="Holumbus logo"/></a> \
    </div> \
    <div id="fhw"> \
      <a href="http://www.fh-wedel.de"><img class="logo" src="hayoo/fhw.gif" alt="FH-Wedel logo"/></a> \
    </div> \
  </div> \
</div> \
';

function showHelp() {
  $("result").replace(help);
}

function showAbout() {
  $("result").replace(about);
}
