<apply template="default">
  <div id="result">
    <div id="status">Enter some search terms above to start a search.</div>
    <div id="abouttext" class="text">
      <h2>About Hayoo!</h2>
      <p>Hayoo! is a search engine specialized on
	<a href="http://www.haskell.org">Haskell</a>
	API documentation. The goal of Hayoo! is to provide an interactive,
	easy-to-use search interface to the documenation of various Haskell
	packages and libraries. Although the Hayoo! data is regularly updated,
	we might miss a package or library. If you think there is some documentation
	for Haskell modules available on the Internet which should be added to Hayoo!,
	just drop us a note at
	<a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a>
	and tell us the location where we can find the documentation.
      </p>
      <h2>Background</h2>
      <p>Hayoo! is an example application of the
	<a href="http://holumbus.fh-wedel.de">Holumbus</a>
	framework and was heavily inspired by <a href="http://www.haskell.org/hoogle">Hoogle</a>.
	The Holumbus library provides the search and indexing backend for Hayoo!. Holumbus and Hayoo!
	have been developed by Sebastian M. Gauck and Timo B. Kranz (formerly Hübel) at
	<a href="http://www.fh-wedel.de">FH Wedel University of Applied Sciences</a>.
	The Holumbus framework provides the basic building blocks for creating highly customizable search engines.
	To demonstrate the flexibility of the framework by a very special use case, the Hayoo!
	Haskell API search was implemented using Holumbus.
      </p>
      <p>Currently, Hayoo! is still in beta stage.
	This means, it can become unavailable unexpectedly,
	as we do some maintenance or add new features.
	Therefore you should not yet rely on Hayoo! as
	primary search engine for Haskell documentation.
      </p>
      <p>Hardware infrastructure for daily index updates is
	generously sponsored by
	<a href="http://www.fortytools.com">fortytools gmbh</a>,
	your friendly Haskell web development company.
      </p>
      <h2>Technical Information</h2>
      <p>Hayoo! is written entirely in Haskell and consists of two main parts:
	The indexer, which regularly checks Hackage for package updates and builds
	the search index and the web frontend. The web frontend formerly
	worked with apache and FastCGI today it's implemented with the use of the
	Snap server and the Heist templating system.
      </p>
      <h2>Feedback</h2>
      <p>We would like to know what you think about Hayoo!,
	therefore you can reach us at
	<a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a>
	and tell us about bugs, suggestions or anything else related to Hayoo!.
      </p>
      <div id="sponsors">
        <div id="hol">
          <a href="http://holumbus.fh-wedel.de">
            <img src="/hayoo/hol.png" alt="Holumbus logo" class="logo"/>
          </a>
        </div>
        <div id="ft">
          <a href="http://www.fortytools.com">
            <img src="/hayoo/ft.png" alt="fortytools logo" class="logo"/>
          </a>
        </div>
        <div id="fhw">
          <a href="http://www.fh-wedel.de">
            <img src="/hayoo/fhw.gif" alt="FH-Wedel logo" class="logo"/>
          </a>
        </div>
      </div>
    </div>
  </div>
 </apply>
