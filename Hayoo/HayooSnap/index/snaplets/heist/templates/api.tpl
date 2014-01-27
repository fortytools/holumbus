<apply template="default">
  <div id="result">
    <div id="status">Enter some search terms above to start a search.</div>
    <div id="helptext" class="text">
      <h2>Hayoo! API</h2>
      <p>Hayoo! provides a JSON-based webservice API, which can be used
	to retrieve search results in a structured format.
	This allows one to include Hayoo! search functionality in other applications.
	Arbitrary queries can be submitted the same way as they would be
	entered them into the search box and results are
	returned encoded in JSON format.
      </p>
      <p>You may use this service for whatever you
	like and without any limitations, although we would be very
	happy to know about any application that uses the Hayoo! webservice API.
	Just drop us a line at
	<a href="mailto:hayoo@holumbus.org">hayoo@holumbus.org</a>.
      </p>
      <h2>Request URI</h2>
      <p>Direct your search request to the following URI:</p>
      <pre>http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=YOUR_QUERY</pre>
      <p>Provide your query as argument to the
	<code>query</code>
	URI parameter. Please note that you need to ensure proper URI encoding
	for the query argument. The syntax for the query is the same as if it
	would be entered into the search box. A detailed
	explanation of the syntax can be found
	<a href="help.html">here</a>.
      </p>
      <h2>Response</h2>
      <p>The response to a search request will be encoded in
	<a href="http://www.json.org">JSON</a>
	format and is structured as follows:
      </p>
      <pre>
        <code>{
  "message":"Found 12 results and 17 completions.",
  "hits":12,
  "functions":[ {
    "name":"map",
    "uri":"http://hackage.haskell.org/...",
    "module":"Data.Map",
    "signature":"(a->b)->[a]->[b]",
    "package":"containers"
  }, ... ],
  "completions":[ {
    "word":"MapM",
    "count":11
  }, ... ],
  "modules":[ {
    "name":"Data",
    "count":19
  }  }, ... ],
  "packages":[ {
    "name":"containers",
    "count":13
  }, ... ]
}</code>
      </pre>
      <p>The <code>message</code>
	field will contain a descriptive status message about the result
	or any errors encountered. The
	<code>hits</code> field will contain the total number of
	functions found. In the
	<code>functions</code> field, an array containing all functions
	found will be returned. For every function,
	a JSON object is included in the array.
      </p>
      <p>Each of these objects contains the function name,
	the URI pointing to the Haddock documentation, the module,
	the signature and the package name in the
	<code>name</code>, <code>uri</code>,
	<code>module</code>, <code>signature</code>
	and <code>package</code> fields,
	respectively.
      </p>
      <p>The <code>completions</code> contains all word completions
	(suggestions) resulting from the query For every completion,
	a JSON object is included in the array, containing the suggested
	word and the total number of occurrences of this word in the
	search result in the <code>word</code> and
	<code>count</code> fields.
      </p>
      <p>The <code>modules</code> and <code>packages</code>
	fields contain arrays with JSON objects denoting the occurrences
	of root modules and packages in the search result. For each element,
	the module/package name is included in the <code>name</code>
	field and the number of occurrences in the <code>count</code> field.
      </p>
    </div>
  </div>
</apply>
