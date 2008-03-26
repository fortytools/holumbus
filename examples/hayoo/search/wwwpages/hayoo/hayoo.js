var lastPress = (new Date()).getTime();

document.observe("dom:loaded", function() {
	checkForQuery();
});

function checkForQuery () {
	var argument = window.location.search.gsub(/\+/, '%20').toQueryParams()['query'];
	
	if (argument) {
		$("querytext").value = argument;
		processQuery();
	}
}

function tryProcessQuery () {
	var escaped = encodeURIComponent($("querytext").value);
	window.setTimeout('checkProcessQuery(\'' + escaped + '\')', 300);
}

function checkProcessQuery (query) {
	if (query == encodeURIComponent($("querytext").value)) {
		processQuery(0);
	}
}

function forceProcessQuery () {
	processQuery(0);
	
	return false;
}

function processQuery (start) {
	var query = encodeURIComponent($("querytext").value);
	if (query.length > 0) {
		$("throbber").show();
		new Ajax.Request("results/hayoo.html?query=" + query + "&start=" + start,
		{
			method:'get',
			onSuccess: function(transport) {
				lastXMLResult = transport.responseXML;
				lastTXTResult = transport.responseText;
				lastQuery = query;
			  displayResult(transport.responseText);
			},
			onFailure: function(){ alert('Something went wrong...') }
		});
	}
}

function displayResult (result) {
	$("result").replace(result);
	
	$("throbber").hide();
}

function replaceInQuery (needle, substitute) {
	checkLastQuery();

	$("querytext").value = $("querytext").value.gsub(needle, substitute);

	processQuery(0);
}

function showPage (page) {
	checkLastQuery();

	processQuery (page);
}

function checkLastQuery () {
	if ($("querytext").value.length == 0) {
		if (typeof lastQuery == "string") {
			$("querytext").value = lastQuery;
		}
	}
}
