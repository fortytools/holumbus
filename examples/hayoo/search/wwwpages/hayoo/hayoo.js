/*

	The Hayoo! AJAX interface

  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Version    : 0.1

*/

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
  var query = $("querytext").value;
	window.setTimeout('checkProcessQuery(\'' + encodeURIComponent(query) + '\')', 300);
}

function checkProcessQuery (lastQuery) {
	if (lastQuery == encodeURIComponent($("querytext").value)) {
		processQuery(0);
	}
}

function forceProcessQuery () {
	processQuery(0);
	
	return false;
}

function processQuery (start) {
	var query = $("querytext").value;
	if (query.length > 0) {
		$("throbber").show();
		new Ajax.Request("results/hayoo.html?query=" + encodeURIComponent(query) + "&start=" + start,
			{
				method:'get',
				onSuccess: function(transport) {
					lastXMLResult = transport.responseXML;
					lastTXTResult = transport.responseText;
					lastQuery = query;
				  displayResult(transport.responseText, query);
				},
				onFailure: function() {
				  var resultError = "<div id=\"result\"><div id=\"status\">Error: Could not execute query.</div><div id=\"words\">&nbsp;</div><div id=\"documents\">&nbsp;</div><div id=\"pager\">&nbsp;</div></div>";		
	
				  $("result").replace(resultError);
				}
			}
		);
	}
}

function displayResult (result, query) {
	$("result").replace(result);
	$("querytext").defaultValue = query;
	$("throbber").hide();
}

function replaceInQuery (needle, substitute) {
	$("querytext").value = $("querytext").value.gsub(needle, substitute);
	processQuery(0);
}

function showPage (page) {
	processQuery (page);
}
