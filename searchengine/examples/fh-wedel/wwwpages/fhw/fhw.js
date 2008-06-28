/*

	The FHW Search AJAX interface

  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Version    : 0.1

*/

prevInput = "";
lastLocation = window.location.href;

Event.observe(window, 'load', checkForQuery);

function checkForQuery () {
	if (lastLocation != window.location.href) {
		lastLocation = window.location.href;
		var prev = window.location.hash.split(":");
	
		if (prev.length == 2) {
			$("querytext").value = prev[1];
			processQuery(parseInt(prev[0].substr(1)));
		}
	}
	
	var argument = window.location.search.gsub(/\+/, '%20').toQueryParams()['query'];
	
	if (argument) {
		$("querytext").value = argument;
		processQuery(0);
	}	
	
	window.setTimeout(checkForQuery, 200);
}

Event.observe(window.location, 'change', checkForQuery);

function tryProcessQuery () {
  var query = $("querytext").value;
	window.setTimeout('checkProcessQuery(\'' + encodeURIComponent(query) + '\')', 300);
}

function checkProcessQuery (query) {
	if ((query != prevInput) && (query == encodeURIComponent($("querytext").value))) {
		prevInput = query;
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
		new Ajax.Request("results/fhw.html?query=" + encodeURIComponent(query) + "&start=" + start,
			{
				method:'get',
				onSuccess: function(transport) {
					lastXMLResult = transport.responseXML;
					lastTXTResult = transport.responseText;
					lastQuery = query;
					window.location.hash = start + ":" + encodeURIComponent(query);
					lastLocation = window.location.href;
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
	if ($("querytext").value == "") {
		$("querytext").value = lastQuery;
	}
	processQuery (page);
}
