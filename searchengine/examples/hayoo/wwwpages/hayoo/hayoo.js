/*

	The Hayoo! AJAX interface

  Copyright  : Copyright (C) 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Version    : 0.2

*/

prevInput = "";
lastLocation = window.location.hash;

Event.observe(window, 'load', checkForInitialQuery);

function checkForInitialQuery() {
  var argument = window.location.search.gsub(/\+/, '%20').toQueryParams()['query'];

  if (argument) {
    $("querytext").value = argument;
    processQuery(0);
  }
}

function checkForQuery () {
  if (lastLocation != window.location.hash) {
    lastLocation = window.location.hash;
    var prev = window.location.hash.split(":");

    if (prev.length == 2) {
      $("querytext").value = decodeURIComponent(prev[1]);
      processQuery(parseInt(prev[0].substr(1)));
    }
    else {
      window.setTimeout(checkForQuery, 200);
    }
  }
  else {
    window.setTimeout(checkForQuery, 200);
  }
}

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
    new Ajax.Request("results/hayoo.html?query=" + encodeURIComponent(query) + "&start=" + start,
      {
        method:'get',
        onSuccess: function(transport) {
          lastXMLResult = transport.responseXML;
          lastTXTResult = transport.responseText;
          lastQuery = query;
          window.location.hash = start + ":" + encodeURIComponent(query);
          lastLocation = window.location.hash;
          displayResult(transport.responseText, query);
          checkForQuery();
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
