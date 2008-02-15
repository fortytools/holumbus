var lastPress = (new Date()).getTime();

document.observe("dom:loaded", function() {
	$("throbber").hide();
	
	checkForQuery();
});

function checkForQuery () {
	var argument = window.location.search.toQueryParams()["query"];
	
	if (argument) {
		$("querytext").value = argument;
		processQuery();
	}
}

function tryProcessQuery () {
	currentPress = (new Date()).getTime();
	delay = currentPress - lastPress;
	lastPress = currentPress;
	if (delay > 500) {
		processQuery();
	}
}

function forceProcessQuery () {
	processQuery();
	
	return false;
}

function processQuery () {
	var query = $("querytext").value;
	if (query.length > 1) {
		$("throbber").show();
		new Ajax.Request("results/hayoo.html?query=" + query,
		{
			method:'get',
			onSuccess: function(transport) {
				lastXMLResult = transport.responseXML;
				lastTXTResult = transport.responseText;
			  displayResult(transport.responseText);
			},
			onFailure: function(){ alert('Something went wrong...') }
		});
	}
}

function replaceInQuery (needle, substitute) {
	$("querytext").value = $("querytext").value.gsub(needle, substitute);
	processQuery();
}

function displayResult (result) {
	$("result").replace(result);
	
	$("throbber").hide();
}
