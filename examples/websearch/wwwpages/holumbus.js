var lastPress = (new Date()).getTime();

function processQuery () {
	currentPress = (new Date()).getTime();
	delay = currentPress - lastPress;
	lastPress = currentPress;
	if (delay > 500) {
		query = document.getElementById("querytext").value;
		if (query.length > 1) {
			new Ajax.Request("holumbus.xml?query=" + query,
			{
				method:'get',
				onSuccess: function(transport) {
					lastXMLResult = transport.responseXML;
					lastTXTResult = transport.responseText;
				  displayResult(transport.responseXML);
				},
				onFailure: function(){ alert('Something went wrong...') }
			});
		}
	}
}

/*
function displayResult (resultString) {
	var txt = document.createTextNode(resultString);
	var resultNode = document.getElementById("test");
	resultNode.replaceChild(txt, resultNode.firstChild);
//	resultNode = appendChild(tmp);
}
*/

function displayResult (result) {
	var msg = result.getElementsByTagName("message")[0];

	var docHits = result.getElementsByTagName("dochits")[0];
	var wordHits = result.getElementsByTagName("wordhits")[0];

	var noDocHits = result.getElementsByTagName("doccount")[0].firstChild.data;
	var maxDocScore = result.getElementsByTagName("docscore")[0].firstChild.data;
	var noWordHits = result.getElementsByTagName("wordcount")[0].firstChild.data;
	var maxWordScore = result.getElementsByTagName("wordscore")[0].firstChild.data;

	if (noDocHits == 0) {
		var txt = "Nothing found yet.";
	}
	else {
		var txt = "Found " + noDocHits + " documents and " + noWordHits + " completions.";
	}
	var stats = document.createTextNode(txt);
	document.getElementById("stats").replaceChild(stats, document.getElementById("stats").firstChild);

	displayWordHits(wordHits, noWordHits, maxWordScore);
	// displayDocHits(docHits);
}

function displayWordHits (hits, count, score) {
	var words = hits.getElementsByTagName("word");

	var cloud = document.createElement("p");
	cloud.setAttribute("class", "cloud");

	for (var i = 0; i < words.length; i++) {
		var word = words[i];
		var w = word.getAttribute("w");
		var s = word.getAttribute("score");
		var size = weightScore(1, 9, s, score).round();
		var node = document.createElement("span");
		node.setAttribute("id", "cloud" + size);
		var text = document.createTextNode(w);
		node.appendChild(text);
		
		cloud.appendChild(node);
		cloud.appendChild(document.createTextNode(" "));
	}
	
	var container = document.getElementById("words");
	container.replaceChild(cloud, container.firstChild);
}

// Transforms val from the range 0.0 - top to a corresponding value in the range min - max
function weightScore (min, max, val, top) {
	return max - ((top - val) / top) * (max - min);
}
