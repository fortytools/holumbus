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

	displayDocHits(docHits, noDocHits, maxDocScore);
	displayWordHits(wordHits, noWordHits, maxWordScore);
}

function displayWordHits (hits, count, score) {
	var words = hits.getElementsByTagName("word");

	var cloud = document.createElement("p");
	cloud.setAttribute("class", "cloud");

	var capacity = (0.6 * (window.innerWidth - 250)).round();
	var i = 0;
	var chars = 0;

	while (i < words.length && chars < capacity) {
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
		
		chars += w.length;
		i++;
	}
	
	if (chars >= capacity) {
		cloud.appendChild(document.createTextNode("..."));
	}
	
	var container = document.getElementById("words");
	container.replaceChild(cloud, container.firstChild);
}

function displayDocHits (hits, count, score) {
	var docs = hits.getElementsByTagName("doc");
	
	var list = document.createElement("div");
	list.setAttribute("id", "list");
	
	for (var i = 0; i < docs.length; i++) {
		var doc = docs[i];
		var href = doc.getAttribute("href");

		var node = document.createElement("p");
		node.setAttribute("class", "entry");
		
		var link = document.createElement("a");
		link.setAttribute("id", "link");
		link.setAttribute("href", href);
		link.appendChild(document.createTextNode(doc.getAttribute("title")));
		var title = document.createElement("div");
		title.setAttribute("id", "title");
		title.appendChild(link);

		var contexts = createContextNode(doc.getElementsByTagName("context"));
		
		var uri = document.createElement("div");
		uri.setAttribute("id", "uri");
		uri.appendChild(document.createTextNode(href));
		
		node.appendChild(title);
		node.appendChild(contexts);
		node.appendChild(uri);
		
		list.appendChild(node);
		list.appendChild(document.createTextNode(" "));
	}
	
	var container = document.getElementById("results");
	container.replaceChild(list, container.firstChild);
}

function createContextNode(contexts) {
	var node = document.createElement("div");
	node.setAttribute("id", "contexts");

	for (var i = 0; i < contexts.length; i++) {
		var context = contexts[i];
		var name = document.createElement("span");
		name.setAttribute("id", "context");
		var txt = context.getAttribute("name");
		var upcase = txt.substr(0, 1).toUpperCase() + txt.substr(1).toLowerCase();
		name.appendChild(document.createTextNode(upcase + ":"));
		node.appendChild(name);
		node.appendChild(document.createTextNode(" "));
		
		var words = context.getElementsByTagName("word");
		for (var j = 0; j < words.length; j++) {
			var word = words[j];
			node.appendChild(document.createTextNode(word.getAttribute("w") + " "));
		}
	}
	
	return node;
}

// Transforms val from the range 0.0 - top to a corresponding value in the range min - max
function weightScore (min, max, val, top) {
	return max - ((top - val) / top) * (max - min);
}
