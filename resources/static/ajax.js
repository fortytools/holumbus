// Create Request Object
function createRequestObject()
{
  var ro;
  var browser=navigator.appName;
 
  if(browser=="Microsoft Internet Explorer")
  {
    ro=new ActiveXObject("Microsoft.XMLHTTP");
  }
  else
  {
    ro=new XMLHttpRequest();
  }
  return ro;
}
 
var http=createRequestObject();

// React on Ajax Response. Insert List Items (the Word-Completions) into Suggestion List.
function handleResponseAutoCompleter()
{
  var output='';
  if(http.readyState==4)
  {
    var suggestions = eval(http.responseText);	
    var i = 0;
    for(var suggestion in suggestions)
    {
	  var sugg = suggestions[suggestion]; 
	  var sugg1 = sugg.substring(0, sugg.indexOf(" ("));
      if(sugg != "")
      {
        output += '<div onmouseover="suggestOverMouse(this)" onmouseout="suggestOut(this)" onmousedown="setSuggestion(\''+sugg1+'\')" id="'+i+'" class="suggest_link">' 
		+ sugg 
		+ '</div>';
      }
      i++;
    }
    selectedSuggestion = '';
    numSuggestions = i;
  }
  document.getElementById("suggestion").innerHTML=output;
  if (i > 0) {
    document.getElementById("suggestion").style.visibility="visible";
  } else {
    document.getElementById("suggestion").style.visibility="hidden";
  }
}

var selectedSuggestion = '';
var numSuggestions = 0;

//Move over function
function suggestOverMouse(div) {
 if (selectedSuggestion != '') {
   suggestOut(document.getElementById(selectedSuggestion));
 }
 selectedSuggestion = div.id;
 div.className = 'suggest_link_over';
}

//Move over function
function suggestOver(div) {
 selectedSuggestion = div.id;
 div.className = 'suggest_link_over';
 document.getElementById("query").value = div.innerHTML.substring(0, div.innerHTML.indexOf(" ("));
}

//Move out function
function suggestOut(div) {
 div.className = 'suggest_link';
}

function keyUpHandler(e)
{
  keyIn = e.keyCode;
  if (keyIn == 38) {
    if (selectedSuggestion != '' && selectedSuggestion > 0) {
      // Up key has been pressed in the form-input.
      suggestOut(document.getElementById(selectedSuggestion));
      suggestOver(document.getElementById(selectedSuggestion - 1));
    }
  } else if (keyIn == 40) {
    // Down key has been pressed in the form-input.
    if (selectedSuggestion == '') {
      suggestOver(document.getElementById('0'));
    } else if (selectedSuggestion < numSuggestions-1) {
      suggestOut(document.getElementById(selectedSuggestion));
      suggestOver(document.getElementById(selectedSuggestion -(-1)));
    }
  } else if (keyIn == 37 || keyIn == 39) {
    // Nothing
  } else {
    // A key has been pressed in the form-input.
    // Send Ajax-Request to retrieve list of word-completions.
    var query=document.getElementById("query").value;
    if(query.length>0)
    {
      http.open('get', 'completions?query='+query);
      http.onreadystatechange=handleResponseAutoCompleter;
      http.send(null);
    } else {
      hide();
    }
  }
}

// Suggestion List Item has been clicked.
// Insert Selected Word-Completion into Form-Input.
function setSuggestion(suggestion)
{
  document.getElementById("query").value=suggestion;
  hide();
  location.href="querypage?query=" + suggestion;
}
 
// Hide Suggestion List.
function hide()
{
  var sugg = document.getElementById("suggestion");
  if ( sugg.hasChildNodes() )
  {
	while ( sugg.childNodes.length >= 1 )
	{
	    sugg.removeChild( sugg.firstChild );       
	} 
  }
  document.getElementById("suggestion").style.visibility="hidden";
}
