<apply template="default">
  <div id="result">
    <div id="status">
      Enter some search terms above to start a search.
    </div>
    <div id="words"> </div>
    <div id="documents">
      <div id="examples">
        Hayoo! will search all packages from <a href="http://hackage.haskell.org">Hackage</a>,
        including all function and type definitions.
        Here are some example queries:
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;map&apos;); return false;" href="hayoo.html?query=map&amp;start=0">map</a>
            searches for everything that contains a word starting with "map" (case insensitive)
            in the function name, module name or description.
          </p>
        </div>
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;name:map&apos;); return false;" href="hayoo.html?query=name%3Amap&amp;start=0">name:map</a>
            searches for everything where the function name starts with "map" (case insensitive).
          </p>
        </div>
        <div class="example">
          <p><a onclick="replaceInQuery(&apos;&apos;,&apos;map OR fold&apos;); return false;" href="hayoo.html?query=map%20OR%20fold&amp;start=0">map OR fold</a>
            searches for everything that contains a word starting with "map" or "fold" (case insensitive)
            in the function name, module name or description.
          </p>
        </div>
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;map package:containers&apos;); return false;" href="hayoo.html?query=map%20package%3Acontainers&amp;start=0">map package:containers</a>
            searches for everything from package "containers" that contains a word starting with "map" (case insensitive)
            in the function name, module name or description.
          </p>
        </div>
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;map hierarchy:Lazy&apos;); return false;" href="hayoo.html?query=map%20hierarchy%3ALazy&amp;start=0">map hierarchy:Lazy</a>
            searches for everything where "Lazy" appears somewhere in the full qualified module
            name and that contains a word starting with "map" (case insensitive) in the function name,
            module name or description.
          </p>
        </div>
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;(map OR fold) module:Data.Map&apos;); return false;" href="hayoo.html?query=(map%20OR%20fold)%20module%3AData.Map&amp;start=0">(map OR fold) module:Data.Map</a>
            searches for everything from module "Data.Map" that contains a word starting with
            "map" or "fold" (case insensitive) in the function name, module name or description.
          </p>
        </div>
        <div class="example">
          <p>
            <a onclick="replaceInQuery(&apos;&apos;,&apos;name:attr module:Text.XML&apos;); return false;" href="hayoo.html?query=name%3Aattr%20module%3AText.XML&amp;start=0">name:attr module:Text.XML</a>
            searches for everything from the whole module hierarchy "Text.XML" where
            the function name starts with "attr" (case insensitive).
          </p>
        </div>
      </div>
    </div>
  </div>
</apply>
