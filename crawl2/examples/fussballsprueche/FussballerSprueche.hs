module FussballerSprueche
where

type Joke = (Int, String, String, String, String)

allJokes :: [Joke]
allJokes = [joke1, joke2, joke3, joke4, joke5, joke6, joke7, joke8, joke9, joke10, joke11, joke12, joke13, joke14, joke15, joke16, joke17, joke18, joke19, joke20, joke21, joke22, joke23, joke24, joke25, joke26, joke27, joke28, joke29, joke30, joke31, joke32, joke33, joke34, joke35, joke36, joke37, joke38, joke39, joke40, joke41, joke42, joke43, joke44, joke45, joke46, joke47, joke48, joke49, joke50, joke51, joke52, joke53, joke54, joke55, joke56, joke57, joke58, joke59, joke60, joke61, joke62, joke63, joke64, joke65, joke66, joke67, joke68, joke69, joke70, joke71, joke72, joke73, joke74, joke75, joke76, joke77, joke78, joke79, joke80, joke81, joke82, joke83, joke84, joke85, joke86, joke87, joke88, joke89, joke90, joke91, joke92, joke93, joke94, joke95, joke96, joke97, joke98, joke99, joke100, joke101, joke102, joke103, joke104, joke105, joke106, joke107, joke108, joke109, joke110, joke111, joke112, joke113, joke114, joke115, joke116, joke117, joke118, joke120, joke121, joke122, joke123, joke124, joke125, joke126, joke127, joke128, joke129, joke130, joke131, joke132, joke133, joke134, joke135, joke136, joke137, joke138, joke139, joke140, joke141, joke142, joke143, joke144, joke145, joke146, joke147, joke148, joke149, joke150, joke151, joke152, joke153, joke154, joke155, joke156, joke157, joke158, joke159, joke160, joke161, joke162, joke163, joke164, joke165, joke166, joke167, joke168, joke169, joke170, joke171, joke172, joke173, joke174, joke175, joke176, joke177, joke178, joke179, joke180, joke181, joke182, joke183, joke184, joke185, joke186, joke187, joke188, joke189, joke190, joke191, joke192, joke193, joke194, joke195, joke196, joke197, joke198, joke199, joke200, joke201, joke202, joke203, joke204, joke205, joke206, joke207, joke208, joke209, joke210, joke211, joke212, joke213, joke214, joke215, joke216, joke217, joke218, joke219, joke220, joke221, joke222, joke223, joke224, joke225, joke226, joke227, joke228, joke229, joke230, joke231, joke232, joke233, joke234, joke235, joke236, joke237, joke238, joke239, joke240, joke241, joke242, joke243, joke244, joke245, joke246, joke247, joke248, joke249, joke250, joke251, joke252, joke253, joke254, joke255, joke256, joke257, joke258, joke259, joke260, joke261, joke262, joke263, joke264, joke265, joke266, joke267, joke268, joke269, joke270, joke271, joke272, joke273, joke274, joke275, joke276, joke277, joke278, joke279, joke280, joke281, joke282, joke283, joke284, joke285, joke286, joke287, joke288, joke289, joke290, joke291, joke292, joke293, joke294, joke295, joke296, joke297, joke298, joke299, joke300, joke301, joke302, joke303, joke304, joke305, joke306, joke307, joke308, joke309, joke310, joke311, joke312, joke313, joke314, joke315, joke316, joke317, joke318, joke319, joke320, joke321, joke322, joke323, joke324, joke325, joke326, joke327, joke328, joke329, joke330, joke331, joke332, joke333, joke334, joke335, joke336, joke337, joke338, joke339, joke340, joke341, joke342, joke343, joke344, joke345, joke346, joke347, joke348, joke349, joke350, joke351, joke352, joke353, joke354, joke355, joke356, joke357, joke358, joke359, joke360, joke361, joke362, joke363, joke364, joke365, joke366, joke367, joke368, joke369, joke370, joke371, joke372, joke373, joke374, joke375, joke376, joke377, joke378, joke379, joke380, joke381, joke382, joke383, joke384, joke385, joke386, joke387, joke388, joke389, joke390, joke391, joke392, joke393, joke394, joke395, joke396, joke397, joke398, joke399, joke400, joke401, joke402, joke403, joke404, joke405, joke406, joke407, joke408, joke409, joke410, joke411, joke412, joke413, joke414, joke415, joke416, joke417, joke418, joke419, joke420, joke421, joke422, joke423, joke424, joke425, joke426, joke427, joke428, joke429, joke430, joke431, joke432, joke433, joke434, joke435, joke436, joke437, joke438, joke439, joke440, joke441, joke442, joke443, joke444, joke445, joke446, joke447, joke448, joke449, joke450, joke451, joke452, joke453, joke454, joke455, joke456, joke457, joke458, joke459, joke460, joke461, joke462, joke463, joke464, joke465, joke466, joke467, joke468, joke469, joke470, joke471, joke472, joke473, joke474, joke475, joke476, joke477, joke478, joke479, joke480, joke481, joke482, joke483, joke484, joke485, joke486, joke487, joke488, joke489, joke490, joke491, joke492, joke493, joke494, joke495, joke496, joke497, joke498, joke499, joke500, joke501, joke502, joke503, joke504, joke505, joke506, joke507, joke508, joke509, joke510, joke511, joke512, joke513, joke514, joke515, joke516, joke517, joke518, joke519, joke520, joke521, joke522, joke523, joke524, joke525, joke526, joke527, joke528, joke529, joke530, joke531, joke532, joke533, joke534, joke535, joke536, joke537, joke538, joke539, joke540, joke541, joke542, joke543, joke544, joke545, joke546, joke547, joke548, joke549, joke550, joke551, joke552, joke553, joke554, joke555, joke556, joke557, joke558, joke560, joke561, joke562, joke563, joke564, joke565, joke566, joke567, joke568, joke569, joke570, joke571, joke572, joke573, joke578, joke584, joke585, joke587, joke588, joke589, joke590, joke592, joke593, joke594, joke595, joke596, joke597, joke598, joke599, joke600, joke601, joke602, joke603, joke604, joke605, joke606, joke607, joke608, joke609, joke610, joke611, joke612, joke613, joke614, joke615, joke616, joke617, joke618, joke619, joke620, joke621, joke622, joke623, joke624, joke625, joke626, joke627, joke628, joke629, joke630, joke631, joke633, joke634, joke635, joke636, joke637, joke639, joke640, joke641, joke642, joke643, joke644, joke645, joke646, joke647, joke648, joke649, joke650, joke651, joke652, joke653, joke654, joke655, joke656, joke657, joke658, joke659, joke660, joke661, joke662, joke663, joke664, joke665, joke666, joke667, joke668, joke669, joke670, joke671, joke672, joke673, joke674, joke675, joke676, joke677, joke678, joke679, joke680, joke681, joke682, joke683, joke684, joke685, joke686, joke687, joke688, joke689, joke690, joke691, joke692, joke693, joke694, joke695, joke696, joke697, joke698, joke699, joke700, joke702, joke704, joke705, joke706, joke707, joke708, joke709, joke710, joke711, joke712, joke713, joke714, joke715, joke717, joke718, joke719, joke720, joke721, joke722, joke723, joke724, joke725, joke726, joke727, joke728, joke729, joke730, joke731, joke733, joke734, joke735, joke736, joke737, joke738, joke739, joke740, joke741, joke742, joke743, joke744, joke745, joke746, joke747, joke748, joke749, joke750, joke751, joke752, joke753, joke754, joke755, joke756, joke757, joke758, joke759, joke760, joke761, joke762, joke763, joke764, joke766, joke767, joke768, joke769, joke770, joke771, joke772, joke773, joke774, joke775, joke776, joke777, joke778, joke779, joke780, joke781, joke782, joke783, joke786, joke787, joke788, joke789, joke790, joke791, joke793, joke794, joke795, joke796, joke797, joke798, joke799, joke800, joke801, joke803, joke804, joke805, joke806, joke807, joke808, joke809, joke810, joke811, joke812, joke813, joke814, joke815, joke816, joke817, joke818, joke819, joke820, joke821, joke823, joke824, joke825, joke826, joke827, joke828, joke829, joke830, joke831, joke832, joke833, joke834, joke835, joke836, joke837, joke838, joke839, joke840, joke841, joke842, joke843, joke844, joke845, joke846, joke847, joke848, joke849, joke850, joke851, joke852, joke853, joke854, joke855, joke856, joke857, joke858, joke859, joke860, joke861, joke862, joke863, joke864, joke865, joke866, joke867, joke868, joke869, joke870, joke871, joke872, joke873, joke874, joke875, joke876, joke877, joke878, joke880, joke881, joke882, joke883, joke884, joke885, joke886, joke887, joke888, joke889, joke890, joke891, joke892, joke893, joke894, joke895, joke897, joke898, joke899, joke900, joke901, joke902, joke903, joke904, joke905, joke906, joke907, joke908, joke909, joke910, joke911, joke912, joke913, joke914, joke915, joke916, joke917, joke918, joke919, joke920, joke921, joke922, joke923, joke924, joke925, joke926, joke927, joke928, joke929, joke930, joke931, joke932, joke933, joke934, joke935, joke936, joke937, joke938]

joke1 :: Joke
joke1 = ( 1 , "Adi Furler" , "Sch\246ne Bilder aus Bremen. Aber eins verstehe ich nicht: Wieso singen die eigentlich \"We want the cup\", die haben den Pokal doch schon?" , "im Sportstudio nach einem Bericht \252ber eine Pokalsiegerfeier in Bremen, bei der Wynton Rufer und die Fans \"We won the cup!\" sangen" , "Reporterspr\252che" )

joke2 :: Joke
joke2 = ( 2 , "Aleksandar Ristic" , "Musst Du trinken, wirst Du Meister. (M\252ller-Milch war damals Sponsor der Schalker.)" , "als Schalke-Trainer schiebt er auf einer Pressekonferenz einen Becher M\252ller-Milch zu Dragoslav Stepanovic" , "Trainerspr\252che" )

joke3 :: Joke
joke3 = ( 3 , "Aleksandar Ristic" , "Wenn man ein 0:2 kassiert, dann ist ein 1:1 nicht mehr m\246glich.\187" , "" , "Trainerspr\252che" )

joke4 :: Joke
joke4 = ( 4 , "Andreas Brehme" , "Also bei mir geht das mit dem linken Fu\223 genauer und mit dem rechten fester! (Moderator: \"Und mit welchem Fu\223 schie\223en sie jetzt auf die Torwand?\") Ja, mit dem rechten!" , "im Sportstudio auf die Frage, mit welchem Fu\223 er schie\223t" , "Spielerspr\252che" )

joke5 :: Joke
joke5 = ( 5 , "Andreas Brehme" , "Bedanken m\246chten wir uns auch bei den Fans, auf denen wir uns immer verlassen konnten.\187" , "" , "Spielerspr\252che" )

joke6 :: Joke
joke6 = ( 6 , "Andreas Brehme" , "Haste Schei\223e am Fu\223, haste Schei\223e am Fu\223!\187" , "" , "Spielerspr\252che" )

joke7 :: Joke
joke7 = ( 7 , "Andreas Brehme" , "Wenn der Mann in Schwarz pfeift, kann der Schiedsrichter auch nichts mehr machen.\187" , "" , "Spielerspr\252che" )

joke8 :: Joke
joke8 = ( 8 , "Andreas Herzog" , "Ab der 60. Minute wird Fu\223ball erst richtig sch\246n. Aber da bin ich immer schon unter der Dusche." , "zu seiner Bayern-Zeit, als er meistens recht fr\252h ausgewechselt wurde" , "Spielerspr\252che" )

joke9 :: Joke
joke9 = ( 9 , "Andreas Herzog" , "Der Pfau, der Superpfau, der Herr Effenberg. Da stolziert er \252ber den Platz und dann verschie\223t er, es gibt doch noch einen Fu\223ballgott." , "nach dem Pokalsieg 1999" , "Spielerspr\252che" )

joke10 :: Joke
joke10 = ( 10 , "Andreas M\246ller" , "Das ist eine Deprimierung.\187" , "" , "Spielerspr\252che" )

joke11 :: Joke
joke11 = ( 11 , "Andreas M\246ller" , "Der Basler, der ist eh doof.\187" , "" , "Spielerspr\252che" )

joke12 :: Joke
joke12 = ( 12 , "Andreas M\246ller" , "Einige haben von einem recht guten Spiel gesprochen. Da frage ich mich, ob ich zum Augen- oder zum Ohrenarzt muss." , "nach einer 1:3-Niederlage des BVB in Bielefeld" , "Spielerspr\252che" )

joke13 :: Joke
joke13 = ( 13 , "Andreas M\246ller" , "Ich habe mit Erich Ribbeck telefoniert, und er hat zu mir gesagt, ich stehe f\252r die Maltareise nicht zur Verf\252gung.\187" , "" , "Spielerspr\252che" )

joke14 :: Joke
joke14 = ( 14 , "Andreas M\246ller" , "Ich hatte vom Feeling her ein gutes Gef\252hl.\187" , "" , "Spielerspr\252che" )

joke15 :: Joke
joke15 = ( 15 , "Andreas M\246ller" , "Mailand oder Madrid - Hauptsache Italien!\187" , "" , "Spielerspr\252che" )

joke16 :: Joke
joke16 = ( 16 , "Andreas M\246ller" , "Mein Problem ist, dass ich immer sehr selbstkritisch bin, auch mir selbst gegen\252ber.\187" , "" , "Spielerspr\252che" )

joke17 :: Joke
joke17 = ( 17 , "Anthony Baffoe" , "Du kannst auf meiner Plantage arbeiten." , "zu einem wei\223en Gegenspieler" , "Spielerspr\252che" )

joke18 :: Joke
joke18 = ( 18 , "Anthony Baffoe" , "Mann, wir Schwatten m\252ssen doch zusammenhalten!" , "nach gelber Karte zum Schiri" , "Spielerspr\252che" )

joke19 :: Joke
joke19 = ( 19 , "Anthony Yeboah" , "Soll ich etwa ein Lagerfeuer im Wohnzimmer machen?" , "auf die Feststellung des \"Kicker\", er wohne \"wie ein deutscher Musterb\252rger\"" , "Spielerspr\252che" )

joke20 :: Joke
joke20 = ( 20 , "Anthony Yeboah" , "Ich wollte den Ball treffen, aber der Ball war nicht da." , "er hatte gegen Michael Schulz nachgetreten" , "Spielerspr\252che" )

joke21 :: Joke
joke21 = ( 21 , "ARD/ZDF-Videotext" , "Der DFB-Elf reichte ein 0:0-Zittersieg gegen die T\252rkei.\187" , "" , "Promis & Presse" )

joke22 :: Joke
joke22 = ( 22 , "Arnim Basche" , "Kickenbacher Offers\187" , "" , "Reporterspr\252che" )

joke23 :: Joke
joke23 = ( 23 , "Auf Schalke" , "Zu die Pressetische." , "Hinweisschild im Parkstadion" , "Promis & Presse" )

joke24 :: Joke
joke24 = ( 24 , "Axel Kruse" , "Beim Football mu\223 man nicht ins Tor schie\223en, sondern oben dr\252ber. Das konnte ich schon immer ganz gut." , "nachdem ihn das Football-Team Berlin Thunder als Kicker eingestellt hatte" , "Spielerspr\252che" )

joke25 :: Joke
joke25 = ( 25 , "Axel Kruse" , "Die Gegner spielen mit f\252nf Mann und wir mit elf." , "als Hertha-Kapit\228n auf die Frage, was sich \228ndern muss, damit die Berliner wieder Erfolg haben" , "Spielerspr\252che" )

joke26 :: Joke
joke26 = ( 26 , "Axel Kruse" , "Ich hatte vor der Saison ein Angebot aus England. W\228re ich blo\223 hingegangen. In England ist Fu\223ball wenigstens noch M\228nnersport - und nichts f\252r Tunten." , "nach einer roten Karte" , "Spielerspr\252che" )

joke27 :: Joke
joke27 = ( 27 , "Bela Rethy" , "Das da vorn, was aussieht wie eine Klob\252rste, ist Valderrama.\187" , "" , "Reporterspr\252che" )

joke28 :: Joke
joke28 = ( 28 , "Bela Rethy" , "Der Oberarm geh\246rt zur Hand.\187" , "" , "Reporterspr\252che" )

joke29 :: Joke
joke29 = ( 29 , "Bela Rethy" , "Portugal spielt heute mit sechs Ausl\228ndern.\187" , "" , "Reporterspr\252che" )

joke30 :: Joke
joke30 = ( 30 , "Bela Rethy" , "Ziege ist da umgeknickt... Scheint sich um eine Schulterverletzung zu handeln.\187" , "" , "Reporterspr\252che" )

joke31 :: Joke
joke31 = ( 31 , "Beni Turnher" , "Der Rasen sieht alt und gebraucht aus, irgendwie erinnert er mich an die Kleider der Kelly Family.\187" , "" , "Reporterspr\252che" )

joke32 :: Joke
joke32 = ( 32 , "Beni Turnher" , "Jetzt, \220berzahl! Zwei gegen zwei!\187" , "" , "Reporterspr\252che" )

joke33 :: Joke
joke33 = ( 33 , "Bernd H\246lzenbein" , "Unser Training war so geheim, dass wir manchmal selbst nicht zuschauen durften.\187" , "" , "Spielerspr\252che" )

joke34 :: Joke
joke34 = ( 34 , "Bernd Krauss" , "Vielleicht liegt das Geheimnis unseres Erfolges darin, dass mich meine Spieler nicht verstehen." , "als Trainer in Spanien" , "Trainerspr\252che" )

joke35 :: Joke
joke35 = ( 35 , "Bernd Krauss" , "Wir wollten unbedingt einen fr\252hen R\252ckstand vermeiden. Das ist uns auch gelungen. Der VfB Stuttgart hat in den ersten zweieinhalb Minuten kein Tor geschossen.\187" , "" , "Trainerspr\252che" )

joke36 :: Joke
joke36 = ( 36 , "Bernd Schuster" , "Dieser Stolperk\246nig ist die H\246chststrafe f\252r jeden Mitspieler." , "\252ber seinen Spieler Holger Gai\223mayer" , "Trainerspr\252che" )

joke37 :: Joke
joke37 = ( 37 , "Bert Papon" , "Irgendwelche Fragen, bevor ich gehe und mich aufh\228nge?" , "der Trainer von Dumfernline auf einer Pressekonferenz nach einer 0:7-Niederlage" , "Trainerspr\252che" )

joke38 :: Joke
joke38 = ( 38 , "Berti Vogts" , "Das Spielfeld war zu lang f\252r Doppelp\228sse.\187" , "" , "Trainerspr\252che" )

joke39 :: Joke
joke39 = ( 39 , "Berti Vogts" , "Die Kroaten sollen ja auf alles treten, was sich bewegt - da hat unser Mittelfeld ja nichts zu bef\252rchten." , "vor dem WM-Spiel gegen Kroatien" , "Trainerspr\252che" )

joke40 :: Joke
joke40 = ( 40 , "Berti Vogts" , "Ha\223 geh\246rt nicht ins Stadion. Solche Gef\252hle soll man gemeinsam mit seiner Frau daheim im Wohnzimmer ausleben.\187" , "" , "Trainerspr\252che" )

joke41 :: Joke
joke41 = ( 41 , "Berti Vogts" , "Hinten spielt die deutsche Mannschaft Mann gegen Mann." , "bei der Frauen-WM in den USA" , "Reporterspr\252che" )

joke42 :: Joke
joke42 = ( 42 , "Berti Vogts" , "Ich bin mir sicher, unserer Mannschaft wird nichts passieren." , "als Mannschaftskapit\228n bei der WM 1978 in Argentinien zur Folterpraxis des dortigen Milit\228rregimes" , "Spielerspr\252che" )

joke43 :: Joke
joke43 = ( 43 , "Berti Vogts" , "Ich glaube, dass der Tabellenerste jederzeit den Spitzenreiter schlagen kann.\187" , "" , "Trainerspr\252che" )

joke44 :: Joke
joke44 = ( 44 , "Berti Vogts" , "Im Halbfinale haben wir die ersten 40 Minuten hervorragenden Fu\223ball gespielt." , "nach der WM 98 im Sportstudio" , "Trainerspr\252che" )

joke45 :: Joke
joke45 = ( 45 , "Berti Vogts" , "Kompliment an meine Mannschaft und meinen Dank an die Mediziner. Sie haben Unmenschliches geleistet.\187" , "" , "Trainerspr\252che" )

joke46 :: Joke
joke46 = ( 46 , "Berti Vogts" , "Sex vor einem Spiel? Das k\246nnen meine Jungs halten, wie sie wollen. Nur in der Halbzeit, da geht nichts.\187" , "" , "Trainerspr\252che" )

joke47 :: Joke
joke47 = ( 47 , "Berti Vogts" , "Vielleicht war es mit mir wie mit Helmut Kohl. Dessen Gesicht haben sie ja auch nicht mehr gewollt.\187" , "" , "Trainerspr\252che" )

joke48 :: Joke
joke48 = ( 48 , "Berti Vogts" , "Wenn ich \252ber's Wasser laufe, dann sagen meine Kritiker, nicht mal schwimmen kann er.\187" , "" , "Trainerspr\252che" )

joke49 :: Joke
joke49 = ( 49 , "Berti Vogts" , "Wenn wir Deutschen tanzen, und nebenan tanzen Brasilianer, dann sieht das bei uns eben aus wie bei K\252hlschr\228nken.\187" , "" , "Trainerspr\252che" )

joke50 :: Joke
joke50 = ( 50 , "Berti Vogts" , "Wir haben ein Abstimmungsproblem - das m\252ssen wir automatisieren.\187" , "" , "Trainerspr\252che" )

joke51 :: Joke
joke51 = ( 51 , "Berti Vogts" , "Wir wissen alle, dass Mario nicht gesagt hat, was er gesagt hat, was er gesagt haben soll, dass er es gesagt hat.\187" , "" , "Trainerspr\252che" )

joke52 :: Joke
joke52 = ( 52 , "Bill Shankley" , "Manche Leute halten Fu\223ball f\252r eine Sache von Leben und Tod. Ich bin von dieser Einstellung sehr entt\228uscht. Ich kann Ihnen versichern, es ist sehr viel wichtiger als das!\187" , "" , "Trainerspr\252che" )

joke53 :: Joke
joke53 = ( 53 , "Borussia Dortmund" , "In Erwartung eines spannenden Westfalenderbys mit einem positiven Ausgang f\252r die schwarz-gelben Farben verbleiche ich herzlichst Ihr Dr. Gerd Niebaum." , "zitiert Pr\228sident Niebaum vor dem 1:3 gegen den Letzten Bielefeld" , "Promis & Presse" )

joke54 :: Joke
joke54 = ( 54 , "Boubacar Diarra" , "Ich bin aufgewacht, habe aus dem Fenster geguckt, den Schnee gesehen - da war f\252r mich klar: Heute ist kein Training. Doch dann ist der Trainer gekommen und hat gesagt, dass wir rausgehen." , "Freiburgs Abwehrspieler aus Mali \252ber seine ersten Erfahrungen mit Schnee" , "Spielerspr\252che" )

joke55 :: Joke
joke55 = ( 55 , "Brian Moore" , "Rosenborg hat 66 Spiele gewonnen, und sie haben in jedem getroffen!\187" , "" , "Trainerspr\252che" )

joke56 :: Joke
joke56 = ( 56 , "Bruno Labbadia" , "Das wird alles von den Medien hochsterilisiert.\187" , "" , "Spielerspr\252che" )

joke57 :: Joke
joke57 = ( 57 , "Bryan Robson" , "W\252rden wir jede Woche so spielen, w\228ren unsere Leistungen nicht so schwankend.\187" , "" , "Trainerspr\252che" )

joke58 :: Joke
joke58 = ( 58 , "Campino" , "Endlich seid Ihr die Schwuchtel los." , "zum Dortmunder Konzertpublikum nach Andreas M\246llers Wechsel zu Schalke 04" , "Promis & Presse" )

joke59 :: Joke
joke59 = ( 59 , "Carmen Thomas" , "Schalke 05.\187" , "" , "Reporterspr\252che" )

joke60 :: Joke
joke60 = ( 60 , "Carsten Fuss" , "Ausw\228rts sind die Greuther st\228rker als in der Fremde.\187" , "" , "Reporterspr\252che" )

joke61 :: Joke
joke61 = ( 61 , "Christine Reinhardt" , "Na, Herr Chapuisat, hat denn Berti Vogts schon bei Ihnen angeklopft?" , "zum Schweizer Nationalspieler" , "Reporterspr\252che" )

joke62 :: Joke
joke62 = ( 62 , "Christoph Daum" , "Das Gegentor fiel zum psychologisch ung\252nstigsten Zeitpunkt. Aber man muss an dieser Stelle auch einmal die Frage stellen, ob es Gegentore gibt, die zu einem psychologisch g\252nstigen Zeitpunkt fallen.\187" , "" , "Trainerspr\252che" )

joke63 :: Joke
joke63 = ( 63 , "Christoph Daum" , "Das ist so, als wenn Dir einer ein Messer in den Bauch rammt, und Du mu\223t noch dabei l\228cheln." , "zur Leistung des Schiedsrichters" , "Trainerspr\252che" )

joke64 :: Joke
joke64 = ( 64 , "Christoph Daum" , "Einige Spieler wissen zwar, wer im Film \180Star Trek\180 welche Rolle spielt, aber nicht, mit wem sie es im n\228chsten Spiel zu tun haben.\187" , "" , "Trainerspr\252che" )

joke65 :: Joke
joke65 = ( 65 , "Christoph Daum" , "Er hat angezeigt, dass er in einer Minute ausgewechselt werden will." , "\252ber einen Stinkefinger von Ulf Kirsten" , "Trainerspr\252che" )

joke66 :: Joke
joke66 = ( 66 , "Christoph Daum" , "Man muss nicht immer die absolute Mehrheit hinter sich haben, manchmal reichen auch 51 Prozent.\187" , "" , "Trainerspr\252che" )

joke67 :: Joke
joke67 = ( 67 , "Christoph Daum" , "Wer in Bochum von Strafraum zu Strafraum geht und sich dabei nicht den Kn\246chel bricht, dem gebe ich einen aus.\187" , "" , "Trainerspr\252che" )

joke68 :: Joke
joke68 = ( 68 , "Christoph Daum" , "Wie soll ich mich f\252hlen!? Ich freue mich immer \252ber Niederlagen!" , "nach einer Niederlage auf seine Gef\252hle angesprochen" , "Trainerspr\252che" )

joke69 :: Joke
joke69 = ( 69 , "Christoph Daum" , "Wir haben ungef\228hr 27 Gruppen im Kader. Wir treten an unter der Pr\228misse der Artenvielfalt." , "\252ber die Gr\252ppchenbildung in seinem Team" , "Trainerspr\252che" )

joke70 :: Joke
joke70 = ( 70 , "Das Bayern-Echo" , "Jetzt fahren wir selbstbewu\223t an die Ostseek\252ste!" , "vor einem Ausw\228rtsspiel in Bremen" , "Promis & Presse" )

joke71 :: Joke
joke71 = ( 71 , "Das Gei\223bock-Echo" , "Borussia Dortmund. Vereinsfarben: Schwarz-Geld." , "angeblich ein Druckfehler" , "Promis & Presse" )

joke72 :: Joke
joke72 = ( 72 , "Dettmar Cramer" , "Es h\228ngt alles irgendwo zusammen. Sie k\246nnen sich am Hintern ein Haar ausrei\223en, dann tr\228nt das Auge.\187" , "" , "Trainerspr\252che" )

joke73 :: Joke
joke73 = ( 73 , "Die Siegener Zeitung" , "...Dahlin, Sohn einer schwedischen Psychologin und eines venezuelanischen Bongotrommlers, kann sich seine Durststrecke beim HSV nicht erkl\228ren...\187" , "" , "Promis & Presse" )

joke74 :: Joke
joke74 = ( 74 , "Die Zeitschrift Sports" , "Eher tritt der Papst aus der Kirche aus, als dass ein M\252nchner vom \252berzeugten Bayer zum Sechziger wird.\187" , "" , "Promis & Presse" )

joke75 :: Joke
joke75 = ( 75 , "Diego Maradona" , "Die haben viereckige F\252\223e. Das sind Robocops." , "\252ber Norwegen und Schottland" , "Spielerspr\252che" )

joke76 :: Joke
joke76 = ( 76 , "Diego Maradona" , "Es war die Hand Gottes." , "auf die Frage, ob er das entscheidende Tor im WM-Viertelfinale gegen England bei der WM 86 mit der Hand erzielt habe" , "Spielerspr\252che" )

joke77 :: Joke
joke77 = ( 77 , "Dieter Eilts" , "Das interessiert mich wie eine geplatzte Currywurst im ostfriesischen Wattenmeer.\187" , "" , "Spielerspr\252che" )

joke78 :: Joke
joke78 = ( 78 , "Dieter Eilts" , "Wenn meine Oma ein Bus w\228re, dann k\246nnte sie hupen!" , "auf eine H\228tte-wenn-und-aber-Frage eines Journalisten" , "Spielerspr\252che" )

joke79 :: Joke
joke79 = ( 79 , "Dieter Eilts" , "Wir sind insgesamt so gefestigt, dass jeder die Meinung des Trainers akzeptiert.\187" , "" , "Spielerspr\252che" )

joke80 :: Joke
joke80 = ( 80 , "Dieter K\252rten" , "Die Stadt ist schwarz voller Menschen in orange.\187" , "" , "Reporterspr\252che" )

joke81 :: Joke
joke81 = ( 81 , "Dieter Nuhr" , "Bei der Fu\223ball-WM habe ich mir \214sterreich gegen Kamerun angeschaut. Warum? Auf der einen Seite Exoten, fremde Kultur, wilde Riten - und auf der anderen Seite Kamerun!\187" , "" , "Promis & Presse" )

joke82 :: Joke
joke82 = ( 82 , "Dino Zoff" , "Es ist das Schicksal aller Trainer, fr\252her oder sp\228ter mit Tomaten beworfen zu werden.\187" , "" , "Trainerspr\252che" )

joke83 :: Joke
joke83 = ( 83 , "Dragoslav Stepanovic" , "Erste Pass, gleich Schei\223e!" , "beim ersten Training nach seinem Comeback in Frankfurt" , "Trainerspr\252che" )

joke84 :: Joke
joke84 = ( 84 , "Dragoslav Stepanovic" , "Montag, Dienstag, Mittwoch, Donnerstag..." , "auf die Frage eines Reporters, was die kommende Woche bringe" , "Trainerspr\252che" )

joke85 :: Joke
joke85 = ( 85 , "Dragoslav Stepanovic" , "Was der Rudi Bommer heute mit seinen 800 Jahren geleistet hat, war schon ph\228nomenal.\187" , "" , "Trainerspr\252che" )

joke86 :: Joke
joke86 = ( 86 , "DSF-Kommentatoren" , "Eigentlich sind wir hier \252berfl\252ssig." , "beim Spiel D\252sseldorf-Karlsruhe" , "Reporterspr\252che" )

joke87 :: Joke
joke87 = ( 87 , "DSF-Kommentatoren" , "Es ist \252berhaupt erstaunlich, dass Wattenscheid a) ins Endspiel und b) ins Finale kommt." , "beim Hallenturnier in Bremen" , "Reporterspr\252che" )

joke88 :: Joke
joke88 = ( 88 , "Edgar Endress" , "Bisher ziehen sich die Bayern toll aus der Atmosph\228re.\187" , "" , "Reporterspr\252che" )

joke89 :: Joke
joke89 = ( 89 , "Edmund Stoiber" , "Unterhaching ist von der gesamttechnischen Perfektion sehr kompakt.\187" , "" , "Promis & Presse" )

joke90 :: Joke
joke90 = ( 90 , "Ein Dortmunder Ordner" , "Was stehst Du so breitbeinig? Hast Du so dicke Eier?" , "bei der Leibesvisitation zu einem Besucher" , "Promis & Presse" )

joke91 :: Joke
joke91 = ( 91 , "Ein Journalist" , "Herr Rehhagel, mit welcher Farbe mu\223 ich zu Hause bei mir die W\228nde streichen?" , "auf einer Pressekonferenz, nachdem Otto sagte, er antworte nur noch auf Fachfragen" , "Promis & Presse" )

joke92 :: Joke
joke92 = ( 92 , "Ein Swissair-Pilot" , "Ich weiss gar nicht, ob wir euch mit nach Hause nehmen sollen." , "zur Schweizer Nationalmannschaft nach einer 0:3-Niederlage in Tschechien" , "Promis & Presse" )

joke93 :: Joke
joke93 = ( 93 , "Eric Meijer" , "Es ist nichts schei\223er als Platz zwei.\187" , "" , "Spielerspr\252che" )

joke94 :: Joke
joke94 = ( 94 , "Eric Meijer" , "Wenn du so gerne das F\228hnchen schwenkst, dann such dir doch 'nen Job am Flughafen." , "zum Linienrichter" , "Spielerspr\252che" )

joke95 :: Joke
joke95 = ( 95 , "Erich Laaser" , "Balakov hat viel Raum - und Platz ohne Ende.\187" , "" , "Reporterspr\252che" )

joke96 :: Joke
joke96 = ( 96 , "Erich Ribbeck" , "Bei uns wird auf dem Platz zu wenig gesprochen. Das k\246nnte an der Kommunikation liegen.\187" , "" , "Trainerspr\252che" )

joke97 :: Joke
joke97 = ( 97 , "Erich Ribbeck" , "Dies kann ein Nachteil oder ein Vorteil sein, sowohl f\252r uns als auch f\252r die gegnerische Mannschaft." , "\252ber die aufgeladene Atmosph\228re vor dem L\228nderspiel in der T\252rkei" , "Trainerspr\252che" )

joke98 :: Joke
joke98 = ( 98 , "Erich Ribbeck" , "Es ist egal, ob ein Spieler bei Bayern M\252nchen spielt oder sonstwo im Ausland.\187" , "" , "Trainerspr\252che" )

joke99 :: Joke
joke99 = ( 99 , "Erich Ribbeck" , "Ich kann es mir als Verantwortlicher f\252r die Mannschaft nicht erlauben, die Dinge subjektiv zu sehen. Grunds\228tzlich werde ich versuchen zu erkennen, ob die subjektiv ge\228u\223erten Meinungen subjektiv sind oder objektiv sind. Wenn sie subjektiv sind, dann werde ich an meinen objektiven festhalten. Wenn sie objektiv sind, werde ich \252berlegen und vielleicht die objektiven subjektiv ge\228u\223erten Meinungen der Spieler mit in meine objektiven einflie\223en lassen.\187" , "" , "Trainerspr\252che" )

joke100 :: Joke
joke100 = ( 100 , "Erich Ribbeck" , "Konzepte sind Kokolores.\187" , "" , "Trainerspr\252che" )

joke101 :: Joke
joke101 = ( 101 , "Erich Rutem\246ller" , "Mach et, Otze!\187" , "" , "Trainerspr\252che" )

joke102 :: Joke
joke102 = ( 102 , "Ernst Happel" , "Haut's Eich in Schnee! (steht auf und geht)" , "w\228hrend einer Pressekonferenz zu den Reportern" , "Trainerspr\252che" )

joke103 :: Joke
joke103 = ( 103 , "Ernst Happel" , "Wann's red'n wollen, m\252ssen's Staubsaugervertreter werden. Ich brauche nur Fu\223baller." , "Hansi M\252ller zu ihm: Trainer, wir m\252ssen miteinander reden." , "Trainerspr\252che" )

joke104 :: Joke
joke104 = ( 104 , "Ernst Kuzorra" , "Bei Schalke." , "erkl\228rt dem schwedischen K\246nig, wo Gelsenkirchen liegt" , "Spielerspr\252che" )

joke105 :: Joke
joke105 = ( 105 , "Ernst Middendorp" , "Hauen Sie ab, Sie Arschloch, Sie Schwein, nehmen Sie das Mikro weg." , "zu einem Reporter einer Bielefelder Tageszeitung" , "Trainerspr\252che" )

joke106 :: Joke
joke106 = ( 106 , "Erwin Kostedde" , "Ich m\246chte nie mehr arbeiten, sondern nur noch am Tresen stehen und saufen.\187" , "" , "Spielerspr\252che" )

joke107 :: Joke
joke107 = ( 107 , "Eugen Drewermann" , "Kein Pferd w\252rde auf den K\246rper eines Menschen treten, der am Boden liegt. Kroatische Spieler schon.\187" , "" , "Promis & Presse" )

joke108 :: Joke
joke108 = ( 108 , "Ewald Lienen" , "Der neue Rahmenterminkalender ist so voll, da gibt es in den n\228chsten zwei Jahren keinen Termin, an dem man mit seiner Frau Kaffee trinken kann.\187" , "" , "Trainerspr\252che" )

joke109 :: Joke
joke109 = ( 109 , "Ewald Lienen" , "Ich habe ihn ausgewechselt, weil ich einen anderen Spieler einwechseln wollte. Da mu\223te ich einen auswechseln.\187" , "" , "Trainerspr\252che" )

joke110 :: Joke
joke110 = ( 110 , "Ewald Lienen" , "Wir sind auf dem richtigen Weg!" , "nach 5 verlorenen Spielen mit dem MSV Duisburg in Folge" , "Trainerspr\252che" )

joke111 :: Joke
joke111 = ( 111 , "Fabrizio Hayer" , "Ich wei\223 auch nicht, wo bei uns der Wurm h\228ngt.\187" , "" , "Spielerspr\252che" )

joke112 :: Joke
joke112 = ( 112 , "Felix Magath" , "Franz B\246hmert hat versucht, Wolfgang Sidka den Abgang so sch\246n wie m\246glich zu machen.\187" , "" , "Trainerspr\252che" )

joke113 :: Joke
joke113 = ( 113 , "Felix Magath" , "H\228tte er die Mentalit\228t eines Schleswig-Holsteiners, k\246nnte er ein noch gr\246\223erer Fu\223baller werden, als er schon ist." , "\252ber Rodolfo Esteban Cardoso" , "Trainerspr\252che" )

joke114 :: Joke
joke114 = ( 114 , "Felix Magath" , "Ja, bis morgen fr\252h um acht." , "nach einem gro\223en Sieg auf die Frage, ob er seiner Mannschaft jetzt frei gebe" , "Trainerspr\252che" )

joke115 :: Joke
joke115 = ( 115 , "Frank Elstner" , "Jaja, der Lothar ist sehr bekannt." , "in Jeopardy, nachdem eine Kandidatin den Namen Klinsmann erw\228hnte" , "Promis & Presse" )

joke116 :: Joke
joke116 = ( 116 , "Frank Mill" , "Der Linienrichter hatte wohl einen Holzarm." , "nach einem Spiel, in dem st\228ndig abseits gegeben wurde" , "Spielerspr\252che" )

joke117 :: Joke
joke117 = ( 117 , "Frank Pagelsdorf" , "Es hat sich gezeigt, dass Haching gerade zuhause so heimstark ist.\187" , "" , "Trainerspr\252che" )

joke118 :: Joke
joke118 = ( 118 , "Frank Pagelsdorf" , "Wir werden nur noch Einzelgespr\228che f\252hren, damit sich keiner verletzt.\187" , "" , "Trainerspr\252che" )

joke120 :: Joke
joke120 = ( 120 , "Franz Beckenbauer" , "Berkant \214ktan ist erst siebzehn. Wenn er Gl\252ck hat, wird er n\228chsten Monat achtzehn.\187" , "" , "Trainerspr\252che" )

joke121 :: Joke
joke121 = ( 121 , "Franz Beckenbauer" , "Damals hat die halbe Nation hinter dem Fernseher gestanden." , "\252ber das WM-Finale 1990" , "Trainerspr\252che" )

joke122 :: Joke
joke122 = ( 122 , "Franz Beckenbauer" , "Das ist Weltrekord in der T\252rkei." , "\252ber den Pr\228sidenten von Besiktas Instanbul, der seit 14 Jahren im Amt war" , "Trainerspr\252che" )

joke123 :: Joke
joke123 = ( 123 , "Franz Beckenbauer" , "Der Grund war nicht die Ursache, sondern der Ausl\246ser.\187" , "" , "Trainerspr\252che" )

joke124 :: Joke
joke124 = ( 124 , "Franz Beckenbauer" , "Der Suker ist nat\252rlich ein Stehgeiger, der rumsteht." , "w\228hrend einer Champions-League-\220bertragung" , "Reporterspr\252che" )

joke125 :: Joke
joke125 = ( 125 , "Franz Beckenbauer" , "Deutschland wird auf Jahre hin unbesiegbar sein." , "nach dem WM-Titel 1990" , "Trainerspr\252che" )

joke126 :: Joke
joke126 = ( 126 , "Franz Beckenbauer" , "Die L\246wen werden das M\252nchner Derby fr\252hestens in 100 Jahren gewinnen." , "kurz vor dem 1:0-Sieg der 60er gegen Bayern" , "Trainerspr\252che" )

joke127 :: Joke
joke127 = ( 127 , "Franz Beckenbauer" , "Die Schweden sind keine Holl\228nder - das hat man ganz genau gesehen.\187" , "" , "Reporterspr\252che" )

joke128 :: Joke
joke128 = ( 128 , "Franz Beckenbauer" , "Ich bin immer noch am \220berlegen, welche Sportart meine Mannschaft an diesem Abend ausge\252bt hat. Fu\223ball war\146s mit Sicherheit nicht." , "nach einer Bayern-Niederlage" , "Trainerspr\252che" )

joke129 :: Joke
joke129 = ( 129 , "Franz Beckenbauer" , "In einem Jahr hab ich mal 15 Monate durchgespielt.\187" , "" , "Trainerspr\252che" )

joke130 :: Joke
joke130 = ( 130 , "Franz Beckenbauer" , "Ja gut, am Ergebnis wird sich nicht mehr viel \228ndern, es sei denn, es schie\223t einer ein Tor.\187" , "" , "Reporterspr\252che" )

joke131 :: Joke
joke131 = ( 131 , "Franz Beckenbauer" , "Ja gut, es gibt nur eine M\246glichkeit: Sieg, Unentschieden oder Niederlage!\187" , "" , "Reporterspr\252che" )

joke132 :: Joke
joke132 = ( 132 , "Franz Beckenbauer" , "Kaiserslautern wird mit Sicherheit nicht ins blinde Messer laufen.\187" , "" , "Trainerspr\252che" )

joke133 :: Joke
joke133 = ( 133 , "Franz Beckenbauer" , "Nun gut, das 0:0, da war nat\252rlich Pech dabei. Also, es waren, es, also simmer zufrieden, ich, m\246glicherweise, um das abzuschlie\223en, vielleicht hat nach den 90 Minuten, wenn man alles zusammenz\228hlt, dass vielleicht keiner den Sieg verdient hat." , "w\228hrend einer Champions-League-\220bertragung" , "Reporterspr\252che" )

joke134 :: Joke
joke134 = ( 134 , "Fredrik Ljungberg" , "Wenn ich in der Nacht vor einem Spiel Sex habe, verliere ich jegliches Gef\252hl in meinen F\252\223en.\187" , "" , "Spielerspr\252che" )

joke135 :: Joke
joke135 = ( 135 , "Friedel Rausch" , "Der Abstieg trifft sicher eine Mannschaft, die noch gar nicht damit rechnet." , "kurz bevor er mit dem Club absteigen musste" , "Trainerspr\252che" )

joke136 :: Joke
joke136 = ( 136 , "Friedel Rausch" , "Ich will jetzt nicht noch zus\228tzlich Feuer ins \214l gie\223en.\187" , "" , "Trainerspr\252che" )

joke137 :: Joke
joke137 = ( 137 , "Friedel Rausch" , "Wenn ich den Martin Schneider weiter aufstelle, glauben die Leute am Ende wirklich noch, ich sei schwul.\187" , "" , "Trainerspr\252che" )

joke138 :: Joke
joke138 = ( 138 , "Friedhelm Funkel" , "Die Situation ist bedrohlich, aber nicht bedenklich.\187" , "" , "Trainerspr\252che" )

joke139 :: Joke
joke139 = ( 139 , "Fritz Langner" , "Ihr f\252nf spielt jetzt vier gegen drei.\187" , "" , "Trainerspr\252che" )

joke140 :: Joke
joke140 = ( 140 , "Fritz Walter jun." , "Der J\252rgen Klinsmann und ich, wir sind ein gutes Trio. (etwas sp\228ter dann) Ich meinte: ein Quartett.\187" , "" , "Spielerspr\252che" )

joke141 :: Joke
joke141 = ( 141 , "Fritz Walter jun." , "Die Sanit\228ter haben mir sofort eine Invasion gelegt.\187" , "" , "Spielerspr\252che" )

joke142 :: Joke
joke142 = ( 142 , "Fritz Walter jun." , "Ha, auch Walter!" , "auf die Frage, wie denn seine Frau hei\223e" , "Spielerspr\252che" )

joke143 :: Joke
joke143 = ( 143 , "Gary Lineker" , "Fu\223ball ist ein Spiel von 22 Leuten, die rumlaufen, den Ball spielen, und einem Schiedsrichter, der eine Reihe dummer Fehler macht, und am Ende gewinnt immer Deutschland.\187" , "" , "Spielerspr\252che" )

joke144 :: Joke
joke144 = ( 144 , "George Best" , "Ich habe viel von meinem Geld f\252r Alkohol, Weiber und schnelle Autos ausgegeben... Den Rest habe ich einfach verpra\223t.\187" , "" , "Spielerspr\252che" )

joke145 :: Joke
joke145 = ( 145 , "George Best" , "Ich k\246nnte den anonymen Alkoholikern beitreten. Das Problem dabei ist nur, ich kann nicht anonym bleiben.\187" , "" , "Spielerspr\252che" )

joke146 :: Joke
joke146 = ( 146 , "Gerald Asamoah" , "Da krieg' ich so den Ball und das ist ja immer mein Problem.\187" , "" , "Spielerspr\252che" )

joke147 :: Joke
joke147 = ( 147 , "Gerd Delling" , "An der Temperatur hat sich nichts ge\228ndert. Es ist noch k\228lter geworden.\187" , "" , "Reporterspr\252che" )

joke148 :: Joke
joke148 = ( 148 , "Gerd Delling" , "Die Luft, die nie drin war, ist raus aus dem Spiel.\187" , "" , "Reporterspr\252che" )

joke149 :: Joke
joke149 = ( 149 , "Gerd Delling" , "Die Schotten feiern richtig sch\246n, ohne Ausschreitungen. Die trinken so viel, da k\246nnen sie gar nicht mehr zuhauen.\187" , "" , "Reporterspr\252che" )

joke150 :: Joke
joke150 = ( 150 , "Gerd M\252ller" , "Marmorkuchen, den ich sehr gerne esse, b\228ckt meine Frau, sooft ich Appetit darauf habe." , "in seiner Autobiographie" , "Promis & Presse" )

joke151 :: Joke
joke151 = ( 151 , "Gerd Niebaum" , "Heute haben die Spieler doch mehr Pressekontakte als Ballkontakte.\187" , "" , "Trainerspr\252che" )

joke152 :: Joke
joke152 = ( 152 , "Gerd Rubenbauer" , "Auch wenn er \252ber links kommt, hat er nur einen rechten Fu\223.\187" , "" , "Reporterspr\252che" )

joke153 :: Joke
joke153 = ( 153 , "Gerd Rubenbauer" , "Die Achillesferse von Bobic ist die rechte Schulter.\187" , "" , "Reporterspr\252che" )

joke154 :: Joke
joke154 = ( 154 , "Gerd Rubenbauer" , "Die Mannschaft hat ihren Libero aufgel\246st - jetzt hat sie einen Mann mehr auf dem Platz.\187" , "" , "Reporterspr\252che" )

joke155 :: Joke
joke155 = ( 155 , "Gerd Rubenbauer" , "Die Rudi-Rufe hat es vorher nur f\252r Uwe Seeler gegeben.\187" , "" , "Reporterspr\252che" )

joke156 :: Joke
joke156 = ( 156 , "Gerd Rubenbauer" , "Einen so harten Ellenbogen hat der in ganz Kolumbien noch nicht erlebt. Aber genaugenommen war es das Knie.\187" , "" , "Reporterspr\252che" )

joke157 :: Joke
joke157 = ( 157 , "Gerd Rubenbauer" , "Jetzt wechselt Jamaika den Torh\252ter aus!" , "der FIFA-Beauftragte zeigt eine Minute Nachspielzeit an" , "Reporterspr\252che" )

joke158 :: Joke
joke158 = ( 158 , "Gerd Rubenbauer" , "Sie spielen wechselnd alternierend.\187" , "" , "Reporterspr\252che" )

joke159 :: Joke
joke159 = ( 159 , "Gerd Rubenbauer" , "Wenn er von hinten kommt, ist er nicht zu halten!\187" , "" , "Reporterspr\252che" )

joke160 :: Joke
joke160 = ( 160 , "Gerd Rubenbauer" , "Zidane hat den Oberk\246rper eines echten Zweik\228mpfers, aber die Fu\223sohlen einer Ballerina.\187" , "" , "Reporterspr\252che" )

joke161 :: Joke
joke161 = ( 161 , "Gerhard Mayer-Vorfelder" , "Das Beste in M\252nchen ist immer das Mittagessen." , "nach einer Niederlage bei den Bayern" , "Trainerspr\252che" )

joke162 :: Joke
joke162 = ( 162 , "Gerhard Mayer-Vorfelder" , "Ich halte es f\252r wichtig, dass die Verhandlungen mit Spielern k\252nftig aus einer Hand gef\252hrt werden. Deshalb wird dem Karlheinz ein Jurist zur Seite gestellt." , "bei der Vorstellung von Karlheinz F\246rster als neuen Vorstand Sport beim VfB Stuttgart" , "Trainerspr\252che" )

joke163 :: Joke
joke163 = ( 163 , "Giovanni Trappatoni" , "Es gibt nur einen Ball. Wenn der Gegner ihn hat, mu\223 man sich fragen: Warum!? Ja, warum? Und was mu\223 man tun? Ihn sich wiederholen!\187" , "" , "Trainerspr\252che" )

joke164 :: Joke
joke164 = ( 164 , "Giovanni Trappatoni" , "Fu\223ball ist Ding, Dang, Dong. Es gibt nicht nur Ding.\187" , "" , "Trainerspr\252che" )

joke165 :: Joke
joke165 = ( 165 , "Giovanni Trappatoni" , "Ich habe fertig.\187" , "" , "Trainerspr\252che" )

joke166 :: Joke
joke166 = ( 166 , "Glenn Hoddle" , "Wenn wir meinen, die Spieler haben es n\246tig, dann lassen wir ihre Frauen und Freundinnen hierher holen, um die Jungs wieder hoch zu kriegen.\187" , "" , "Trainerspr\252che" )

joke167 :: Joke
joke167 = ( 167 , "Guido Buchwald" , "Wir schaffen es halt immer noch nicht, den Gegner auf spielerische Art und Weise auszuspielen.\187" , "" , "Trainerspr\252che" )

joke168 :: Joke
joke168 = ( 168 , "G\252nter Jauch" , "F\252r alle Zuschauer, die erst jetzt eingeschaltet haben, das erste Tor ist schon gefallen." , "beim legend\228ren Spiel Real Madrid - BVB, kurz nachdem das Tor umgekippt ist" , "Reporterspr\252che" )

joke169 :: Joke
joke169 = ( 169 , "G\252nter Netzer" , "Da haben Spieler auf dem Spielfeld gestanden, gestandene Spieler...\187" , "" , "Reporterspr\252che" )

joke170 :: Joke
joke170 = ( 170 , "G\252nter Netzer" , "Da war ein sinnliches Verh\228ltnis zu meinem Objekt, das bei jedem Fu\223tritt anders reagierte, das stets anders behandelt werden wollte.\187" , "" , "Reporterspr\252che" )

joke171 :: Joke
joke171 = ( 171 , "G\252nter Netzer" , "Ich hoffe, dass die deutsche Mannschaft auch in der 2. Halbzeit eine runde Leistung zeigt, das w\252rde die Leistung abrunden!\187" , "" , "Reporterspr\252che" )

joke172 :: Joke
joke172 = ( 172 , "G\252nter Netzer" , "Man muss feststellen, dass der Spruch auch nicht mehr stimmt, dass der Sch\252tze nicht selber schie\223en soll. Ich stelle fest, dass der Sch\252tze sehr wohl den Elfmeter selber schie\223t.\187" , "" , "Reporterspr\252che" )

joke173 :: Joke
joke173 = ( 173 , "G\252nter Netzer" , "So etwas gibt es im Fu\223ball nicht." , "bei der WM 98 auf die Frage, was passiert wenn S\252dkorea gegen Holland gewinnt" , "Reporterspr\252che" )

joke174 :: Joke
joke174 = ( 174 , "Gyula Lorant" , "Der Ball ist rund. W\228re er eckig, w\228re er ja ein W\252rfel.\187" , "" , "Trainerspr\252che" )

joke175 :: Joke
joke175 = ( 175 , "Gyula Lorant" , "Wenn ich ihm sein linkes Bein wegnehme, f\228llt er einfach um, weil kein rechtes Bein da ist.\187" , "" , "Trainerspr\252che" )

joke176 :: Joke
joke176 = ( 176 , "Hans Deckert" , "Sie haben gro\223artig gespielt." , "als DFB-Spielausschussvorsitzender nach einem Spiel zum nicht eingesetzten Duisburger Spieler Herbert B\252ssers" , "Trainerspr\252che" )

joke177 :: Joke
joke177 = ( 177 , "Hans Krankl" , "Ich muss versuchen die Mannschaft so zu formen, dass wir gleich im ersten Spiel, auf jeden Fall aber so schnell wie m\246glich, punkten.\187" , "" , "Trainerspr\252che" )

joke178 :: Joke
joke178 = ( 178 , "Hans Krankl" , "Wir m\252ssen gewinnen, alles andere ist prim\228r.\187" , "" , "Spielerspr\252che" )

joke179 :: Joke
joke179 = ( 179 , "Hansi Kreische" , "Aus Schei\223e kann man keine Bonbons machen!" , "nach einer Niederlage seiner Mannschaft Dynamo Dresden" , "Trainerspr\252che" )

joke180 :: Joke
joke180 = ( 180 , "Hansi M\252ller" , "Es st\246rt mich nicht, dass in Deutschland ein paar hunderttausend Wellensittiche 'Hansi' hei\223en.\187" , "" , "Spielerspr\252che" )

joke181 :: Joke
joke181 = ( 181 , "Hansi M\252ller" , "Ich wei\223 gar nicht, warum die nicht alle die neue Traction-Sohle von adidas haben wie Klinsmann, der f\228llt wenigstens nicht um." , "als Co-Moderator und adidas-Repr\228sentant bei der EM 96 in der Halbzeitpause eines von Rutschpartien gepr\228gten Spiels" , "Reporterspr\252che" )

joke182 :: Joke
joke182 = ( 182 , "Hans-Peter Briegel" , "\220berhaupt nicht. Ich w\228re aber auch nicht \252berrascht gewesen, wenn Frau Rehhagel diese Position \252bernommen h\228tte." , "nach seinem R\252cktritt als Sportlicher Leiter des 1. FC Kaiserslautern im Kicker auf die Frage, ob er \252berrascht sei, dass seine bisherige Stelle nicht neu besetzt werde" , "Trainerspr\252che" )

joke183 :: Joke
joke183 = ( 183 , "Harald Schmidt" , "Deutschland besiegt die Amerikaner auf franz\246sischem Boden. Viele \228ltere Zuschauer hatten Tr\228nen in den Augen!" , "w\228hrend der WM 98" , "Promis & Presse" )

joke184 :: Joke
joke184 = ( 184 , "Harald Schmidt" , "Ein Trainerstab ist kein Vibrator.\187" , "" , "Promis & Presse" )

joke185 :: Joke
joke185 = ( 185 , "Harald Schmidt" , "Golden Goal ist schei\223e. Man wei\223 nie, ob man sich noch ein Bier holen soll.\187" , "" , "Promis & Presse" )

joke186 :: Joke
joke186 = ( 186 , "Harald Schmidt" , "J\252rgen Klinsmann ist inzwischen 694 Minuten ohne Tor. Das hat vor ihm, glaube ich, nur Sepp Maier geschafft.\187" , "" , "Promis & Presse" )

joke187 :: Joke
joke187 = ( 187 , "Harald Sp\246rl" , "Das war ein typisches 0:0-Spiel." , "nach einer 1:2-Niederlage" , "Spielerspr\252che" )

joke188 :: Joke
joke188 = ( 188 , "Helmut Rahn" , "Ich zieh ab mit dem linken Fu\223, und dat gibt son richtigen Aufsetzer. Wat dann passiert is, dat wisst ihr ja." , "\252ber sein 3:2-Siegtor im WM-Finale 1954" , "Spielerspr\252che" )

joke189 :: Joke
joke189 = ( 189 , "Helmut Schulte" , "Ball rund muss in Tor eckig!\187" , "" , "Trainerspr\252che" )

joke190 :: Joke
joke190 = ( 190 , "Helmut Schulte" , "Das gr\246\223te Problem beim Fu\223ball sind die Spieler. Wenn wir die abschaffen k\246nnten, w\228re alles gut.\187" , "" , "Trainerspr\252che" )

joke191 :: Joke
joke191 = ( 191 , "Helmut Schulte" , "Wer hinten steht, hat das Pech der Gl\252cklosen.\187" , "" , "Trainerspr\252che" )

joke192 :: Joke
joke192 = ( 192 , "Henry Vogt" , "Yeboah blieb heute sehr blass.\187" , "" , "Reporterspr\252che" )

joke193 :: Joke
joke193 = ( 193 , "Herbert Finken" , "Mein Name ist Finken, und Du wirst gleich hinken." , "der Berliner Tasmane begr\252\223t seinen Gegenspieler" , "Spielerspr\252che" )

joke194 :: Joke
joke194 = ( 194 , "Heribert Fa\223bender" , "Da kommt ein Spieler der Uerdinger Mannschaft frei, aber doch ungedeckt, zum Kopfball.\187" , "" , "Reporterspr\252che" )

joke195 :: Joke
joke195 = ( 195 , "Heribert Fa\223bender" , "Da singen sie: We are red, we are white, we are Danish Dynamite - Wir sind rote, wir sind wei\223e wir sind d\228nische..., \228h...\187" , "" , "Reporterspr\252che" )

joke196 :: Joke
joke196 = ( 196 , "Heribert Fa\223bender" , "Den Schiedsrichter, den sollte man zur\252ck in die Pampa schicken!" , "\252ber den argentinischen Schiri beim WM-Achtelfinale 1990 zwischen Holland und Deutschland" , "Reporterspr\252che" )

joke197 :: Joke
joke197 = ( 197 , "Heribert Fa\223bender" , "Es steht 1:1, genauso gut k\246nnte es umgekehrt stehen.\187" , "" , "Reporterspr\252che" )

joke198 :: Joke
joke198 = ( 198 , "Heribert Fa\223bender" , "Jeremies ist kein Eilts!\187" , "" , "Reporterspr\252che" )

joke199 :: Joke
joke199 = ( 199 , "Heribert Fa\223bender" , "Jetzt sind auch die Fans begeistert. Sie singen \"Oh, wie bist Du sch\246n!\"\187" , "" , "Reporterspr\252che" )

joke200 :: Joke
joke200 = ( 200 , "Heribert Fa\223bender" , "Koemann. Der hei\223t schon so. Dem w\252rde ich auch nicht \252ber den Weg trauen!\187" , "" , "Reporterspr\252che" )

joke201 :: Joke
joke201 = ( 201 , "Heribert Fa\223bender" , "Nicht verwandt mit dem Schlagers\228nger... der hei\223t \252brigens gar nicht so!" , "\252ber den Spieler Rebrov" , "Reporterspr\252che" )

joke202 :: Joke
joke202 = ( 202 , "Heribert Fa\223bender" , "Rivaldo ist ein Super-Techniker, oh, \228h, das ist ja Cafu!\187" , "" , "Reporterspr\252che" )

joke203 :: Joke
joke203 = ( 203 , "Heribert Fa\223bender" , "Roberto Baggio, der Mozart des Fu\223balls - nicht nur, was sein Freisto\223genie angeht.\187" , "" , "Reporterspr\252che" )

joke204 :: Joke
joke204 = ( 204 , "Heribert Fa\223bender" , "Roberto Carlos hat Ronaldo heute fest im Griff." , "beim WM-Finale 1998" , "Reporterspr\252che" )

joke205 :: Joke
joke205 = ( 205 , "Heribert Fa\223bender" , "So, jetzt ziehen wir mal ein Fazit!" , "nach f\252nf gespielten Minuten" , "Reporterspr\252che" )

joke206 :: Joke
joke206 = ( 206 , "Heribert Fa\223bender" , "Tags\252ber, wenn die Sonne scheint, ist es hier noch w\228rmer!" , "auf Teneriffa" , "Reporterspr\252che" )

joke207 :: Joke
joke207 = ( 207 , "Heribert Fa\223bender" , "Und da sehen Sie den schwedischen Vollmond.\187" , "" , "Reporterspr\252che" )

joke208 :: Joke
joke208 = ( 208 , "Heribert Fa\223bender" , "Und jetzt skandieren die Fans wieder: \"T\252rkiye! T\252rkiye!\", was so viel hei\223t wie \"T\252rkei! T\252rkei!\"\187" , "" , "Reporterspr\252che" )

joke209 :: Joke
joke209 = ( 209 , "Hermann Gerland" , "Bevor man untern Torf kommt, macht man einiges mit im Leben.\187" , "" , "Trainerspr\252che" )

joke210 :: Joke
joke210 = ( 210 , "Hermann Gerland" , "H\228tte, wenn und aber, alles nur bl\246des Gelaber!\187" , "" , "Trainerspr\252che" )

joke211 :: Joke
joke211 = ( 211 , "Hermann Gerland" , "Heute hatten wir Schei\223e anne F\252\223e!\187" , "" , "Trainerspr\252che" )

joke212 :: Joke
joke212 = ( 212 , "Hermann Gerland" , "Nach 20 Minuten waren zwei von denen angeschlagen. Die sind gehumpelt! Aber die sind schneller gehumpelt als wir gelaufen!" , "nach der 0:2-Niederlage von Arminia Bielefeld beim SSV Ulm" , "Trainerspr\252che" )

joke213 :: Joke
joke213 = ( 213 , "Hermann Gerland" , "Sicher, Reina hat das Tor erstklassig erzielt. Aber er durfte die Kugel doch gleich dreimal wie ein Artist hochhalten und dann reinhauen. Das h\228tte es fr\252her nicht gegeben. Da w\228re einer dazwischen gefegt und Billy w\228re erst wieder vor seiner alten Haust\252r in Unna gelandet.\187" , "" , "Trainerspr\252che" )

joke214 :: Joke
joke214 = ( 214 , "Hermann Neuberger" , "Die Breite an der Spitze ist dichter geworden.\187" , "" , "Trainerspr\252che" )

joke215 :: Joke
joke215 = ( 215 , "Holger Fach" , "Ja, aber das war kurz nach dem Zweiten Weltkrieg." , "auf die Frage, ob er schon einmal vor dem DFB-Sportgericht stand" , "Spielerspr\252che" )

joke216 :: Joke
joke216 = ( 216 , "Holger Obermann" , "Und wieder ein Konter - wieder Cha Bum - was macht er? - wieder dr\252ber!!" , "kommentiert eine Wiederholung, ohne es zu merken" , "Reporterspr\252che" )

joke217 :: Joke
joke217 = ( 217 , "Holger Obermann" , "Zwei Minuten gespielt, noch immer hohes Tempo.\187" , "" , "Reporterspr\252che" )

joke218 :: Joke
joke218 = ( 218 , "Horst Heldt" , "An die f\252nf lebenswichtigen Bausteine in Nutella." , "auf die \"Kicker\"-Frage, woran er glaube" , "Spielerspr\252che" )

joke219 :: Joke
joke219 = ( 219 , "Horst Hrubesch" , "Ich sage nur ein Wort: \"Vielen Dank!\"\187" , "" , "Spielerspr\252che" )

joke220 :: Joke
joke220 = ( 220 , "Horst Hrubesch" , "Manni Bananenflanke, ich Kopf, Tor!" , "schildert die Entstehung eines seiner Tore" , "Spielerspr\252che" )

joke221 :: Joke
joke221 = ( 221 , "Horst Hrubesch" , "Wir m\252ssen das alles nochmal Paroli laufen lassen.\187" , "" , "Trainerspr\252che" )

joke222 :: Joke
joke222 = ( 222 , "Horst Szymaniak" , "Ein Drittel? Nee, ich will mindestens ein Viertel.\187" , "" , "Spielerspr\252che" )

joke223 :: Joke
joke223 = ( 223 , "Ian Rush" , "Es war, als w\252rde ich im Ausland spielen." , "\252ber seine Zeit bei Juventus Turin" , "Spielerspr\252che" )

joke224 :: Joke
joke224 = ( 224 , "Ingo Anderbr\252gge" , "Das Tor geh\246rt zu 70 % mir und zu 40 % dem Wilmots.\187" , "" , "Spielerspr\252che" )

joke225 :: Joke
joke225 = ( 225 , "Jan Kocian" , "Der Dieter und ich, wir haben uns \252berlegt, dass wir von jetzt an nur noch Foul spielen wenn es n\246tig ist." , "angesprochen auf zwei rote Karten gegen ihn und Dieter Schlindwein" , "Spielerspr\252che" )

joke226 :: Joke
joke226 = ( 226 , "Jan-Aage Fj\246rtoft" , "Tore Pedersen ist ein sehr guter Freund. Ich bin jetzt hier, um f\252r ihn eine Frau zu finden." , "\252ber den Grund seines Wechsels zu Eintracht Frankfurt" , "Spielerspr\252che" )

joke227 :: Joke
joke227 = ( 227 , "Jean L\246ring" , "Du hast hier nichts mehr zu sagen, Du Wichser!" , "entlie\223 mit diesen Worten seinen Trainer Toni Schumacher in der Halbzeitpause" , "Reporterspr\252che" )

joke228 :: Joke
joke228 = ( 228 , "Jean L\246ring" , "Ich als Verein musste ja reagieren.\187" , "" , "Reporterspr\252che" )

joke229 :: Joke
joke229 = ( 229 , "Jean-Paul Sartre" , "Bei einem Fu\223ballspiel verkompliziert sich allerdings alles durch die Anwesenheit der gegnerischen Mannschaft.\187" , "" , "Promis & Presse" )

joke230 :: Joke
joke230 = ( 230 , "Jens Jeremies" , "Das ist Schnee von morgen.\187" , "" , "Spielerspr\252che" )

joke231 :: Joke
joke231 = ( 231 , "Joachim B\246ttcher" , "Jan Furtok, ein St\252rmer, wie man ihn an der Ramschtheke im Supermarkt zu Dutzenden findet...\187" , "" , "Reporterspr\252che" )

joke232 :: Joke
joke232 = ( 232 , "Joachim B\246ttcher" , "Jemand sollte Jan Furtok mal die polnische \220bersetzung der Memoiren Casanovas schenken, da steht n\228mlich drin, wie man seine Chancen nutzt!\187" , "" , "Reporterspr\252che" )

joke233 :: Joke
joke233 = ( 233 , "Joachim Hopp" , "Man muss sehen, dass man keine Pickel am Arsch kriegt und was unterschieben." , "\252ber sein Dasein als Duisburger Dauerreservist" , "Spielerspr\252che" )

joke234 :: Joke
joke234 = ( 234 , "Johannes B. Kerner" , "Dass er im Abseits stand, k\246nnen Sie an den Streifen im Rasen erkennen, die nach FIFA-Regeln gem\228ht wurden.\187" , "" , "Reporterspr\252che" )

joke235 :: Joke
joke235 = ( 235 , "Johannes B. Kerner" , "Gleich ziehen die deutschen Fans durch Frankreich." , "bei der WM 98 nach dem 2:2-Ausgleich Deutschlands gegen Jugoslawien in Lens, vor den Ausschreitungen deutscher Hooligans" , "Reporterspr\252che" )

joke236 :: Joke
joke236 = ( 236 , "Johannes B. Kerner" , "Ich schlage vor, Sie halten sich die Augen zu. Ich sage Ihnen jetzt n\228mlich die Bundesliga-Ergebnisse." , "in der Vorschau zum Aktuellen Sportstudio" , "Reporterspr\252che" )

joke237 :: Joke
joke237 = ( 237 , "Johannes B. Kerner" , "Oh, ich habe da gar kein Handspiel gesehen." , "Spieler tritt anderem fast die Z\228hne aus, Schiri pfeift" , "Reporterspr\252che" )

joke238 :: Joke
joke238 = ( 238 , "Johannes B. Kerner" , "P\228sse der Marke Lothar Matth\228us - da m\246chte man Ball sein.\187" , "" , "Reporterspr\252che" )

joke239 :: Joke
joke239 = ( 239 , "Johannes Rau" , "Wie soll das denn dann hei\223en? Ernst-Kuzorra-seine-Frau-ihr-Stadion?" , "zum Vorschlag, Fu\223ballstadien nach Frauen zu benennen" , "Promis & Presse" )

joke240 :: Joke
joke240 = ( 240 , "John Toshack" , "Am Montag nehme ich mir vor, zur n\228chsten Partie zehn Spieler auszuwechseln. Am Dienstag sind es sieben oder acht, am Donnerstag noch vier Spieler. Wenn es dann Samstag wird, stelle ich fest, dass ich doch wieder dieselben elf Schei\223kerle einsetzen muss wie in der Vorwoche." , "als Trainer von Real Madrid" , "Trainerspr\252che" )

joke241 :: Joke
joke241 = ( 241 , "J\246rg Berger" , "Resignation ist der Egoismus der Schwachen.\187" , "" , "Trainerspr\252che" )

joke242 :: Joke
joke242 = ( 242 , "J\246rg Dahlmann" , "Da geht er, ein gro\223er Spieler. Ein Mann wie Steffi Graf!" , "zum Abschied von Lothar Matth\228us:" , "Reporterspr\252che" )

joke243 :: Joke
joke243 = ( 243 , "J\246rg Dahlmann" , "Julio Cesar hat sich heute nacht fortgepflanzt. Victoria hei\223t die Kleine.\187" , "" , "Reporterspr\252che" )

joke244 :: Joke
joke244 = ( 244 , "J\246rg Dahlmann" , "M\246ller und Chappi befruchten sich gegenseitig.\187" , "" , "Reporterspr\252che" )

joke245 :: Joke
joke245 = ( 245 , "J\246rg Wontorra" , "Baslers Freist\246\223e sind wie das wirkliche Leben: Mal weich und kurz, mal hart und lang.\187" , "" , "Reporterspr\252che" )

joke246 :: Joke
joke246 = ( 246 , "J\246rg Wontorra" , "Das ist fast wie bei Lady Di!" , "KSC-Fans hatten nach der Sch\228fer-Entlassung Blumen vor dem Stadion niedergelegt" , "Reporterspr\252che" )

joke247 :: Joke
joke247 = ( 247 , "J\246rg Wontorra" , "Marc Wilmots, der Held vom Borsigplatz.\187" , "" , "Reporterspr\252che" )

joke248 :: Joke
joke248 = ( 248 , "J\246rg Wontorra" , "Telefonieren Sie mit uns oder rufen Sie uns an!\187" , "" , "Reporterspr\252che" )

joke249 :: Joke
joke249 = ( 249 , "Junior Baiano" , "Bremen war ein Alptraum. Es mu\223 die k\228lteste Stadt auf dieser Erde sein. Ich habe immer gefroren, ich habe niemanden verstanden, und mir ging es schlecht.\187" , "" , "Spielerspr\252che" )

joke250 :: Joke
joke250 = ( 250 , "Jupp Heynckes" , "Mark Hughes ist ein typich briticher Cht\252rmer. Weder Fich noch Fleich.\187" , "" , "Trainerspr\252che" )

joke251 :: Joke
joke251 = ( 251 , "J\252rgen Klinsmann" , "Das sind Gef\252hle, wo man schwer beschreiben kann.\187" , "" , "Spielerspr\252che" )

joke252 :: Joke
joke252 = ( 252 , "J\252rgen Kurbjuhn" , "Wenn ich nicht will, lauf ich im Spiel nicht mehr als einen Kilometer und da ist der Weg von und zu der Kabine schon drin." , " und da ist der Weg von und zu der Kabine schon drin." , "Spielerspr\252che" )

joke253 :: Joke
joke253 = ( 253 , "J\252rgen R\246ber" , "Wir m\252ssen jetzt mit dem Boden auf den F\252\223en bleiben.\187" , "" , "Trainerspr\252che" )

joke254 :: Joke
joke254 = ( 254 , "J\252rgen Wegmann" , "Das mu\223 man verstehen, dass er Schwierigkeiten hat sich einzugew\246hnen. Er ist die deutsche Sprache noch nicht m\228chtig.\187" , "" , "Spielerspr\252che" )

joke255 :: Joke
joke255 = ( 255 , "J\252rgen Wegmann" , "Ich bin giftiger als die giftigste Kobra.\187" , "" , "Spielerspr\252che" )

joke256 :: Joke
joke256 = ( 256 , "J\252rgen Wegmann" , "Ich habe immer gesagt, dass ich niemals nach \214sterreich wechseln w\252rde." , "auf die Frage, ob er zum FC Basel wechselt" , "Spielerspr\252che" )

joke257 :: Joke
joke257 = ( 257 , "J\252rgen Wegmann" , "Zuerst hatten wir kein Gl\252ck, und dann kam auch noch Pech dazu.\187" , "" , "Spielerspr\252che" )

joke258 :: Joke
joke258 = ( 258 , "Juri Sawitschew" , "Elfmeterschie\223en, das ist irgendwie wie mit Frauen und Autos - reine Gl\252ckssache!\187" , "" , "Spielerspr\252che" )

joke259 :: Joke
joke259 = ( 259 , "Karl-Heinz K\246rbel" , "Den gr\246\223ten Fehler, den wir jetzt machen k\246nnten, w\228re, die Schuld beim Trainer zu suchen." , "als Trainer von Eintracht Frankfurt" , "Trainerspr\252che" )

joke260 :: Joke
joke260 = ( 260 , "Karl-Heinz K\246rbel" , "Die Eintracht ist vom Pech beg\252nstigt.\187" , "" , "Trainerspr\252che" )

joke261 :: Joke
joke261 = ( 261 , "Karl-Heinz K\246rbel" , "Mit dieser phantastisch k\228mpfenden Mannschaft ist die Meisterschaft bald drin!" , "nach der Vorrunde in der Abstiegssaison von Eintracht Frankfurt" , "Trainerspr\252che" )

joke262 :: Joke
joke262 = ( 262 , "Karl-Heinz Rummenigge" , "Das war nicht ganz unrisikovoll.\187" , "" , "Reporterspr\252che" )

joke263 :: Joke
joke263 = ( 263 , "Karl-Heinz Rummenigge" , "Eine gef\228hrliche Parabole aufs Tor.\187" , "" , "Reporterspr\252che" )

joke264 :: Joke
joke264 = ( 264 , "Karl-Heinz Rummenigge" , "In letzter Konsequenz waren wir nicht konsequent genug.\187" , "" , "Reporterspr\252che" )

joke265 :: Joke
joke265 = ( 265 , "Karl-Heinz Rummenigge" , "Riedle mu\223 man nur f\252ttern. Dann kommt irgendetwas dabei raus.\187" , "" , "Reporterspr\252che" )

joke266 :: Joke
joke266 = ( 266 , "Karl-Heinz Rummenigge" , "Wenn man \252ber rechts kommt, muss die hintere Mitte links wandern, da es sonst vorn Einbr\252che gibt.\187" , "" , "Reporterspr\252che" )

joke267 :: Joke
joke267 = ( 267 , "Karl-Heinz Thielen" , "Erstes Ziel ist es, die gr\246\223ten Flaschen zu verkaufen. Gibt es noch Pfand daf\252r, super. Gibt es nichts, auch gut." , "ehemaliger Vizepr\228sident des 1.FC K\246ln" , "Trainerspr\252che" )

joke268 :: Joke
joke268 = ( 268 , "Klaus Fischer" , "Ich lese keine B\252cher." , "auf die Frage nach seinem Lieblingsbuch" , "Spielerspr\252che" )

joke269 :: Joke
joke269 = ( 269 , "Klaus Hilpert" , "Das wundert mich nicht. Wir haben die Mannschaft ganz karibisch zusammengestellt." , "auf die Frage, warum es beim VfL Bochum so gut laufe" , "Trainerspr\252che" )

joke270 :: Joke
joke270 = ( 270 , "Klaus Lufen" , "Auch gr\246\223enm\228\223ig ist es der gr\246\223te Nachteil, dass die Torh\252ter in Japan nicht die allergr\246\223ten sind!\187" , "" , "Reporterspr\252che" )

joke271 :: Joke
joke271 = ( 271 , "Klaus Schwarze" , "Saarbr\252cken bezwang Freiburg mit 1:1.\187" , "" , "Reporterspr\252che" )

joke272 :: Joke
joke272 = ( 272 , "Klaus T\228uber" , "Heute knall ich mir die Birne voll, bis mir das Bier zu den Ohren rausl\228uft!\187" , "" , "Spielerspr\252che" )

joke273 :: Joke
joke273 = ( 273 , "Klaus Thomforde" , "In der ersten Liga die B\228lle zu halten find ich total geil. Da geht mir voll einer ab!\187" , "" , "Spielerspr\252che" )

joke274 :: Joke
joke274 = ( 274 , "Klaus Toppm\246ller" , "Meine Jungs sitzen noch in der Kabine. Sie wollen alle hierbleiben und n\228chste Woche wieder hier spielen." , "nach zwei kurz aufeinanderfolgenden Ausw\228rtssiegen - Pokal- und Punktspiel - mit dem VfL Bochum beim 1.FC Kaiserslautern" , "Trainerspr\252che" )

joke275 :: Joke
joke275 = ( 275 , "Klaus-Dieter Wollitz" , "Was ist denn mehr?" , "auf die Frage, ob er sein Gehalt brutto oder netto haben wolle" , "Spielerspr\252che" )

joke276 :: Joke
joke276 = ( 276 , "Kommentator" , "Es war so leise im Stadion, dass man die ber\252hmte Stecknadel im Heu suchen konnte.\187" , "" , "Reporterspr\252che" )

joke277 :: Joke
joke277 = ( 277 , "Kommentator" , "Statistiken sind hinterh\228ltige Begleiter. Jedenfalls, wenn man sie \252berinterpretiert. Zum Beispiel Slowenien. (Er \252berlegt.) Zu Slowenien habe ich gar keine Statistik, die gibt es erst seit ein paar Jahren.\187" , "" , "Reporterspr\252che" )

joke278 :: Joke
joke278 = ( 278 , "Lawrie McMenemy" , "Jeder hat so seine Probleme. Auch die blonde Nummer 17 der Deutschen. Der m\252sste sich die Haare anders f\228rben." , "der nordirische Nationaltrainer \252ber Thomas Strunz" , "Trainerspr\252che" )

joke279 :: Joke
joke279 = ( 279 , "Leo Beenhakker" , "Haben Sie eine Stunde Zeit?" , "auf die Frage, wie dem deutschen Fu\223ball zu helfen sei" , "Trainerspr\252che" )

joke280 :: Joke
joke280 = ( 280 , "Leo Beenhakker" , "Ich bin jetzt seit 34 Jahren Trainer, da habe ich gelernt, dass 2 und 2 niemals 4 ist.\187" , "" , "Trainerspr\252che" )

joke281 :: Joke
joke281 = ( 281 , "Lorenz-G\252nther K\246stner" , "Die holen sich einen Popel aus der Nase und brechen sich noch den Finger dabei." , "\252ber seine ungl\252cklich agierenden Spieler" , "Trainerspr\252che" )

joke282 :: Joke
joke282 = ( 282 , "Lothar Emmerich" , "Gib mich die Kirsche!\187" , "" , "Spielerspr\252che" )

joke283 :: Joke
joke283 = ( 283 , "Lothar Matth\228us" , "Das Chancenplus war ausgeglichen.\187" , "" , "Spielerspr\252che" )

joke284 :: Joke
joke284 = ( 284 , "Lothar Matth\228us" , "Das ist das erste Interview, wo sie macht." , "\252ber seine Freundin Maren M\252ller-Wohlfahrt" , "Spielerspr\252che" )

joke285 :: Joke
joke285 = ( 285 , "Lothar Matth\228us" , "Die Frauen haben sich entwickelt in den letzten Jahren. Sie stehen nicht mehr zufrieden am Herd, waschen W\228sche und passen aufs Kind auf. M\228nner m\252ssen das akzeptieren." , "in einem Playboy-Interview" , "Spielerspr\252che" )

joke286 :: Joke
joke286 = ( 286 , "Lothar Matth\228us" , "Ein Lothar Matth\228us l\228\223t sich nicht von seinem K\246rper besiegen, ein Lothar Matth\228us entscheidet selbst \252ber sein Schicksal.\187" , "" , "Spielerspr\252che" )

joke287 :: Joke
joke287 = ( 287 , "Lothar Matth\228us" , "Ein Lothar Matth\228us spricht kein Franz\246sisch.\187" , "" , "Spielerspr\252che" )

joke288 :: Joke
joke288 = ( 288 , "Lothar Matth\228us" , "Ein Wort gab das andere - wir hatten uns nichts zu sagen.\187" , "" , "Spielerspr\252che" )

joke289 :: Joke
joke289 = ( 289 , "Lothar Matth\228us" , "Es ist wichtig, dass man neunzig Minuten mit voller Konzentration an das n\228chste Spiel denkt.\187" , "" , "Spielerspr\252che" )

joke290 :: Joke
joke290 = ( 290 , "Lothar Matth\228us" , "Ey, M\228dels, unser Schwarzer hat den L\228ngsten!" , "zur Basketballnationalmannschaft der Damen" , "Spielerspr\252che" )

joke291 :: Joke
joke291 = ( 291 , "Lothar Matth\228us" , "He, Brrrand, du - du bist doch bolitisch, bist du doch, du Gr\252ner, machst auf sozial und hetzt hier den Schirri gegen uns auf." , "im Pokalhalbfinale Bayern-Rostock zu Christian Brand" , "Spielerspr\252che" )

joke292 :: Joke
joke292 = ( 292 , "Lothar Matth\228us" , "I hope, we have a little bit lucky." , "bei seiner ersten Pressekonferenz in" , "Spielerspr\252che" )

joke293 :: Joke
joke293 = ( 293 , "Lothar Matth\228us" , "Ich bin sicher, da\223 ich in vier oder sechs Wochen Interviews auf Englisch geben kann, die auch der Deutsche verstehen wird.\187" , "" , "Spielerspr\252che" )

joke294 :: Joke
joke294 = ( 294 , "Lothar Matth\228us" , "Ich hab gleich gemerkt, das ist ein Druckschmerz, wenn man drauf dr\252ckt.\187" , "" , "Spielerspr\252che" )

joke295 :: Joke
joke295 = ( 295 , "Lothar Matth\228us" , "Manchmal spreche ich zuviel.\187" , "" , "Spielerspr\252che" )

joke296 :: Joke
joke296 = ( 296 , "Lothar Matth\228us" , "Schiedsrichter kommt f\252r mich nicht in Frage, schon eher etwas, das mit Fu\223ball zu tun hat." , "auf die Frage, was er nach seiner Karriere plane" , "Spielerspr\252che" )

joke297 :: Joke
joke297 = ( 297 , "Lothar Matth\228us" , "Wir d\252rfen jetzt nur nicht den Sand in den Kopf stecken!\187" , "" , "Spielerspr\252che" )

joke298 :: Joke
joke298 = ( 298 , "Lothar Matth\228us" , "Wir sind eine gut intrigierte Truppe.\187" , "" , "Spielerspr\252che" )

joke299 :: Joke
joke299 = ( 299 , "Ludwig K\246gl" , "Entweder ich gehe links vorbei, oder ich gehe rechts vorbei.\187" , "" , "Spielerspr\252che" )

joke300 :: Joke
joke300 = ( 300 , "Ludwig K\246gl" , "I spui mei Spui.\187" , "" , "Spielerspr\252che" )

joke301 :: Joke
joke301 = ( 301 , "Luis Arragones" , "Wenn sie begriffen haben, dass zum Fu\223ball auch Arbeit geh\246rt, ist es zu sp\228t. Dann werden sie Trainer." , "\252ber spanische Fu\223baller" , "Trainerspr\252che" )

joke302 :: Joke
joke302 = ( 302 , "Manfred Kaltz" , "Was, der Kapellmann wird Arzt? Der wird doch Doktor!\187" , "" , "Spielerspr\252che" )

joke303 :: Joke
joke303 = ( 303 , "Manfred Krafft" , "Meine Mannschaft ist 15- oder 16mal ins Abseits gerannt. Das haben wir auch die ganze Woche ge\252bt.\187" , "" , "Trainerspr\252che" )

joke304 :: Joke
joke304 = ( 304 , "Manfred Schwabl" , "Wenn's lafft, dann lafft's. Wenn net, dann net. Aber bei uns lafft's.\187" , "" , "Spielerspr\252che" )

joke305 :: Joke
joke305 = ( 305 , "Marcel Reif" , "Auch ohne Matthias Sammer hat die deutsche Mannschaft bewiesen, dass sie in der Lage ist, ihn zu ersetzen.\187" , "" , "Reporterspr\252che" )

joke306 :: Joke
joke306 = ( 306 , "Marcel Reif" , "Die Spieler von Ghana erkennen Sie an den gelben Stutzen." , "beim L\228nderspiel Deutschland-Ghana" , "Reporterspr\252che" )

joke307 :: Joke
joke307 = ( 307 , "Marcel Reif" , "Ich darf als Reporter ja nicht parteiisch sein... ich will auch nicht parteiisch sein - aber... lauft, meine kleinen schwarzen Freunde, lauft!!!" , "beim WM-Spiel Kamerun-Argentinien beim Stand von 1:0" , "Reporterspr\252che" )

joke308 :: Joke
joke308 = ( 308 , "Marcel Reif" , "Je l\228nger das Spiel dauert, desto weniger Zeit bleibt.\187" , "" , "Reporterspr\252che" )

joke309 :: Joke
joke309 = ( 309 , "Marcel Reif" , "Klemmt der D\246del?" , "im Glauben, gerade nicht auf Sendung zu sein" , "Reporterspr\252che" )

joke310 :: Joke
joke310 = ( 310 , "Marcel Reif" , "Und dieser \246ffnende Pass brachte wieder 57 cm Raumgewinn!\187" , "" , "Reporterspr\252che" )

joke311 :: Joke
joke311 = ( 311 , "Marcel Reif" , "Wenn Sie dieses Spiel atemberaubend finden, dann haben Sie's an den Bronchien.\187" , "" , "Reporterspr\252che" )

joke312 :: Joke
joke312 = ( 312 , "Marco Rehmer" , "Wir sind hierher gefahren und haben gesagt: Okay, wenn wir verlieren, fahren wir wieder nach Hause.\187" , "" , "Spielerspr\252che" )

joke313 :: Joke
joke313 = ( 313 , "Marco Reich" , "Fr\252her war ich ein gro\223er Fan von M\246nchengladbach. Doch da hatte ich noch keine Ahnung vom Fu\223ball.\187" , "" , "Spielerspr\252che" )

joke314 :: Joke
joke314 = ( 314 , "Mario Basler" , "Da mu\223 jemand einen Fu\223ball in die Sonne geschossen haben." , "erkl\228rt die Sonnenfinsternis" , "Spielerspr\252che" )

joke315 :: Joke
joke315 = ( 315 , "Mario Basler" , "Das habe ich ihm dann auch verbal gesagt.\187" , "" , "Spielerspr\252che" )

joke316 :: Joke
joke316 = ( 316 , "Mario Basler" , "Die V\246gel haben noch nicht gezwitschert, als ich gegangen bin." , "nach einer Geburtstagfeier von Didi Hamann, die er erst um 3.00 Uhr fr\252h verlie\223" , "Spielerspr\252che" )

joke317 :: Joke
joke317 = ( 317 , "Mario Basler" , "Eigentlich bin ich ein Supertyp. Aber ich kann wohl auch ein richtiger Arsch sein!\187" , "" , "Spielerspr\252che" )

joke318 :: Joke
joke318 = ( 318 , "Mario Basler" , "Ganz gut. Ich hab\180mich die ganze Nacht um seine Frau gek\252mmert!" , "auf die Frage, wie es Dietmar Hamann nach seinem Schlaganfall geht" , "Spielerspr\252che" )

joke319 :: Joke
joke319 = ( 319 , "Mario Basler" , "Ich gr\252\223e meine Mama, meinen Papa und ganz besonders meine Eltern.\187" , "" , "Spielerspr\252che" )

joke320 :: Joke
joke320 = ( 320 , "Mario Basler" , "Ich habe immer gesagt, dass ich kein Dauerl\228ufer bin, sonst k\246nnte ich ja gleich beim Marathon starten.\187" , "" , "Spielerspr\252che" )

joke321 :: Joke
joke321 = ( 321 , "Mario Basler" , "Im ersten Moment war ich nicht nur gl\252cklich, ein Tor geschossen zu haben, sondern auch, dass der Ball reinging.\187" , "" , "Spielerspr\252che" )

joke322 :: Joke
joke322 = ( 322 , "Mario Basler" , "Jede Seite hat zwei Medaillen\187" , "" , "Spielerspr\252che" )

joke323 :: Joke
joke323 = ( 323 , "Mario Basler" , "Jetzt sieht er aus wie ein frisch lackierter Totalschaden!" , "\252ber den frischgeschorenen Glatzkopf Christian Ziege" , "Spielerspr\252che" )

joke324 :: Joke
joke324 = ( 324 , "Mario Basler" , "Jetzt wei\223 man, dass Erich Ribbeck wirklich keine Ahnung hat." , "nach dem Aus der deutschen Nationalmannschaft bei der EM 2000" , "Spielerspr\252che" )

joke325 :: Joke
joke325 = ( 325 , "Mario Basler" , "Kritik macht mich nur noch st\228rker. Wenn mich in Dortmund von 55 000 Zuschauer 50 000 hassen, mir am liebsten ein Bein abhacken w\252rden, mich mit \"Arschloch\" begr\252\223en, dann f\252hle ich mich wie Arnold Schwarzenegger gegen den Rest der Welt. Das ist geil f\252r mich.\187" , "" , "Spielerspr\252che" )

joke326 :: Joke
joke326 = ( 326 , "Mario Basler" , "Quatsch, wir zocken nie um viel Geld. H\246chstens um 3000 DM!" , "im Trainingslager der deutschen Nationalmannschaft" , "Spielerspr\252che" )

joke327 :: Joke
joke327 = ( 327 , "Martin Driller" , "Fu\223ball ist wie eine Frikadelle, man wei\223 nie, was drin ist.\187" , "" , "Spielerspr\252che" )

joke328 :: Joke
joke328 = ( 328 , "Martin Pieckenhagen" , "Das ist schon so lange her, da muss ich noch Dauerwelle gehabt haben." , "auf die Frage, wann Hansa Rostock das letzte Mal zu null gespielt habe" , "Spielerspr\252che" )

joke329 :: Joke
joke329 = ( 329 , "Martin Pieckenhagen" , "Wenn die Leute meinen, wir seien unter Druck, dann m\252ssen wir wohl die n\228chsten 5 Spieltage auf dem Klo verbringen.\187" , "" , "Spielerspr\252che" )

joke330 :: Joke
joke330 = ( 330 , "Matthias Sammer" , "Das n\228chste Spiel ist immer das n\228chste.\187" , "" , "Spielerspr\252che" )

joke331 :: Joke
joke331 = ( 331 , "Matthias Sammer" , "Die Bremer lassen sich hier feiern, als w\228ren sie Deutscher Meister geworden, dabei haben die sich hier einen Schei\223 zusammengespielt..." , "nach einer 0:4-Niederlage des BVB in Bremen" , "Spielerspr\252che" )

joke332 :: Joke
joke332 = ( 332 , "Matthias Sammer" , "Wenn ich am Ende vorn stehe, k\246nnen mich die Leute auch Arschloch nennen. Das ist mir egal." , "\252ber seinen Spitznamen \"Motzki\"" , "Spielerspr\252che" )

joke333 :: Joke
joke333 = ( 333 , "Max Merkel" , "Der Basler spielt wie eine Parkuhr. Er steht rum und die Bayern stopfen Geld rein.\187" , "" , "Trainerspr\252che" )

joke334 :: Joke
joke334 = ( 334 , "Max Merkel" , "Der Dettmar Cramer hat doch nur den Schwarzen im Senegal beigebracht, wie man Kakteen umdribbelt. (Die Antwort von Dettmar Cramer: Hier ist der Kollege Merkel schlecht in Geographie: Im Senegal gibt es gar keine Kakteen.)\187" , "" , "Trainerspr\252che" )

joke335 :: Joke
joke335 = ( 335 , "Max Merkel" , "Der sollte von der Innsbrucker Universit\228t ausgestellt werden. Einen Menschen mit so wenig Hirn gibt's ja net." , "\252ber Friedl Koncilia" , "Trainerspr\252che" )

joke336 :: Joke
joke336 = ( 336 , "Max Merkel" , "Die wissen nicht einmal, dass im Ball Luft ist. Die glauben doch, der springt, weil ein Frosch drin ist." , "\252ber deutsche Funktion\228re" , "Trainerspr\252che" )

joke337 :: Joke
joke337 = ( 337 , "Max Merkel" , "Eine Stra\223enbahn hat mehr Anh\228nger als Uerdingen.\187" , "" , "Trainerspr\252che" )

joke338 :: Joke
joke338 = ( 338 , "Max Merkel" , "Er wird nie Kopfweh bekommen, weil er seinen Kopf nie zum Denken benutzen wird. Ehe er Nationalspieler wird, werde ich S\228nger an der Metropolitan Opera." , "\252ber R\252diger Abramczik" , "Trainerspr\252che" )

joke339 :: Joke
joke339 = ( 339 , "Max Merkel" , "Im Training habe ich mal die Alkoholiker meiner Mannschaft gegen die Antialkoholiker spielen lassen. Die Alkoholiker gewannen 7:1. Da war's mir wurscht. Da hab i g'sagt: Sauft's weiter.\187" , "" , "Trainerspr\252che" )

joke340 :: Joke
joke340 = ( 340 , "Max Merkel" , "In D\228nemark habe ich nur Eier und Butter geholt, aber keine Fu\223baller.\187" , "" , "Trainerspr\252che" )

joke341 :: Joke
joke341 = ( 341 , "Max Merkel" , "Spieler vertragen kein Lob. Sie m\252ssen t\228glich die Peitsche im Nacken f\252hlen.\187" , "" , "Trainerspr\252che" )

joke342 :: Joke
joke342 = ( 342 , "Mehmet Scholl" , "Die Brisanz dieses Spieles hat man daran erkannt, dass sich Franz Beckenbauer \252ber unsere Tore gefreut hat." , "nach einem der Derby der Bayern gegen die L\246wen" , "Spielerspr\252che" )

joke343 :: Joke
joke343 = ( 343 , "Mehmet Scholl" , "Die sch\246nsten Tore sind diejenigen, bei denen der Ball sch\246n flach oben rein geht.\187" , "" , "Spielerspr\252che" )

joke344 :: Joke
joke344 = ( 344 , "Mehmet Scholl" , "Es ist mir v\246llig egal, was es wird. Hauptsache, er ist gesund." , "als werdender Vater" , "Spielerspr\252che" )

joke345 :: Joke
joke345 = ( 345 , "Mehmet Scholl" , "Gesundheit!" , "auf die Frage, was er denn zum rum\228nischen Mittelfeldstar Hagi sagen werde" , "Spielerspr\252che" )

joke346 :: Joke
joke346 = ( 346 , "Mehmet Scholl" , "H\228ngt die Gr\252nen, solange es noch B\228ume gibt!" , "auf die Frage nach seinem Lebensmotto" , "Spielerspr\252che" )

joke347 :: Joke
joke347 = ( 347 , "Mehmet Scholl" , "Ich hatte noch nie Streit mit meiner Frau. Bis auf das eine Mal, als sie mit auf's Hochzeitsfoto wollte.\187" , "" , "Spielerspr\252che" )

joke348 :: Joke
joke348 = ( 348 , "Mehmet Scholl" , "Meine Frau und ich, wir sind jetzt seit zwei Jahren verheiratet und bei uns l\228uft alles gut." , "ausweichend auf die Frage, wann ihm Rehagel denn mitgeteilt habe, dass er wieder nur Ersatzspieler sein werde" , "Spielerspr\252che" )

joke349 :: Joke
joke349 = ( 349 , "Mehmet Scholl" , "Meine Unbek\252mmertheit wandelte sich in kontollierte Spontaneit\228t.\187" , "" , "Spielerspr\252che" )

joke350 :: Joke
joke350 = ( 350 , "Michael B\252skens" , "Ja, da hab' ich gedacht, mein Gott, da rentieren sich mal endlich die 5 Millionen, die ich hier Jahr f\252r Jahr kriege." , "nach einem Torerfolg" , "Spielerspr\252che" )

joke351 :: Joke
joke351 = ( 351 , "Michael Lusch" , "Ich kann mich an kein Spiel erinnern, beim dem so viele Spieler mit der Barriere vom Platz getragen wurden.\187" , "" , "Spielerspr\252che" )

joke352 :: Joke
joke352 = ( 352 , "Michael Meier" , "M\246ller hat mit seinem Berater bei uns um mehr Geld gepokert, gleichzeitig gesagt, er st\252nde bei einem anderen Club im Wort. Dann hat er offenbart, dass er nach Schalke gehen will. Wir haben ihm nicht gesagt, dass er bekloppt ist. Aber gedacht haben wir es schon.\187" , "" , "Trainerspr\252che" )

joke353 :: Joke
joke353 = ( 353 , "Michael Meier" , "Sie k\246nnen mir nicht absprechen, da\223 ich ohne Konzept eingekauft h\228tte.\187" , "" , "Trainerspr\252che" )

joke354 :: Joke
joke354 = ( 354 , "Michael Wiese" , "Wolfsburg hat die letzten drei Heimspiele verloren zu Hause.\187" , "" , "Reporterspr\252che" )

joke355 :: Joke
joke355 = ( 355 , "Michel Platini" , "Wenn die Deutschen gut spielen, dann werden sie Weltmeister, wenn sie schlecht spielen, dann kommen sie ins Finale!" , "vor der WM 94" , "Trainerspr\252che" )

joke356 :: Joke
joke356 = ( 356 , "Mini Jacobsen" , "Ich habe 2 1/2 Jahre bei Young Boys Bern, 6 Monate bei Lierse und 53 Minuten bei Duisburg gespielt.\187" , "" , "Spielerspr\252che" )

joke357 :: Joke
joke357 = ( 357 , "Moshe Sinai" , "F\252r englischen Fu\223ball." , "der ehemalige israelische Nationalspieler und heutige Trainer auf die Frage, f\252r was er sich - au\223er Fu\223ball - noch interessiere" , "Trainerspr\252che" )

joke358 :: Joke
joke358 = ( 358 , "Nathalie Pires" , "Ich wundere mich immer, wie schnell mein Mann laufen kann. Zu Hause geht ihm schon beim Treppensteigen zu uns in den dritten Stock die Luft aus." , "\252ber ihren Ehemann, den franz\246sischen Nationalspieler Robert Pires" , "Promis & Presse" )

joke359 :: Joke
joke359 = ( 359 , "Nationaltrainer" , "Die Zeitung: Russische Juden sind mit die besten St\252rmer der Welt. Das Originalzitat: Rush an' Hughes are some of the best attackers in the world.\187" , "" , "Promis & Presse" )

joke360 :: Joke
joke360 = ( 360 , "Norbert Dickel" , "Der ist mit allen Abwassern gewaschen." , "\252ber Frank Mill" , "Spielerspr\252che" )

joke361 :: Joke
joke361 = ( 361 , "Norbert Nachtweih" , "Der springt so hoch, wenn der wieder runter kommt, liegt auf seiner Glatze Schnee." , " wenn der wieder runter kommt, liegt auf seiner Glatze Schnee." , "Spielerspr\252che" )

joke362 :: Joke
joke362 = ( 362 , "Norbert Nigbur" , "Offenbach h\228tte 3:0 gewonnen, wenn ich nicht ein Papstbild in der Tasche gehabt h\228tte.\187" , "" , "Spielerspr\252che" )

joke363 :: Joke
joke363 = ( 363 , "Norbert Pflippen" , "Im \252brigen vermarkte ich lieber Frauen. Die haben zwei Vorteile: Sie wissen, was Geld ist und sie k\246nnen keine Spielerfrauen heiraten.\187" , "" , "Promis & Presse" )

joke364 :: Joke
joke364 = ( 364 , "Olaf Thon" , "Ich habe ihn nur ganz leicht retuschiert.\187" , "" , "Spielerspr\252che" )

joke365 :: Joke
joke365 = ( 365 , "Olaf Thon" , "Ich sehe einen positiven Trend: Tiefer kann es nicht mehr gehen.\187" , "" , "Spielerspr\252che" )

joke366 :: Joke
joke366 = ( 366 , "Olaf Thon" , "In erster Linie stehe ich voll hinter dem Trainer, in zweiter Linie hat er recht.\187" , "" , "Spielerspr\252che" )

joke367 :: Joke
joke367 = ( 367 , "Olaf Thon" , "Man darf das Spiel doch nicht so schlecht reden wie es wirklich war.\187" , "" , "Spielerspr\252che" )

joke368 :: Joke
joke368 = ( 368 , "Olaf Thon" , "Man soll auch die anderen Mannschaften nicht unter dem Teppich kehren lassen.\187" , "" , "Spielerspr\252che" )

joke369 :: Joke
joke369 = ( 369 , "Olaf Thon" , "Mensch, mir ist aber kalt jetzt! Ehrlich! Boah! (sprach's und ging)" , "ausweichend auf die Frage, warum das Spiel denn so schlecht gelaufen sei" , "Spielerspr\252che" )

joke370 :: Joke
joke370 = ( 370 , "Olaf Thon" , "Wenn ihr so weiterspielt, werdet ihr am Ende Meister." , "nach der 0:3-Niederlage von Schalke 04 am 1. Spieltag der Saison 98/99 gegen den sp\228teren Absteiger Borussia M\246nchengladbach zu deren Trainer Friedel Rausch" , "Spielerspr\252che" )

joke371 :: Joke
joke371 = ( 371 , "Olaf Thon" , "Wir lassen uns nicht nerv\246s machen, und das geben wir auch nicht zu!\187" , "" , "Spielerspr\252che" )

joke372 :: Joke
joke372 = ( 372 , "Olaf Thon" , "Wir spielen hinten Mann gegen Mann, und ich spiel gegen den Mann.\187" , "" , "Spielerspr\252che" )

joke373 :: Joke
joke373 = ( 373 , "Oliver Kahn" , "Das einzige Tier bei uns zu Hause bin ich.\187" , "" , "Spielerspr\252che" )

joke374 :: Joke
joke374 = ( 374 , "Oliver Kahn" , "Irgendwann mal wieder gewinnen, und das versuchen wir zu probieren." , "auf die Frage, was man denn gegen die Krise tun k\246nne" , "Spielerspr\252che" )

joke375 :: Joke
joke375 = ( 375 , "Oliver Kahn" , "Jaa, erst Eckball und dann Tor." , "auf die Frage: \"Herr Kahn, wie haben Sie die Situation gesehen, als jemand, der unmittelbar dabei war?\" zu Manchesters 2:1 im CL-Finale" , "Spielerspr\252che" )

joke376 :: Joke
joke376 = ( 376 , "Oliver Kahn" , "Wir br\252llen beide so laut, dass wir uns \252ber\180s Spielfeld unterhalten k\246nnen." , "\252ber seinen Torwart-Kollegen Peter Schmeichel" , "Spielerspr\252che" )

joke377 :: Joke
joke377 = ( 377 , "Ottmar Hitzfeld" , "Das ist doch blo\223 wieder eine dieser Gruppen, die damit versucht, bekannt zu werden." , "\252ber den Song \"Bayern\" der \"Toten Hosen\"" , "Trainerspr\252che" )

joke378 :: Joke
joke378 = ( 378 , "Ottmar Hitzfeld" , "Von der Anzahl her hatten wir mehr Chancen\187" , "" , "Trainerspr\252che" )

joke379 :: Joke
joke379 = ( 379 , "Ottmar Hitzfeld" , "Wenn alle Spieler so engagiert w\228ren wie Oliver Kahn, w\228re das gef\228hrlich f\252r die Mannschaft.\187" , "" , "Trainerspr\252che" )

joke380 :: Joke
joke380 = ( 380 , "Ottmar Hitzfeld" , "Wir brauchen das Geld." , "auf die Frage, warum Bayern M\252nchen die Talente Alexander Bugera und Berkant G\246ktan vorl\228ufig abgegeben habe" , "Trainerspr\252che" )

joke381 :: Joke
joke381 = ( 381 , "Otto Pfister" , "Da hilft nur: Bein aufs\228gen und Jahresringe z\228hlen." , "\252ber das Alter von Anthony Yeboah" , "Trainerspr\252che" )

joke382 :: Joke
joke382 = ( 382 , "Otto Rehhagel" , "Franz ist wie Marlene Dietrich. Ein alternder Star, den man nach wie vor bewundern mu\223.\187" , "" , "Trainerspr\252che" )

joke383 :: Joke
joke383 = ( 383 , "Otto Rehhagel" , "Mit 50 bist Du als Fu\223balltrainer reif f\252r die Klapsm\252hle. Wenn Du genug Geld verdient hast, kannst Du wenigstens erster Klasse liegen.\187" , "" , "Trainerspr\252che" )

joke384 :: Joke
joke384 = ( 384 , "Otto Rehhagel" , "Wozu braucht meine Mannschaft Doping? Sie hat ja mich.\187" , "" , "Trainerspr\252che" )

joke385 :: Joke
joke385 = ( 385 , "Paul Breitner" , "...aber solche Leute wie den Ratinho oder den Ailton, die kannst Du an der Copacabana im Rudel mit 'nem Lasso einfangen." , "nachdem er beim Spiel Bayern-Leverkusen die Qualit\228t der in beiden Mannschaften spielenden Brasilianer gelobt hatte" , "Reporterspr\252che" )

joke386 :: Joke
joke386 = ( 386 , "Paul Breitner" , "Da kam dann das Elfmeterschie\223en. Wir hatten alle die Hosen voll, aber bei mir lief's ganz fl\252ssig.\187" , "" , "Spielerspr\252che" )

joke387 :: Joke
joke387 = ( 387 , "Paul Breitner" , "Ich habe nur immer meinen Finger in Wunden gelegt, die sonst unter den Tisch gekehrt worden w\228ren.\187" , "" , "Reporterspr\252che" )

joke388 :: Joke
joke388 = ( 388 , "Paul Breitner" , "In diesem Schei\223verein kann man nicht mal richtig feiern." , "\252ber Bayern M\252nchen" , "Spielerspr\252che" )

joke389 :: Joke
joke389 = ( 389 , "Paul Breitner" , "Schiri, pfeif ab. I mog nimmer." , "nach 30 Minuten in einem Europacupspiel beim Stand von 3:0 gegen die Bayern" , "Spielerspr\252che" )

joke390 :: Joke
joke390 = ( 390 , "Paul Breitner" , "Sie sollen nicht glauben, dass sie Brasilianer sind, nur weil sie aus Brasilien kommen." , "kommentiert die schwachen Leistungen der Dortmunder Brasilianer Dede und Evanilson" , "Reporterspr\252che" )

joke391 :: Joke
joke391 = ( 391 , "Paul Gascoigne" , "Ich mache nie Voraussagen und werde das auch niemals tun.\187" , "" , "Spielerspr\252che" )

joke392 :: Joke
joke392 = ( 392 , "Paul Ince" , "Tackling ist viel sch\246ner als Sex!\187" , "" , "Spielerspr\252che" )

joke393 :: Joke
joke393 = ( 393 , "Paul Steiner" , "Nein, die spielen immer mittwochs, da habe ich keine Zeit." , "auf die Frage, ob die Nationalmannschaft f\252r ihn ein Thema sei" , "Spielerspr\252che" )

joke394 :: Joke
joke394 = ( 394 , "Peter K\246zle" , "Ich hab' spekuliert, was ich machen soll. Offensichtlich habe ich zu gut spekuliert, aber nicht getroffen." , "nach zwei vergebenen hundertprozentigen Torchancen" , "Spielerspr\252che" )

joke395 :: Joke
joke395 = ( 395 , "Peter Neururer" , "Das letzte Mal, dass ich so hoch verloren hab, war gegen meinen Bruder im Tipp-Kick (Neururer wurde anschlie\223end entlassen)." , "nach einer 0:7-Niederlage mit Rot-Wei\223 Essen" , "Trainerspr\252che" )

joke396 :: Joke
joke396 = ( 396 , "Peter Neururer" , "Die Stimmung ist eigentlich wie vor dem Spiel. Mit dem kleinen Unterschied, dass wir aus dieser \228u\223erst gro\223en Minimalchance, minimaler geht's gar nicht mehr, eine etwas kleinere gemacht haben, die gr\246\223er geworden ist.\187" , "" , "Trainerspr\252che" )

joke397 :: Joke
joke397 = ( 397 , "Peter Neururer" , "W\228re es k\228lter gewesen, w\228r' vielleicht einer von ihnen am Boden festgefroren." , "nach einer 0:3-Niederlage seines 1.FC K\246ln beim MSV Duisburg \252ber seine Spieler" , "Trainerspr\252che" )

joke398 :: Joke
joke398 = ( 398 , "Peter Neururer" , "Wir fahren hin, hau'n die weg und fahren wieder zur\252ck.\187" , "" , "Trainerspr\252che" )

joke399 :: Joke
joke399 = ( 399 , "Peter Neururer" , "Wir waren alle vorher \252berzeugt davon, dass wir das Spiel gewinnen. So war auch das Auftreten meiner Mannschaft, zumindest in den ersten zweieinhalb Minuten.\187" , "" , "Trainerspr\252che" )

joke400 :: Joke
joke400 = ( 400 , "Peter Pacult" , "Ja, der FC Tirol hat eine Obduktion auf mich.\187" , "" , "Spielerspr\252che" )

joke401 :: Joke
joke401 = ( 401 , "Peter Weiand" , "In M\252nchen beginnt der Vordere Orient. Da herrschen andere Gesetze." , "als Pr\228sident des 1.FC K\246ln" , "Trainerspr\252che" )

joke402 :: Joke
joke402 = ( 402 , "Pierre Littbarski" , "In der ersten Halbzeit haben wir ganz gut gespielt, in der zweiten fehlte uns die Kontinu..., \228h Kontuni..., ach schei\223 Fremdw\246rter: Wir waren nicht best\228ndig genug!\187" , "" , "Spielerspr\252che" )

joke403 :: Joke
joke403 = ( 403 , "Pierre Littbarski" , "Lieber ein Ende mit Schrecken als ein Schrecken mit Ende." , "\252ber die Entlassung von Toni Schumacher bei Fortuna K\246ln" , "Spielerspr\252che" )

joke404 :: Joke
joke404 = ( 404 , "Rainer Bonhof" , "Sylvester Stallone und Arnold Schwarzenegger in der Abwehr, Bruce Willis im Mittelfeld und Jean Claude van Damme im Sturm." , "auf die Frage, wie er die verletzten Spieler zu ersetzen gedenke" , "Trainerspr\252che" )

joke405 :: Joke
joke405 = ( 405 , "ran online" , "Um den Leverkusener Spielaufbau machte sich vor allem das Trio Emerson und Beinlich verdient." , "im Spielbericht zum CL-Spiel Maribor - Leverkusen" , "Promis & Presse" )

joke406 :: Joke
joke406 = ( 406 , "Reiner Calmund" , "Als ich zuletzt Sergio in Eurosport gesehen habe, dachte ich mir auch nur: Das kann er nicht sein, da muss sich einer maskiert haben.\187" , "" , "Trainerspr\252che" )

joke407 :: Joke
joke407 = ( 407 , "Reiner Calmund" , "Am Sonntag um 13.00 Uhr haben wir ein Wei\223wurst-Wettfressen mit der Bayern-Spitze. Da bin ich gut dabei.\187" , "" , "Trainerspr\252che" )

joke408 :: Joke
joke408 = ( 408 , "Reiner Calmund" , "Das w\228re manchem St\252rmer schwergefallen, den so reinzuschie\223en. Gut gemacht. H\228tte nur noch gefehlt, dass er danach hochgesprungen w\228re." , "\252ber das Eigentor von Torben Hoffmann beim 1:4 gegen Bayern M\252nchen" , "Trainerspr\252che" )

joke409 :: Joke
joke409 = ( 409 , "Reiner Calmund" , "Die italienischen Vereine sagen mir: Von der Abl\246sesumme f\252r Emerson k\246nnt ihr euch doch zwei Spieler kaufen. Ich antworte denen dann immer: Dann kauft euch die doch selbst.\187" , "" , "Trainerspr\252che" )

joke410 :: Joke
joke410 = ( 410 , "Reiner Calmund" , "Ich kann nicht jeden, der nicht spielt, nuckeln und ihn schaukeln." , "\252ber St\252rmer Erik Meijer, der einen Stammplatz einforderte" , "Trainerspr\252che" )

joke411 :: Joke
joke411 = ( 411 , "Reiner Calmund" , "Mann Willi, Du siehst ja echt aus, als sei 'ne Hungersnot ausgebrochen! (Willis Antwort: Und Du siehst so aus, als seist Du schuld daran!)" , "zu Willi Lemke" , "Trainerspr\252che" )

joke412 :: Joke
joke412 = ( 412 , "Reiner Calmund" , "Zum Schluss mussten wir Markus Happe einen Kompa\223 geben, damit er den Weg in die Kabine findet.\187" , "" , "Trainerspr\252che" )

joke413 :: Joke
joke413 = ( 413 , "Ren\233 Hiepen" , "Ist das jetzt schon der Eckball oder noch die Wiederholung?... Die Fans jubeln, auf der Gegenseite mu\223 etwas passiert sein... J\252rgen R\246ber sollte seiner Mannschaft sagen, da\223 sie nur auf der linken Au\223enbahn spielen soll, damit wir wenigstens etwas sehen." , "beim \"Nebelspiel\" zwischen Hertha BSC Berlin und dem FC Barcelona" , "Reporterspr\252che" )

joke414 :: Joke
joke414 = ( 414 , "Richard Golz" , "Ich habe nie an unserer Chancenlosigkeit gezweifelt.\187" , "" , "Spielerspr\252che" )

joke415 :: Joke
joke415 = ( 415 , "Richard Golz" , "Vor lauter Philosophieren \252ber Schopenhauer kommen wir gar nicht mehr zum Trainieren." , "auf die Frage, was beim sogenannten Studentenklub SC Freiburg anders sei" , "Spielerspr\252che" )

joke416 :: Joke
joke416 = ( 416 , "Robert Seeger" , "Der Ball geht ins Seitenout - und es gibt Eckball.\187" , "" , "Reporterspr\252che" )

joke417 :: Joke
joke417 = ( 417 , "Robert Seeger" , "Nein, tut's uns das nicht auch noch an." , "der ORF-Kommentator, als beim 0:9 Debakel der \214sterreicher in Spanien eine Nachspielzeit von 4 Minuten angezeigt wird" , "Reporterspr\252che" )

joke418 :: Joke
joke418 = ( 418 , "Roland Schmider" , "F\252r uns war die Trainerfrage nie eine Trainerfrage.\187" , "" , "Trainerspr\252che" )

joke419 :: Joke
joke419 = ( 419 , "Roland Wohlfahrt" , "Zwei Chancen, ein Tor - das nenne ich hundertprozentige Chancenauswertung.\187" , "" , "Spielerspr\252che" )

joke420 :: Joke
joke420 = ( 420 , "Rolf Kramer" , "Diese Kameruner, sie sind wie eine Schlange, die in der Sonne liegt und schl\228ft. Pl\246tzlich sind sie da, auf einmal z\252ngeln sie, bei\223en sie, und schon wieder haben sie die Weltmeisterschaft vergiftet!\187" , "" , "Reporterspr\252che" )

joke421 :: Joke
joke421 = ( 421 , "Rolf R\252ssmann" , "Wenn wir hier nicht gewinnen, dann treten wir ihnen wenigstens den Rasen kaputt.\187" , "" , "Trainerspr\252che" )

joke422 :: Joke
joke422 = ( 422 , "Rolf Schafstall" , "Dreck, wo du hinguckst... In der Kabine steht keiner auf, h\246rt keiner zu. Kein Anstand. Alles Ossis. (kurze Zeit sp\228ter wurde er entlassen)" , "bei Dynamo Dresden" , "Trainerspr\252che" )

joke423 :: Joke
joke423 = ( 423 , "Rolf T\246pperwien" , "Dies ist \252berlebensnotwichtig f\252r den Verein.\187" , "" , "Reporterspr\252che" )

joke424 :: Joke
joke424 = ( 424 , "Rolf T\246pperwien" , "Jetzt! Jetzt betritt Otto Rehhagel deutschen Boden!" , "bei der R\252ckkehr von Werder Bremen nach dem Europapokalsieg 1992" , "Reporterspr\252che" )

joke425 :: Joke
joke425 = ( 425 , "Ron Atkinson" , "Ich wage mal eine Prognose: Es k\246nnte so oder so ausgehen.\187" , "" , "Trainerspr\252che" )

joke426 :: Joke
joke426 = ( 426 , "Ronald Koeman" , "Die deutschen Spieler h\246ren erst dann auf zu k\228mpfen, wenn sie im Bus sitzen.\187" , "" , "Spielerspr\252che" )

joke427 :: Joke
joke427 = ( 427 , "Roy Pr\228ger" , "Auffe Bank sitzen is schei\223e, da tut dir der Arsch weh.\187" , "" , "Spielerspr\252che" )

joke428 :: Joke
joke428 = ( 428 , "Roy Pr\228ger" , "Im Endeffekt sind Regeln dazu da, gebrochen zu werden.\187" , "" , "Spielerspr\252che" )

joke429 :: Joke
joke429 = ( 429 , "Roy Pr\228ger" , "Jetzt kommt es darauf an, dass wir die entscheidenden Punkte gegen den Nicht-Abstieg sammeln!\187" , "" , "Spielerspr\252che" )

joke430 :: Joke
joke430 = ( 430 , "Rudi Assauer" , "Die Alte ist trotzdem unheimlich in Ordnung! Ich habe sie kennengelernt, als ihr Zahnarzt mir das E\223zimmer neu tapeziert hat. Kerstin merkte sofort, dass ich bei der Behandlung ein bi\223chen Schiss hatte. Sie hielt mir ganz lieb das H\228ndchen." , "\252ber seine neue 24j\228hrige Lebensgef\228hrtin, die als Zahnarzthelferin arbeitet" , "Trainerspr\252che" )

joke431 :: Joke
joke431 = ( 431 , "Rudi V\246ller" , "Ja gut, der arbeitet von morgens bis abends. Ja gut, sowas nennt man im Volksmund glaube ich Alcoholic." , "\252ber Reiner Calmund" , "Trainerspr\252che" )

joke432 :: Joke
joke432 = ( 432 , "Rudi V\246ller" , "Was meine Frisur betrifft, da bin ich Realist.\187" , "" , "Spielerspr\252che" )

joke433 :: Joke
joke433 = ( 433 , "Rudi V\246ller" , "Zu 50 Prozent stehen wir im Viertelfinale, aber die halbe Miete ist das noch lange nicht!\187" , "" , "Spielerspr\252che" )

joke434 :: Joke
joke434 = ( 434 , "Rui Costa" , "Zum Gl\252ck hatten wir Gl\252ck.\187" , "" , "Spielerspr\252che" )

joke435 :: Joke
joke435 = ( 435 , "Sabine T\246pperwien" , "...wie Statisten ausgerechnet haben...\187" , "" , "Reporterspr\252che" )

joke436 :: Joke
joke436 = ( 436 , "Sammy Kuffour" , "Wenn wir die drei Titel holen, dann ich Chef in Ghana.\187" , "" , "Spielerspr\252che" )

joke437 :: Joke
joke437 = ( 437 , "Sascha Rufer" , "Als ich das mal versuchte, trug ich danach drei Wochen eine Halskrause." , "nach einem Fallr\252ckzieher-Tor" , "Reporterspr\252che" )

joke438 :: Joke
joke438 = ( 438 , "Sascha Rufer" , "Bei diesem Spiel erkennt man den Unterschied zwischen guten und schlechten Herzschrittmachern\187" , "" , "Reporterspr\252che" )

joke439 :: Joke
joke439 = ( 439 , "Sascha Rufer" , "Die Stimmung auf den R\228ngen kommt mir vor wie bei der Einweihung einer Kl\228ranlage.\187" , "" , "Reporterspr\252che" )

joke440 :: Joke
joke440 = ( 440 , "Sascha Rufer" , "Stancovic hat die Zukunft noch vor sich.\187" , "" , "Reporterspr\252che" )

joke441 :: Joke
joke441 = ( 441 , "Sean Dundee" , "Ich bleibe auf jeden Fall wahrscheinlich beim KSC.\187" , "" , "Spielerspr\252che" )

joke442 :: Joke
joke442 = ( 442 , "Sebastian Deisler" , "Ich hoffe, dass dieses Spiel nicht mein einziges Deb\252t bleibt." , "nach seinem ersten L\228nderspiel:" , "Spielerspr\252che" )

joke443 :: Joke
joke443 = ( 443 , "Sepp Maier" , "Morgens um sieben ist die Welt noch in Dortmund.\187" , "" , "Trainerspr\252che" )

joke444 :: Joke
joke444 = ( 444 , "Sport1" , "Das Spiel der nigerianischen Nationalmannschaft gegen Nigeria fand am Donnerstag nachmittag statt.\187" , "" , "Promis & Presse" )

joke445 :: Joke
joke445 = ( 445 , "Stefan Effenberg" , "Die Meisterschaft ist viel mehr wert als dieses schei\223 X." , "zur Verleihung des FuX des Jahres an Emerson" , "Spielerspr\252che" )

joke446 :: Joke
joke446 = ( 446 , "Stefan Effenberg" , "In Gladbach ist so etwas bestimmt 20-mal passiert. Das geh\246rt dazu. Ich find' das gut." , "\252ber die Ohrfeige, die Lizarazu im Training Matth\228us verpasste" , "Spielerspr\252che" )

joke447 :: Joke
joke447 = ( 447 , "Stefan Effenberg" , "Jeder ist schon mal mit 1,07 Promille gefahren." , "nachdem er bei einer Polizei-Kontrolle erwischt wurde" , "Spielerspr\252che" )

joke448 :: Joke
joke448 = ( 448 , "Stefan Reuter" , "Zur Schiedsrichterleistung will ich gar nichts sagen, aber das war eine Frechheit, was da gepfiffen wurde!\187" , "" , "Spielerspr\252che" )

joke449 :: Joke
joke449 = ( 449 , "Steffen Baumgart" , "Keine Ahnung, ich hab' nicht nachgeschaut." , "auf die Frage, ob seine Mannschaft heute die Hosen voll hatte" , "Spielerspr\252che" )

joke450 :: Joke
joke450 = ( 450 , "Steffen Baumgart" , "Wir haben gen\252gend Potenz f\252r die Bundesliga.\187" , "" , "Spielerspr\252che" )

joke451 :: Joke
joke451 = ( 451 , "Steffen Freund" , "Es war ein wundersch\246ner Augenblick, als der Bundestrainer sagte: \"Komm Stefan, zieh deine Sachen aus, jetzt geht's los.\"\187" , "" , "Spielerspr\252che" )

joke452 :: Joke
joke452 = ( 452 , "Steffen Simon" , "Hier werden Spatzen zu Moorh\252hnern." , "beim 9:1 Sieg Leverkusens in Ulm" , "Reporterspr\252che" )

joke453 :: Joke
joke453 = ( 453 , "Tagesspiegel" , "Stuttgart im Rausch, auf der Galerie und im Gras! Elber schl\228gt die P\228sse schon per Fallr\252ckzieher und auch Fredi Bobic trifft wie er will. Sogar seine Freundin ist schwanger.\187" , "" , "Promis & Presse" )

joke454 :: Joke
joke454 = ( 454 , "Terri Venables" , "Ich denke, wenn die Geschichte sich wiederholt, k\246nnen wir nochmal das gleiche erwarten.\187" , "" , "Trainerspr\252che" )

joke455 :: Joke
joke455 = ( 455 , "The Express" , "Sie konnten nicht gewinnen wie eine normale Mannschaft. Nicht diese M\228nner. Nicht diese bemerkenswerten, unaufhaltsamen und unschlagbaren M\228nner von Manchester. Fergusons G\246tter." , "\252ber ManU nach deren Sieg im CL-Finale gegen Bayern" , "Promis & Presse" )

joke456 :: Joke
joke456 = ( 456 , "The Mirror" , "Die besten zwei Minuten in der Geschichte des Sports." , "zur Schlussphase des CL-Finales ManU gegen Bayern M\252nchen" , "Promis & Presse" )

joke457 :: Joke
joke457 = ( 457 , "Thomas Berthold" , "Ich bin es leid gegen solche Mannschaften zu verlieren. Was will denn Schalke im UEFA-CUP? (Schalke gewann in der darauffolgenden Saison den besagten Pokal.)" , "nach einer Niederlage gegen die Gelsenkirchner" , "Spielerspr\252che" )

joke458 :: Joke
joke458 = ( 458 , "Thomas Doll" , "Ich brauche keinen Butler. Ich habe eine junge Frau! (Diese ist mittlerweile \252brigens mit Olaf Bodden verheiratet.)\187" , "" , "Spielerspr\252che" )

joke459 :: Joke
joke459 = ( 459 , "Thomas H\228\223ler" , "Eine Drehung mehr, und ich w\228re im Rasen verschwunden." , "\252ber den sehr tiefen neuverlegten Rasen im Westfalenstadion" , "Spielerspr\252che" )

joke460 :: Joke
joke460 = ( 460 , "Thomas H\228\223ler" , "Herzlichen Gl\252ckwunsch an Marco Kurz. Seine Frau ist zum zweiten Mal Vater geworden.\187" , "" , "Spielerspr\252che" )

joke461 :: Joke
joke461 = ( 461 , "Thomas H\228\223ler" , "Ich bin k\246rperlich und physisch topfit.\187" , "" , "Spielerspr\252che" )

joke462 :: Joke
joke462 = ( 462 , "Thomas H\228\223ler" , "In der Schule gab's f\252r mich H\246hen und Tiefen. Die H\246hen waren der Fu\223ball.\187" , "" , "Spielerspr\252che" )

joke463 :: Joke
joke463 = ( 463 , "Thomas H\228\223ler" , "Ja ich sach ma, die Irl\228nder waren am Anfang stark..." , "nach einem L\228nderspiel gegen Nordirland" , "Spielerspr\252che" )

joke464 :: Joke
joke464 = ( 464 , "Thomas H\228\223ler" , "Wenn man mir die Freude am Fu\223ball nimmt, h\246rt der Spa\223 bei mir auf!" , "in seiner Dortmunder Zeit" , "Spielerspr\252che" )

joke465 :: Joke
joke465 = ( 465 , "Thomas H\228\223ler" , "Wir wollten in Bremen kein Gegentor kassieren. Das hat auch bis zum Gegentor ganz gut geklappt.\187" , "" , "Spielerspr\252che" )

joke466 :: Joke
joke466 = ( 466 , "Thomas Hermann" , "Die letzten zwei Heimspiele hat M\252nchen 60 kein Tor gegen den VfL Bochum erzielt, heute schon zwei. Das ist eine Steigerung um 200 %.\187" , "" , "Reporterspr\252che" )

joke467 :: Joke
joke467 = ( 467 , "Thomas Hermann" , "Herzog hat nur einen linken Fu\223 und deshalb stand diese Mauer zu weit links. Wohlfahrt h\228tte sie besser stellen m\252ssen. Das Einzige, was derzeit f\252r die \214sterreicher wohl zu weit links steht, ist diese Mauer." , "\252ber den \246sterreichischen Keeper Franz Wohlfahrt nach dem 1:0 der Bremer gegen Stuttgart" , "Reporterspr\252che" )

joke468 :: Joke
joke468 = ( 468 , "Thomas Klementz" , "Sutter hat in den F\252\223en mehr Gef\252hl als andere in den H\228nden. Bei ihm m\246chte man Ball sein.\187" , "" , "Reporterspr\252che" )

joke469 :: Joke
joke469 = ( 469 , "Thomas Strunz" , "Alles andere als die Nicht-Meisterschaft w\228re ja eine Katastrophe gewesen.\187" , "" , "Spielerspr\252che" )

joke470 :: Joke
joke470 = ( 470 , "Thomas Strunz" , "Das Sch\246nste an Stuttgart ist die Autobahn nach M\252nchen.\187" , "" , "Spielerspr\252che" )

joke471 :: Joke
joke471 = ( 471 , "Thomas Strunz" , "Es ist ein Sehnenabri\223 am Schambeinknochen. H\246rt sich lustig an - ist aber trotzdem beim Fu\223ball passiert.\187" , "" , "Spielerspr\252che" )

joke472 :: Joke
joke472 = ( 472 , "Thomas Wark" , "Axel Kruse, er hat in Rostock mehrere Pferdchen laufen.\187" , "" , "Reporterspr\252che" )

joke473 :: Joke
joke473 = ( 473 , "Timo Konietzka" , "Die Laufduelle von Matth\228us mit Turbo Zenden nahmen schon fast dramatische Formen an. Matth\228us kam mir vor wie ein Schmetterling, der gegen den Wind startet.\187" , "" , "Trainerspr\252che" )

joke474 :: Joke
joke474 = ( 474 , "Tita" , "Kleines, dickes Bandito." , "\252ber Reiner Calmund" , "Spielerspr\252che" )

joke475 :: Joke
joke475 = ( 475 , "Toni Pfeffer" , "Hoch werden wir nicht mehr gewinnen." , "\214sterreichs Libero in der Halbzeit des 0:9-Debakels in Spanien" , "Spielerspr\252che" )

joke476 :: Joke
joke476 = ( 476 , "Toni Polster" , "Das ist Wahnsinn! Da gibt's Spieler im Team, die laufen noch weniger als ich!" , "nach einer Niederlage" , "Spielerspr\252che" )

joke477 :: Joke
joke477 = ( 477 , "Toni Polster" , "Das, was ich schon die letzten 20 Jahre gemacht hab\180, mich wichtig machen und deppert reden!" , "auf die Frage, was er im Gladbach-Management tun werde" , "Spielerspr\252che" )

joke478 :: Joke
joke478 = ( 478 , "Toni Polster" , "Der behandelt den Ball wie ein rohes Ei, obwohl noch gar nicht Ostern ist.\187" , "" , "Reporterspr\252che" )

joke479 :: Joke
joke479 = ( 479 , "Toni Polster" , "Ein Denkmal will ich nicht sein, darauf schei\223en ja nur die Tauben." , "auf die Frage, ob er in K\246ln schon ein Denkmal sei" , "Spielerspr\252che" )

joke480 :: Joke
joke480 = ( 480 , "Toni Polster" , "Es gibt Leute, die denken so, und es gibt Leute, die denken so. Das ist immer so, wenn viele Leute zusammenkommen.\187" , "" , "Spielerspr\252che" )

joke481 :: Joke
joke481 = ( 481 , "Toni Polster" , "F\252r mich gibt es nur \"entweder-oder\". Also entweder voll oder ganz!\187" , "" , "Spielerspr\252che" )

joke482 :: Joke
joke482 = ( 482 , "Toni Polster" , "Ich bin Optimist. Sogar meine Blutgruppe ist positiv.\187" , "" , "Spielerspr\252che" )

joke483 :: Joke
joke483 = ( 483 , "Toni Polster" , "Ich habe es mir sehr genau \252berlegt und dann spontan zugesagt." , "\252ber seinen Wechsel zu Borussia M\246nchengladbach" , "Spielerspr\252che" )

joke484 :: Joke
joke484 = ( 484 , "Toni Polster" , "Ich kann nicht mehr als schie\223en. Au\223erdem standen da 40 Leute auf der Linie." , "\252ber eine vergebene Torchance" , "Spielerspr\252che" )

joke485 :: Joke
joke485 = ( 485 , "Toni Polster" , "Man hetzt die Leute auf mit Tatsachen, die nicht der Wahrheit entsprechen.\187" , "" , "Spielerspr\252che" )

joke486 :: Joke
joke486 = ( 486 , "Toni Polster" , "Wir lassen uns beide von unseren Frauen scheiden und ziehen zusammen." , "\252ber sein verbessertes Verh\228ltnis zu Trainer Peter Neururer" , "Spielerspr\252che" )

joke487 :: Joke
joke487 = ( 487 , "Toni Schumacher" , "Dann zahl' ich ihm seine Jacketkronen." , "in Bezug auf das Foul an Battiston" , "Spielerspr\252che" )

joke488 :: Joke
joke488 = ( 488 , "Toni Schumacher" , "Das h\228tte in der T\252rkei passieren d\252rfen, aber nicht in der zivilisierten Welt." , "zum Flutlichtausfall w\228hrend seines Abschiedsspiels in K\246ln" , "Spielerspr\252che" )

joke489 :: Joke
joke489 = ( 489 , "Toni Schumacher" , "Seither bem\252he ich mich, bei jeder leichten Ber\252hrung, bei jedem Zusammensto\223, bei jedem Foul im Gegner zuerst den Menschen zu sehen." , "in Bezug auf das Foul an Battiston" , "Spielerspr\252che" )

joke490 :: Joke
joke490 = ( 490 , "Toni Schumacher" , "Wir stecken zur Zeit in einer Ergebniskrise." , "als Trainer von Fortuna K\246ln" , "Trainerspr\252che" )

joke491 :: Joke
joke491 = ( 491 , "Torsten Legat" , "Die Bayern vertragen keine H\228rte, und ich bin der erste, der anf\228ngt damit." , "vor dem Spiel VfB Stuttgart gegen Bayern M\252nchen 1996" , "Spielerspr\252che" )

joke492 :: Joke
joke492 = ( 492 , "Torsten Legat" , "Die hab ich noch nicht probiert, aber im Allgemeinen mag ich Gefl\252gel." , "nach seinem Wechsel zum VfB Stuttgart auf die Frage, wie er denn Sp\228tzle f\228nde" , "Spielerspr\252che" )

joke493 :: Joke
joke493 = ( 493 , "Torsten Legat" , "Es war toll, es war klasse, es war wie ein Albtraum." , "nach einem hohen Heimsieg" , "Spielerspr\252che" )

joke494 :: Joke
joke494 = ( 494 , "Torsten Legat" , "Ich glaube nicht, dass der Verein mir Steine in den Vertrag legt." , "zu evtl. Wechselproblemen mit Eintracht Frankfurt" , "Spielerspr\252che" )

joke495 :: Joke
joke495 = ( 495 , "Torsten Legat" , "Ich h\228tte auch woanders ins Ausland gehen k\246nnen." , "gibt seinen Wechsel nach Frankfurt bekannt" , "Spielerspr\252che" )

joke496 :: Joke
joke496 = ( 496 , "Torsten Legat" , "Immer Castroper Stra\223e rauf." , "im Sportstudio auf die Frage, wie er zum Bodybuilding gekommen sei" , "Spielerspr\252che" )

joke497 :: Joke
joke497 = ( 497 , "Torsten Legat" , "Verst\228rken k\246nnen die sich, aber nicht auf der rechten Seite. Da bin ich. Ich komme selber aussem Pott. Mein Vater war auf der H\252tte. Wenn ich wieder fit bin, zeig ich dehnen, wat malochen hei\223t.\187" , "" , "Spielerspr\252che" )

joke498 :: Joke
joke498 = ( 498 , "Tschik Cajkovski" , "Winschte, Maschine stirzt ab." , "nach einem 1:8 des 1.FC K\246ln in Dundee" , "Trainerspr\252che" )

joke499 :: Joke
joke499 = ( 499 , "TV-Reporter" , "Herr Basler, was sagen Sie zum Spiel?" , "nach dem 0:3 Deutschlands gegen Portugal bei der EM 2000 zu Marco Bode" , "Reporterspr\252che" )

joke500 :: Joke
joke500 = ( 500 , "Udo Lattek" , "Sie spielen taktisch gut, obwohl sie ohne Taktik spielen.\187" , "" , "Reporterspr\252che" )

joke501 :: Joke
joke501 = ( 501 , "Ulf Kirsten" , "Wenn bei einem Ausw\228rtsspiel keiner ruft: \"Kirsten, Du Arschloch\", dann wei\223 ich genau, dass ich schlecht bin.\187" , "" , "Spielerspr\252che" )

joke502 :: Joke
joke502 = ( 502 , "Uli Borowka" , "Ihr seid n\228mlich auch die, die den Pokal geh\246ren!" , "als Pokalsieger auf dem Bremer Rathausbalkon zu den Fans" , "Spielerspr\252che" )

joke503 :: Joke
joke503 = ( 503 , "Uli Hoene\223" , "Der Alain Sutter muss nur mal ab und zu auf sein M\252sli verzichten und sich einen ordentlichen Schweinebraten einverleiben. (die Antwort von Sutter: Wie man aussieht, wenn man zu viel Schweinebraten isst, sieht man ja an Herrn Hoene\223.)\187" , "" , "Trainerspr\252che" )

joke504 :: Joke
joke504 = ( 504 , "Uli Hoene\223" , "Der kann noch 100 Jahre spielen, der wird uns nie \252berholen." , "\252ber Christoph Daum" , "Trainerspr\252che" )

joke505 :: Joke
joke505 = ( 505 , "Uli Hoene\223" , "Ich glaube nicht, da\223 wir das Spiel verloren h\228tten, wenn es 1:1 ausgegangen w\228re.\187" , "" , "Trainerspr\252che" )

joke506 :: Joke
joke506 = ( 506 , "Uli Hoene\223" , "Wir haben einen Hund zu Hause, der ist acht Wochen alt. Der hat am letzten Sonntag v\246llig verst\246rt unter dem Sofa hervor geschaut. Denn als Bremen in Dortmund das 3:1 erzielt hat, habe ich einen Schrei losgelassen, dass die W\228nde wackelten.\187" , "" , "Trainerspr\252che" )

joke507 :: Joke
joke507 = ( 507 , "Uli Stielike" , "Mal ist die Suppe dick, mal ist sie d\252nn... nur wenn nix Fl\252ssiges drin ist, gibt es einen \228tzenden Geruch.\187" , "" , "Trainerspr\252che" )

joke508 :: Joke
joke508 = ( 508 , "Uwe Bahn" , "Die einzigen Techniker beim HSV vor der \196ra von Trainer Pagelsdorf waren die Stadion-Elektriker.\187" , "" , "Reporterspr\252che" )

joke509 :: Joke
joke509 = ( 509 , "Uwe Fuchs" , "Nationalmannschaft f\252r ihn noch ein Thema sei) Im Moment nicht, Yeboah und Chapuisat sind besser drauf." , "im Sportstudio auf die Frage, ob die (deutsche" , "Spielerspr\252che" )

joke510 :: Joke
joke510 = ( 510 , "Uwe Klimaschewski" , "Unsere Spieler k\246nnen 50-Meter-P\228sse spielen: f\252nf Meter weit und 45 Meter hoch.\187" , "" , "Trainerspr\252che" )

joke511 :: Joke
joke511 = ( 511 , "Uwe Morawe" , "Der geht mir auf den Sack, der Jovic, das ist ein Schei\223spieler.\187" , "" , "Reporterspr\252che" )

joke512 :: Joke
joke512 = ( 512 , "Uwe Seeler" , "Ein Mittelst\252rmer verbringt die meiste Zeit seines Lebens im Strafraum.\187" , "" , "Spielerspr\252che" )

joke513 :: Joke
joke513 = ( 513 , "Vlado Saric" , "Ich fair foul gespielt. Ich nicht getreten.\187" , "" , "Spielerspr\252che" )

joke514 :: Joke
joke514 = ( 514 , "Volker Finke" , "Ich habe zwei verschiedene Halbzeiten gesehen.\187" , "" , "Trainerspr\252che" )

joke515 :: Joke
joke515 = ( 515 , "Waldemar Hartmann" , "Guten Abend, meine Damen und Herren, und - bonne noir.\187" , "" , "Reporterspr\252che" )

joke516 :: Joke
joke516 = ( 516 , "Waldemar Hartmann" , "Was sie hier auf dem Rasen sehen, kostet viele viele viele Millionen Geld, wenn man diese Spieler kauft.\187" , "" , "Reporterspr\252che" )

joke517 :: Joke
joke517 = ( 517 , "Werner Hansch" , "Aumanns Trikot ist voller Schlamm. Wenn der sich jetzt auf ne Heizung setzt, kann er sich mit nem Hammer ausziehen.\187" , "" , "Reporterspr\252che" )

joke518 :: Joke
joke518 = ( 518 , "Werner Hansch" , "Das Gesicht hat er vom Gesichtsverleih." , "\252ber den schlecht gelaunt dreinblickenden Ottmar Hitzfeld" , "Reporterspr\252che" )

joke519 :: Joke
joke519 = ( 519 , "Werner Hansch" , "Dressels Beitrag zum Mozart-Jahr: ein Foul aus dem Kn\246chelverzeichnis.\187" , "" , "Reporterspr\252che" )

joke520 :: Joke
joke520 = ( 520 , "Werner Hansch" , "Ich glaube, sein Problem liegt zwischen den Ohren." , "\252ber Uwe Leifeld, nachdem dieser mehrerer hochkar\228tige Chancen vergeben hatte" , "Reporterspr\252che" )

joke521 :: Joke
joke521 = ( 521 , "Werner Hansch" , "Ingo Anderbr\252gge, der Mann mit dem h\228rtesten Schu\223 der Liga. Alle denken, jetzt holt er den Hammer raus. Aber es war nur der Glasschneider...\187" , "" , "Reporterspr\252che" )

joke522 :: Joke
joke522 = ( 522 , "Werner Hansch" , "Ja, Statistiken. Aber welche Statistik stimmt schon? Nach der Statistik ist jeder vierte Mensch ein Chinese, aber hier spielt gar kein Chinese mit.\187" , "" , "Reporterspr\252che" )

joke523 :: Joke
joke523 = ( 523 , "Werner Hansch" , "Nein, liebe Zuschauer, das ist keine Zeitlupe, der l\228uft wirklich so langsam.\187" , "" , "Reporterspr\252che" )

joke524 :: Joke
joke524 = ( 524 , "Werner Hansch" , "Und wieder nur 500 Zuschauer im K\246lner S\252dstadion, rufen Sie an und ich gebe Ihnen die Namen durch.\187" , "" , "Reporterspr\252che" )

joke525 :: Joke
joke525 = ( 525 , "Werner Hansch" , "Wenn das keine Chance war, dann war das zumindest eine gro\223e M\246glichkeit.\187" , "" , "Reporterspr\252che" )

joke526 :: Joke
joke526 = ( 526 , "Werner Hansch" , "Wir m\252ssen gehen, wir m\252ssen mit ihnen gehen, durch die H\246lle, durch die H\246lle des Elfmeterschie\223ens. Ein Blick nach oben. Ein Sto\223gebet. Ist der Papst, die Frage muss ja noch gestellt werden, Mitglied beim FC Schalke 04? Man muss nicht dran glauben. Das ist keine Glaubensfrage. Ich kenne wohl einen Weihbischof aus Essen, der ist Mitglied beim FC Schalke. Der sollte jetzt auch mal die Daumen dr\252cken." , "in seiner Reportage vom UEFA-Cup-Sieg der Schalker" , "Reporterspr\252che" )

joke527 :: Joke
joke527 = ( 527 , "Werner Lorant" , "Erich Ribbeck ist vom Fu\223ball so weit weg wie die Erde vom Mars.\187" , "" , "Trainerspr\252che" )

joke528 :: Joke
joke528 = ( 528 , "Werner Lorant" , "Ich wechsle nur aus, wenn sich einer ein Bein bricht.\187" , "" , "Trainerspr\252che" )

joke529 :: Joke
joke529 = ( 529 , "Werner Lorant" , "Spieler haben vielleicht ein Problem mit mir, aber ich nicht mit ihnen.\187" , "" , "Trainerspr\252che" )

joke530 :: Joke
joke530 = ( 530 , "Werner Lorant" , "Vieles was darin geschrieben wurde, ist auch wahr." , "\252ber sein Buch \"Eine beinharte Story\"" , "Trainerspr\252che" )

joke531 :: Joke
joke531 = ( 531 , "Werner Zimmer" , "Das bedeutet, dass der Zuschauerschnitt unterboten wurde, und zwar negativ.\187" , "" , "Reporterspr\252che" )

joke532 :: Joke
joke532 = ( 532 , "Wilfried Mohren" , "Auch die Schiedsrichterassistenten an der Linie haben heute ganz ordentlich gepfiffen.\187" , "" , "Reporterspr\252che" )

joke533 :: Joke
joke533 = ( 533 , "Wilfried Mohren" , "Die Schotten sind meistens eher zu Hause als ihre Postkarten." , "zu den WM-Leistungen der Kilt-Tr\228ger" , "Reporterspr\252che" )

joke534 :: Joke
joke534 = ( 534 , "Wilfried Mohren" , "Die Spieler haben einen Blick f\252r Spiel\252bersicht.\187" , "" , "Reporterspr\252che" )

joke535 :: Joke
joke535 = ( 535 , "Wilfried Mohren" , "Sein Vater ist w\228hrend der EM verstorben. Das wird ihm die Freude \252ber einen m\246glichen Titelgewinn etwas versalzen." , "w\228hrend der EM 2000 \252ber den franz\246sischen Nationaltrainer Lemerre" , "Reporterspr\252che" )

joke536 :: Joke
joke536 = ( 536 , "Wilfried Mohren" , "Wie auch immer es ausgehen mag, es war ein schwer erk\228mpfter Sieg f\252r die Bayern.\187" , "" , "Reporterspr\252che" )

joke537 :: Joke
joke537 = ( 537 , "Wilfried Mohren" , "Wir n\228hern uns jetzt dem Punkt im Spiel, der der Dreh- und Angelpunkt sein kann, mu\223 aber wohlgemerkt nicht der Kulminationspunkt sein.\187" , "" , "Reporterspr\252che" )

joke538 :: Joke
joke538 = ( 538 , "\" Lippens\">Willi \"Ente\" Lippens" , "Berti Vogts hatte vor jedem Spiel gegen mich D\252nnschiss.\187" , "" , "Spielerspr\252che" )

joke539 :: Joke
joke539 = ( 539 , "\" Lippens\">Willi \"Ente\" Lippens" , "Ich habe nie eine Torchance \252berhastet vergeben. Lieber habe ich sie vert\228ndelt.\187" , "" , "Spielerspr\252che" )

joke540 :: Joke
joke540 = ( 540 , "\" Lippens\">Willi \"Ente\" Lippens" , "In Wanne Eickel ist ein Hecht gestorben und die Karpfen sind alle dahin zur Beerdigung." , "wird beim Angeln gefragt, warum er denn noch nichts gefangen habe" , "Spielerspr\252che" )

joke541 :: Joke
joke541 = ( 541 , "\" Lippens\">Willi \"Ente\" Lippens" , "Schiri zeigt gelb und sagt: \"Ich verwarne Ihnen!\" Ente: \"Ich danke Sie!\" (Schiri zeigt rot)\187" , "" , "Spielerspr\252che" )

joke542 :: Joke
joke542 = ( 542 , "Willi Entenmann" , "Unser Schiff hat Schlagseite. Es l\228\223t sich nur wieder aufrichten, wenn wir alle auf dieselbe Seite gehen.\187" , "" , "Trainerspr\252che" )

joke543 :: Joke
joke543 = ( 543 , "Willi Landgraf" , "Jung, ich komm aus Bottrop - da wirsse get\246tet, wenne datt inne Muckibude machs!" , "beim Step-Aerobic-Training, auf die Frage, ob so etwas vorher schonmal gemacht habe" , "Spielerspr\252che" )

joke544 :: Joke
joke544 = ( 544 , "Willi Lemke" , "Das beste Trainingslager ist eine Frau, die eigene nat\252rlich.\187" , "" , "Trainerspr\252che" )

joke545 :: Joke
joke545 = ( 545 , "Willi Reimann" , "Einige m\252ssen ihre Bauchmuskeln trainieren, obwohl sie nicht mal wissen, was das ist. Wenn wir die trainieren, kommen sie am n\228chsten Tag an und meinen, sie haben was mit dem Blinddarm." , "\252ber den Trainingseifer seiner Spieler" , "Trainerspr\252che" )

joke546 :: Joke
joke546 = ( 546 , "Willi Reimann" , "Haben Sie genug Zeit?" , "auf die Frage, was er beim HSV alles verbessern m\252sse" , "Trainerspr\252che" )

joke547 :: Joke
joke547 = ( 547 , "Wolf-Dieter Poschmann" , "Schalten wir 'r\252ber zum SV Schalke.\187" , "" , "Reporterspr\252che" )

joke548 :: Joke
joke548 = ( 548 , "Wolf-Dieter Poschmann" , "Und wie sieht's in Brasilien aus, dem Mutterland des Fu\223balls?\187" , "" , "Reporterspr\252che" )

joke549 :: Joke
joke549 = ( 549 , "Wolf-Dieter Poschmann" , "Von J\252rgen Kohler, den sie alle nur \"Kokser\" nennen, zur\252ck zum heutigen Gegner Kolumbien - eine gelungende \220berleitung wie ich finde.\187" , "" , "Reporterspr\252che" )

joke550 :: Joke
joke550 = ( 550 , "Wolf-Dieter Poschmann" , "Wir erinnern uns: Gegen Portugal gab es schon m\252hevolle Unentschieden- und Remis-Spiele.\187" , "" , "Reporterspr\252che" )

joke551 :: Joke
joke551 = ( 551 , "Wolfgang Ley" , "Die Choten machen die Choten dicht.\187" , "" , "Reporterspr\252che" )

joke552 :: Joke
joke552 = ( 552 , "Wolfgang Ley" , "H\228\223ler verliert das Kopfballduell. Das mu\223te ja mal so kommen.\187" , "" , "Reporterspr\252che" )

joke553 :: Joke
joke553 = ( 553 , "Wolfgang Ley" , "Ja, das ist Arigo Sacchi. Es hei\223t ja Forza Italia, und bald hat er seinen letzten gelassen!" , "beim Spiel Italien-Nigeria kurz vor Schluss, Stand 0:1" , "Reporterspr\252che" )

joke554 :: Joke
joke554 = ( 554 , "Wolfgang Ley" , "Letchkov, der die Deutschen bei der WM \252ber den Jordan brachte.\187" , "" , "Reporterspr\252che" )

joke555 :: Joke
joke555 = ( 555 , "Wolfgang Ley" , "Schauen sie nur seinen Mund an. Der ist so breit, wenn der lacht, bekommen die Ohren Besuch!\187" , "" , "Reporterspr\252che" )

joke556 :: Joke
joke556 = ( 556 , "Wolfgang Sidka" , "Wir wohnen in 5-Sterne-Hotels und die Mannschaft l\228uft durch die Hotelhalle wie ein Kegelclub am Ballermann 6." , "auf die Frage, warum er seiner Mannschaft eine Anzugspflicht verordnete" , "Trainerspr\252che" )

joke557 :: Joke
joke557 = ( 557 , "Wolfram Esser" , "Das Spiel ist zu weit, zu eng.\187" , "" , "Reporterspr\252che" )

joke558 :: Joke
joke558 = ( 558 , "Wolfram Esser" , "Die 'Aida' haben wir auch schon in stimmungsvolleren Stadien geh\246rt... in Italien... (es wurde \252brigens der Zigeunerchor aus dem 'Troubadour' gespielt)\187" , "" , "Reporterspr\252che" )

joke560 :: Joke
joke560 = ( 560 , "Wolfram Wuttke" , "Jetzt schei\223 Dir mal nicht vor Dir selber in die Hose, Mann!" , "zu einem Linienrichter" , "Spielerspr\252che" )

joke561 :: Joke
joke561 = ( 561 , "Youri Djorkaeff" , "Ich musste ihm eine Ohrfeige geben. Wie h\228tte ich das sonst meiner Frau erkl\228ren sollen?" , "nachdem ihn sein Gegenspieler Thorsten Fink w\228hrend eines Spiels auf den Mund gek\252sst hatte" , "Spielerspr\252che" )

joke562 :: Joke
joke562 = ( 562 , "Zeitung" , "Der Werder-Trainer sah nach dem Spiel aus, als ob er gerade eine Heizdecke von Woolworth gewonnen h\228tte und nicht die Eintrittskarte f\252rs Berliner Olympiastadion.\187" , "" , "Promis & Presse" )

joke563 :: Joke
joke563 = ( 563 , "Zeitung" , "Gef\228hrlichster St\252rmer der Bayern ist K.-H. Rummenigge (der damals schon Vizepr\228sident war)." , "vor dem Spiel FC Bayern-Raith Rovers" , "Promis & Presse" )

joke564 :: Joke
joke564 = ( 564 , "Z\252rich" , "Wir woll'n keine Z\252rcher Schweine!" , "beim Derby gegen den FC Z\252rich" , "Promis & Presse" )

joke565 :: Joke
joke565 = ( 565 , "Aad de Mos" , "Ich spiele weiterhin mit Risiko. Schlie\223lich profitieren alle davon: Wir, das Publikum und auch der Gegner." , "als Trainer von Werder Bremen" , "Trainerspr\252che" )

joke566 :: Joke
joke566 = ( 566 , "Achim Stocker" , "Ich werde immer \228lter, immer d\252mmer und damit immer geeigneter f\252r den Staatsdienst." , "der Pr\228sident des SC Freiburg ist auch Oberfinanzdirektor" , "Trainerspr\252che" )

joke567 :: Joke
joke567 = ( 567 , "Adi Furler " , "Liebe Zuschauer, meine Kollegen haben geackert wie die K\252mmelt\252rken, um Ihnen diesen Bericht noch zeigen zu k\246nnen!\187" , "" , "Reporterspr\252che" )

joke568 :: Joke
joke568 = ( 568 , "Alexander Schur   " , "Die Schalker machen Picknick in Bremen und wir k\228mpfen um unsere Existenz. Die Dortmunder haben nicht mal gegr\228tscht. F\252r mich ist das alles Mafia." , "der Frankfurter beklagt sich \252ber die Einstellung der Gegner der Abstiegskampfkonkurrenten" , "Spielerspr\252che" )

joke569 :: Joke
joke569 = ( 569 , "Andre Lenz " , "Jetzt falle ich nicht mehr so tief." , "der Torwart von Alemannia Aachen, nachdem der Platz begradigt wurde" , "Spielerspr\252che" )

joke570 :: Joke
joke570 = ( 570 , "Andreas Brehme" , "Die Brasilianer sind ja auch alle technisch serviert." , "als Co-Kommentator bei der WM 98" , "Reporterspr\252che" )

joke571 :: Joke
joke571 = ( 571 , "Anna Doubek " , "Ich wei\223 halt, da spielt jetzt Manchester gegen Deutschland im Finale." , "die tm3-Chefredakteurin zur Champions-League" , "Promis & Presse" )

joke572 :: Joke
joke572 = ( 572 , "Campino " , "Jeder sollte an irgend etwas glauben, und wenn es an Fortuna D\252sseldorf ist.\187" , "" , "Promis & Presse" )

joke573 :: Joke
joke573 = ( 573 , "Martin Schneider" , "Das wohl schwierigste Hobby hat er sich zum Nebenjob gemacht" , "in einem Bericht \252ber den Torwart von San Marino" , "Reporterspr\252che" )

joke578 :: Joke
joke578 = ( 578 , "Stefan Effenberg" , "Ja gut, im ersten Moment dachte ich, den muss der W\228chter halten, aber jetzt wo ich das ganze noch mal in der Wiederholung sehe, muss ich meine Meinung korrigieren. Den Schuss muss er mit dem Fu\223 stoppen und dann wegschie\223en.\n" , "beim CL-Spiel Arsenal London - Hamburger SV" , "Spielerspr\252che" )

joke584 :: Joke
joke584 = ( 584 , "Luca Toni" , "Ich hoffe, dass ich mich hier nicht so aufregen muss wie Giovanni Trapattoni." , "bei seinem ersten Auftritt im Presse-St\252berl der Bayern." , "Spielerspr\252che" )

joke585 :: Joke
joke585 = ( 585 , "Uwe Morawe" , "Sch\246n zu sehen, wie sich Cambiasso die Haare raufen wollte. Aber da ist ja nichts mehr." , "\252ber den fast kahlk\246pfigen Argentinier Esteban Cambiasso" , "Reporterspr\252che" )

joke587 :: Joke
joke587 = ( 587 , "Berti Vogts" , "Wenn es wirklich wahr ist, dass Boulahrouz vor diesem wichtigen Champions-League-Qualispiel seine Verletzung nur vorget\228uscht hat um f\252r einen anderen Verein spielberechtigt zu sein, dann hoffe ich, dass er bei Chelsea zum Trib\252nenigel verkommen wird." , "in der DSF-Spieltagsanalyse zum kurzfristigen Wechsel von Khalid Boulahrouz vom Hamburger SV zu Chelsea London" , "Trainerspr\252che" )

joke588 :: Joke
joke588 = ( 588 , "Christian von Perfall" , "Felix Magath ist auch ein ganz sch\246ner Schleifstein! \n" , "in gem\252tlicher Kneipenrunde bei der Bundesliga-Konferenz" , "Amateurspr\252che" )

joke589 :: Joke
joke589 = ( 589 , "Robert Hoene" , "Wenn wir aus den n\228chsten drei Spielen zehn Punkte holen sind wir durch!" , "in der Kabine nach dem Spiel mitten im Abstiegskampf" , "Amateurspr\252che" )

joke590 :: Joke
joke590 = ( 590 , "Harald Koslowski" , "Das Spiel gegen Brunsbek war wirklich unansehnbar!" , "als Zuschauer nach einem Pokalspiel des TuS Hoisdorf" , "Amateurspr\252che" )

joke592 :: Joke
joke592 = ( 592 , "Lothar Matth\228us" , "Das ist schon sehr beengend. Da f\252hlt man sich wie in einer \214lsardine." , "bei einer PR-Veranstaltung, nachdem er aus einem Formel 1-Mercedes gestiegen ist" , "Spielerspr\252che" )

joke593 :: Joke
joke593 = ( 593 , "Klaus Lufen" , "Auch gr\246\223enm\228\223ig ist es der gr\246\223te Nachteil, dass die Torh\252ter in Japan nicht die allergr\246\223ten sind. \187" , "" , "Promis & Presse" )

joke594 :: Joke
joke594 = ( 594 , "Andreas Goldau" , "Bis zur 70. Minute haben wir ganz gut mitgehalten, aber dann haben wir die Unordnung verloren." , "kurz nach Spielende" , "Amateurspr\252che" )

joke595 :: Joke
joke595 = ( 595 , "Lukas Podolski" , "Ottmar Hitzfeld hat jetzt schon mehr mit mir gesprochen als Felix Magath in der gesamten letzten Saison." , "nach 3 Wochen Vorbereitung auf die neue Saison" , "Spielerspr\252che" )

joke596 :: Joke
joke596 = ( 596 , "Harald Koslowski" , "Der Barbarez ist ein Spieler, den siehst du 90 Minuten nicht, aber in der 89. Minute macht er pl\246tzlich sein Tor." , "Der bekennende HSV-Fan w\228hrend einer Diskussion \252ber die Rothosen." , "Amateurspr\252che" )

joke597 :: Joke
joke597 = ( 597 , "Michael Antwerpes" , "Der Kugelblitz mit Zebrastreifen." , "\252ber den Neu-Duisburger Ailton" , "Reporterspr\252che" )

joke598 :: Joke
joke598 = ( 598 , "Kai Dittmann" , "Mutig von der Wespe, sich mit Oliver Kahn anzulegen." , "\252ber den Bayern-Keeper, der von einer Wespe gestochen wurde." , "Reporterspr\252che" )

joke599 :: Joke
joke599 = ( 599 , "Frank Schulz" , "Ich bin ein richtiger Fussballholik." , "Der ehemalige Bundesliga-Profi in einem Sport-Bild Interview" , "Trainerspr\252che" )

joke600 :: Joke
joke600 = ( 600 , "John Toshak" , "Am Montag nehme ich mir vor, zur n\228chsten Partie zehn Spieler auszuwechseln. Am Dienstag sind es sieben oder acht, am Donnerstag noch vier Spieler. Wenn es dann Samstag wird, stelle ich fest, dass ich doch wieder dieselben elf Schei\223kerle einsetzen muss wie in der Vorwoche. \n" , "als Trainer bei Real Madrid" , "Trainerspr\252che" )

joke601 :: Joke
joke601 = ( 601 , "Mario Basler" , "Der J\228ggi ist ja gerade f\252r mich ein graues Tuch." , "im DSF zu einer Trainerentlassung des FCK-Boss" , "Spielerspr\252che" )

joke602 :: Joke
joke602 = ( 602 , "Mario Basler" , "Ailton wird der Knut der Liga !\nFast wie im Zoo ! Alle kommen ins Stadion und wollen ihn knuddeln." , "in seiner Bild-Kolumne \" Super Mario gibt Gas \"" , "Promis & Presse" )

joke603 :: Joke
joke603 = ( 603 , "Peter Neururer" , "Wenn wir ein Quiz machen w\252rden unter den Trainern in Deutschland, wer am meisten Ahnung hat von Trainingslehre, Psychologie, und der Trainer mit den besten Ergebnissen kriegt den besten Klub - dann w\228re ich bald bei Real Madrid.\"\187" , "" , "Trainerspr\252che" )

joke604 :: Joke
joke604 = ( 604 , "Carlos Alberto Parreira " , "Lasst uns die Leiche mit Anstand begraben und aus der Asche st\228rker zur\252ckkehren als je zuvor." , "Brasiliens Trainer Carlos Alberto Parreira nach dem Viertelfinale-Aus bei der WM 2006" , "Trainerspr\252che" )

joke605 :: Joke
joke605 = ( 605 , "Hans Meyer" , "Ich habe die Sorge, dass, wenn wir Fu\223ball-Weltmeister werden sollten, Achtj\228hrige anfangen, \252ber Gummiringe zu springen." , "N\252rnberg-Trainer Hans Meyer \252ber die teilweise ungew\246hnlichen Trainingsmethoden von J\252rgen Klinsmann." , "Trainerspr\252che" )

joke606 :: Joke
joke606 = ( 606 , "Mark Viduka" , "Ich dachte manchmal, ich m\252sste in einen Krieg ziehen, so hart haben wir trainiert. Es war unglaublich." , "Australiens Kapit\228n Mark Viduka \252ber die intensive WM-Vorbereitung unter Trainer Guus Hiddink." , "Spielerspr\252che" )

joke607 :: Joke
joke607 = ( 607 , "Diego Armando Maradona" , "Wenn ich das argentinische Trikot \252berziehe, bin ich ein anderer Mensch." , "Argentiniens Fu\223ball-Legende Diego Maradona" , "Spielerspr\252che" )

joke608 :: Joke
joke608 = ( 608 , "Pele" , "Der Junge, der so hei\223t wie eine Whiskey-Marke. Swines Tiger." , "Brasiliens Fu\223ball-Idol Pel\233 \252ber Bastian Schweinsteiger" , "Trainerspr\252che" )

joke609 :: Joke
joke609 = ( 609 , "Anthony Yeboah" , "Bl\246d an meinem Job: Ich muss arbeiten wenn auf NDR 2 Fu\223ball l\228uft. \187" , "" , "Spielerspr\252che" )

joke610 :: Joke
joke610 = ( 610 , "Michael Owen" , "Als ich ihr am Telefon erkl\228rte, dass ich verletzt bin und meine WM dahin ist, sagte sie nur: Kannst du sp\228ter noch mal anrufen, ich gucke gerade Fernsehen." , "Englands St\252rmer Michael Owen \252ber Tochter Gemma Rose (3)." , "Spielerspr\252che" )

joke611 :: Joke
joke611 = ( 611 , "Oliver Kahn" , "Die Holl\228nder sind vorne vom Feinsten best\252ckt. \187" , "" , "Spielerspr\252che" )

joke612 :: Joke
joke612 = ( 612 , "Hans Meyer" , "H\246ren Sie, ich bin von Haus aus Kommunist, das hei\223t ich bin von Haus aus arm!" , "Bei seinem Amtsantritt auf dem B\246ckelberg auf die Frage, warum er zur Borussia gekommen sei" , "Trainerspr\252che" )

joke613 :: Joke
joke613 = ( 613 , "Hans Meyer" , "Schweizer sind schlimmer als Schotten. Sie kriegen nicht mal 6 Mark zusammen, gehen lieber sammeln." , "Meyer auf die Ank\252ndigung von Torwart J\246rg Stiel, dass man derzeit in der Mannschaft 6 Mark sammle, damit sich der Trainer eine weitere Flasche Sekt leisten k\246nne" , "Trainerspr\252che" )

joke614 :: Joke
joke614 = ( 614 , "Hans Meyer" , "Bemerkenswert finde ich die Tatsache, dass 3000 unserer Fans in St. Pauli waren und davon waren h\246chstens 2000 wegen der Reeperbahn da." , "Auf der Pressekonferenz nach dem Spiel in Hamburg-St.Pauli" , "Trainerspr\252che" )

joke615 :: Joke
joke615 = ( 615 , "Hans Meyer" , "Das kann ich jetzt nicht sagen. Sonst bekomme ich Probleme, wenn meine Frau an Heiligabend mit dem P\228ckchen mit der Feinripp-Unterw\228sche ankommt. " , "Auf die Frage, ob der Punkt bei den Bayern das sch\246nste Weihnachtsgeschenk sei" , "Trainerspr\252che" )

joke616 :: Joke
joke616 = ( 616 , "Hans Meyer" , "Das Trainingsgel\228nde war hier mein Heiligtum. Platzwart Erich Hage hat mit der Gartenschere das Unkraut gerupft. Wenn sich ein Rabe auf dem Rasen niedergelassen hatte, wurde der erschossen.\"" , "\220ber alte Jenaer Zeiten" , "Trainerspr\252che" )

joke617 :: Joke
joke617 = ( 617 , "Hans Meyer" , "Sie k\246nnen sich die Antwort aussuchen:\n1. Mir war langweilig \n2. Ich brauche Geld ! \n3. Meine Frau l\228\223t mich nicht mehr in den Garten, weil ich die Rosen nicht von Blumenkohl unterscheiden kann !\n" , "Auf die Frage, warum er sich f\252r N\252rnberg entschied" , "Trainerspr\252che" )

joke618 :: Joke
joke618 = ( 618 , "Mehmet Scholl" , "Ich werde nie Golf spielen. Erstens ist das f\252r mich kein Sport, und zweitens habe ich noch regelm\228\223ig Sex." , "Auf die Frage wie er nach dem Karriereende seine Freizeit gestaltet" , "Spielerspr\252che" )

joke619 :: Joke
joke619 = ( 619 , "Fabian Boll" , "Jetzt wollen wir die Nr. 1 am Rhein werden" , "nach dem 1:0 im Pokal gegen Leverkusen und vor dem Saisonstart gegen K\246ln." , "Spielerspr\252che" )

joke620 :: Joke
joke620 = ( 620 , "Steffen Bohl" , "Erst gehen wir in F\252hrung und dann machen wir kurz vor Schluss noch das dumme Gegentor zum Ausgleich." , "Im Interview nach dem 1:1 gegen Borussia M\246nchengladbach" , "Spielerspr\252che" )

joke621 :: Joke
joke621 = ( 621 , "Christoph Daum" , "Die Bayern haben unter dem Bruchstrich nicht 70 sondern vielleicht nur 40 Millionen ausgegeben, wenn man bedenkt wie gut sie gewirtschaftet haben." , "Im DSF-Interview auf die Frage was er von den vielen Millioneneink\228ufen des Rekordmeisters halten w\252rde" , "Trainerspr\252che" )

joke622 :: Joke
joke622 = ( 622 , "Mehmet Scholl" , "Unter Trapattoni haben wir gespielt wie der \"FC Hoch-und-Weit\".\nDa brauchte ich nach dem Spiel immer ein \"ABC-Pflaster\" f\252r meinen\nNacken, weil der Ball immer nur \252ber mich hinweggeflogen ist." , "in einem Interview bei \"Blickpunkt-Sport\"" , "Spielerspr\252che" )

joke623 :: Joke
joke623 = ( 623 , "HSV - Fans" , "Geh! Aber lass Sylvie hier!" , "Spruchband der HSV-Fans \252ber den abwanderungswilligen Rafael van der Vaart" , "Amateurspr\252che" )

joke624 :: Joke
joke624 = ( 624 , "Karl-Heinz K\246rbel" , "H\246chstens als Karnevalsprinz." , "Auf die Frage, ob er sich vorstellen k\246nne, als Trainer auch nach Mainz zu gehen" , "Trainerspr\252che" )

joke625 :: Joke
joke625 = ( 625 , "Reiner Calmund" , "Das Prinzip war: Kamerad, greif Du an, ich geh Proviant holen." , "\252ber das Abwehrverhalten seiner Mannschaft im Spiel gegen den FC Bayern" , "Promis & Presse" )

joke626 :: Joke
joke626 = ( 626 , "Otto Rehhagel" , "Mit 50 bist du als Fu\223balltrainer reif f\252r die Klapsm\252hle. Wenn du genug Geld verdient hast, kannst du wenigstens erster Klasse liegen!\187" , "" , "Trainerspr\252che" )

joke627 :: Joke
joke627 = ( 627 , "Dieter Hecking" , "Wie immer - leicht behaart am Kopf." , "auf die Frage, wie er seinen Profi Jiri Stajner beim 3:2-Sieg gegen den VfL Bochum gesehen habe" , "Trainerspr\252che" )

joke628 :: Joke
joke628 = ( 628 , "Rudi V\246ller" , "Wir haben so viele hervorragende Torh\252ter. Die Engl\228nder w\252rden selbst die Nummer zehn von uns mit Kusshand nehmen." , "\252ber die Qualit\228t der deutschen Keeper" , "Promis & Presse" )

joke629 :: Joke
joke629 = ( 629 , "Berkant G\246ktan" , "Weil wir innerhalb der Mannschaft alle geil aufeinander sind." , "auf die Frage eines Reporters warum es bei den M\252nchenern zurzeit so gut l\228uft" , "Spielerspr\252che" )

joke630 :: Joke
joke630 = ( 630 , "Wolfram Wuttke" , "Immer, wenn ich breit bin, werd ich spitz.\n\187" , "" , "Spielerspr\252che" )

joke631 :: Joke
joke631 = ( 631 , "Ralf Rangnick" , "Ronaldinho kommt f\252r uns nicht infrage. Der ist schon \252ber 25." , "auf die Frage ob der Superstar beim Zweitligisten ein Thema sei." , "Trainerspr\252che" )

joke633 :: Joke
joke633 = ( 633 , "Ernst Kuzorra" , "Wenn ich nicht wusste, wohin mit dem Ball, hab ich n einfach reingewichst!\187" , "" , "Spielerspr\252che" )

joke634 :: Joke
joke634 = ( 634 , "Dr. Frank Bartel" , "Ich kenne Paules K\246rper besser als seine Frau." , "Rostocks Mannschaftsarzt \252ber Mittelfeldspieler Stefan Beinlich" , "Trainerspr\252che" )

joke635 :: Joke
joke635 = ( 635 , "Marcelo Bordon" , "Er hat zwei Herzen und zwei Lungen." , "\252ber seinen neuen Mitspieler Jermaine Jones" , "Spielerspr\252che" )

joke636 :: Joke
joke636 = ( 636 , "Tim Wiese" , "Mir war ziemlich langweilig." , "nach dem 8:1-Sieg gegen Arminia Bielefeld" , "Spielerspr\252che" )

joke637 :: Joke
joke637 = ( 637 , "Miroslav Klose" , "Wann hat er mal aufgelegt?" , "angesprochen auf seine Torvorlage und das gute Zusammenspiel mit Luca Toni" , "Spielerspr\252che" )

joke639 :: Joke
joke639 = ( 639 , "Miroslav Klose" , "Ich habe es nicht gesehen, aber es w\228re sch\246n gewesen, wenn es der Linienrichter gesehen h\228tte." , "zu seinem regul\228ren, aber nicht anerkannten Treffer" , "Spielerspr\252che" )

joke640 :: Joke
joke640 = ( 640 , "Andreas Herzog" , "Ich habe gar nicht gewusst, dass es in \214sterreich 10 000 Idioten gibt." , "angesprochen auf die Aktion \"\214sterreich zeigt R\252ckgrat - f\252r eine \214sterreich-freie Fu\223ball-EM\", bei der schon 10.000 Unterschriften gesammelt wurden" , "Promis & Presse" )

joke641 :: Joke
joke641 = ( 641 , "Edmund Becker" , "Das kann ich gar nicht. Da f\228llt mir ja das k\252nstliche H\252ftgelenk raus." , "auf die Frage, ob er nach dem Schalke-Sieg einen Luftsprung machte" , "Trainerspr\252che" )

joke642 :: Joke
joke642 = ( 642 , "Z\233 Roberto" , "Vielleicht Z\233 Roberto 04 ?" , "auf die Frage welchen Namen sein von Schalke 04 umworbener Namensvetter auf dem Trikot tragen solle." , "Spielerspr\252che" )

joke643 :: Joke
joke643 = ( 643 , "Armin Veh" , "Ich habe alle umarmt und bin dann auf die Toilette - aber nur, weil ich w\228hrend des Spiels so viel trinke." , "nach dem 1:0-Sieg seiner Mannschaft gegen Leverkusen" , "Trainerspr\252che" )

joke644 :: Joke
joke644 = ( 644 , "Armin Veh" , "Wenn mir einer in der Vorbereitung gesagt h\228tte, wir treten mit der Mannschaft gegen Leverkusen an, h\228tte ich ihm gesagt, dann machst du es selbst." , "zu den Personalproblemen beim VfB" , "Trainerspr\252che" )

joke645 :: Joke
joke645 = ( 645 , "Hans Meyer" , "Ich w\252nsche Dir, Felix, dass \246fter Mannschaften kommen, die eine moderne Raumdeckung spielen" , "mit bissiger Ironie \252ber das 1:3 des \"Club\" zu seinem Wolfsburger Kollegen Felix Magath" , "Trainerspr\252che" )

joke646 :: Joke
joke646 = ( 646 , "Albert Streit" , "F\252r so ein Spiel h\228tte ich kein Geld gezahlt." , "nach der m\252den \"Nullnummer\" gegen Hannover 96" , "Spielerspr\252che" )

joke647 :: Joke
joke647 = ( 647 , "Peter H\252tten" , "Nur jeder f\252nfte Bundesb\252rger treibt regelm\228\223ig Sport. Das ergab eine Umfrage unter den Spielern des 1. FC K\246ln." , "in der Comedy-Sendung Kr\252gers Woche" , "Promis & Presse" )

joke648 :: Joke
joke648 = ( 648 , "Christoph Daum" , "In der Schlu\223phase war der Pfosten der Einzige, auf den wir uns 100%ig verlassen konnten." , "im Interview nach einem Spiel" , "Trainerspr\252che" )

joke649 :: Joke
joke649 = ( 649 , "Eduard Geyer" , "Manche junge Spieler haben eine Einstellung zum Leistungssport wie die Nutten auf St. Pauli. Die rauchen, saufen und huren rum, gehen morgens um 6 Uhr ins Bett. \187" , "" , "Trainerspr\252che" )

joke650 :: Joke
joke650 = ( 650 , "Bobby Robson" , "Die ersten 90 Minuten sind die schwersten.\187" , "" , "Trainerspr\252che" )

joke651 :: Joke
joke651 = ( 651 , "Christoph Metzelder" , "Ich habe gerade mit ihm telefoniert, er war gerade auf dem Weg nach Osnabr\252ck, um sich dort vorzustellen." , "\252ber die Aussage von DFB-Keeper Jens Lehmann, er w\252rde auch in die Zweite Liga wechseln, um bei der EM dabei zu sein." , "Spielerspr\252che" )

joke652 :: Joke
joke652 = ( 652 , "Bernd Ehrich" , "M\228nner, die haben heute den Keeper aus der Ersten im Tor. Nicht schie\223en!" , "Trainer vom TSV Trittau II in der Mannschaftsbesprechung vor dem Spiel gegen eine andere zweite Mannschaft " , "Amateurspr\252che" )

joke653 :: Joke
joke653 = ( 653 , "Ewald Reil" , "Gerade ihr m\252sst hier und heute eure ganze Erfahrung in die Wurfschale legen." , "in der Mannschaftsbesprechung vor einem wichtigen Spiel zu Robert Hoene und Norman Habenicht" , "Amateurspr\252che" )

joke654 :: Joke
joke654 = ( 654 , "Michael Allers" , "Das Spiel ist jetzt schon gelaufen. In der zweiten Halbzeit m\252ssen wir aber wenigstens noch Kosmetikkorrektur betreiben." , "in der Halbzeitansprache beim Stand von 0:3" , "Amateurspr\252che" )

joke655 :: Joke
joke655 = ( 655 , "Sepp Piontek" , "Ja, immer wenn wir ein L\228nderspiel hatten, musste ich vorher in den streng bewachten Palast kommen und ihm Aufstellung und Taktik mitteilen. Und dann gab\180s auch Taschengeld von ihm, das sie gerade frisch im Palast unten gedruckt hatten. Die Scheine waren immer noch feucht, klebten zusammen und rochen nach Farbe. Aber ich konnte ohne Probleme damit bezahlen. " , "Sepp Piontek \252ber sein Verh\228ltnis zum Diktator von Haiti, als er dort Nationaltrainer war" , "Trainerspr\252che" )

joke656 :: Joke
joke656 = ( 656 , "Kevin Hofland" , "Eigentlich war es ja schon mein zweites, aber das erste war ein Eigentor. " , "nach seinem ersten Bundesliga-Tor" , "Spielerspr\252che" )

joke657 :: Joke
joke657 = ( 657 , "Klaus Toppm\246ller" , "Ein Fr\252hling macht noch keinen Sommer.\187" , "" , "Trainerspr\252che" )

joke658 :: Joke
joke658 = ( 658 , "Franz Beckenbauer" , "Das einzige, was sich hier bewegt hat, war der Wind." , "\252ber die Deutsche Nationalmannschaft gegen Kamerun" , "Promis & Presse" )

joke659 :: Joke
joke659 = ( 659 , "Dieter Hecking" , "Vor der Leistung einiger Spieler muss ich wirklich den Hut zollen." , "nach einem 1:1 gegen Uerdingen" , "Spielerspr\252che" )

joke660 :: Joke
joke660 = ( 660 , "Ernst Fricke" , "Unsere Ausw\228rtsschw\228che ist st\228rker geworden." , "Pr\228sident von Eintracht Braunschweig" , "Promis & Presse" )

joke661 :: Joke
joke661 = ( 661 , "Werner Hansch" , "Deutlich ging f\252r mich der Ellenbogen zur Hand.\187" , "" , "Reporterspr\252che" )

joke662 :: Joke
joke662 = ( 662 , "Rainer Bonhof" , "Ich kann schlecht G\252nter Netzer um seine alten Latschen bitten. Mit dessen Gr\246\223e w\252rden die in Indonesien Wasserski fahren." , "\252ber ein Hilfsprojekt in Indonesien f\252r das er Fussballschuhe organisieren will." , "Trainerspr\252che" )

joke663 :: Joke
joke663 = ( 663 , "Felix Magath" , "Wenn mein Vater da gewesen w\228re, h\228tte sich mein Leben vollkommen anders entwickelt. Viel zielgerichteter. Dann w\228re meine Mutter zu Hause gewesen. Ich h\228tte vern\252nftig f\252r die Schule gearbeitet, einen normalen Beruf erlernt und w\228re nicht in den Fu\223ball abgedriftet. \187" , "" , "Trainerspr\252che" )

joke664 :: Joke
joke664 = ( 664 , "Ottmar Hitzfeld" , "Wolfsburg hat einige starke Spieler. Dejagah, Krzynowek, GRANITE..." , "in der Pressekonferenz vor dem Spiel gegen Wolfsburg" , "Spielerspr\252che" )

joke665 :: Joke
joke665 = ( 665 , "Lukas Podolski" , "Ich habe nackt in der Kabine getanzt." , "lachend, als er auf das EM-Aus der Engl\228nder angesprochen wurde." , "Spielerspr\252che" )

joke666 :: Joke
joke666 = ( 666 , "Andreas M\246ller" , "Andere k\246nnen sich ja gerne vor dem Spiel die Eier hart kochen.\n" , "Nachdem ihm gesagt wurde, dass er ein Weichei sei" , "Spielerspr\252che" )

joke667 :: Joke
joke667 = ( 667 , "Christoph Daum" , "Da muss erst einmal ein Schnupf... \228hhm Schneepflug dr\252ber." , "\220ber die Platzverh\228ltnisse in M\252nchen" , "Trainerspr\252che" )

joke668 :: Joke
joke668 = ( 668 , "Erik Meijer" , "Es ist nichts schei\223er als Platz zwei." , "Nach Erreichen des 2. Platzes in der 2. Liga." , "Spielerspr\252che" )

joke669 :: Joke
joke669 = ( 669 , "Felix Magath" , "Ich habe viel mit Mario Basler gemeinsam.\nWir sind beide Fu\223baller, wir trinken beide gern mal einen,\nich allerdings erst nach der Arbeit!\187" , "" , "Spielerspr\252che" )

joke670 :: Joke
joke670 = ( 670 , "Hans Meyer" , "Wir gehen nachts mit Fackeln in den Wald, damit die Spieler ihre Angst verlieren." , "Der Trainer der abstiegsbedrohten N\252rnberger \252ber \252ber seine Ma\223nahmen im Trainingslager." , "Trainerspr\252che" )

joke671 :: Joke
joke671 = ( 671 , "Bela Rethy" , "Portugal spielt heute mit sechs Ausl\228ndern.\187" , "" , "Reporterspr\252che" )

joke672 :: Joke
joke672 = ( 672 , "Claudio Pizarro" , "Nein! Was ist das?" , "Auf die Frage, ob er die Laufwege von Giovanne Elber schon kenne." , "Spielerspr\252che" )

joke673 :: Joke
joke673 = ( 673 , "Giovanni van Bronckhorst" , "Wenn sein Schuss reingegangen w\228re, h\228tte das Ergebnis anders aussehen k\246nnen.\187" , "" , "Spielerspr\252che" )

joke674 :: Joke
joke674 = ( 674 , "Uli Hoene\223" , "Was mich besonders freut: Wir haben vor allem in den Heimspielen die Leute fast immer total begeistert." , "Manager Uli Hoene\223 in der Allianz Arena in einem Interview mit dem Stadionsprecher vor dem 0:0 gegen den MSV Duisburg" , "Promis & Presse" )

joke675 :: Joke
joke675 = ( 675 , "NDR 2 Radio Konferenz" , "Mike Hanke mit dem1:0 fuer Hannover!\nErziehlt per Fuss und nicht per Kopf. Und vor allem nicht Per Mertesacker." , "Kommentar zum Nordderby zwischen Hannover 96 und Werder Bremen" , "Reporterspr\252che" )

joke676 :: Joke
joke676 = ( 676 , "Christian Eichner" , "Ich trinke jetzt schnellstens zwei, drei Bier - manche vielleicht auch mehr -, um den sp\228ten Ausgleich zu verdauen." , "zum 1:1 des HSV in der Nachspielzeit" , "Spielerspr\252che" )

joke677 :: Joke
joke677 = ( 677 , "Helmut Sch\246n" , "Da gehe ich mit Ihnen ganz chlorophorm.\187" , "" , "Trainerspr\252che" )

joke678 :: Joke
joke678 = ( 678 , "Horst Hrubesch" , "Da hab ich gedacht, da tu ich ihn ihm rein in ihm sein Tor." , "das ehemalige Kopfball-Ungeheuer in einem Interview" , "Spielerspr\252che" )

joke679 :: Joke
joke679 = ( 679 , "Andreas Brehme" , "Uns steht ein hartes Programm ins Gesicht.\187" , "" , "Trainerspr\252che" )

joke680 :: Joke
joke680 = ( 680 , "Guido Buchwald" , "Ich hab eine Oberschenkelzerrung im linken Fu\223.\187" , "" , "Spielerspr\252che" )

joke681 :: Joke
joke681 = ( 681 , "Christian Beeck" , "Wir haben mit der notwendigen fairen Brutalit\228t gespielt.\187" , "" , "Spielerspr\252che" )

joke682 :: Joke
joke682 = ( 682 , "Luca Toni" , "Immer wenn ich sage, ich bin 100-prozentig fit, schie\223e ich kein Tor. Also sage ich lieber: Ich bin nicht 100-prozentig fit." , "auf die Frage, ob er f\252r das Spiel gegen Aris Thessaloniki fit sei." , "Spielerspr\252che" )

joke683 :: Joke
joke683 = ( 683 , "Klaus Allofs" , "Vielleicht ist er ja bis dahin wieder au\223er Form." , "Der Werder-Manager \252ber Boubacar Sanogo, der wohl im Januar zum Afrika-Cup muss." , "Promis & Presse" )

joke684 :: Joke
joke684 = ( 684 , "Berti Vogts" , "Der Deutsche hat nie Angst.\187" , "" , "Trainerspr\252che" )

joke685 :: Joke
joke685 = ( 685 , "Oliver Bierhoff" , "Als Deutschland muss man gegen Finnland gewinnen!\187" , "" , "Trainerspr\252che" )

joke686 :: Joke
joke686 = ( 686 , "Lothar Matth\228us" , "Gewollt hab ich schon gemocht, aber gedurft ham sie mich nicht gelassen!\187" , "" , "Spielerspr\252che" )

joke687 :: Joke
joke687 = ( 687 , "Bernd Schmelzer" , "Demichelis - Im Vergleich zu Makaay die Aldi-Variante.\187" , "" , "Reporterspr\252che" )

joke688 :: Joke
joke688 = ( 688 , "Torsten Frings" , "Das interessiert mich nicht. Das interessiert mich wirklich nicht." , "zur Verpflichtung von J\252rgen Klinsmann als Trainer des FC Bayern M\252nchen" , "Spielerspr\252che" )

joke689 :: Joke
joke689 = ( 689 , "Heribert Bruchhagen" , "Das ist sicher ein hochinteressantes Experiment. Da bekommen die Bayern zu ihren vier Experten noch einen f\252nften dazu." , "zur Verpflichtung von J\252rgen Klinsmann als Bayern-Coach" , "Promis & Presse" )

joke690 :: Joke
joke690 = ( 690 , "Dietmar Hamann" , "Dass er mich umgesto\223en und mir damit den Ball weggenommen hat,konnt ich grad noch verkraften,\naber als er mich dann einen \"Pardon\" nannte, habe ich die Nerven verloren und nachgetreten!\n\n" , "Vor dem DFB-Sportgericht nach einer Roten Karte" , "Spielerspr\252che" )

joke691 :: Joke
joke691 = ( 691 , "Zvjezdan Misimovic" , "Wir d\252rfen jetzt nicht den Sand in den Kopf stecken." , "nach dem 1:1 gegen Hansa Rostock" , "Spielerspr\252che" )

joke692 :: Joke
joke692 = ( 692 , "Rolf Dohmen" , "Es kommt kein Sponsor mehr, wenn man in Hannover 2:2 spielt.\187" , "" , "Promis & Presse" )

joke693 :: Joke
joke693 = ( 693 , "Marcel Sch\228fer" , "Bei meinen Sch\252ssen m\246chte ich auch nicht im Tor stehen." , "\252ber seinen haltbaren Treffer zum 1:1 gegen den MSV Duisburg" , "Spielerspr\252che" )

joke694 :: Joke
joke694 = ( 694 , "Marc Stein" , "F\252r den Sieg gibt es zwar nur drei Punkte, aber auf dem Weg zum Klassenverbleib k\246nnten die f\252r uns doppelt z\228hlen." , "Rostocks Abwehrspieler nach dem 1:0 gegen Eintracht Frankfurt" , "Spielerspr\252che" )

joke695 :: Joke
joke695 = ( 695 , "Benjamin Auer" , "Der Kloppo ist schon eine Initiative in Mainz." , "er meinte wohl Institution..." , "Spielerspr\252che" )

joke696 :: Joke
joke696 = ( 696 , "Mario Gomez" , "Normalerweise respektiere ich im Fu\223ball jeden Gegenspieler. Aber dieses Arschloch...\187 " , "\252ber seinen Karlsruher Kontrahenten Maik Franz" , "Spielerspr\252che" )

joke697 :: Joke
joke697 = ( 697 , "Edmund Becker" , "Das sind hei\223bl\252tige Jungs, die am Rande der Legalit\228t spielen." , "KSC-Trainer Becker \252ber Mario Gomez und Maik Franz" , "Trainerspr\252che" )

joke698 :: Joke
joke698 = ( 698 , "Edmund Becker" , "Wir m\252ssen schauen, ob wir in Karlsruhe zwei oder drei Banken ausrauben k\246nnen, um einen Spieler dieser Qualit\228t verpflichten zu k\246nnen.\187" , "" , "Trainerspr\252che" )

joke699 :: Joke
joke699 = ( 699 , "Uli Hoene\223" , "Wir wollen ihn nicht zum Sozialfall machen." , "Der Bayern-Manager auf die Frage, ob Profi Mark van Bommel nach seinem Platzverweis mit einer vereinsinternen Geldstrafe rechnen muss" , "Promis & Presse" )

joke700 :: Joke
joke700 = ( 700 , "Malik Fatih" , "Ich hatte bisher zwei praktische Pr\252fungen, eine in Fu\223ball und eine in Turnen. Bei beiden habe ich 13 Punkte erreicht. Im Fu\223ball ist das nat\252rlich jetzt nicht so doll, aber \252ber die Note im Turnen bin ich sehr zufrieden." , "Hertha-Profi Malik Fathi, der nebenbei an der HU Berlin noch Sportwissenschaften studiert" , "Spielerspr\252che" )

joke702 :: Joke
joke702 = ( 702 , "G\252nter Delzepich" , "Wir haben Krafttraining mit Medizinb\228llen gemacht. Dann lag der Ball am Sechzehner. Ich sagte: Ich mach ihn rein. Die anderen wetteten dagegen. Na ja, dann hab ich ihn dr\252ber geschossen." , "Aachens fr\252herer fussballprofi, knapp 100 Kilo schwer, in der Zeitschrift \"11 Freunde\" " , "Spielerspr\252che" )

joke704 :: Joke
joke704 = ( 704 , "Werner Hansch" , "\"....ein Schu\223, wie durch Gelee gezogen!\"\187" , "" , "Reporterspr\252che" )

joke705 :: Joke
joke705 = ( 705 , "Ioannis Amanatidis" , "Ich wei\223 nicht, was in seinem Kopf vorgeht. Viel wird das nicht sein." , "\252ber seinen \252bermotivierten Gegenspieler Maik Franz vom Karlsruher SC" , "Spielerspr\252che" )

joke706 :: Joke
joke706 = ( 706 , "Fans vom Karlsruher SC" , "Maik Franz foult nicht, seine Gegner knien vor ihm nieder." , "Spruchband der KSC-Fans" , "Promis & Presse" )

joke707 :: Joke
joke707 = ( 707 , "Huub Stevens" , "Ich m\246chte Dortmund einladen, an der niederl\228ndischen Meisterschaft teilzunehmen, damit ich dann auch immer Punkte holen kann." , "Der am Saisonende vom HSV zu PSV Eindhoven wechselnde Trainer Huub Stevens, der seit zw\246lf Spielen gegen Dortmund ohne Niederlage ist" , "Trainerspr\252che" )

joke708 :: Joke
joke708 = ( 708 , "Mario Gomez" , "Ich habe immer auf die Anzeigetafel geschaut und gesehen, dass Luca Toni schon zwei Tore hatte. Da dachte ich: Jetzt fange ich auch mal an - und hinten ist es einfacher." , "nach seinem Eigentor gegen Hansa Rostock \252ber sein Duell mit Bayern-Star Toni um die Torj\228ger-Kanone" , "Spielerspr\252che" )

joke709 :: Joke
joke709 = ( 709 , "Uli Hoene\223" , "Das ist die gr\246\223te Karl-May-Geschichte \252berhaupt." , "zu Spekulationen \252ber die Verpflichtung von Torh\252ter Artur Boruc von Celtic Glasgow" , "Promis & Presse" )

joke710 :: Joke
joke710 = ( 710 , "Oliver Kahn" , "Was hei\223t da heute?" , "auf die Feststellung eines Reporters, dass er heute wie ein 25- J\228hriger gespielt habe" , "Spielerspr\252che" )

joke711 :: Joke
joke711 = ( 711 , "Stefan Kuntz" , "Vielleicht gehe ich nach Istanbul zur\252ck und fahre ein Jahr nach Australien oder gehe ins Dschungelcamp." , "Bochums Manager Stefan Kuntz \252ber seine ungewisse Zukunft" , "Promis & Presse" )

joke712 :: Joke
joke712 = ( 712 , "Jens Ulrich Seffen" , "Stimmt, aber f\252r Euch reicht\180s noch !" , "Auf den Zuruf \"Fliegenf\228nger\" von der gegnerischen Bank beim Aufstiegsentscheidungsspiel zur Stadtliga." , "Amateurspr\252che" )

joke713 :: Joke
joke713 = ( 713 , "Markus Schuler" , "Das war ein klarer Griff in die Familienplanung, die bei mir noch nicht abgeschlossen ist. Es hat wehgetan." , "nach dem Griff des Hamburger Rots\252nders David Jarolim in seinen Unterleib" , "Spielerspr\252che" )

joke714 :: Joke
joke714 = ( 714 , "Thomas von Heesen" , "Jacques kann knacken. Er kann hart gegen den Mann gehen. Das ist es, was man im Abstiegskampf braucht." , "N\252rnbergs Trainer nach dem gelungenen Auftritt von Innenverteidiger Jacques Abardonado beim 1:1 gegen Bayern M\252nchen" , "Trainerspr\252che" )

joke715 :: Joke
joke715 = ( 715 , "Mirko Slomka" , "Ich habe Kevin auf dem Platz gesehen." , "zur Behauptung, Nationalspieler Kevin Kuranyi sei im 0:0-Spiel beim KSC praktisch nicht zu sehen gewesen" , "Trainerspr\252che" )

joke717 :: Joke
joke717 = ( 717 , "Michael A. Roth" , "Wieso sollte ich Angst haben. Ich habe den gr\246\223ten Teil meines Lebens doch schon hinter mir." , "N\252rnbergs Pr\228sident auf die Frage, ob er Angst gehabt habe, als er vom Spielfeld aus die FCN-Fans beruhigen wollte" , "Promis & Presse" )

joke718 :: Joke
joke718 = ( 718 , "Huub Stevens" , "Ich verliere lieber einmal und gewinne dann n\228chste Woche, als dass ich zweimal unentschieden spielen. Dann habe ich n\228mlich drei statt zwei Punkte." , "nach dem 0:1 in Stuttgart" , "Trainerspr\252che" )

joke719 :: Joke
joke719 = ( 719 , "Felix Magath" , "Madlung hat einen Hexenschuss. Das kann jedem passieren bei so einem Sauwetter.\187" , "" , "Trainerspr\252che" )

joke720 :: Joke
joke720 = ( 720 , "Sebastian Kehl" , "Wei\223! Blau darf ich ja nicht." , "vor dem M\252nzwurf zur Platzwahl in Bremen angesichts der Rivalit\228t der Borussia zu Schalke 04" , "Spielerspr\252che" )

joke721 :: Joke
joke721 = ( 721 , "Mike B\252skens" , "Jeder will lieber nach Barcelona reisen als nach Wladikawkas." , "zu den internationalen Ambitionen der Schalker" , "Trainerspr\252che" )

joke722 :: Joke
joke722 = ( 722 , "Rudi Bommer" , "Mit den ersten zwei, drei Minuten war ich gar nicht zufrieden." , "nach dem 1:1 in Bochum" , "Trainerspr\252che" )

joke723 :: Joke
joke723 = ( 723 , "Dieter Hoene\223" , "Das Ziel f\252r dieses Jahr ist erreicht. Es fehlen nur ein paar Punkte.\187" , "" , "Promis & Presse" )

joke724 :: Joke
joke724 = ( 724 , "Mehmet Scholl" , "Ich kann alle beruhigen, der Vater ist wohlauf, hat die Geburt gut \252berstanden." , "zu einem Reporter kurz nach der Geburt seiner Tochter Josefine" , "Promis & Presse" )

joke725 :: Joke
joke725 = ( 725 , "Mark van Bommel" , "Das gibt - wie hei\223t das in Deutschland? H\228hnchenfell? G\228nsehaut?" , "Mark van Bommel auf die Frage, was die Meisterfeier bei ihm ausl\246se." , "Spielerspr\252che" )

joke726 :: Joke
joke726 = ( 726 , "Ottmar Hitzfeld" , "Ich hoffe nicht, dass ich so unvern\252nftig bin und noch einmal Bundesliga-Trainer werde." , "Der k\252nftige Schweizer Nationaltrainer nach seinem letzten Spiel als Vereinstrainer" , "Reporterspr\252che" )

joke727 :: Joke
joke727 = ( 727 , "Mirko Ruppe" , "Die H\228lfte davon brauch ich." , "Antwort auf das er die h\228lfte der Spieler nicht brauche." , "Amateurspr\252che" )

joke728 :: Joke
joke728 = ( 728 , "Mario Basler" , "Wir wissen ja aus Dortmund, dass f\252r ihn ein 100-Meter-Sprint zum Acht-Stunden-Arbeitstag werden kann." , "vor dem EM-Spiel gegen Kroatien \252ber Verteidiger Robert Kovac" , "" )

joke729 :: Joke
joke729 = ( 729 , "Lukas Podolski" , "Fu\223ball is\180 wie Schach - nur ohne W\252rfel." , "in einem Interview w\228hrend der EM" , "Trainerspr\252che" )

joke730 :: Joke
joke730 = ( 730 , "Gerd Rubenbauer" , "Jetzt wechselt Jamaica den Torwart aus!" , "nachdem der Schiedsrichterassistent 1 Minute Nachspielzeit angezeigt hatte" , "Reporterspr\252che" )

joke731 :: Joke
joke731 = ( 731 , "Roy Makaay" , "Ich war auch schon mal in Spanien deutscher Meister!" , "Bei einem Interview nach der gewonnenen Meisterschaft" , "Spielerspr\252che" )

joke733 :: Joke
joke733 = ( 733 , "Stefan Raab" , "Was Olli Kahn nicht wusste: im Pokal hatte sich Phillip Lahm versteckt!" , "Nach dem DFB-Pokalfinale 2008" , "Promis & Presse" )

joke734 :: Joke
joke734 = ( 734 , "Heinrich L\252bke (Bundespr\228sident)" , "Es war ein Tor. Ich habe es genau gesehen, meine Herren!" , "\252ber das Wembley-Tor" , "Promis & Presse" )

joke735 :: Joke
joke735 = ( 735 , "Franz Beckenbauer" , "Abseits ist, wenn der Schiedsrichter pfeifft\187" , "" , "Trainerspr\252che" )

joke736 :: Joke
joke736 = ( 736 , "Krassimir Balakow" , "F\252r mich spielen Punkte keine Rolle. Ich schaue immer nach oben weil ich am Ende dort sthehen m\246chte.\187" , "" , "Trainerspr\252che" )

joke737 :: Joke
joke737 = ( 737 , "Beni Turnheer" , "In einer Herde von schwarzen Schafen ist das weisse Schaf das schwarze Schaf." , "Schweden - Trinidad & Tobago, in Anspielung auf den ersten weissen Spieler von T&T seit 40 Jahren." , "Reporterspr\252che" )

joke738 :: Joke
joke738 = ( 738 , "John Motson (BBC Kommentator)" , "Die Weltmeisterschaft ist wirklich ein internationales Turnier.\187" , "" , "Reporterspr\252che" )

joke739 :: Joke
joke739 = ( 739 , "Berti Vogts" , "Die Realit\228t sieht anders aus als die Wirklichkeit.\187" , "" , "Trainerspr\252che" )

joke740 :: Joke
joke740 = ( 740 , "Uwe Seeler" , "Also, ein normales Foul ist f\252r mich nicht unfair.\187" , "" , "Spielerspr\252che" )

joke741 :: Joke
joke741 = ( 741 , "Brian Clough" , "Akne ist ein gr\246sseres Problem als Verletzungen.\187" , "" , "Trainerspr\252che" )

joke742 :: Joke
joke742 = ( 742 , "Dettmar Cramer" , "Die Wahrscheinlichkeit, nicht Meister zu werden, ist gr\246sser als die Wahrscheinlichkeit, dem Abstieg zu entgehen. \187" , "" , "Trainerspr\252che" )

joke743 :: Joke
joke743 = ( 743 , "Fans des FC Middlesborough" , "Du bist fett, und schl\228gst deine Frau." , "Schlachtruf zur Begr\252ssung von Paul Cascoigne vor jedem Spiel 1998" , "Amateurspr\252che" )

joke744 :: Joke
joke744 = ( 744 , "J\246rg Wontorra" , "Ich bin ganz sicher, dass Mario Basler in der 2.Halbzeit vielleicht noch kommen wird.\187" , "" , "Reporterspr\252che" )

joke745 :: Joke
joke745 = ( 745 , "Robert Wieschemann" , "F\252r uns sind Nichteurop\228er genauso Menschen wie Europ\228er!\187" , "" , "Amateurspr\252che" )

joke746 :: Joke
joke746 = ( 746 , "Matthias Sammer" , "Ich bin auch ein Mensch.\187" , "" , "Trainerspr\252che" )

joke747 :: Joke
joke747 = ( 747 , "Kevin Keegan" , "Ich glaube nicht, das irgendwer gr\246sser oder kleiner ist als Maradona.\187" , "" , "Trainerspr\252che" )

joke748 :: Joke
joke748 = ( 748 , "Mehmet Scholl" , "Ich fliege irgendwo in den S\252den - vielleicht nach Kanada oder so.\187" , "" , "Spielerspr\252che" )

joke749 :: Joke
joke749 = ( 749 , "Eike Immel" , "Im Grossen und Ganzen war es ein Spiel, das, wenn es anders l\228uft, auch anders h\228tte laufen k\246nnen.\187" , "" , "Spielerspr\252che" )

joke750 :: Joke
joke750 = ( 750 , "SAT.1" , "In der letzten Saison hat der VFL Bochum von 5 Elfmetern 6 verschossen.\187" , "" , "Promis & Presse" )

joke751 :: Joke
joke751 = ( 751 , "Otto Rehhagel" , "Mal verliert man und mal gewinnen die anderen.\187" , "" , "Trainerspr\252che" )

joke752 :: Joke
joke752 = ( 752 , "Werner Altegoer" , "Man tritt immer nur dann ein Tor, wenn man auf die Bude schiesst.\187" , "" , "Amateurspr\252che" )

joke753 :: Joke
joke753 = ( 753 , "Kevin Keegan" , "Steve McManaman wird mit Steve Highway verglichen, dabei ist er \252berhaupt nicht wie er, und ich weiss auch warum: Weil er ein bisschen anders ist.\187" , "" , "Trainerspr\252che" )

joke754 :: Joke
joke754 = ( 754 , "Rudi V\246ller" , "Wie so oft liegt auch hier die Mitte in der Wahrheit.\187" , "" , "Trainerspr\252che" )

joke755 :: Joke
joke755 = ( 755 , "Reiner Calmund" , "Wir sind nur die Underducks.\187" , "" , "Trainerspr\252che" )

joke756 :: Joke
joke756 = ( 756 , "Bela Rethy" , "Jetzt kommt der Mann, dessen Name hervorragend zum Wedau-Stadion passt: Marcus Wedau.\187" , "" , "Reporterspr\252che" )

joke757 :: Joke
joke757 = ( 757 , "Bela Rethy" , "Auch in dieser Szene beweist der Schalker seine Schusskraft, nutzt die gesamte H\246he des Platzes." , "kommentiert eine Szene von J\246rg B\246hme" , "Reporterspr\252che" )

joke758 :: Joke
joke758 = ( 758 , "Bela Rethy" , "Pinto fordert den Ball, aber Soldo entschlie\223t sich zu einem Fehlpass.\187" , "" , "Reporterspr\252che" )

joke759 :: Joke
joke759 = ( 759 , "Bela Rethy" , "Jetzt kommt einer, der in der Lage ist, im 1-gegen-1 \220berzahl zu schaffen.\187" , "" , "Reporterspr\252che" )

joke760 :: Joke
joke760 = ( 760 , "Beni Thurnheer" , "Das irische Publikum wird von seinen Fans ausgebuht.\187" , "" , "Reporterspr\252che" )

joke761 :: Joke
joke761 = ( 761 , "Charly Leitner" , "Beide Teams haben die letzten Spiele verloren, hoffentlich wird heute wieder eines davon punkten." , "kommentiert Tirol-Salzburg" , "Reporterspr\252che" )

joke762 :: Joke
joke762 = ( 762 , "Dani Wyler" , "Eine Riesenchance. Das Tor war leer, bis auf den Torh\252ter.\187" , "" , "Reporterspr\252che" )

joke763 :: Joke
joke763 = ( 763 , "Frank Schmettau" , "Gelb-Rot gegen den gelb-roten Braun, der die Braunen gefoult hatte." , "beim Spiel St. Pauli (braun) gegen Karlsruhe (gelb-rot)" , "Reporterspr\252che" )

joke764 :: Joke
joke764 = ( 764 , "Fritz von Thurn und Taxis" , "Frankreich, der erste Anw\228rter auf die Titelverteidigung." , "kommentiert das Er\246ffnungsspiel der WM 2002" , "Spielerspr\252che" )

joke766 :: Joke
joke766 = ( 766 , "G\252nter Netzer" , "Die meisten Spiele, die 1:0 ausgingen, wurden gewonnen.\187" , "" , "Reporterspr\252che" )

joke767 :: Joke
joke767 = ( 767 , "Werner Schneyder" , "Zu spielen: noch eine halbe Stunde, sogar noch etwas dr\252ber, also noch 15 Minuten.\187" , "" , "Reporterspr\252che" )

joke768 :: Joke
joke768 = ( 768 , "Sport1.de" , "Zuvor jedoch mussten die BVB-Fans zittern, als K\246ln den Angriffsdruck erh\246hte und Anthony Lurling wenige Sekunden vor seiner Einwechslung (72.) nur den Innenpfosten traf.\187" , "" , "Promis & Presse" )

joke769 :: Joke
joke769 = ( 769 , "ARD WM Service Team" , "Ihre Kritik an Herrn Fa\223bender ist sicherlich berechtigt, jedoch gibt es w\228hrend dieser WM kaum noch Chancen, ihn auszutauschen. Das h\228ngt auch damit zusammen, dass er als Leiter des WDR-Sports ein Moderationsvorrecht genie\223t und dieses dementsprechend ausnutzt. Wir bedauern, Ihnen keine bessere Mitteilung machen zu k\246nnen." , "antwortet auf eine Anfrage der Frankfurter Rundschau, warum ausgerechnet Heribert Fa\223bender die WM-Spiele der deutschen Nationalmannschaft kommentiere" , "Promis & Presse" )

joke770 :: Joke
joke770 = ( 770 , "Bernd H\246lzenbein" , "Fr\252her war man als Kind bei Schl\228gereien auch zu Hause st\228rker als in Nachbars Garten." , "\252ber die Frankfurter Ausw\228rtsschw\228che" , "Amateurspr\252che" )

joke771 :: Joke
joke771 = ( 771 , "Werner Hansch" , "Wer hinten so offen ist, kann nicht ganz dicht sein.\187" , "" , "Reporterspr\252che" )

joke772 :: Joke
joke772 = ( 772 , "Christian Genau" , "Die spielerischen Defizite konnten meistens mit Kampf wettgemacht werden, die k\228mpferische Einstellung fehlte jedoch v\246llig." , "der Pressesprecher des Wuppertaler SV Borussia nach einem 0:4 gegen die BVB-Amateure" , "Amateurspr\252che" )

joke773 :: Joke
joke773 = ( 773 , "Dieter Hoene\223" , " Wir m\252ssen jetzt erstmal nachdenken. Nachdenken bedeutet, dass man \252ber alles m\246gliche nachdenkt." , "nach Herthas Pokalniederlage bei Werder Bremen" , "Promis & Presse" )

joke774 :: Joke
joke774 = ( 774 , "Franz Beckenbauer" , "Wenn du die Meisterschale \252berreicht bekommst, dann bist du Meister.\187" , "" , "Promis & Presse" )

joke775 :: Joke
joke775 = ( 775 , "Michael Ballack" , "Rituale habe ich nicht. Bis auf die Dinge, die man immer gleich macht." , "in einem Interview vor dem EM-Halbfinale gegen die T\252rkei" , "Trainerspr\252che" )

joke776 :: Joke
joke776 = ( 776 , "Torsten Legat" , "Che Guevara war ein Rebell, ein K\228mpfer f\252r sein Land. Das will ich auch sein. Ich will den Schwachen helfen. Das ist im Fu\223ball genauso, da muss man den schwachen Gegner auch aufbauen. Das ist so eine eigene Logik von mir, dazu will ich gar nicht viel sagen.\187" , "" , "Trainerspr\252che" )

joke777 :: Joke
joke777 = ( 777 , "Klaus Augenthaler" , "Es ist egal, ob man 24 Stunden am Tag arbeitet. Wenn man unter der Woche drei Purzelb\228ume macht und das Spiel gewinnt, hat man alles richtig gemacht.\187" , "" , "Trainerspr\252che" )

joke778 :: Joke
joke778 = ( 778 , "Otto Rehhagel" , "Jeder kann sagen, was ich will. \187" , "" , "Trainerspr\252che" )

joke779 :: Joke
joke779 = ( 779 , "G\252nter Netzer" , "Der Pfosten ist ein Freund des Torwarts, auf den er sich nicht verlassen kann. \n\187" , "" , "Trainerspr\252che" )

joke780 :: Joke
joke780 = ( 780 , "Beni Turnheer" , "Bei der T\252rkey spielt manch einer der in diesem Spiel noch nicht zum Einsatz gekommen ist." , "kommentiert das EM-Halbfinalspiel zwischen Deutschland - T\252rkey" , "Reporterspr\252che" )

joke781 :: Joke
joke781 = ( 781 , "Michael Ballack" , "Der zweite Platz ist doch ein beachtlicher Erfolg, wenn man die Turnierbesetzung bedenkt: Niederlande, Italien, Holland..." , "im Interview nach dem verlorenen EM-Finale gegen Spanien" , "Spielerspr\252che" )

joke782 :: Joke
joke782 = ( 782 , "Michael Maier" , "Steh auf und h\246r auf rumzuflennen, wir sind hier nicht auffer Schmertterlingsfarm." , "Nach offensichlicher Schwalbe von einem Gegenspieler des TuS Riehe 2 beim Spiel der 3. Kreisklasse Schaumburg am ersten Spieltag 2008/2009" , "Amateurspr\252che" )

joke783 :: Joke
joke783 = ( 783 , "Franz Drillinger" , "\"............und Pat, zieht vorne ruhig mal ab, auch wenn der Ball 11 Meter dr\252ber geht, hauptsache er geht aufs Tor!\"" , "Vorbereitung aufs Spiel. Ansprache und recht heitere Stimmung. Vor dieser Aussage bat der Trainer um Konzentration, die war danach nat\252rlich wieder vorbei." , "Trainerspr\252che" )

joke786 :: Joke
joke786 = ( 786 , "Jens Lehmann" , "Wenn der Ball so aufgesprungen w\228re, wie ich gedacht habe, h\228tte ich ihn gehalten, glaube ich." , "\252ber den ersten Gegentreffer beim 0:2 gegen Leverkusen" , "Spielerspr\252che" )

joke787 :: Joke
joke787 = ( 787 , "J\252rgen Klopp" , "Die sieben Punkte, die wir bisher haben, interessieren doch in Dortmund niemanden. Es ist nur wichtig, gegen Schalke zu gewinnen." , "zum gelungenen Saison-Auftakt der Borussia" , "Trainerspr\252che" )

joke788 :: Joke
joke788 = ( 788 , "Christofer Heimeroth" , "Ganz so blind bin ich ja wohl doch nicht." , "Borussia M\246nchengladbachs Torwart, der nach dem Bundesliga-Auftakt in die Kritik geriet" , "Spielerspr\252che" )

joke789 :: Joke
joke789 = ( 789 , "Mladen Krstajic" , "Zum Schluss haben wir 9 gegen 14 gespielt." , "nach dem 3:3 in Dortmund, bei dem Pander und Ernst vom Platz gestellt wurden und er auch das Schiedsrichtergespann gegen sich w\228hnte" , "Spielerspr\252che" )

joke790 :: Joke
joke790 = ( 790 , "Torsten Frings" , "Wenn du richtig mit dem Ellenbogen etwas abbekommst, dann ist es egal, ob du eine Maske hast oder ob der alte Bruch schon zwei oder drei Wochen her ist. Dann ist sie wieder durch." , "\252ber seinen Verzicht auf eine Schutzmaske" , "Spielerspr\252che" )

joke791 :: Joke
joke791 = ( 791 , "J\252rgen Klopp" , "Wenn es hier nach Schwei\223 stinkt, bin ich das. Das Spiel war so aufregend." , "zu seinem Schalker Kollegen Fred Rutten auf der Fahrstuhl-Fahrt zur Pressekonferenz" , "Spielerspr\252che" )

joke793 :: Joke
joke793 = ( 793 , "G\252nter Netzer" , "Er ist konstant nicht schnell genug gewesen." , "\252ber Marcell Jansen" , "Promis & Presse" )

joke794 :: Joke
joke794 = ( 794 , "Franz Beckenbauer" , "Das war heute so ein Tag, an dem man sich beim Nasebohren den Finger bricht." , "nach dem 2:5 gegen Bremen" , "Reporterspr\252che" )

joke795 :: Joke
joke795 = ( 795 , "Mark van Bommel" , "Letztes Jahr 4:0 f\252r uns, heute 2:5. Von der Tordifferenz haben wir gewonnen." , "nach der 2:5-Heimpleite gegen Werder Bremen" , "Spielerspr\252che" )

joke796 :: Joke
joke796 = ( 796 , "Benjamin Huggel" , "Ich denke nicht an morgen, sondern nur an heute. Und heute [Freitag] ist f\252r mich das Spiel gegen YB am Samstag." , "Interview von Peter M. Birrer mit Benjamin Huggel auf www. bazonline.ch am 26.9.2008 ver\246ffentlicht." , "Spielerspr\252che" )

joke797 :: Joke
joke797 = ( 797 , "Rudi V\246ller" , "Bis zum 1:0 haben wir gut gespielt. Leider fiel das Tor schon in der 6. Minute." , "Der Sportdirektor von Bayer Leverkusen, nach dem 2:0-Sieg bei Eintracht Frankfurt" , "Promis & Presse" )

joke798 :: Joke
joke798 = ( 798 , "J\252rgen Klopp" , "Erst 2:1, dann 2:3 - wer sich da nicht \252ber einen Punkt freut, muss geisteskrank sein." , "nach dem 3:3 bei Werder Bremen" , "Trainerspr\252che" )

joke799 :: Joke
joke799 = ( 799 , "Armin Veh" , "Berlin ist immer eine Reise wert. F\252r uns aber leider nur privat." , "nachdem sein Team seit 17 Jahren in Berlin nicht gewinnen konnte" , "Spielerspr\252che" )

joke800 :: Joke
joke800 = ( 800 , "Uli Hoene\223" , "Sie haben gek\228mpft wie die L\246wen - oder wie die Tiger. L\246wen d\252rfen wir ja nicht sagen." , "Bayern-Manager Uli Hoene\223 nach dem 1:0-Sieg der M\252nchner in Karlsruhe. \171L\246wen\187 werden die Spieler des Lokalrivalen TSV 1860 M\252nchen genannt" , "Promis & Presse" )

joke801 :: Joke
joke801 = ( 801 , "Marco Kurz" , "Vor plusminus einer Woche" , "Antwort auf die Frage, wann er den suspendierten Berkant G\246ktan das letzte Mal gesehen habe." , "Trainerspr\252che" )

joke803 :: Joke
joke803 = ( 803 , "Hans Meyer" , "Ich war sehr aufgeregt und musste vorher zweimal mehr pinkeln als sonst. " , "zu seinem Comeback bei Borussia M\246nchengladbach" , "Trainerspr\252che" )

joke804 :: Joke
joke804 = ( 804 , "Hans Meyer" , "Meyer kommt - und alle bleiben weg." , "zum Zuschauer-Minusrekord in dieser Saison von 42 051 Besuchern beim 1:0 gegen den Karlsruher SC" , "Trainerspr\252che" )

joke805 :: Joke
joke805 = ( 805 , "Jonas Kamper" , "Die Jungs sind danach noch mehr gelaufen und haben alles gegeben. Da ich jetzt am Dienstag frei habe, werde ich an der Massagebank aushelfen, um die Jungs wieder fit zu kriegen." , "der beim 0:0 auf Schalke nach einer Stunde mit Gelb-Rot vom Platz gestellt wurde, zu seinen Pl\228nen bis zum Heimspiel gegen Cottbus" , "Spielerspr\252che" )

joke806 :: Joke
joke806 = ( 806 , "Bastian Schweinsteiger" , "Der Pausentee war lecker. Wir haben eine Packung Herztropfen genommen und dann lief es besser." , "zur Leistungssteigerung des FC Bayern in der zweiten Spielh\228lfte beim 4:2-Sieg gegen Wolgsburg" , "Spielerspr\252che" )

joke807 :: Joke
joke807 = ( 807 , "Uli Hoene\223" , "In der Halbzeit habe ich gedacht, wir werden h\246chstens Hausmeister." , "nach dem Sieg gegen Wolfsburg auf die Frage, was er nach dem Pausenr\252ckstand \252ber seine Prognose vom Gewinn der Herbstmeisterschaft gedacht habe" , "Promis & Presse" )

joke808 :: Joke
joke808 = ( 808 , "Uli Hoene\223" , "Es war schwer genug gegen einen Gegner, der mit 17 Mann hinten drin gestanden hat." , "zur Defensivtaktik von Arminia Bielefeld" , "Promis & Presse" )

joke809 :: Joke
joke809 = ( 809 , "Lukas Podolski" , "Ich habe mir den Gegner Liechtenstein nicht ausgesucht. Ich werde Jogi L\246w jetzt bitten, mich nur gegen die Top 20 der Welt einzusetzen." , "\252ber die Kritik, dass er die Nationalelf auch deshalb besser spiele, weil die Gegner nicht die besten seien" , "Spielerspr\252che" )

joke810 :: Joke
joke810 = ( 810 , "Felix Magath" , "Endlich bin ich mal zufrieden. Es war das erste Mal, dass wir unterlegen waren und verloren haben. Deswegen muss ich mich nicht gr\228men." , "nach dem 0:2 bei Bayer Leverkusen" , "Trainerspr\252che" )

joke811 :: Joke
joke811 = ( 811 , "Lukas Podolski" , "Wir sind zufrieden mit den drei Punkten." , "im Interview nach dem 1:1 in Florenz" , "Spielerspr\252che" )

joke812 :: Joke
joke812 = ( 812 , "Wilfried Sanou" , "Ich habe 15 Jahre Gymnastik in Burkina Faso gemacht." , "K\246lns Torsch\252tze \252ber seinen Flickflack-Jubel nach dem 2:1-Siegtor gegen Hannover 96" , "Spielerspr\252che" )

joke813 :: Joke
joke813 = ( 813 , "Hans Meyer" , "Er hat mir gesagt, Herr Meyer - und er hat wirklich Herr Meyer gesagt - ich fliege heute nicht runter. Das verspreche ich ihnen." , "\252ber sein Gespr\228ch mit dem rot- gef\228hrdeten Marko Marin an der Seitenlinie w\228hrend der Partie in Bielefeld" , "Trainerspr\252che" )

joke814 :: Joke
joke814 = ( 814 , "Michael Frontzeck" , "Macht doch mal das Fenster auf, dann k\246nnen wir h\246ren, wie einige Fans da drau\223en meinen Kopf fordern." , "Bitte des Bielefelder Trainers an die Journalisten w\228hrend der Pressekonferenz" , "Trainerspr\252che" )

joke815 :: Joke
joke815 = ( 815 , "Michael Frontzeck" , "Hab ich etwa jetzt schon Halos?" , "nachdem keine \171Trainer-Raus\187-Rufe durch die ge\246ffneten Fenster zu h\246ren waren" , "Trainerspr\252che" )

joke816 :: Joke
joke816 = ( 816 , "Christian Eichner" , "Wenn er jede Woche ein Tor macht und eins vorbereitet, muss er von mir aus auch nicht 90 Minuten rauf und runter rennen. Dann laufe ich f\252r ihn, bis ich wund bin." , "Karlsruhes Verteidiger zur Leistung von Regisseur Antonio da Silva beim 3:3 gegen Leverkusen" , "Spielerspr\252che" )

joke817 :: Joke
joke817 = ( 817 , "Karl-Heinz Rummenigge" , "1899 Hoffenheim...da fragt man sich doch: Wo haben die sich \252ber 100 Jahre lang versteckt?" , "auf der Jahreshauptversammlung des FC Bayern M\252nchen" , "Spielerspr\252che" )

joke818 :: Joke
joke818 = ( 818 , "Franz Beckenbauer" , "Wir sind damals zusammen Weltmeister geworden, 1919." , "\252ber J\252rgen Klinsmann" , "Promis & Presse" )

joke819 :: Joke
joke819 = ( 819 , "Franz Beckenbauer" , "Es war nicht die Hand Gottes, sondern die Watschn eines Sechzigers." , "\252ber die Ursache f\252r seinen Wechsel zu den Bayern" , "Promis & Presse" )

joke820 :: Joke
joke820 = ( 820 , "Franz Beckenbauer" , "Da staunst Du, Mark. Das sind so viele wie Einwohner in Holland." , "zu Kapit\228n Mark van Bommel \252ber die Anzahl der Fanklub-Mitglieder des FC Bayern" , "Promis & Presse" )

joke821 :: Joke
joke821 = ( 821 , "Karl-Heinz Rummenigge" , "Wissen Sie, wann 1860 M\252nchen hier erstmals urkundlich erw\228hnt wurde? Im Alten Testament. Da stand n\228mlich: Sie trugen seltsame Gew\228nder und irrten planlos umher." , "\252ber den M\252nchner Lokalrivalen" , "Promis & Presse" )

joke823 :: Joke
joke823 = ( 823 , "Volkan Demirayak" , "Steck die H\228nde aus der Tasche!" , "Zu einem Mitspieler beim Spiel 5 gegen 2 im Training" , "Amateurspr\252che" )

joke824 :: Joke
joke824 = ( 824 , "Max Merkel" , "Lieber 9 Minuten Maradona beim Autowaschen zuschauen, als 90 Minuten Hansi Pfl\252gler beim Fu\223ball!\187" , "" , "Reporterspr\252che" )

joke825 :: Joke
joke825 = ( 825 , "Michael Ballack" , "Keiner verliert ungerne!" , "Zu Besuch bei WETTEN DASS..?" , "Spielerspr\252che" )

joke826 :: Joke
joke826 = ( 826 , "Florian Fromlowitz" , "Ich h\228tt vor \196rger \252berall reinbei\223en k\246nnen...- besonders in den Schiedsrichter!" , "Nach einem kassierten Ausgleich in letzter Minute" , "Spielerspr\252che" )

joke827 :: Joke
joke827 = ( 827 , "Marcel Koller" , "Wenn man sich nicht mehr aufregen darf und wie ein Klostersch\252ler da sitzen muss, dann ist es besser, man h\246rt auf." , "der wegen Reklamierens vom Schiedsrichter auf die Trib\252ne geschickt wurde" , "Trainerspr\252che" )

joke828 :: Joke
joke828 = ( 828 , "Ralf Rangnick" , "Inoffizielle Titel sind sowieso nicht so interessant." , "auf die Frage, ob die Herbstmeisterschaft ein Thema sei" , "Trainerspr\252che" )

joke829 :: Joke
joke829 = ( 829 , "Ralf Rangnick" , "Der Freisto\223 war kreuzkrabbenunn\246tig." , "\252ber die Szene, die zum 2:2 f\252r Wolfsburg f\252hrte" , "Trainerspr\252che" )

joke830 :: Joke
joke830 = ( 830 , "Hans Meyer" , "J\252rgen Klinsmann wird es verkraften. Uns hilft der eine Punkt in unserer Lage mehr als er den Bayern im Kampf um die Meisterschaft schadet." , "\252ber die Folgen des Unentschiedens gegen die Bayern" , "Trainerspr\252che" )

joke831 :: Joke
joke831 = ( 831 , "Arne Friedrich" , "Je weiter es in der Tabelle nach oben geht, desto ausgefallener werden die T\228nze." , "zu den Feierlichkeiten im Berliner Olympiastadion nach dem 2:1-Erfolg \252ber den Hamburger SV" , "Spielerspr\252che" )

joke832 :: Joke
joke832 = ( 832 , "Udo Lattek" , "In Holland w\252rde ich vielleicht einen Wohnwagen kaufen, aber keinen Fu\223ballspieler!" , "beim DSF-Stammtisch \252ber die Schalker Einkaufspolitik" , "Promis & Presse" )

joke833 :: Joke
joke833 = ( 833 , "Hans-Hermann Schwick" , "Immer wenn Schnee f\228llt, muss ein Favorit dran glauben. Ich habe mir schon \252berlegt, vor unserem n\228chsten Heimspiel Schneekanonen aus dem Sauerland aufzustellen." , "Bielefelds Pr\228sident am Samstag nach dem 2:1 \252ber den bisherigen Tabellenf\252hrer Bayer Leverkusen" , "Promis & Presse" )

joke834 :: Joke
joke834 = ( 834 , "Bruno Labbadia" , "Die sollen von der Bank nicht immer so tun, als ob jemand get\246tet wurde." , "\252ber die Reaktion der Bielefelder Bank nach dem mit einem Platzverweis bestraften Foul von Sascha Dum an R\252diger Kauf" , "Trainerspr\252che" )

joke835 :: Joke
joke835 = ( 835 , "Markus Miller" , "Vielleicht m\252ssen wir in eine Sekte eintreten oder UFOs beschw\246ren." , "\252ber die Misere beim Karlsruher SC" , "Spielerspr\252che" )

joke836 :: Joke
joke836 = ( 836 , "Fred Rutten" , "Es macht mir Spa\223, mir das Leben schwer zu machen. Deswegen bin ich Fu\223ball-Lehrer geworden." , "als er auf seine Erkl\228rung f\252r die Leistung von Kevin Kuranyi angesprochen wurde" , "Trainerspr\252che" )

joke837 :: Joke
joke837 = ( 837 , "Francisco Copado" , "Bayern kann ruhig Herbstmeister werden - wenn wir Meister werden." , "nach dem 3:0 gegen Arminia Bielefeld" , "Spielerspr\252che" )

joke838 :: Joke
joke838 = ( 838 , "Hans Meyer" , "Ich habe einen Zahnarzttermin, bekomme ein neues Gebiss. Ich werde nicht beim Training sein. Schreibt deshalb nicht, ich w\228re gefeuert." , "zu Journalisten nach dem 1:3 gegen Cottbus" , "Trainerspr\252che" )

joke839 :: Joke
joke839 = ( 839 , "Hans Meyer" , "Fr\252her habt ihr solche Schei\223spiele nicht so einfach hingenommen. Ihr seid gleichg\252ltiger geworden." , "zu Journalisten nach dem 1:3 gegen Cottbus" , "Trainerspr\252che" )

joke840 :: Joke
joke840 = ( 840 , "Uli Hoene\223" , "Andy, lass Dich nicht unterkriegen. Schon gar nicht von einer Zeitung, von der die H\228lfte der Redakteure mit Rudi Assauer im Bett liegt." , "im DSF-Doppelpass zu Schalke Manager Andreas M\252ller" , "Promis & Presse" )

joke841 :: Joke
joke841 = ( 841 , "Miroslav Klose" , "Es hat sch\246n Aua gemacht. Ich habe ihn schon beschimpft." , "nachdem er bei einer Abwehraktion von Torh\252ter Michael Rensing angesprungen wurde" , "Spielerspr\252che" )

joke842 :: Joke
joke842 = ( 842 , "Ralf Rangnick" , "Vor zwei Jahren haben wir im Gr\252nwalder Stadion gegen die Bayern-Amateure deutlich verdienter verloren als heute." , "nach der Niederlage gegen den FC Bayern M\252nchen" , "Trainerspr\252che" )

joke843 :: Joke
joke843 = ( 843 , "Cristiano Ronaldo" , "Wenn ich nicht Fu\223baller geworden w\228re, w\228re ich Superman geworden.\187" , "" , "Spielerspr\252che" )

joke844 :: Joke
joke844 = ( 844 , "Stefan Effenberg" , "Und ich w\228re, wenn ich nicht Fu\223baller geworden w\228re, Batman geworden." , "auf die \196u\223erung von Ronaldo, dass er Superman geworden w\228re, wenn er nicht Fu\223baller geworden w\228re" , "Reporterspr\252che" )

joke845 :: Joke
joke845 = ( 845 , "Franzi" , "Dann nehme ich Sonntag - 15.00 Uhr!" , "Meine Freundin wollte Karten fuer ein Bundesligaspiel erwerben. Auf die Aussage der Verkaeuferin, dass das Spiel noch nicht terminiert ist und es am Freitag, Samstag oder Sonntag stattfinden koennte kam obige Antwort! " , "Amateurspr\252che" )

joke846 :: Joke
joke846 = ( 846 , "Udo Nielsen" , "Bei dieser Spielweise kriege ich spontan Augenkrebs" , "17. Okt. 2004 : Punktspiel: Post SV Buxtehude - TuS Oldendorf III 2:16 im Jahnstadion Buxtehude ( 4. Kreisklasse Stade, also letzte Klasse \252berhaupt ), Nachden zwischenzeitlichen 1:10 noch vor der Halbzeit" , "Amateurspr\252che" )

joke847 :: Joke
joke847 = ( 847 , "Boubacar Sanogo" , "Ich war \252berrascht, die Jungs laufen 90 Minuten nonstop, das habe ich noch nicht erlebt. Ich verstehe jetzt, warum sie Erster sind." , "Hoffenheims neuer St\252rmer nach seiner Premiere im Trikot des Tabellenf\252hrers \252ber die Eigenschaften seines neuen Teams" , "Spielerspr\252che" )

joke848 :: Joke
joke848 = ( 848 , "Hans Meyer" , "Der alte Fuchs Meyer hat einen Fehler gemacht, indem er nach dem R\252ckstand mehr Offensivkr\228fte gebracht hat." , "zu seinen Einwechslungen nach dem 0:1-R\252ckstand" , "Trainerspr\252che" )

joke849 :: Joke
joke849 = ( 849 , "Hans Meyer" , "Wenn ich vor f\252nf Wochen gesagt habe, dass uns die Neuverpflichtungen helfen k\246nnen, werde ich jetzt nicht sagen, ich h\228tte Mist gekauft." , "Gladbachs Trainer zu seiner Einkaufspolitik" , "Trainerspr\252che" )

joke850 :: Joke
joke850 = ( 850 , "Christoph Daum" , "Jeder hat gesehen, wie Ishiaku im Treibsand versinkt." , "zum 1:1-Ausgleich des Wolfsburger Brasilianers" , "Trainerspr\252che" )

joke851 :: Joke
joke851 = ( 851 , "Thomas Hitzlsperger" , "Ich w\228re auch mit 80 Stundenkilometern zufrieden gewesen." , "zu seinem Tor beim 4:2 gegen Bayer Leverkusen, bei dem eine Schussgeschwindigkeit von 125 km/h gemessen wurde" , "Spielerspr\252che" )

joke852 :: Joke
joke852 = ( 852 , "Sebastian Freis" , "Ich habe noch eine Platzwunde am Kopf. So frei stand ich dann wohl doch nicht." , "auf die Frage, warum er unbedr\228ngt das 3:2-Siegtor gegen den Hamburger SV erzielen konnte" , "Spielerspr\252che" )

joke853 :: Joke
joke853 = ( 853 , "Martin Jol" , "Willkommen in der Bundesliga." , "an die Adresse von Neuzugang Michael Gravgaard, der bei seinem Deb\252t zwei Gegentreffer verschuldete" , "Trainerspr\252che" )

joke854 :: Joke
joke854 = ( 854 , "Hans Meyer" , "Wenn diese tapferen Karlsruher in allerletzter Minute gewinnen und Michael Frontzeck mit Arminia Bielefeld schon vier Punkte in der R\252ckrunde geholt hat, dann tr\228gt das sicherlich nicht dazu bei, dass wir in der Kabine singen." , "zur Situation im Abstiegskampf" , "Trainerspr\252che" )

joke855 :: Joke
joke855 = ( 855 , "Klaus Allofs" , "Ich wage es gar nicht mehr, auf die Tabelle zu schauen." , "nach dem 0:1 bei Schalke 04" , "Promis & Presse" )

joke856 :: Joke
joke856 = ( 856 , "J\252rgen Klopp" , "Keiner wird glauben, dass wir Hoffenheim jetzt einen Kringel in den Hals spielen und 5:0 nach Hause schicken." , "Der BVB-Coach zum n\228chsten Heimspiel" , "Trainerspr\252che" )

joke857 :: Joke
joke857 = ( 857 , "Friedhelm Funkel" , "Das kostet viel Geld. Da kann ich mir im Sommer wieder einen Spieler weniger leisten." , "zu Ausschreitungen Frankfurter Fans beim 1:0-Sieg in Karlsruhe" , "Trainerspr\252che" )

joke858 :: Joke
joke858 = ( 858 , "Hans Meyer" , "Ich glaube, dass wir mit vier Punkten R\252ckstand allen Grund haben aufzugeben." , "nach dem 1:2 in Berlin auf die Frage, ob seine junge Mannschaft im Abstiegskampf bestehen k\246nne" , "Trainerspr\252che" )

joke859 :: Joke
joke859 = ( 859 , "Neven Subotic" , "Er muss ja nicht unbedingt dahin laufen, wo ich hingr\228tsche." , "nach seinem vieldiskutierten Foul an Hoffenheims St\252rmer Demba Ba" , "Spielerspr\252che" )

joke860 :: Joke
joke860 = ( 860 , "Heribert Bruchhagen" , "So etwas hatten wir immer wieder in den letzten 15 Jahren seit Dietrich Weise hier Trainer war." , "Frankfurts Vorstandsvorsitzender zum Pfeifkonzert gegen Trainer Friedhelm Funkel nach der Auswechslung von Caio" , "Promis & Presse" )

joke861 :: Joke
joke861 = ( 861 , "Christoph Daum" , "Gegen Bielefeld musst du das eine oder andere Tor auch mal reinl\252gen." , "\252ber ein seiner Meinung nach probates Mittel, das beim 1:1 gegen die Ostwestfalen aber nicht zum Erfolg f\252hrte" , "Trainerspr\252che" )

joke862 :: Joke
joke862 = ( 862 , "Mark van Bommel" , "Ein Vogel macht noch keinen Fr\252hling" , "nach dem Champions-League-Spiel bei Sporting Lissabon. 5:0 f\252r den FC Bayern" , "Spielerspr\252che" )

joke863 :: Joke
joke863 = ( 863 , "Filip Daems" , "Ich habe nicht zweimal gepinkelt." , "zu einer angeblich nicht ordnungsgem\228\223en Dopingprobe" , "Spielerspr\252che" )

joke864 :: Joke
joke864 = ( 864 , "Landon Donovan" , "Vielleicht einen Drei-Jahres-Vertrag?" , "auf die Frage, ob er nach seinem letzten Einsatz f\252r den FC Bayern ein Abschiedsgeschenk erhalte" , "Spielerspr\252che" )

joke865 :: Joke
joke865 = ( 865 , "Tobias Levels" , "Das war eine Hormonaussch\252ttung wie ich sie zuletzt mit f\252nf Jahren im Phantasialand hatte." , "nach seinem ersten Bundesligator" , "Spielerspr\252che" )

joke866 :: Joke
joke866 = ( 866 , "Jermaine Jones" , "Alle, die immer erz\228hlen, sie h\228tten ein Herz f\252r Schalke, sollten jetzt mal einen Gang zur\252ckschalten und Trainer, Manager und Mannschaft in Ruhe lassen." , "nach dem 1:0 gegen den 1. FC K\246ln" , "Spielerspr\252che" )

joke867 :: Joke
joke867 = ( 867 , "Karl-Heinz Rummenigge" , "Wir sind alle angenehm \252berrascht, dass unsere Frauen so erfolgreich sind. Die Mannschaft ist sehr jung und im Gegensatz zu den Herren auch noch g\252nstig." , "Bayerns Vorstandsvorsitzender zum Frauen-Team der M\252nchner, das aussichtsreich um die deutsche Meisterschaft spielt" , "Promis & Presse" )

joke868 :: Joke
joke868 = ( 868 , "Rolf Dohmen" , "Wenn man so ein richtungweisendes Spiel verliert und jetzt noch tiefer in der Schei\223e h\228ngt, kann man erstmal keinen klaren Gedanken fassen." , "KSC-Manager Rolf Dohmen nach dem 0:1 gegen Bielefeld" , "Promis & Presse" )

joke869 :: Joke
joke869 = ( 869 , "Hans Meyer" , "Ich habe mich entschuldigt, dass ich ihn ausgewechselt habe." , "Gladbachs Trainer zur Auswechslung von Alexander Baumjohann beim 4:2-Sieg in K\246ln" , "Trainerspr\252che" )

joke870 :: Joke
joke870 = ( 870 , "Rolf Dohmen" , "Wir stecken den Kopf solange aus dem Sand raus, bis kein Sand mehr da ist." , "nach der 0:1-Niederlage bei Bayern M\252nchen auf die Frage, ob man im Abstiegskampf den Kopf schon in den Sand stecke" , "Promis & Presse" )

joke871 :: Joke
joke871 = ( 871 , "Mark van Bommel" , "Ich habe keine Erkl\228rung, aber ich bin froh, dass wir gewonnen haben. Manchmal ist das eine Ausrede, um das nicht erkl\228ren zu m\252ssen." , "nach dem 1:0 gegen den Karlsruher SC" , "Spielerspr\252che" )

joke872 :: Joke
joke872 = ( 872 , "Markus Pr\246ll" , "Ich kann ja nicht dastehen wie eine Jungfrau." , "Frankfurts Torh\252ter \252ber den 1:1-Ausgleich durch den Leverkusener Michal Kadlec, bei dem er keine gl\252ckliche Figur machte" , "Spielerspr\252che" )

joke873 :: Joke
joke873 = ( 873 , "Mario Eggimann" , "Wir gehen heute erhobenen Hauptes nach Hause - auch wenn das Haupt schmerzt." , "nach dem 2:2 bei 1899 Hoffenheim. Der Schweizer hatte mit einem Stirnverband wegen einer Platzwunde das 2:1 f\252r die G\228ste gek\246pft" , "Spielerspr\252che" )

joke874 :: Joke
joke874 = ( 874 , "Hans Meyer" , "Das darf ich eigentlich gar nicht sagen. Jetzt bekomme ich sicher Morddrohungen. Aber ich h\228tte lieber in K\246ln verloren und heute gegen Bochum gewonnen." , "nach dem 0:1 gegen Bochum" , "Trainerspr\252che" )

joke875 :: Joke
joke875 = ( 875 , "Felix Magath" , "Ich w\228re mit dem FC Bayern nicht sicher gewesen, ob der Vorsprung f\252r die Meisterschaft reicht. Und ich bin auch mit dem VfL Wolfsburg nicht ganz sicher, ob das reicht, um einen UEFA-Cup-Platz zu schaffen." , "nach dem 5:1-Sieg im Top-Spiel \252ber Bayern M\252nchen und dem Sprung an die Bundesliga-Tabellenspitze" , "Trainerspr\252che" )

joke876 :: Joke
joke876 = ( 876 , "Markus Babbel" , "Jens Lehmann wollte die Mannschaft mit dieser Aktion wachr\252tteln." , "Stuttgarts Trainer Markus Babbel zum Fehler von Lehmann beim Gegentor in Bochum" , "Trainerspr\252che" )

joke877 :: Joke
joke877 = ( 877 , "HSV-Fans" , "1887 - Uns trennen mehr als 12 Jahre." , "Spruchband der HSV-Anh\228nger mit Verweis auf das Gr\252ndungsdatum von 1899 Hoffenheim" , "Amateurspr\252che" )

joke878 :: Joke
joke878 = ( 878 , "Uwe Wolf" , "Ich habe Thorsten Fink nicht beleidigt. Ich habe ihm nur gesagt, dass er die Fresse halten und auf seinen Platz zur\252ckgehen soll." , "Uwe Wolf zu seiner Verbannung auf die Trib\252ne, beim 3:2 Ausw\228rtssieg in Ingolstadt." , "Trainerspr\252che" )

joke880 :: Joke
joke880 = ( 880 , "J\252rgen Klinsmann" , "J\246rgs Mitspieler haben nicht daf\252r gesorgt, ein bisschen Revanche gezeigt zu haben. Vor 15, 20 Jahren h\228tte ich ihn mir selber genommen und \252ber die Au\223enlinie gedroschen." , "\252ber ein Foul von Thierry Henry an J\246rg Butt in der Champions League gegen Barcelona" , "Trainerspr\252che" )

joke881 :: Joke
joke881 = ( 881 , "Mark van Bommel" , "Ich wollte gegen Barcelona treten. Aber manchmal kommt man einfach nicht ran an den Mann und der Gegner ist viel besser, da kann man machen, was man will.\187" , "" , "Spielerspr\252che" )

joke882 :: Joke
joke882 = ( 882 , "Uli Hoene\223" , "Beim Franz wei\223 man nie so genau, wie er das meint." , "zur Aussage von Franz Beckenbauer, wonach die Meisterschaft f\252r die M\252nchner kein Muss ist" , "Promis & Presse" )

joke883 :: Joke
joke883 = ( 883 , "Sebastian Freis" , "Die Minutenz\228hlerei geht einem auf den Keks." , "nachdem er nach 752 Minuten die Torflaute des Karlsruher SC beendete" , "Spielerspr\252che" )

joke884 :: Joke
joke884 = ( 884 , "Ralf Rangnick" , "Wir haben keine Krise." , "nach dem zehnten Spiel ohne Sieg in Serie" , "Trainerspr\252che" )

joke885 :: Joke
joke885 = ( 885 , "Christoph Daum" , "Und wenn wir von den letzten sechs Spielen drei gewinnen, dann haben wir hier Karneval." , "nach dem achten sieglosen Heimspiel in Serie" , "Trainerspr\252che" )

joke886 :: Joke
joke886 = ( 886 , "Christian Eichner" , "Solange nicht der letzte Nagel in den Sarg geschlagen ist, liegen wir nicht im Grab." , "Interimskapit\228n Christian Eichner vom Tabellenletzten Karlsruher SC" , "Spielerspr\252che" )

joke887 :: Joke
joke887 = ( 887 , "Christian Eichner" , "Wie die Eichh\246rnchen. Am 34. Spieltag wissen wir, ob es verhungert ist oder nicht." , "auf die Frage, wie der KSC im Saison-Endspurt noch Punkte sammeln will" , "Spielerspr\252che" )

joke888 :: Joke
joke888 = ( 888 , "Beni Thurnheer" , "H\228tte er Tiefstrasser geheissen, w\228re es wohl noch gef\228hrlich geworden!" , "Nach einem zu hohen Freistoss von Xavier Hochstrasser beim schweizer Cup-Halbfinal zwischen BSC YB und dem FC Basel" , "Reporterspr\252che" )

joke889 :: Joke
joke889 = ( 889 , "J\246rg Stiel" , "Ich schmunzele w\228hrend des Spiels immer, denn Fu\223ball macht Spa\223.\n" , "(Gladbachs Torh\252ter Stiel nach Ansicht der TV-Bilder seines elfmeterreifen Fouls an Klose)" , "Spielerspr\252che" )

joke890 :: Joke
joke890 = ( 890 , "NDR 2 Radio Konferenz " , "Mike Hanke mit dem1:0 f\252r Hannover! Erziehlt per Fuss und nicht per Kopf. Und vor allem nicht Per Mertesacker.\187" , "" , "Reporterspr\252che" )

joke891 :: Joke
joke891 = ( 891 , "Giovanni Trappatoni" , "Man darf bei Fu\223ball nicht denken wie Beamter - 0:0 halten bis Feierabend!\187" , "" , "Trainerspr\252che" )

joke892 :: Joke
joke892 = ( 892 , "Heribert Fa\223bender" , "Die Polen darf man nicht untersch\228tzen. Diese Balkan-Kicker sind unberechenbar!\187" , "" , "Reporterspr\252che" )

joke893 :: Joke
joke893 = ( 893 , "Heribert Fa\223bender" , "Die Saudis sind \252brigens Asienmeister, obwohl das ebensowenig Asiaten sind wie die T\252rken Europ\228er. Die Saudis haben ja gar keine Mandelaugen, wie man das von Asiaten erwartet. Das sind eher Araber statt Asiaten.\187" , "" , "Reporterspr\252che" )

joke894 :: Joke
joke894 = ( 894 , "Heribert Fa\223bender" , "Fu\223ball ist inzwischen Nr.1 in Frankreich. Handball \252brigens auch. \187" , "" , "Reporterspr\252che" )

joke895 :: Joke
joke895 = ( 895 , "Kiyoshi Inoue" , "Auch ein paar Schwarze spielen f\252r Deutschland. Auch Deutschland hatte ja viele Kolonien in Afrika.\n\n" , "japanischer Fu\223ball-Kommentator w\228hrend der WM 06" , "Spielerspr\252che" )

joke897 :: Joke
joke897 = ( 897 , "Franz Beckenbauer" , "Barcelona spielt, mehr oder weniger, auf ein Tor!" , "28.04.2009    Premiere Halbzeitanalyse des Spiels \"FC Barcelona vs. FC Chelsea London\"" , "Promis & Presse" )

joke898 :: Joke
joke898 = ( 898 , "Dani Wyler" , "Jetzt kommt er hier in die Patrouille!" , "kommentiert das Champions-League-Spiel zwischen Manchester United und Arsenal vom 29.04.09 (Er meinte wohl Bedruille)" , "Reporterspr\252che" )

joke899 :: Joke
joke899 = ( 899 , "Daniel Ryser" , "Jetzt hab ich es schon zum 10. Mal gesagt, da sag ich es sicher nicht noch ein 2. Mal!" , "Der Junioren-B-Trainer des FC W\252" , "Amateurspr\252che" )

joke900 :: Joke
joke900 = ( 900 , "Bastian Schweinsteiger" , "Bayern M\252nchen ist gleich Meisterschaft." , "nach dem 2:1 gegen Gladbach" , "Spielerspr\252che" )

joke901 :: Joke
joke901 = ( 901 , "Giovanni Federico" , "Ich h\228tte das Loch gar nicht tief genug buddeln k\246nnen, um darin zu versinken." , "nach seiner vergebenen Riesenchance beim 0:0 gegen Energie Cottbus" , "Spielerspr\252che" )

joke902 :: Joke
joke902 = ( 902 , "Rolf Dohmen" , "Ein Punkt ist wie leere H\228nde." , "KSC-Manager Rolf Dohmen" , "Promis & Presse" )

joke903 :: Joke
joke903 = ( 903 , "Jupp Heynckes" , "Hermann ist lebendig. Wir kennen uns ewig schon. Heute habe ich an der Wade noch den Stollenabdruck, als er gegen mich gespielt hat." , "\252ber seinen Assistenten Hermann Gerland" , "Trainerspr\252che" )

joke904 :: Joke
joke904 = ( 904 , "Hans Meyer" , "Ich k\246nnte mir gut vorstellen, dass ich gegen J\252rgen Klinsmann heute auch verloren h\228tte." , "nach der Niederlage in M\252nchen" , "Trainerspr\252che" )

joke905 :: Joke
joke905 = ( 905 , "Markus Babbel" , "Das ist ein Motto von mir: Wenn ich ein Spiel nicht gewinnen kann, muss ich es nicht verlieren." , "zum hart erk\228mpften 2:2 seiner Mannschaft in Bielefeld" , "Trainerspr\252che" )

joke906 :: Joke
joke906 = ( 906 , "Beni Thurnheer" , "Der Ball geht dr\252ber, die H\246he jedoch war optimal!" , "kommentiert einen Freistoss von Dani Alves beim Champions League-Halbfinalspiel zwischen Barcelona und Chelsea vom 6.05.2009" , "Reporterspr\252che" )

joke907 :: Joke
joke907 = ( 907 , "Felix Magath" , "F\252r uns wird es jetzt schwerer. Mit einem Sieg w\228re es leichter geworden." , "\252ber die Auswirkungen der 1:4-Niederlage seines VfL Wolfsburg beim VfB Stuttgart f\252r den Titelkampf" , "Trainerspr\252che" )

joke908 :: Joke
joke908 = ( 908 , "Mario Gomez" , "Ich bin der Letzte, der sagt, Jungs, ihr habt es nicht erreicht, jetzt verpiss ich mich." , "zur Frage, ob er den VfB verlassen werde, falls dieser die UEFA-Pokal-Teilnahme verpasse" , "Spielerspr\252che" )

joke909 :: Joke
joke909 = ( 909 , "Jens Lehmann" , "Das ist ein tolles Gef\252hl f\252r einen Spieler. Mir war das leider nie verg\246nnt." , "zum Gomez-Viererpack" , "Spielerspr\252che" )

joke910 :: Joke
joke910 = ( 910 , "Zvjezdan Misimovic" , "Magath stand doch nicht auf dem Platz." , "auf die Frage, ob das Wechsel-Theater um Trainer Felix Magath Grund f\252r die 1:4-Pleite in Stuttgart war" , "Spielerspr\252che" )

joke911 :: Joke
joke911 = ( 911 , "J\252rgen Klopp" , "Es gibt keinen guten Zeitpunkt, um gegen eine Mannschaft wie den VfL Wolfsburg zu spielen. Aber es ist kein guter Zeitpunkt, gegen uns zu spielen." , "mit Blick auf das Spiel seiner zuletzt sieben Mal siegreichen Mannschaft am Dienstag beim Spitzenreiter" , "Trainerspr\252che" )

joke912 :: Joke
joke912 = ( 912 , "Ralf Rangnick" , "Der einzige Fehler, den wir in der ersten Halbzeit gemacht haben, war, dass wir nichts gemacht haben." , "\252ber die Leistung seiner Mannschaft in der ersten Halbzeit beim 2:0-Sieg gegen den 1. FC K\246ln" , "Trainerspr\252che" )

joke913 :: Joke
joke913 = ( 913 , "Bojan Prasnikar" , "Wenn es in Cottbus nochmals eine \220berraschung f\252r Bayern gegeben h\228tte, w\228re es ja keine \220berraschung mehr." , "Energie-Trainer Bojan Prasnikar, dessen Team nach dem 2:0-Sieg im Vorjahr dieses Mal gegen die M\252nchner mit 1:3 unterlag" , "Trainerspr\252che" )

joke914 :: Joke
joke914 = ( 914 , "Rudi Assauer" , "Ich kann ihn doch nicht kleiner machen und ihm die Beine abs\228gen." , "\252ber die vergebenen Chancen des schlaksigen St\252rmers Victor Agali bei der 0:2-Heimniederlage gegen Bochum" , "Promis & Presse" )

joke915 :: Joke
joke915 = ( 915 , "Bernd Hoffmann" , "Vielleicht kann ich besser Zwillinge zeugen als Trainer entlassen." , "zu den Begleitumst\228nden der Trennung von Coach Kurt Jara. Die Ehefrau des HSV-Vorstandsvorsitzenden erwartet zum zweiten Mal Zwillinge" , "Promis & Presse" )

joke916 :: Joke
joke916 = ( 916 , "Michael Hofmann" , "Er ist ein Typ wie Beckenbauer. Der kann irgendeiner Frau ein Kind machen - und es wird ihm in der \214ffentlichkeit verziehen." , "\252ber DFB-Teamchef Rudi V\246ller" , "Spielerspr\252che" )

joke917 :: Joke
joke917 = ( 917 , "Reiner Calmund" , "Wir haben zwar das Spiel kontrolliert, aber das war kontrollierter K\228se!" , "zur Vorstellung seiner Mannschaft beim 0:0 in K\246ln" , "Promis & Presse" )

joke918 :: Joke
joke918 = ( 918 , "Uli Hoene\223" , "Die Nummer 9 ist immer etwas ganz Besonderes. Das sind die spektakul\228ren Spieler, die auch die Zuschauer ins Stadion ziehen." , "anl\228\223lich der offiziellen Pr\228sentation des neuen St\252rmerstars Roy Makaay - der Niederl\228nder tr\228gt die 10" , "Promis & Presse" )

joke919 :: Joke
joke919 = ( 919 , "Peter Neururer" , "Er hat nicht verstanden, wie er sich verhalten sollte. Es lag wohl an der sprachlichen Barriere, aber wir konnten auf dem Platz keinen Sprachkurs mehr abhalten." , "zur besonderen Ma\223nahme, den in der 73. Minute eingewechselten Brasilianer Edu neun Minuten sp\228ter wieder rauszunehmen" , "Trainerspr\252che" )

joke920 :: Joke
joke920 = ( 920 , "Uli Hoene\223" , "Insgeheim wird Felix Magath schon ein Glas Wein auf die Meisterschaft getrunken haben." , "nach dem 2:2 in Hoffenheim" , "Promis & Presse" )

joke921 :: Joke
joke921 = ( 921 , "Rudi V\246ller" , "Als die Fans Ausw\228rtsspiel riefen, h\228tte man mitsingen m\246gen." , "nach dem 5:0 und ersten Sieg im D\252sseldorfer Exil gegen Borussia M\246nchengladbach, bei dem 25 000 G\228ste-Fans unter den 43 000 Zuschauern waren" , "Promis & Presse" )

joke922 :: Joke
joke922 = ( 922 , "Martin Jol" , "Man kann nicht f\252r 42 Millionen Euro verkaufen und dann hoffen, dass man Meister wird." , "nach der 0:1-Heimpleite gegen K\246ln" , "Trainerspr\252che" )

joke923 :: Joke
joke923 = ( 923 , "Uli Hoene\223" , "Ich habe in Latein gelernt: \"Si tacuisses philosophus manisses.\" Wenn du geschwiegen h\228ttest, w\228rst du Philosoph geblieben." , "zum Fernsehauftritt von Ex- Trainer J\252rgen Klinsmann" , "Promis & Presse" )

joke924 :: Joke
joke924 = ( 924 , "Lukas Podolski" , "Ich komme noch mal in die Champions League, keine Angst." , "nach seinem letzten Spiel f\252r Bayern M\252nchen" , "Spielerspr\252che" )

joke925 :: Joke
joke925 = ( 925 , "Bastian Schweinsteiger" , "Wenn du beim FC Bayern einen Vertrag unterschreibst, steht drin, dass du deutscher Meister werden musst.\187" , "" , "Spielerspr\252che" )

joke926 :: Joke
joke926 = ( 926 , "Jupp Heynckes" , "Ich habe mal bei Real Madrid gesagt, Madrid ist nicht der beste Club, da gibt es einen Besseren: Den FC Bayern. Da waren sie nat\252rlich b\246se auf mich." , "der mit Real Madrid die Champions League gewann" , "Trainerspr\252che" )

joke927 :: Joke
joke927 = ( 927 , "Manuel Neuer" , "Dass Borussia Dortmund nur Sechster ist." , "nach dem 2:3 gegen Hoffenheim auf die Frage, ob es etwas Positives am letzten Saison-Spieltag gebe" , "Spielerspr\252che" )

joke928 :: Joke
joke928 = ( 928 , "Mario Gomez" , "Wir treffen uns am Dienstag und bis dahin sollte der ganze Alkohol raus sein." , "\252ber die kurze Zeit zwischen der Saison-Abschlussparty mit dem VfB und der Asienreise der DFB-Auswahl" , "Spielerspr\252che" )

joke929 :: Joke
joke929 = ( 929 , "Uli Hoene\223" , "\"Der soll hierherkommen und nicht st\228ndig in Kalifornien rumtanzen und uns hier den Schei\223 machen lassen.\"" , "\220ber J\252rgen Klinsmann (damals Bundestrainer): " , "Promis & Presse" )

joke930 :: Joke
joke930 = ( 930 , "Uli Hoene\223" , "\"Solange Karl-Heinz Rummenigge und ich etwas beim FC Bayern zu sagen haben, wird der bei diesem Verein nicht mal Greenkeeper im neuen Stadion\"" , "\220ber Lothar Matth\228us (2000)" , "Promis & Presse" )

joke931 :: Joke
joke931 = ( 931 , "Uli Hoene\223" , "\"Der sagt zu allem irgendwas. Sto\223en in Tschechien zwei Spieler mit dem Kopf zusammen, wei\223 er, dass das in Leverkusen 1934 auch schon passiert ist.\"" , "\220ber Reiner Calmund" , "Promis & Presse" )

joke932 :: Joke
joke932 = ( 932 , "Uli Hoene\223" , "Drei\223ig Minuten nach Spielschluss werden schon wieder Karten gespielt und Spr\252che geklopft. Sie essen Scampis und ich habe eine schlaflose Nacht." , "\220ber die Bayern-Profis nach der 1:2-Blamage gegen den FC St.Pauli (2002)" , "Spielerspr\252che" )

joke933 :: Joke
joke933 = ( 933 , "Uli Hoene\223" , "\"For me, its schei\223egal.\"\n" , " Auf die Frage nach seinem Wunschgegner f\252rs Champions-League-Finale:" , "Promis & Presse" )

joke934 :: Joke
joke934 = ( 934 , "Uli Hoene\223" , "\"Zu viele Leute haben ihm nach der WM Puderzucker in den Arsch geblasen. Den klopfe ich nun wieder raus.\"" , "\220ber junge Stars wie Bastian Schweinsteiger (2006)" , "Promis & Presse" )

joke935 :: Joke
joke935 = ( 935 , "Robert Wieschemann" , "\"Wir haben hier alle ein Defizit an Durchblick!\"" , "Beim Doppelpass zur Krise in der Lautrer Vereinsf\252hrung" , "Trainerspr\252che" )

joke936 :: Joke
joke936 = ( 936 , "Stefan Kuntz" , "\"Das ist die umfangsreichste Arbeit meines Lebens. Aber ich gehe abends mit einem L\228cheln ins Bett. Und das hat nichts damit zu tun, dass da meine Frau liegt!\"" , "Wenige Wochen nach seinem Amtsantritt beim FCK 2008" , "Trainerspr\252che" )

joke937 :: Joke
joke937 = ( 937 , "Marcel Reif" , "\"Sauf nicht so viel!\"" , "mal wieder in dem Glauben, nicht auf Sendung zu sein" , "Reporterspr\252che" )

joke938 :: Joke
joke938 = ( 938 , "Marcel Reif" , "\"Man muss den Bayern immerhin zugute halten: es steht nur 0:4!\"" , "Beim CHampionsleague-Hinspiel gegen Barcelona" , "Reporterspr\252che" )



