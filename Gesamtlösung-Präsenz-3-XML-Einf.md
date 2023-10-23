# Lösungen Woche 3: XML-Einführung

## Studierendenfragen

### Was ist die Aufgabe eines XML-Parser und XML-Prozesseor? Wie unterscheiden sie sich?

Ein XML-Parser dient dazu, XML-Dokumente zu lesen und deren Struktur zu erkennen. Er stellt sicher, dass das Dokument den XML-Regeln entspricht. Ein XML-Prozessor hingegen führt spezifische Aufgaben mit XML-Daten aus, wie beispielsweise deren Transformation oder Abfrage. Während der Parser primär die Gültigkeit und Struktur von XML prüft, fokussiert sich der Prozessor auf die Verarbeitung und Manipulation dieser Daten.

### Was genau sind die "Definition Makros / "Parameter-Entity-Deklarationen auf p.29

Wie bei JS => Variable wird erst später definiert und den Wert eingefügt. Man kann nicht nur auf Strings referenzieren, sondern auch auf ganze Elemente. Als Beispiel wird in diesem XML definiert, dass im ganzen Dokument `Grand Cru` auch als `&cg` eingefügt werden kann. Das `&` ist standardmässig das Zeichen um zu definieren, dass etwas eingefügt werden soll.

```XML
<! ENTITY cg "Grand Cru"> wird eingesetzt als
<wein><name> Chateau La Rose &cg; </name> </wein>
```

Man kann auch Elemente mit dieser Funktionalität definieren und einfügen. Zum Beispiel hier wird definiert, wie ein `Adresselement` aussehen muss. Dies kann dann später an anderen Stellen eingefügt werden um Platz zu sparen:

```XML
<!ENTITY %adresselement "(ort, plz, strasse)" >
```

Daraus resultiert, dass die folgenden beiden Beispiele equivalent zueinander sind:

```XML
<! ELEMENT adresse %adresselement; >
```

```XML
<!ELEMENT adresse (ort, plz, strasse) >
```

### Unterschied zwischen ATTLIST und ELEMENT bei XML

In XML-DTDs (Document Type Definitions) werden ATTLIST und ELEMENT verwendet, um die Struktur und die Eigenschaften von XML-Dokumenten zu definieren. Beide haben unterschiedliche Rollen und tangieren jeweils andere Bausteine von XML. [Hier noch ein Link der das ganze gut darstellt.](https://www.a-coding-project.de/ratgeber/xml/document-type-definition-dtd)

ELEMENT:

- Mit ELEMENT wird die allgemeine Struktur eines XML-Dokuments beschrieben.
- Es definiert, welche Elemente (Tags) im XML-Dokument erlaubt sind und wie diese Elemente miteinander in Beziehung stehen (z.B. welche Kinder-Elemente sie haben dürfen).
- Beispiel:

```DTD
<!ELEMENT buch (titel, autor)>
```

Das würde erlauben ein XML zu erstellen mit folgendem Aufbau (XML und DTD Definition fehlen):

```XML
<buch>
    <titel></titel>
    <autor></autor>
</buch>
```

ATTLIST:

- Mit ATTLIST werden die Attribute definiert, die ein bestimmtes Element haben kann.
- Es gibt auch die Möglichkeit, Standardwerte oder Wertebereiche für Attribute festzulegen.
- Beispiel:

```DTD
  <!ATTLIST buch sprache CDATA "deutsch">
```

In Kombination mit dem vorherigen Beispiel wäre also ein XML zu erstellen mit folgendem Aufbau möglich (XML und DTD Definition fehlen):

```XML
<buch sprache="deutsch">
    <titel></titel>
    <autor></autor>
</buch>
```

Das vollständige Beispiel mit DTD würde so aussehen:

```XML
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE buch [
    <!ELEMENT buch (titel, autor)>
    <!ATTLIST buch sprache CDATA "deutsch">
    <!ELEMENT titel (#PCDATA)>
    <!ELEMENT autor (#PCDATA)>
]>
<buch sprache="deutsch">
    <titel>Der Herr der Ringe</titel>
    <autor>J.R.R. Tolkien</autor>
</buch>
```

Zusammengefasst:

- ELEMENT definiert die Struktur und Reihenfolge von XML-Elementen.`</br>`
- ATTLIST legt die Attribute und deren mögliche Werte für ein bestimmtes Element fest.

### \<!DOCTYPE Wurzelelement PUBLIC " Public Identifier">  //W3C//DTD XHTML 1.0 Strict//EN" Was genau ist ein Public Identifier?

Ein Public Identifier ist ein Bestandteil einer Dokumenttypdeklaration (DOCTYPE) in XML. Er bietet eine öffentliche, menschenlesbare Bezeichnung für einen bestimmten Dokumenttyp, oft in Verbindung mit einer Dokumenttypdefinition (DTD) oder einer XML-Schema-Definition.

Der Public Identifier wird oft verwendet, um auf eine spezifische DTD-Version oder eine XML-Schema-Definition zu verweisen, die die Regeln und die Struktur für ein Dokument dieses Typs definiert. Es hilft Browsern und Parsern, das richtige "Regelwerk" für die Interpretation und Validierung des Dokuments zu bestimmen.

### Besonders im Kontext von den Java API's. Was bei den vielen APIs ist ein Parser und was nicht? --> dieser Teil verwirrt mich am meisten: "Für diese gibt es unterschiedliche Implementationen, also Parser" (p.34)

TODO:

## Hausaufgaben

### Aufgabe A

Geben Sie für JAVA-Kriterien der "Wohlgeformtheit" an. Was unterscheidet diese Form der Wohlgeformtheit von der bezüglich XML (Denken Sie zum Beispiel dran, wann diese Eigenschaft für welche Zwecke jeweils geprüft wird)?

Kriterien:

<ul>
<li>Korrekte Syntax: Z.B kann eine Funktion nur mit folgendem Syntax definiert werden `visibility-modifier return-type name (parameter...) {}`. Anderer Syntax wird nicht erlaubt, da der Code sonst nicht ausgeführt werden kann. </li>
<li>Korrekte Deklaration von Typen: Einem String `BFH` darf nicht der Datentyp `int` gegeben werden. </li>
<li>Kompilierbarkeit</li>
<li>Java-Code kann ohne korrekte «Wohlgeformtheit» in Bezug auf die Syntax nicht ausgeführt bzw. kompiliert werden.</li>
</ul>

Hierbei ist zu beachten, dass beim Kompilieren und Ausführen des Codes nur die Syntax überprüft wird. Die Semantik muss beim programmieren selbst beachtet werden.

### Aufgabe B

Nutzen Sie den XML-Validator, um Fehler in folgendem Code zu finden & zu verbessern.

```xml
<party date="31.12.01" drunk=T>
    <guest name="Albert"> 
        <drink>wine</drink>
</party>
```

#### Lösung:

Bei XML sind alle Werte von Attributen in Anführungs- und Schlusszeichen verpackt (siehe drunk="T").

#### Falsch:

```xml
<party date="31.12.01" drunk=T>
```

#### Richtig:

```xml
<party date="31.12.01" drunk=”T”>
```

Bei XML haben alle Tags die geöffnete werden auch einen Gegenpart, welchen den Tag wieder schliesst (siehe `<guest>` `</guest>`). Es ist zu beachten, dass es bei XML auch selbstschliessende Tags gibt, z. B. \<snack />.

TODO: Evtl. Self Closing Tags erklären.

#### Falsch:

```xml
<party date="31.12.01" drunk=”T”>
    <guest name="Albert"> 
        <drink>wine</drink>
</party>
```

#### Richtig:

```xml
<party date="31.12.01" drunk=”T”>
    <guest name="Albert"> 
        <drink>wine</drink>
    </guest>
</party>
```

#### Gesamtlösung:

```xml
<party date="31.12.01" drunk=”T”>
    <guest name="Albert"> 
        <drink>wine</drink>
    </guest>
</party>
```

### Aufgabe C

Wenn alle vorkommenden Elemente in einer DTD-Datei definiert werden müssen, wo sind dann die Wurzelelemente SYSTEM und PUBLIC definiert?

Diese Informationen, einschliesslich der Verweise auf die DTD (PUBLIC und SYSTEM), werden normalerweise im DOCTYPE-Deklarationsblock in einem XML-Dokument angegeben. Die DTD selbst definiert nicht die Wurzelelemente SYSTEM und PUBLIC, sondern nur die Struktur und die Regeln für die in den XML-Dokumenten verwendeten Elemente.

### Aufgabe D

Warum ist name vom Anbaugebiet ein Attribut, aber weinname ein Element? Erweitern Sie das DTD auf Folie 27 so, dass Erzeuger auch nichtalkoholische Getränke verkaufen können.

Der Name vom Anbaugebiet ist ein Attribut weil es in der DTD so definiert wurde. Der Weinname wurde dort als Element definiert.

```DTD
<!ATTLIST anbaugebiet name CDATA #REQUIRED >
<!ELEMENT weinname (#PCDATA)
```

Im allgemeinen wäre sowohl ein Element wie auch ein Attribut valide um eine Information wie den Namen darzustellen. Es gibt aber gewisse Gründe wieso man eines davon bevorzugen könnte. Falls der Name weitere Informationen beinhält, wie zum Beispiel den Hersteller oder die Marke, dann bevorzugt man ein Element. Falls die Information atomar ist, also man sie nicht weiter untergliedern oder Informationen hinzufügen kann, ist ein Element die bessere Wahl. ZB für eine ID verwendet man am besten ein Attribut.

Um nichtalkoholische Getränke zu verkaufen müssen z. B. folgende Elemente hinzugefügt werden:

```DTD
<!ELEMENT alkoholfrei (getraenke+) >
<!ELEMENT getraenke (getraenkename) >
<!ELEMENT getraenkename (#PCDATA) >
```

Anschliessend muss im bestehenden Element "Erzeuger" das Element "Alkoholfrei" hinzugefügt werden:

```DTD
<!ELEMENT erzeuger (weingut?, alkoholfrei?, rotweine?, weissweine?)>
```

Nun können alkoholfreie Getränke als Element hinzugefügt werden und diese werden auch korrekt von der DTD erkannt bzw. validiert.

### Aufgabe E

JDOM nutzt selbst SAX zum Parsen. Beschreiben Sie kurz die Notwendigkeit.
In R gibt es https://www.stat.auckland.ac.nz/~paul/Reports/DOM/Intro/DOM-Intro.html. Diskutieren Sie kurz, ob es das braucht für die Praxis.

JDOM, eine Java-Bibliothek zur Bearbeitung von XML-Dokumenten, verwendet intern SAX zum Parsen von XML. Dies kombiniert die Speichereffizienz von SAX, einer ereignisbasierten Parsing-Technologie, mit der Benutzerfreundlichkeit und Flexibilität von JDOMs baumbasiertem Zugriff.  Zum Beispiel, wenn SAX auf ein Start-Tag wie \<element> stößt, löst es ein "Start-Element"-Ereignis aus. Dieser ereignisbasierte Ansatz ermöglicht eine hohe Speichereffizienz, da nicht das gesamte Dokument geladen werden muss.Durch die Nutzung von SAX profitiert JDOM von dessen Geschwindigkeit und optimiert gleichzeitig die XML-Verarbeitung für den Benutzer.

Auch in R kann es Sinn machen Pakete zu verwenden die es einem erlauben Elemente über den DOM abzurufen. So können zum Beispiel Datensätze aus vielen HL7 CDA Dokumenten erstellt werden, da diese strukturiert sind.

## Übungsaufgabe

#### Erzeugen Sie auf Basis der DTD auf Folie 27 ein entsprechendes XML-File. Finden Sie eine Website, mit der Sie das XML-File validieren können. Warum reicht https://www.w3schools.com/xml/xml_validator.asp nicht?

#### DTD Datentyp CDATA vs. ID

Der DTD-Datentyp ID schreibt vor, dass IDs nicht mit einer Nummer beginnen dürfen. Daher wird bei der ID des Getränks die ID "G1" gewählt. Diese ID muss auch Unique sein.

```xml
<alkoholfrei>
    <getraenke ID="G1">
        <getraenkename> Irgendwas </getraenkename>
    </getraenke>
</alkoholfrei>

<rotweine>
    <wein ID="1">
        <weinname>Rotwein</weinname>
        <jahrgang>2007</jahrgang>
    </wein>
</rotweine>
```

Da in der DTD beim Element Wein die das Attribut ID als CDATA definiert ist, kann diese mit 1 beginnen. Im vergleich zum ID-Datentyp kann hier eine ID auch mehrfach vorkommen, da CDATA nicht eine Eindeutigkeit überprüft.

```xml
<!ATTLIST getraenke ID ID #REQUIRED>
<!ATTLIST wein ID CDATA #REQUIRED >
```

**Gesamtlösung des XML inkl. DTD**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE weinkatalog [
    <!ELEMENT weinkatalog (region*) >
    <!ELEMENT region (name, land, anbaugebiet+)>
    <!ELEMENT name (#PCDATA)>
    <!ELEMENT land (#PCDATA)>
    <!ELEMENT anbaugebiet (erzeuger*) >
    <!ATTLIST anbaugebiet name CDATA #REQUIRED >
    <!ELEMENT erzeuger (weingut?, alkoholfrei?, rotweine?, weissweine?)>
    <!ELEMENT rotweine (wein+) >
    <!ELEMENT weissweine (wein+) >
    <!ELEMENT alkoholfrei (getraenke+) >
    <!ELEMENT getraenke (getraenkename) >
    <!ATTLIST getraenke ID ID #REQUIRED>
    <!ELEMENT getraenkename (#PCDATA) >
    <!ELEMENT wein (weinname, jahrgang) >
    <!ELEMENT weinname (#PCDATA) >
    <!ELEMENT jahrgang (#PCDATA)>
    <!ELEMENT weingut (#PCDATA)>
    <!ATTLIST wein ID CDATA #REQUIRED >
]>

<weinkatalog>
    <region>
        <name>Twann</name>
        <land>Schweiz</land>
        <anbaugebiet name="Twann">
            <erzeuger>
                <weingut>Schloss Johannisberg</weingut>
                <alkoholfrei>
                    <getraenke ID="G1">
                        <getraenkename> Irgendwas </getraenkename>
                    </getraenke>
                </alkoholfrei>
                <rotweine>
                    <wein ID="1">
                        <weinname>Rotwein</weinname>
                        <jahrgang>2007</jahrgang>
                    </wein>
                </rotweine>
                <weissweine>
                    <wein ID="2">
                        <weinname>Weisswein</weinname>
                        <jahrgang>2007</jahrgang>
                    </wein>
                </weissweine>
            </erzeuger>
        </anbaugebiet>
    </region>
</weinkatalog>
```

</ol>
