
import java.util.HashMap;

/*

- ajout du source dans le repertoire de l'API Java de krakatoa

  cd .../lib/java_api
  unzip src.zip java/util/HashMap.java

- resol pb syntaxe dans HashMap.java

  commenté (avec //KML) 4 occurrences de
 
    HashMap.this.expr ...

  dans le code (pb support inner classes par Krakatoa)

  de toute facon: le code des methodes de l'API n'est pas utilise par Krakatoa, seulement les spec

- HashMap etend AbstractMap: besoin de

  unzip src.zip java/util/AbstractMap.java

- de nouveau 4 occurrences de 

    AbstractMap.this.... 

  commentees avec //KML

- import explicite de java.util.Map.Entry dans AbstractMap: commenté

- AbstractMap implemente Map -> ajouter Map

  unzip src.zip java/util/Map.java

- Map utilise Set et Collection ->

  unzip src.zip java/util/Set.java
  unzip src.zip java/util/Collection.java

- HashMap: champ transient Entry[] table; commente

- HashMap: methode getEntry commentee

- HashMap: methode removeEntryForKey commentee

- HashMap: methode removeMapping commentee

- ajout de Iterator

  unzip src.zip java/util/Iterator.java

- methodes de serialization commentees

*/

class HashMapTest {

    public static void main(String argv[]) {

	HashMap m = new HashMap();

	Integer zero = new Integer(0);

	Integer one = new Integer(1);

	m.put(zero,one);

	Object o = m.get(zero);

	//@ assert o instanceof Integer ; 
	// && o == 1;
	
	// System.out.println("o = " + o);

    }

}
	