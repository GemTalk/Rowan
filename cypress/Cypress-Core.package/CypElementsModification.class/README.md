Abstract. Represents a modification to the elements of some code entity. "Modification" means that the code entity exists both before and after, but has differences in some of its elements.

Instvars:

elementsAdded		Dictionary elementKey -> element
elementsRemoved	Dictonary elementKey -> element
elementsModified	Dictionary elementKey -> CypModification  (key may have changed -- if so the key here is the old key)