private
definitionsFor: loadedThings

	| result |
	result := Dictionary new: loadedThings size.
	loadedThings
		do: [:loadedThing | result at: loadedThing key put: loadedThing asDefinition].
	^result