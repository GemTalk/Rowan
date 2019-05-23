comparing
compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after modification |
			before := baseDictionary at: key ifAbsent: [elementClass new].
			after := myDictionary at: key ifAbsent: [elementClass new].
			modification := after compareAgainstBase: before.
			modification isEmpty
				ifFalse: [anElementsModification addElementModification: modification]]