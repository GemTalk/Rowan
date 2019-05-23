comparing
compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys
		do: [ :key | 
			| before after modification |
			before := baseDictionary at: key ifAbsent: [ elementClass new ].
			after := myDictionary at: key ifAbsent: [ elementClass new ].
			modification := after compareExtensionMethodsAgainstBase: before.
			modification
				isMeta: isClassMeta;
				classDefinition: self.
			modification isEmpty
				ifFalse: [ anElementsModification addElementModification: modification ] ]