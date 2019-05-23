comparing
compareDictionary: myDictionary againstBaseDictionaryForNewClassVersion: baseDictionary into: anElementsModification elementClass: elementClass isMeta: isClassMeta

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys
		do: [ :key | 
			(baseDictionary
				at: key
				ifAbsent: [ 
					"additions do not apply for new class version - the original modification should not be changed"
					 ])
				ifNotNil: [ :before | 
					(myDictionary
						at: key
						ifAbsent: [ 
							"deletions do not apply for new class version - the original modification should not be changed"
							 ])
						ifNotNil: [ :after | 
							| modification |
							modification := after compareAgainstBaseForNewClassVersion: before.
							modification
								isMeta: isClassMeta;
								classDefinition: self.
							modification isEmpty
								ifFalse: [ anElementsModification addElementModification: modification ] ] ] ]