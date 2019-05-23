snapshotting
snapshot

	| definitions |
	definitions := OrderedCollection new.
	self classes
		do: [ :classStructure | 
			definitions add: classStructure asCypressClassDefinition.
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ] ].
	self extensions
		do: [ :classStructure | 
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					| mdef |
					mdef := methodStructure asCypressMethodDefinition.
					mdef isExtensionMethod: true.
					definitions add: mdef ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					| mdef |
					mdef := methodStructure asCypressMethodDefinition.
					mdef isExtensionMethod: true.
					definitions add: mdef ] ].
	^ CypressSnapshot definitions: definitions