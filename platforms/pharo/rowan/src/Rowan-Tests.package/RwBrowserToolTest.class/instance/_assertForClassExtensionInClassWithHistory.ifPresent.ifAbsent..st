private
_assertForClassExtensionInClassWithHistory: classHistory ifPresent: presentBlock ifAbsent: absentBlock

	"validate loaded class extension registry structure"

	| found |
	found := IdentitySet new.
	Rowan image symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry classExtensionRegistry at: classHistory ifAbsent: [  ])
						ifNotNil: [ :loadedClassExtensionSet | found addAll: loadedClassExtensionSet ] ] ].
	found isEmpty
		ifFalse: [ ^ presentBlock cull: found ].
	^ absentBlock value