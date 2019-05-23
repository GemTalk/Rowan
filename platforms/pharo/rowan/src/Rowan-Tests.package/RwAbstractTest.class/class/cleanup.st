cleanup
cleanup

	"self cleanup"

	| user symListP symListT dictP dictT index |
	user := System myUserProfile.
	symListP := user symbolList.
	symListT := GsCurrentSession currentSession symbolList.

	self _symbolDictionaryNames
		do: [ :symDictName | 
			dictP := symListP objectNamed: symDictName.
			dictP
				ifNotNil: [ 
					(dictP at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListP indexOf: dictP.
					index > 0
						ifTrue: [ user removeDictionaryAt: index ] ].

			dictT := symListT objectNamed: symDictName.
			dictT
				ifNotNil: [ 
					(dictT at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListT indexOf: dictT.
					index > 0
						ifTrue: [ symListT removeAtIndex: index ] ].
 ].
	GsPackagePolicy current refreshSessionMethodDictionary.
	ClassOrganizer clearCachedOrganizer
