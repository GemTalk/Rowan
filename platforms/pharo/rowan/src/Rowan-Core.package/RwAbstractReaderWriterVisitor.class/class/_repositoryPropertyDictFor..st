private
_repositoryPropertyDictFor: packagesRoot

	| propertiesFile |
	propertiesFile := packagesRoot / 'properties', 'st'.
	propertiesFile exists
		ifFalse: [ ^ Dictionary new ].
	^ self _readObjectFrom: propertiesFile