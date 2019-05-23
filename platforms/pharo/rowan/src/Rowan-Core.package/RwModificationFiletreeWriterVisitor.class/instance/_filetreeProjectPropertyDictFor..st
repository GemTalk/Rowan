project writing
_filetreeProjectPropertyDictFor: aProjectModification

	| propertiesFile |
	propertiesFile := self _projectSourceDir /  '.filetree'.
	propertiesFile exists
		ifFalse: [
			"create default properties file"
			propertiesFile
				writeStreamDo: [:aStream |
				| dict |
				dict := (self class orderedDictionaryClass new)
					at: 'packageExtension' put: '.', self packageExtension;
					at: 'Metadata' put: self monticelloMetadata;
					yourself.
				aStream 
					<< (self _toJSON: dict )
					<< self  _newLine ] ].
	^ self _readObjectFrom: propertiesFile