package writing
_filetreePackagePropertyDictFor: aPackageModification

	| propertiesFile |
	propertiesFile := self _packageSourceDir /  '.filetree'.
	propertiesFile exists
		ifFalse: [
			"create default properties file"
			propertiesFile
				writeStreamDo: [:aStream |
				aStream 
					<< (self _toJSON:
 					(self class orderedDictionaryClass new
						add: 'noMethodMetaData' -> self noMethodMetaData;
						add: 'separateMethodMetaAndSource' -> self separateMethodMetaAndSource;
						add: 'useCypressPropertiesFile' -> self useCypressPropertiesFile;
						yourself))
					<< self  _newLine ] ].
	^ self _readObjectFrom: propertiesFile