Extension { #name : 'RwAbstractRowanProjectLoadComponentV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractRowanProjectLoadComponentV2 class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			Rowan projectTools trace trace: '--- reading component ' , filePath asString.
			[^ self _readStonFrom: fileStream ] 
				on: STONReaderError
				do: [:ex |
					ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 
						lineNumberStringForOffset: fileStream position 
						fileName: filePath asFileReference fullName).
					ex pass ] ]
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractRowanProjectLoadComponentV2 class >> orderedDictionaryClass [

	^ Rowan platform orderedDictionaryClass
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractRowanProjectLoadComponentV2 >> _exportToUrl: directoryUrl [
	| url |
	url := directoryUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			| fileRef |
			fileRef := url pathForDirectory asFileReference / self name , 'ston'.
			fileRef parent ensureCreateDirectory.
			fileRef
				writeStreamDo: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	url schemeName = 'memory'
		ifTrue: [ 
			FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory
				/ self name , 'ston'
				writeStreamDo: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
	^ nil	"otherwise a noop"
]
