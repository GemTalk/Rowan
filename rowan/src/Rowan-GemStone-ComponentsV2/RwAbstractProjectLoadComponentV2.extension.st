Extension { #name : 'RwAbstractProjectLoadComponentV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectLoadComponentV2 class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ self _readStonFrom: stream ]
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectLoadComponentV2 class >> orderedDictionaryClass [

	^ Rowan platform orderedDictionaryClass
]

{ #category : '*rowan-gemstone-componentsv2' }
RwAbstractProjectLoadComponentV2 >> _exportToUrl: fileUrl [

	| url |
	url := fileUrl asRwUrl.
	url schemeName = 'file'
		ifTrue: [ 
			Rowan fileUtilities
				writeStreamFor: self name , '.ston'
				in: url pathForDirectory
				do: [ :stream | 
					| string |
					string := STON toStringPretty: self.
					stream nextPutAll: string.
					^ self ] ].
  url schemeName = 'memory'
    ifTrue: [ 
		(FileSystem currentMemoryFileSystem workingDirectory / url pathForDirectory / self name , 'ston')
			writeStreamDo: [ :stream | 
			  | string |
			  string := STON toStringPretty: self.
			  stream nextPutAll: string.
			  ^ self ] ].
	^ nil	"otherwise a noop"
]
