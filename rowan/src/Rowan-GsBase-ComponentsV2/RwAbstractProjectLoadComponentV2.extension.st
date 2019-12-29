Extension { #name : 'RwAbstractProjectLoadComponentV2' }

{ #category : '*rowan-gsbase-componentsv2' }
RwAbstractProjectLoadComponentV2 class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			^ self _readStonFrom: stream ]
]

{ #category : '*rowan-gsbase-componentsv2' }
RwAbstractProjectLoadComponentV2 class >> orderedDictionaryClass [

	^ GsTonelOrderedDictionary
]

{ #category : '*rowan-gsbase-componentsv2' }
RwAbstractProjectLoadComponentV2 >> _exportToUrl: fileUrl [
	self error: 'Not yet implemented'
]
