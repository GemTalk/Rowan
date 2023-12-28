Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			[
				^ (STON fromStream: stream)
					initializeForImport;
					yourself ]
				on: STONReaderError
				do: [:ex |
					ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 lineNumberStringForOffset: stream position fileName: filePath).
					ex pass ] ]
]
