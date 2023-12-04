Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream spec |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			[spec := (STON fromStream: stream)
				initializeForImport;
				yourself ]
					on: STONReaderError
					do: [:ex |
						ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 lineNumberStringForOffset: stream position fileName: filePath asFileReference fullName).
						ex pass ].
			Rowan projectTools trace
				trace: '--- reading ' , spec class label , filePath asString.
			^ spec ]
]
