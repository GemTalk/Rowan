Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| spec |
			[ 
			spec := (STON fromStream: fileStream)
				initializeForImport;
				yourself ]
				on: STONReaderError
				do: [ :ex | 
					ex
						addText:
							(RwRepositoryResolvedProjectTonelReaderVisitorV2
								lineNumberStringForOffset: fileStream position
								fileName: filePath asFileReference fullName).
					ex pass ].
			Rowan projectTools trace
				trace: '--- reading ' , spec class label , filePath asString.
			^ spec ]
]
