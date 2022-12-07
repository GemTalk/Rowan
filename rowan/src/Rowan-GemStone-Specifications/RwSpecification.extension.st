Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| spec |
			spec := (STON fromStream: fileStream)
				initializeForImport;
				yourself.
			Rowan projectTools trace
				trace: '--- reading ' , spec class label , filePath asString.
			^ spec ]
]
