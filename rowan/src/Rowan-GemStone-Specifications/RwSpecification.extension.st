Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification class >> fromFile: filePath [
	filePath asFileReference
		readStreamDo: [ :fileStream | 
			| stream spec |
			stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
			spec := (STON fromStream: stream)
				initializeForImport;
				yourself.
			(SessionTemps current at: #'ROWAN_TRACE' otherwise: nil) == #'gciLogServer'
				ifTrue: [ GsFile gciLogServer: '--- reading ' , spec class label , filePath asString ].
			^ spec ]
]
