Extension { #name : 'RwCommonProjectLoadComponentV2' }

{ #category : '*rowan-gemstone-componentsv2' }
RwCommonProjectLoadComponentV2 class >> fromUrl: specNameOrUrl [

	"self fromUrl: 'file:/home/dhenrich/rogue/_homes/rogue/_home/shared/repos/RowanSample1/configs/Default.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ self error: 'scheme must be file: or https:' ].
	url scheme = 'file'
		ifTrue: [ 
			CypressFileUtilities current
				readStreamFor: url fileName
				in: url pathForDirectory
				do: [ :stream | ^ self _readStonFrom: stream ] ].
	url scheme asString = 'https'
		ifTrue: [ 
self error: 'not yet supported'.
"
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ self _readStonFrom: response decodeFromUTF8
" ].
	self error: 'Unknown scheme: ' , url scheme printString
]

{ #category : '*rowan-gemstone-componentsv2' }
RwCommonProjectLoadComponentV2 >> _exportToUrl: fileUrl [

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
