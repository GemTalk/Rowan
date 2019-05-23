instance creation
fromUrl: specNameOrUrl

	"self fromUrl: 'file:/export/foos1/users/dhenrich/dev/_home/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston'"

	"self fromUrl: 'sampleLoadSpec'"

	"self fromUrl: 'https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston'"

	| url |
	url := specNameOrUrl asRwUrl.
	url scheme isNil
		ifTrue: [ 
			^ Rowan image
				loadedProjectNamed: specNameOrUrl
				ifAbsent: [ self error: 'No project named ' , specNameOrUrl printString , ' found' ] ].
	url scheme = 'file'
		ifTrue: [ 
			CypressFileUtilities current
				readStreamFor: url fileName
				in: url pathForDirectory
				do: [ :stream | 
					^ (STON fromStream: stream)
						initializeForImport;
						yourself ] ].
	url scheme asString = 'https'
		ifTrue: [ 
			| client response |
			GsSecureSocket disableCertificateVerificationOnClient.
			client := (Rowan globalNamed: 'ZnClient') new.
			response := client
				beOneShot;
				enforceHttpSuccess: true;
				get: url.
			^ (STON fromString: response decodeFromUTF8)
				initializeForImport;
				yourself ].
	self error: 'Unknown scheme: ' , url scheme printString
