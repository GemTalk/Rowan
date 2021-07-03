Extension { #name : 'RwGsFileUtilities' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsFileUtilities class >> directoryExists: aDirectory [

	"handle the case where GsFile class>>existsOnServer: returns nil"
	aDirectory ifNil: [ ^false ].
	^ (GsFile existsOnServer: aDirectory) == true
]
