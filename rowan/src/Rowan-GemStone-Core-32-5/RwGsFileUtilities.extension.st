Extension { #name : 'RwGsFileUtilities' }

{ #category : '*rowan-gemstone-core-32-5' }
RwGsFileUtilities class >> directoryExists: aDirectory [

	"handle the case where GsFile class>>existsOnServer: returns nil"
	^ (GsFile existsOnServer: aDirectory) == true
]
