smalltalk api
commitPackagesForSpecUrl: aSpecUrlString message: messageString

	"commit repository ... packages must be written out independently with write tool"

	^ self
		commitPackagesForSpecification: (RwSpecification fromUrl: aSpecUrlString)
		message: messageString