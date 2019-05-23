smalltalk api
cloneSpecUrl: aString gitRootPath: gitRootPath useSsh: useSsh

	^self 
		cloneSpecification: (RwSpecification fromUrl: aString) 
		gitRootPath: gitRootPath 
		useSsh: useSsh
