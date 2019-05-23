enumeration
loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock

	loadedInstanceMethods
		valuesDo: [ :loadedMethod | 
			loadedInstanceMethodBlock
				cull:  self loadedProject
				cull: self loadedPackage
				cull: self 
				cull: loadedMethod
				
	].
	loadedClassMethods
		valuesDo: [ :loadedMethod | 
			loadedClassMethodBlock
				cull: self loadedProject
				cull: self loadedPackage
				cull: self 
				cull: loadedMethod
	]