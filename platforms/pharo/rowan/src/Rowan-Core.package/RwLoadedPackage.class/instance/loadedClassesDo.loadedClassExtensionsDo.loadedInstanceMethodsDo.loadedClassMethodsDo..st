enumeration
loadedClassesDo: loadedClassBlock loadedClassExtensionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock

	loadedClasses
		valuesDo: [ :loadedClass | 
			loadedClassBlock 
				cull: loadedClass
				cull: self loadedProject 
				cull: self. 
			loadedClass
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ].
	loadedClassExtensions
		valuesDo: [ :loadedClassExtension | 
			loadedClassExtensionBlock
				cull: loadedClassExtension
				cull: self loadedProject
				cull: self.
			loadedClassExtension
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ]
