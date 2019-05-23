enumeration
loadedClassesDo: loadedClassBlock loadedClassExtensionsDo: loadedClassExtensionBlock 

	loadedClasses
		valuesDo: [ :loadedClass | 
			loadedClassBlock 
				cull: loadedClass
				cull: self loadedProject 
				cull: self].
	loadedClassExtensions
		valuesDo: [ :loadedClassExtension | 
			loadedClassExtensionBlock
				cull: loadedClassExtension
				cull: self loadedProject
				cull: self ]
