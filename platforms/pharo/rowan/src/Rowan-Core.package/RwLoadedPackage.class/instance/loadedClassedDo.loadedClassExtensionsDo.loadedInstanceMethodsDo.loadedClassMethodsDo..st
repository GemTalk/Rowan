enumeration
loadedClassedDo: loadedClassBlock loadedClassExtensionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock

	loadedClasses
		valuesDo: [ :loadedClass | 
			loadedClassBlock cull: self loadedProject cull: self cull: loadedClass.
			loadedClass
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ].
	loadedClassExtensions
		valuesDo: [ :loadedClassExtension | 
			loadedClassExtensionBlock
				cull: self loadedProject
				cull: self
				cull: loadedClassExtension.
			loadedClassExtension
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ]
