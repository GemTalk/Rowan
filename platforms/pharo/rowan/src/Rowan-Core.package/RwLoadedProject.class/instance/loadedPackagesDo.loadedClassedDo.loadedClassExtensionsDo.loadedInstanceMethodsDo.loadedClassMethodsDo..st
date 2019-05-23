enumeration
loadedPackagesDo: loadedPackageBlock loadedClassedDo: loadedClassBlock loadedClassExtensionsDo: loadedClassExtensionBlock loadedInstanceMethodsDo: loadedInstanceMethodBlock loadedClassMethodsDo: loadedClassMethodBlock

	loadedPackages
		valuesDo: [ :loadedPackage | 
			loadedPackageBlock cull: self cull: loadedPackage.
			loadedPackage
				loadedClassedDo: loadedClassBlock
				loadedClassExtensionsDo: loadedClassExtensionBlock
				loadedInstanceMethodsDo: loadedInstanceMethodBlock
				loadedClassMethodsDo: loadedClassMethodBlock ]