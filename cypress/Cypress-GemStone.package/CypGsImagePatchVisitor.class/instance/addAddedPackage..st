"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addAddedPackage: aPackageDefinition

	currentPackage := aPackageDefinition.
	patchSet addAddedPackage: currentPackage.
	self addClasses: currentPackage classDefinitions.
	aPackageDefinition classExtensions do: 
			[:classExtension |
			classExtension classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addAddedClassMethod: classMethodDefinition
						inClass: classExtension
						inPackage: aPackageDefinition].
			classExtension instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addAddedInstanceMethod: instanceMethodDefinition
						inClass: classExtension
						inPackage: aPackageDefinition]]