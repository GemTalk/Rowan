"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addPackages: packageDefinitions

	packageDefinitions do: 
			[:packageDefinition |
			currentPackage := packageDefinition.
			patchSet addAddedPackage: currentPackage.
			self addClasses: currentPackage classDefinitions.
			packageDefinition classExtensions do: 
					[:classExtension |
					classExtension classMethodDefinitions do: 
							[:classMethodDefinition |
							patchSet
								addAddedClassMethod: classMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition].
					classExtension instanceMethodDefinitions do: 
							[:instanceMethodDefinition |
							patchSet
								addAddedInstanceMethod: instanceMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition]]]