"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
deletePackages: packageDefinitions

	packageDefinitions do: 
			[:packageDefinition |
			currentPackage := packageDefinition.
			patchSet addDeletedPackage: currentPackage.
			self deleteClasses: packageDefinition classDefinitions.
			packageDefinition classExtensions do: 
					[:classExtension |
					classExtension classMethodDefinitions do: 
							[:classMethodDefinition |
							patchSet
								addDeletedClassMethod: classMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition].
					classExtension instanceMethodDefinitions do: 
							[:instanceMethodDefinition |
							patchSet
								addDeletedInstanceMethod: instanceMethodDefinition
								inClass: classExtension
								inPackage: packageDefinition]]]