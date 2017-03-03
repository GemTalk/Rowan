"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addClasses: classDefinitions

	classDefinitions do: 
			[:classDefinition |
			patchSet addAddedClass: classDefinition inPackage: currentPackage.
			classDefinition classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addAddedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage].
			classDefinition instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addAddedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage]]