"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
deleteClasses: classDefinitions

	classDefinitions do: 
			[:classDefinition |
			patchSet addDeletedClass: classDefinition inPackage: currentPackage.
			classDefinition classMethodDefinitions do: 
					[:classMethodDefinition |
					patchSet
						addDeletedClassMethod: classMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage].
			classDefinition instanceMethodDefinitions do: 
					[:instanceMethodDefinition |
					patchSet
						addDeletedInstanceMethod: instanceMethodDefinition
						inClass: classDefinition
						inPackage: currentPackage]]