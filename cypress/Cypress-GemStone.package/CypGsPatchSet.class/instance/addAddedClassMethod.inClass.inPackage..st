"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: building
"
addAddedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition

	addedMethods add: (CypGsMethodAdditionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)