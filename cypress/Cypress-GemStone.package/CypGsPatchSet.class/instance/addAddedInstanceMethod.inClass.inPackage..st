"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: building
"
addAddedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition

	addedMethods add: (CypGsMethodAdditionPatch
				forMethod: aClassMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)