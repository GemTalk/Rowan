"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: building
"
addDeletedInstanceMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition

	deletedMethods add: (CypGsMethodDeletionPatch
				forMethod: aClassMethodDefinition
				isMeta: false
				inClass: aClassDefinition
				inPackage: aPackageDefinition)