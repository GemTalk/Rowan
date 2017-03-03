"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: building
"
addDeletedClassMethod: aClassMethodDefinition inClass: aClassDefinition inPackage: aPackageDefinition

	deletedMethods add: (CypGsMethodDeletionPatch
				forMethod: aClassMethodDefinition
				isMeta: true
				inClass: aClassDefinition
				inPackage: aPackageDefinition)