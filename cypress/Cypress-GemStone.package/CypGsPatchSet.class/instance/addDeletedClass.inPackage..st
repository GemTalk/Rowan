"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: building
"
addDeletedClass: aClassDefinition inPackage: aPackageDefinition

	deletedClasses add: (CypGsClassDeletionPatch for: aClassDefinition
				inPackage: aPackageDefinition)