"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: modifying image
"
applyModification: aPackageSetModification

	| visitor patchSet |
	visitor := CypGsImagePatchVisitor new.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet apply