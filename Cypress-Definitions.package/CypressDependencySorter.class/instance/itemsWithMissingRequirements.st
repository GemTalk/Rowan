"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
