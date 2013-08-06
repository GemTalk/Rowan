"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Structure
"
asCypressPropertyObject

	| result |
	result := self class new: self size.
	self associationsDo: [:assoc | result at: assoc key put: assoc value asCypressPropertyObject].
	^result.
