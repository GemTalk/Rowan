"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
updatePackage: aPackage withSnapshot: aSnapshot
	"Answer the loader used to apply the update."

	^(self new)
		updatePackage: aPackage withSnapshot: aSnapshot;
		load