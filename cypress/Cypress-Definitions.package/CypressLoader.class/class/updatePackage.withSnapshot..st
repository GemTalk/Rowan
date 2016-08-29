"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: loading
"
updatePackage: aPackage withSnapshot: aSnapshot
	"Answer the loader used to apply the update."

	^(self new)
		updatePackage: aPackage withSnapshot: aSnapshot;
		load