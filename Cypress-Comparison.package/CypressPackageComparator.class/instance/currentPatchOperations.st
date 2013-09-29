"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
