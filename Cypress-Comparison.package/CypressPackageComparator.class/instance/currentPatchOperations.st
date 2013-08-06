"
notice: This work is protected by copyright. All rights reserved.
category: comparing - private
"
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
