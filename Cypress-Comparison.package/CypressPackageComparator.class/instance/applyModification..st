"
notice: This work is protected by copyright. All rights reserved.
category: comparing - private
"
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
