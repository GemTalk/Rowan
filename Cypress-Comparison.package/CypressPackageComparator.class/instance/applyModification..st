"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
