"
notice: This work is protected by copyright. All rights reserved.
category: private
"
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
