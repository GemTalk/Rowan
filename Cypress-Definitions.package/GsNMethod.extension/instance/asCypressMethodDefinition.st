"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Definitions
"
asCypressMethodDefinition

	^CypressMethodDefinition
		className: self methodClass theNonMetaClass name
		classIsMeta: self methodClass isMeta
		selector: self selector
		category: self category
		source: self sourceString