"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-Definitions-gs
"
asCypressMethodDefinition
	"Try to coerce Unicode source to simple Strings when possible."

	^CypressMethodDefinition
		className: self methodClass theNonMetaClass name
		classIsMeta: self methodClass isMeta
		selector: self selector
		category: self category
		source: self sourceString asString