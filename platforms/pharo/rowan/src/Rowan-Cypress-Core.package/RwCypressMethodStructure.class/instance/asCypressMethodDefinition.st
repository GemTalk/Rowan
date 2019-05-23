converting
asCypressMethodDefinition

	"Try to coerce Unicode source to simple Strings when possible."

	^ RwCypressMethodDefinition
		className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source asString