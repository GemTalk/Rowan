"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: creating
"
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
