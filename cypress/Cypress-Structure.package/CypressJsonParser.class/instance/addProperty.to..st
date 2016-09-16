"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: adding
"
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
