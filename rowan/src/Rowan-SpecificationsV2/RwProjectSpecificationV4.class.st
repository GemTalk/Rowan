"
RwProjectSpecificationV4 is a project specification that is use by Rowan V3.4.0 and beyond to specify project-specific attributes for Rowan.

Projects managed using this class write tonel files that conform to the tonel spec. Specifically this class writes packages sorting methods based  Unicode codePoint sort
"
Class {
	#name : 'RwProjectSpecificationV4',
	#superclass : 'RwProjectSpecificationV3',
	#category : 'Rowan-SpecificationsV2'
}

{ #category : 'accessing' }
RwProjectSpecificationV4 >> methodSortOrder [
	"methods in tonel projects will be sorted in unicode codePoint sort order"

	^ 'codePoint'
]
