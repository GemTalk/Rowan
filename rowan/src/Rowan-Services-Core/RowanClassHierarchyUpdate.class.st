Class {
	#name : 'RowanClassHierarchyUpdate',
	#superclass : 'RowanClassesUpdate',
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> addHierarchyFor: aClass [
	classes add: (self hierarchyFor: aClass).

]

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> addSelectionHierarchyFor: aClass [
	selections addAll: (self hierarchyFor: aClass)

]

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> allClasses [

	| allClasses |
 
	allClasses := Set new. 
	classes do:[:hierarchy | 
		allClasses addAll: hierarchy]. 
	^allClasses asArray

]

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> hierarchyFor: aClass [
	| currentClass hierarchy |
	currentClass := aClass.
	hierarchy := Array new. 
	[currentClass notNil] whileTrue: 
			[hierarchy add: currentClass.
			currentClass := currentClass superclass].
	^hierarchy

]

{ #category : 'jade' }
RowanClassHierarchyUpdate >> writeHierarchyTo: writeStream [
	
	classes do:[:hierarchy |
		hierarchy do:[:aClass | 
			self writeClass: aClass to: writeStream].
		writeStream lf].
	writeStream nextPut: $%; lf.

]

{ #category : 'jade' }
RowanClassHierarchyUpdate >> writeSelectionHierarchyTo: writeStream [
	
	super writeSelectionsTo: writeStream

]

{ #category : 'jade' }
RowanClassHierarchyUpdate >> writeTestCaseInfoTo: writeStream [
	"ignore test case info - method going away"
	writeStream nextPutAll: 'false';
		tab;
		lf
]
