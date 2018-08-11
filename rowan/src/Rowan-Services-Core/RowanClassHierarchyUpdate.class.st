Class {
	#name : 'RowanClassHierarchyUpdate',
	#superclass : 'RowanClassesUpdate',
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> addHierarchyFor: aClass [
	classNames add: ((self hierarchyFor: aClass) collect:[:class | class name]).

]

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> addSelectionHierarchyFor: aClass [
	self selections addAll: ((self hierarchyFor: aClass) collect:[:class | class asString])

]

{ #category : 'accessing' }
RowanClassHierarchyUpdate >> allClasses [

	| allClasses |
 
	allClasses := Set new. 
	classNames do:[:hierarchy | 
		allClasses addAll: (hierarchy collect: [:name | self objectInBaseNamed: name])]. 
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
	
	classNames do:[:hierarchy |
		hierarchy do:[:name | 
			self writeClass: (self objectInBaseNamed: name) to: writeStream].
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
