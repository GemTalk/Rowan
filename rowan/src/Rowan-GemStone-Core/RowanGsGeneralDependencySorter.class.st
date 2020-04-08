"
Copied from CypressGsGeneralDependencySorter
"
Class {
	#name : 'RowanGsGeneralDependencySorter',
	#superclass : 'Object',
	#instVars : [
		'candidates',
		'dependsOnConverter',
		'dependentConverter',
		'individualDependencyMap',
		'dependencyGraphs',
		'candidateAliasMap'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RowanGsGeneralDependencySorter class >> on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock [
	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.
	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.
	 anotherOneArgBlock is used to evaluate the key of the candidate itself."

	^self new
		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;
		yourself.

]

{ #category : 'sorting - private' }
RowanGsGeneralDependencySorter >> determineGraphRoots [
  ^ dependencyGraphs
    selectAssociations: [ :each | (candidateAliasMap includesKey: each key) not ]

]

{ #category : 'initializing - private' }
RowanGsGeneralDependencySorter >> initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock [

	candidates := someCandidates.
	dependsOnConverter := aOneArgBlock.
	dependentConverter := anotherOneArgBlock.
	individualDependencyMap := Dictionary new.
	dependencyGraphs := Dictionary new.
	candidateAliasMap := Dictionary new

]

{ #category : 'sorting' }
RowanGsGeneralDependencySorter >> inOrder [
  | sorted sortedRoots |
  sorted := OrderedCollection new.
  self mapCandidatesIntoGraphs.
  sortedRoots := SortedCollection sortBlock: [ :a :b | a key <= b key ].
  self determineGraphRoots associationsDo: [ :assoc | sortedRoots add: assoc ].
  sortedRoots do: [ :assoc | self transcribeGraph: assoc value into: sorted ].
  ^ sorted

]

{ #category : 'sorting - private' }
RowanGsGeneralDependencySorter >> mapCandidatesIntoGraphs [

	| dependsOnKey dependentKey |
	candidates do: 
			[:each |
			| individualDependency |
			dependsOnKey := dependsOnConverter value: each.
			dependentKey := dependentConverter value: each.
			candidateAliasMap at: dependentKey put: each.
			individualDependencyMap at: dependsOnKey ifAbsentPut: [Dictionary new].
			individualDependencyMap at: dependentKey ifAbsentPut: [Dictionary new].
			individualDependency := individualDependencyMap
						associationAt: dependsOnKey.
			(dependencyGraphs includesKey: dependsOnKey)
				ifFalse: [dependencyGraphs add: individualDependency].
			individualDependency value
				add: (individualDependencyMap associationAt: dependentKey)]

]

{ #category : 'sorting - private' }
RowanGsGeneralDependencySorter >> transcribeGraph: subtree into: sorted [
  (subtree keys asSortedCollection: [ :a :b | a <= b ])
    do: [ :name | | subsubtree |
      subsubtree := subtree at: name.
      sorted add: (candidateAliasMap at: name).
      self transcribeGraph: subsubtree into: sorted ]

]
