"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressGsGeneralDependencySorter',
	#superclass : 'Object',
	#instVars : [
		'candidates',
		'dependsOnConverter',
		'dependentConverter',
		'individualDependencyMap',
		'dependencyGraphs',
		'candidateAliasMap'
	],
	#category : 'Cypress-Structure'
}

{ #category : 'instance creation' }
CypressGsGeneralDependencySorter class >> on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock [
	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.
	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.
	 anotherOneArgBlock is used to evaluate the key of the candidate itself."

	^self new
		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;
		yourself.

]

{ #category : 'sorting - private' }
CypressGsGeneralDependencySorter >> determineGraphRoots [
  ^ dependencyGraphs
    selectAssociations: [ :each | (candidateAliasMap includesKey: each key) not ]

]

{ #category : 'initializing - private' }
CypressGsGeneralDependencySorter >> initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock [

	candidates := someCandidates.
	dependsOnConverter := aOneArgBlock.
	dependentConverter := anotherOneArgBlock.
	individualDependencyMap := Dictionary new.
	dependencyGraphs := Dictionary new.
	candidateAliasMap := Dictionary new

]

{ #category : 'sorting' }
CypressGsGeneralDependencySorter >> inOrder [
  | sorted sortedRoots |
  sorted := OrderedCollection new.
  self mapCandidatesIntoGraphs.
  sortedRoots := SortedCollection sortBlock: [ :a :b | a key <= b key ].
  self determineGraphRoots associationsDo: [ :assoc | sortedRoots add: assoc ].
  sortedRoots do: [ :assoc | self transcribeGraph: assoc value into: sorted ].
  ^ sorted

]

{ #category : 'sorting - private' }
CypressGsGeneralDependencySorter >> mapCandidatesIntoGraphs [

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
CypressGsGeneralDependencySorter >> transcribeGraph: subtree into: sorted [
  (subtree keys asSortedCollection: [ :a :b | a <= b ])
    do: [ :name | | subsubtree |
      subsubtree := subtree at: name.
      sorted add: (candidateAliasMap at: name).
      self transcribeGraph: subsubtree into: sorted ]

]
