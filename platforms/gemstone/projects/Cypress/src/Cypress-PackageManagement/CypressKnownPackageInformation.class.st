"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressKnownPackageInformation',
	#superclass : 'CypressAbstractPackageInformation',
	#instVars : [
		'repositories',
		'digests'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'Instance Creation' }
CypressKnownPackageInformation class >> fromUnknown: aPackageInformation [

	^self named: aPackageInformation name

]

{ #category : 'Updating' }
CypressKnownPackageInformation >> addRepository: aRepository [

	self repositories at: aRepository url put: aRepository.
	self updateDigestsFromImageAndRepository: aRepository.

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> determineDigestFromImage [

	^self packageStructure digest

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> determineDigestFromRepository: aRepository [

	^ (aRepository readPackageStructureForPackageNamed: self name) digest.

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> digestFor: source [

	^self digestFor: source or: [nil]

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> digestFor: source or: aBlock [

	^self digests at: source ifAbsent: aBlock

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> digests [

	^digests

]

{ #category : 'Initializing - private' }
CypressKnownPackageInformation >> digests: anIdentityDictionary [
	"Key: Source of package definitions (#IMAGE or a Repository url)
	 Value: (e.g. MD5) Digest of the package's defintions or an empty string.
	 A nil digest means there are no definitions for the given source."

   digests := anIdentityDictionary

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> imageDigest [

	^self digestFor: #IMAGE.

]

{ #category : 'Initializing - private' }
CypressKnownPackageInformation >> initialize [

	super initialize.
	self
		repositories: IdentityDictionary new;
		digests: IdentityDictionary new.

]

{ #category : 'Testing' }
CypressKnownPackageInformation >> isKnown [

	^true

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> packageStructure [

	^CypressPackageStructure
		fromPackage: (CypressPackageDefinition named: self name)

]

{ #category : 'Updating' }
CypressKnownPackageInformation >> removeRepository: aRepository [

	self repositories removeKey: aRepository url ifAbsent: [].
	self digests removeKey: aRepository url ifAbsent: [].
	self updateDigestsFromImage.

]

{ #category : 'Accessing' }
CypressKnownPackageInformation >> repositories [

	^repositories

]

{ #category : 'Initializing - private' }
CypressKnownPackageInformation >> repositories: anIdentityDictionary [
	"Key: Repository url
	 Value: Repository"

	repositories := anIdentityDictionary

]

{ #category : 'Accessing - digests' }
CypressKnownPackageInformation >> repositoryDigests [

	^self repositories collect: [:each | self digestFor: each url]

]

{ #category : 'Updating - digests' }
CypressKnownPackageInformation >> updateDigestsFromAllRepositories [

	self repositories do: [:each | self updateDigestsFromRepository: each]

]

{ #category : 'Updating - digests' }
CypressKnownPackageInformation >> updateDigestsFromImage [

	self digests at: #IMAGE put: self determineDigestFromImage

]

{ #category : 'Updating - digests' }
CypressKnownPackageInformation >> updateDigestsFromImageAndAllRepositories [

	self
		updateDigestsFromImage;
		updateDigestsFromAllRepositories

]

{ #category : 'Updating - digests' }
CypressKnownPackageInformation >> updateDigestsFromImageAndRepository: aRepository [

	self
		updateDigestsFromImage;
		updateDigestsFromRepository: aRepository

]

{ #category : 'Updating - digests' }
CypressKnownPackageInformation >> updateDigestsFromRepository: aRepository [

	self digests at: aRepository url put: (self determineDigestFromRepository: aRepository).

]

{ #category : 'Writing' }
CypressKnownPackageInformation >> writeChangesToAllRepositories [

	| imageDigest changedDigests changedRepositories |
	self updateDigestsFromImageAndAllRepositories.
	imageDigest := self imageDigest.
	changedDigests := self repositoryDigests reject: [:each | each = imageDigest].
	changedRepositories := changedDigests keys collect: [:each | self repositories at: each].
	self writePackageToRepositories: changedRepositories.

]

{ #category : 'Writing' }
CypressKnownPackageInformation >> writePackageToRepositories: someRepositories [

	| packageStructure |
	packageStructure := self packageStructure.
	^someRepositories
		do: [:each | each writePackageStructure: packageStructure]

]
