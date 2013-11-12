" Package: Cypress-MesssageDigest"!


" Class Declarations "!

(WriteStream
	subclass: 'MessageDigestStream'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-MesssageDigest';
		comment: ''!

" Class Implementation for MessageDigestStream"!

" ------------------- Class methods for MessageDigestStream"!

!MessageDigestStream class methodsFor: 'instance creation'!
bytes

	^self on: ByteArray new! !
!MessageDigestStream class methodsFor: 'instance creation'!
characters

	^self on: String new! !
" ------------------- Instance methods for MessageDigestStream"!

!MessageDigestStream methodsFor: 'digests'!
md5sum

	^self contents md5sum! !
!MessageDigestStream methodsFor: 'digests'!
sha1Sum

	^self contents sha1Sum! !
!MessageDigestStream methodsFor: 'digests'!
sha256Sum

	^self contents sha256Sum! !
!MessageDigestStream methodsFor: 'digests'!
sha512Sum

	^self contents sha512Sum! !
" Class Extensions "!

" Class Extension for CypressStructure"!

" ------------------- Instance methods for CypressStructure"!

!CypressStructure methodsFor: '*Cypress-MesssageDigest'!
addToDigest: aMessageDigestStream

	self subclassResponsibility: #addToDigest:! !
!CypressStructure methodsFor: '*Cypress-MesssageDigest'!
digest
	"Answer a digest of the receiver, unless it is a skeleton (never populated).
	 The digest is not constrained, but is typically a ByteArray or an Integer.
	 In the case of a skeleton, answer nil so there is a distinction between
	 no such package and an empty package."

	| stream |
	self isSkeleton ifTrue: [^nil].
	stream := MessageDigestStream characters.
	self addToDigest: stream.
	^stream md5sum! !
!CypressStructure methodsFor: '*Cypress-MesssageDigest'!
isSkeleton

	^self subclassResponsibility: #isSkeleton! !
" Class Extension for CypressClassStructure"!

" ------------------- Instance methods for CypressClassStructure"!

!CypressClassStructure methodsFor: '*Cypress-MesssageDigest'!
addToDigest: aMessageDigestStream

	aMessageDigestStream
		tab; tab; nextPutAll: self class name; cr;
		tab; tab; tab; nextPutAll: 'extension:'; nextPutAll: self isClassExtension printString; cr;
		tab; tab; tab; nextPutAll: 'comment:'; nextPutAll: self comment; cr;
		tab; tab; tab; nextPutAll: 'properties:'; cr;
		tab; tab; tab; tab.
	self properties writeCypressJsonOn: aMessageDigestStream indent: 4.
	aMessageDigestStream
		cr;
		tab; tab; tab; nextPutAll: 'class methods:'; cr.
	(self classMethods asSortedCollection: [:a :b | (a isMetaclass printString, a selector) < (b isMetaclass printString, b selector)])
		do: [:each | each addToDigest: aMessageDigestStream].
	aMessageDigestStream
		tab; tab; tab; nextPutAll: 'instance methods:'; cr.
	(self instanceMethods asSortedCollection: [:a :b | (a isMetaclass printString, a selector) < (b isMetaclass printString, b selector)])
		do: [:each | each addToDigest: aMessageDigestStream].
! !
!CypressClassStructure methodsFor: '*Cypress-MesssageDigest'!
isSkeleton

	^instanceMethods isNil
		and: [classMethods isNil
		and: [comment isNil
		and: [isClassExtension isNil]]]! !
" Class Extension for CypressPackageStructure"!

" ------------------- Instance methods for CypressPackageStructure"!

!CypressPackageStructure methodsFor: '*Cypress-MesssageDigest'!
addToDigest: aMessageDigestStream

	aMessageDigestStream
		nextPutAll: self class name; cr;
		tab; nextPutAll: 'name:'; nextPutAll: self name; cr;
		tab; nextPutAll: 'properties:'; cr;
		tab; tab.
	self properties writeCypressJsonOn: aMessageDigestStream indent: 2.
	aMessageDigestStream
		cr;
		tab; nextPutAll: 'classes:'; cr.
	(self classes asSortedCollection: [:a :b | a name < b name])
		do: [:each | each addToDigest: aMessageDigestStream].
	aMessageDigestStream
		tab; nextPutAll: 'extensions:'; cr.
	(self extensions asSortedCollection: [:a :b | a name < b name])
		do: [:each | each addToDigest: aMessageDigestStream].
! !
!CypressPackageStructure methodsFor: '*Cypress-MesssageDigest'!
isSkeleton

	^(properties isNil or: [properties isEmpty])
		and: [classes isNil
		and: [extensions isNil]]! !
" Class Extension for CypressMethodStructure"!

" ------------------- Instance methods for CypressMethodStructure"!

!CypressMethodStructure methodsFor: '*Cypress-MesssageDigest'!
addToDigest: aMessageDigestStream

	aMessageDigestStream
		tab; tab; tab; tab; nextPutAll: self class name; cr;
		tab; tab; tab; tab; tab; nextPutAll: self selector; cr;
		tab; tab; tab; tab; tab; nextPutAll: 'properties:'; cr;
		tab; tab; tab; tab; tab; tab.
	self properties writeCypressJsonOn: aMessageDigestStream indent: 6.
	aMessageDigestStream
		cr;
		tab; tab; tab; tab; nextPutAll: 'source:'; nextPutAll: self source; cr.
! !
!CypressMethodStructure methodsFor: '*Cypress-MesssageDigest'!
isSkeleton

	^source isNil
		and: [classStructure isNil
		and: [isMetaclass isNil]]! !
" Class initializers "!




" End of Package: Cypress-MesssageDigest"!


