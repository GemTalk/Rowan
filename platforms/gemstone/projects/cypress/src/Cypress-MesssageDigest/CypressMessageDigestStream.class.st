"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressMessageDigestStream',
	#superclass : 'WriteStream',
	#category : 'Cypress-MesssageDigest'
}

{ #category : 'instance creation' }
CypressMessageDigestStream class >> bytes [

	^self on: ByteArray new

]

{ #category : 'instance creation' }
CypressMessageDigestStream class >> characters [

	^self on: String new

]

{ #category : 'Documentation' }
CypressMessageDigestStream class >> comment [
"WriteStreamLegacy has a wired in comment method (in 2.3.15), so we
	need this method to answer correctly provide the comment for this 
	class"
" As of GS/64 3.1, comments are now recorded in the class extraDict
  dictionary under the key #comment.  Comment information formerly
  recorded as a GsClassDocumentation under the key #description are
  converted to a string and placed under #comment during DB 
  conversion/upgrade. "

  | cmt |
  cmt := self _extraDictAt: #comment.
  ^ cmt isNil ifTrue: [ '' ] ifFalse: [ cmt ]

]

{ #category : 'digests' }
CypressMessageDigestStream >> md5sum [

	^self contents md5sum

]

{ #category : 'digests' }
CypressMessageDigestStream >> sha1Sum [

	^self contents sha1Sum

]

{ #category : 'digests' }
CypressMessageDigestStream >> sha256Sum [

	^self contents sha256Sum

]

{ #category : 'digests' }
CypressMessageDigestStream >> sha512Sum [

	^self contents sha512Sum

]
