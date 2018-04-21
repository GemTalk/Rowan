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
