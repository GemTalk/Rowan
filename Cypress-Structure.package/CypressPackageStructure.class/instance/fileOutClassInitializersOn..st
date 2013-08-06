"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutClassInitializersOn: aStream

	self fileOutClassInitializersPreambleOn: aStream.
	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].
	self fileOutClassInitializersPostambleOn: aStream.
