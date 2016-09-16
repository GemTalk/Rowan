"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassInitializersOn: aStream

	self fileOutClassInitializersPreambleOn: aStream.
	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].
	self fileOutClassInitializersPostambleOn: aStream.
