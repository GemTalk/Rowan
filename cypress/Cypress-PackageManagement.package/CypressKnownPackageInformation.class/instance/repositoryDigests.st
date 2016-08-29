"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: Accessing - digests
"
repositoryDigests

	^self repositories collect: [:each | self digestFor: each url]