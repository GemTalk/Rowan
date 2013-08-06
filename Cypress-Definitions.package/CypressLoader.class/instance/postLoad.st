"
notice: This work is protected by copyright. All rights reserved.
category: loading
"
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
