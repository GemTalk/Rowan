"
notice: This work is protected by copyright. All rights reserved.
category: conversion
"
asCypressJson

	| stream |
    	stream := WriteStream on: String new.
    	self writeJsonOn: stream.
	^stream contents
