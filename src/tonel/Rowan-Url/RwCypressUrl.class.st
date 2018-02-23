"
This class implements the cypress: scheme:

 cypress:/opt/git/shared/repos/rowan/sample/repository/

The cypress: url specifies the file system path to the directory where a disk-based Smalltalk package repository resides. The name comes from the original name used to specifay a cross-platform package disk layout[1].

This url does not encode the specific disk format used by the repository (like the filetree: and tonel: schemes). The disk format is specified in a .cypress file in the directory using STON ike the following:

  { #format : 'filetree' }
  { #format : 'tonel' }
  { #format : 'cypress' }

When the cypress: url is resolved, the appropriate platform-specific repository reader will be used to read the packages from disk.

[1] https://raw.githubusercontent.com/CampSmalltalk/Cypress/master/img/CypressStructure-STIC2012.png
"
Class {
	#name : 'RwCypressUrl',
	#superclass : 'RwFileUrl',
	#category : 'Rowan-Url'
}

{ #category : 'constants' }
RwCypressUrl class >> schemeName [
  ^ 'cypress'
]
