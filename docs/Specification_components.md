## Components

### Components
A **component** is the loadable unit in Rowan.
A **component** specifies a complete and independent unit of functionality.
A **component** has a name and is located on disk in the *components directory*, specified by the *components path* of the *project specification*.

A **component** is composed of:
- A list of *dependent project names*. When a **component** is loaded, the *dependent projects* are loaded as well.
- A *preload doit name*. 
The *preload doit* is performed before the **component** itself is loaded.
- A *postload doit name*. 
The *postload doit* is performed after the **component** itself is loaded.
- A list of *package names*. 
When a **component** is loaded, the listed packages are loaded.
- A list of *component names*. 
When a **component** is loaded, the packages listed by the referenced components are loaded.
The list may reference **components** or **subcomponents** by name and recursive references (direct or indirect) are allowed.

An active **component** is visible in a code browser and as such provides organization for classes within the browser.
#### Components and Tests
A **component** should have an associated set of *tests* that validates the public API for the component.
As a loadable unit there is an implied contract between the developers and users of the component that the public API implemented by the classes and methods loaded by the component and the test* are expected to validate that contract.

The tests may be defined by packages named in the component itself, or they may be defined by packages named in subcomponents of the component. It is recommended that test* be defined in one or more `Tests` subcomponents that are conditional on a `test` *conditional attribute*. In this way end users can control whether or not they want tests loaded into an image.
### Subcomponents
A **subcomponent** has a name and is located on disk in the *components directory* or a subdirectory of the *components directory*. If the **subcomponent** is located in a subdirectory, then the name of the **subcomponent** must be the path to the **subcomponent** relative to the *components directory*.

Similar to the **component**, a **subcomponent** is composed of:
- A list of *dependent project names*.
- A *preload doit name*. 
- A *postload doit name*. 
- A list of *package names*. 
- A list of *component names*.

An `active` **subcomponent** may be visible in a code browser and as such can be used to provide finer grained organization for classes within the browser, than that provided by the **components**.

#### Conditional Subcomponents
A **subcomponent** has a *condition* associated with it. 
A *condition* is a single *conditional attribute*.

If a **subcomponent** is referenced either directly or indirectly from a **component** that is slated to be loaded (i.e., listed in the *component names* of the *load specification* used to *resolve* the *project*), the **subcomponent** *condition* is matched against the *platform conditional attributes* and if there is a match, then the **subcomponent** will be `active` and it's packages, components, doits and projects will participate in the load.

A **subcomponent** that references another **subcomponent** via its list of *component names* is an effective `AND` of the two *conditions* for the **referenced subcomponent**

#### Subcomponents and Tests
A **subcomponent** may only be loaded via a direct or indirect reference from a **component**.
There is no implied API for a **subcomponent**, so no associated *test* are expected.

### LeafComponents
A **leafComponent** is a special case of **subcomponent**, where the *condition* is a collection of *conditional attributes* and/or *platform version patterns*.

[TO BE CONTINUED]

###Component directory structure
As mentioned earlier, it is expected that **components** be located in the *components directory*. It then follows that **subcomponents** should be located in subdirectories, to segregate the **subcomponents** and **components**.
Since **subcomponents** have a *condition* it makes sense to organize **subcomponents** by the value of their *conditions*
