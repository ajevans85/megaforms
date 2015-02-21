# Forms API

A forms api for handling the state of a form, mapping values into individual field values, and validation of 
field values.

Very much influenced by the play framework forms api, but without no dependencies except the Scala standard
library.

## Api overview
The forms API describes how data should be mapped and validated in a form. It is entirely decoupled from
the actual presentation tech used (component framework or whatever), but provides a way to map between
complex types (model classes for example) into a flat tree of data values that are suitable for use
together with form fields.

The main public api is available through the immutable class```Form[T]``` and the factory methods for 
mappings available from ```Forms._```.

It also contains a separate uncoupled API for validation, ```Coinstraint[T]``` with predefined constraints in
 the object ```Constraints```.

Example of how to create a mapping and a form instance:
```scala
import megaforms.Forms._
import Constraints._

case class Fruit(name: String)
object Apple extends Fruit("apple")
object Orange extends Fruit("orange")

case class MyModel(id: Int, name: String, fruits: Seq[Fruit])

// a mapping describes how to go to and from individual fields and the type T of the mapping
// it can also contain validation constraints
val mapping: Mapping[MyModel] = mapping(
  "id" -> integer,
  "name" -> text.verifying(nonEmptyText),
  "fruits" -> seq(
    mapping(
      "name" -> text
    )(Fruit)(Fruit.unapply)
  ).verifying(Constraint(fs => if (fs.isEmpty) Option("You must have at least one fruit") else None))
)

val form: Form[MyModel] = Form(mapping)
```

Given the form from the example above we can then fill it with data from an instance of ```MyModel```:

```scala

val myModel = MyModel(1, "Gadget", Seq(Apple, Orange)
val formWithInitialData = form.fill(myModel)

// the form state is now:
Form(
  mapping = mapping,
  value = Some(myModel),
  data = Map("id" -> 1, "name" -> "Gadget", "fruits.[0].name" -> "apple", "fruits.[1].name" -> "orange"),
  errors = Seq()
)
```

Interfacing the form from a form component or integration with a ui framework of some kind, you work against
the data map, preferably indirectly, like this:
```scala 
val changedForm = form.update("name", "New Name")

// the changed form is now
Form(
  mapping = mapping,
  value = Some(new model with updated name),
  data = Map("id" -> 1, "name" -> "New Name", "fruits.[0].name" -> "apple", "fruits.[1].name" -> "orange"),
  errors = Seq()
)
```

Updating a value also performs validation, so if we would update the form with data that makes 
invalid:
```scala
val invalidForm = form.update("name", "") // we had a constraint nonEmptyText

// the changed form is now
Form(
  mapping = mapping,
  value = None, // because the data is invalid
  data = Map("id" -> 1, "name" -> "", "fruits.[0].name" -> "apple", "fruits.[1].name" -> "orange"),
  errors = Seq(FormError("name", "Field cannot be empty"))
```

For more details, check out the unit tests and docs on individual classes and objects.
  