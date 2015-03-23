type form =
|Lit of int
|And of form*form
|Or of form*form
|Imply of form*form
|Not of form
