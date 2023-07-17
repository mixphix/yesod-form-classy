# yesod-form-classy

### Type classes for working with HTML forms in Yesod

This library takes a different approach than `yesod-form` to handling user input from HTML forms. Using data kinds, type classes, and type families, it allows the programmer

1. to specify the shape of the result based on whether the input is optional or multiple;
2. to minimize the number of abstractions they must be aware of, by passing only a list of attributes instead of dealing with `FieldSettings` and `FieldView`s;
3. to uniquely describe the behaviour of an input field based on the expected type.

This way the programmer can spend less time jostling clumsy abstractions and finding the right `Field` value, and more time fine-tuning the accessibility and user-friendliness of their forms.