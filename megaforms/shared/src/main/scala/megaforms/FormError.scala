package megaforms

/**
 * @param path The mapping key that had the error, might be empty if the error is top level
 * @param message String with error message (possibly localized i18n key in the future?)
 */
final case class FormError(path: String, message: String)