namespace ImmutableHashCollections


/// Reprensents a element operation (Set/Remove) without its key.
/// Typically datastructures will hold (key * ElementOperation) tuples.
[<Struct>]
type ElementOperation<'T> =
    /// Set the associated key to a specific value.
    | Set of 'T
    /// Remove the associated key.
    | Remove