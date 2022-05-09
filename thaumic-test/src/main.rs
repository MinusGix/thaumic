use thaumic::thaumic;

// Parse the structure as a thaumic structure
// Note that this means that normal field access is _not_ necessarily possible!
#[thaumic]
// This causes it to be allocated on the heap automatically
// This is required for certain features, such as significant parts of DST since we can't forge fat
// pointers
#[alloc]
struct DynamicSizedData {
    // Tell it how to store the dst
    // This tells it store the length somewhere (left undefined) before any dynamically
    // sized values.
    // This is more important in structures with multiple dsts, as if you did
    // `length=prefix` (where it is put before), there would be more dynamic
    // calculations to get the value
    id: u32,

    // There are several different fields for a dst
    // There's the length, which tells it what type the length is, and where to store it
    // Then there is the default value (which by default is Default::default) which has to
    // be filled in for `length` bytes
    #[dst()]
    // default_value = 2;
    // access_name = ""; // gives it a blank access name, so `get()` rather than `get_values()`
    // access_name = "value" // `get_value()` the default is simply the name of the field
    // raw_access = false; // whether to generate methods to get a raw pointer
    // mutable_access = true; // whether to generate functions to modify it
    values: [u64],
    // other: [u32],
}
// This would approximately generate
// struct DynamicSizedDataReal {
//     length: u16,
//     values: [u64]
// }
// However, it is not the same, as ours allows more direct construction of it
// (even from data like vectors, though that involves copying)
// and ours would store a pointer to the data
// This is because we don't know the size and so we have to allocate it.
// Currently we can't actually even represent it as `DynamicSizedDataReal` as that requires creating
// a fat pointer and we can't do that.
// Thaumic, however, does try to minimize heap allocations if possible. So, if it ever becomes
// possible to create a real owned version of it, then Thaumic will use that.

// Ideas:
// - #[from(Vec/Iter/Whatever)] (and a try_from version? autogenerate that for from for non-usize lengths?)
// - We can hide internal functions with a trait in the generated private module
// - We could have a weird embedding thing, since theoretically with a type name, we can get the info needed to embed it.
//     #[thaumic_embed] on the field and it is integrated into the surrounding thaumic data
//     if it implements thaumic
//     though, maybe we should only allow that if the original type has an explicit #[allow_embedding?]

fn main() {
    println!("Hello, world!");
    let value = DynamicSizedData::new_with(42, [4, 2, 3].into_iter()).unwrap();
}
