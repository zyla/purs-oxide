#[salsa::interned]
pub struct Symbol {
    #[return_ref]
    pub text: String,
}
