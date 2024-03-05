#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct PSString(pub Vec<PSChar>);

/// Strings in PureScript can contain arbitrary Unicode code points, not just USVs.
/// There are values which don't map to Rust `char`, e.g. `0xD800`.
pub type PSChar = u32;

impl From<&str> for PSString {
    fn from(value: &str) -> Self {
        PSString(value.chars().map(|x| x as PSChar).collect())
    }
}

pub struct NonUsvChar;

impl TryInto<String> for PSString {
    type Error = NonUsvChar;
    fn try_into(self) -> Result<String, Self::Error> {
        self.0
            .iter()
            .map(|x| char::from_u32(*x).ok_or(NonUsvChar))
            .collect()
    }
}

impl PSString {
    pub fn to_string_lossy(&self) -> String {
        self.0
            .iter()
            .map(|x| {
                char::from_u32(*x)
                    .unwrap_or_else(|| todo!("replace this with Unicode replacement character"))
            })
            .collect()
    }
}
