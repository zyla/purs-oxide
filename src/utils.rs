pub mod tests {
    pub trait DropSalsaId {
        fn drop_salsa_id(&mut self) -> Self;
    }

    impl DropSalsaId for String {
        fn drop_salsa_id(&mut self) -> String {
            self.lines()
                .filter(|l| !l.contains("[salsa id]"))
                .collect::<Vec<_>>()
                .join("\n")
        }
    }
}
