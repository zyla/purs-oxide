#![allow(unused_must_use)]
use purs_oxide::Diagnostics;
use test_generator::test_resources;

#[test_resources("tests/purs/compile_pass/*.purs")]
fn test_compile_pass(input_filename: &str) -> anyhow::Result<()> {
    env_logger::builder().is_test(true).try_init()?;

    let source = std::fs::read_to_string(input_filename)?;
    let db = &mut purs_oxide::Database::new();

    let module_id = db.add_source_file(input_filename.into(), source)?;
    let errors: Vec<_> =
        purs_oxide::typecheck::typecheck_module::accumulated::<Diagnostics>(db, module_id)
            .iter_mut()
            .map(|d| d.pp(db))
            .collect();

    errors.clone().into_iter().for_each(|d| println!("{}", d));
    assert_eq!(errors, vec![]);

    Ok(())
}
