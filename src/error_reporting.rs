use codespan_reporting::diagnostic::Label;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFile};

use crate::source_location::SourceLocation;

pub trait CompilerError {
    fn new(message: &str, location: SourceLocation) -> Self;
    fn location(&self) -> SourceLocation;
    fn message(&self) -> &str;
    fn error_type(&self) -> &str;
}

pub fn errors_from_file(filename: &str, text: &str, errors: Vec<impl CompilerError>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(filename, text);
    for error in errors {
        let start = error.location().start;
        let end = error.location().end;
        let diagnostic: Diagnostic<()> = Diagnostic::error()
            .with_message(error.message())
            .with_code(error.error_type())
            .with_labels(vec![
                Label::primary((), start..end).with_message(error.message())
            ]);

        let _emit = term::emit(&mut writer.lock(), &config, &file, &diagnostic);
    }
}
