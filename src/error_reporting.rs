use codespan_reporting::diagnostic::Label;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFile};

use crate::parser::ParseErrorList;

pub fn errors_from_file(filename: &str, text: &str, errors: ParseErrorList) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let file = SimpleFile::new(filename, text);

    for error in errors {
        let start = error.location.start;
        let end = error.location.end;
        let diagnostic: Diagnostic<()> = Diagnostic::error()
            .with_message(error.to_string())
            .with_code("ParseError")
            .with_labels(vec![Label::primary((), start..end)]);

        let _emit = term::emit(&mut writer.lock(), &config, &file, &diagnostic);
    }
}
