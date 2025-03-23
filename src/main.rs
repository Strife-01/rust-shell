#[allow(unused_imports)]
use std::{
    self, env, fmt, fs,
    io::{self, Write},
    path::Path,
    process,
};

use walkdir::WalkDir;

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Context;
use rustyline::Helper;
use rustyline::{Editor, Result};

static mut IS_BELL_RANGING: bool = false;
static mut SHOWN_COMPLETIONS: bool = false;

struct MyCompleter {
    commands: Vec<String>,
}

impl Helper for MyCompleter {}
impl Hinter for MyCompleter {
    type Hint = String;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<String> {
        None // Simple implementation that doesn't provide hints
    }
}
impl Highlighter for MyCompleter {
    /* ... */
}
impl Validator for MyCompleter {
    /* ... */
}

impl Completer for MyCompleter {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &rustyline::Context<'_>,
    ) -> Result<(usize, Vec<Pair>)> {
        let (start, word) = find_word_at_pos(line, pos);

        if start == 0 {
            let mut command_matches: Vec<String> = self
                .commands
                .iter()
                .filter(|cmd| cmd.starts_with(word))
                .cloned()
                .collect();
            command_matches.sort();

            let mut path_matches = find_files_in_path_PATH(word);
            path_matches.sort_by(|a, b| a.replacement.cmp(&b.replacement));

            let path_exec_names: Vec<String> = path_matches
                .iter()
                .map(|pair| pair.replacement.trim_end().to_string())
                .collect();

            let mut all_matches = Vec::new();
            all_matches.extend(command_matches.iter().cloned());
            all_matches.extend(path_exec_names.iter().cloned());

            let mut unique = std::collections::HashSet::new();
            all_matches = all_matches
                .into_iter()
                .filter(|name| unique.insert(name.clone()))
                .collect();

            if !all_matches.is_empty() {
                let common_prefix = find_longest_common_prefix(&all_matches);

                if common_prefix.len() > word.len() {
                    let is_complete_match = all_matches.contains(&common_prefix);

                    let has_prefix_matches = all_matches
                        .iter()
                        .any(|m| m.starts_with(&common_prefix) && *m != common_prefix);

                    if is_complete_match && !has_prefix_matches {
                        return Ok((
                            start,
                            vec![Pair {
                                display: common_prefix.clone() + " ",
                                replacement: common_prefix + " ",
                            }],
                        ));
                    } else {
                        return Ok((
                            start,
                            vec![Pair {
                                display: common_prefix.clone(),
                                replacement: common_prefix,
                            }],
                        ));
                    }
                }
            }

            let mut match_pairs: Vec<Pair> = all_matches
                .clone()
                .into_iter()
                .map(|m| {
                    let is_prefix_of_others = all_matches
                        .iter()
                        .any(|other| other.starts_with(&m) && *other != m);

                    if !is_prefix_of_others {
                        Pair {
                            display: m.clone() + " ",
                            replacement: m + " ",
                        }
                    } else {
                        Pair {
                            display: m.clone(),
                            replacement: m,
                        }
                    }
                })
                .collect();

            let mut unique_pairs = std::collections::HashSet::new();
            match_pairs = match_pairs
                .into_iter()
                .filter(|pair| unique_pairs.insert(pair.replacement.clone()))
                .collect();

            if match_pairs.len() == 1 {
                unsafe {
                    IS_BELL_RANGING = false;
                    SHOWN_COMPLETIONS = false;
                }
                return Ok((start, match_pairs));
            }

            unsafe {
                if !match_pairs.is_empty() {
                    if IS_BELL_RANGING && !SHOWN_COMPLETIONS {
                        println!(
                            "\n{}",
                            match_pairs
                                .iter()
                                .map(|pair| pair.display.clone())
                                .collect::<Vec<_>>()
                                .join(" ")
                                .trim()
                        );

                        SHOWN_COMPLETIONS = true;

                        return Ok((
                            start,
                            vec![Pair {
                                display: word.to_string(),
                                replacement: word.to_string(),
                            }],
                        ));
                    } else if SHOWN_COMPLETIONS {
                        return Ok((start, match_pairs));
                    } else {
                        IS_BELL_RANGING = true;
                        return Ok((start, Vec::new()));
                    }
                } else {
                    if IS_BELL_RANGING {
                        IS_BELL_RANGING = false;
                    } else {
                        IS_BELL_RANGING = true;
                        return Ok((start, Vec::new()));
                    }

                    return Ok((
                        start,
                        vec![Pair {
                            display: word.to_string(),
                            replacement: word.to_string(),
                        }],
                    ));
                }
            }
        }

        let dir = std::path::Path::new(".");

        let matches: Vec<Pair> = std::fs::read_dir(dir)
            .unwrap_or_else(|_| return std::fs::read_dir(".").unwrap())
            .filter_map(|result| result.ok())
            .map(|entry| entry.file_name().to_string_lossy().to_string())
            .filter(|name| name.starts_with(word))
            .map(|name| {
                let path = std::path::Path::new(".").join(name.clone());
                if path.is_dir() {
                    Pair {
                        display: name.clone() + "/",
                        replacement: name.clone() + "/",
                    }
                } else {
                    let is_unique = std::fs::read_dir(dir)
                        .unwrap_or_else(|_| std::fs::read_dir(".").unwrap())
                        .filter_map(|r| r.ok())
                        .map(|e| e.file_name().to_string_lossy().to_string())
                        .filter(|n| n.starts_with(word))
                        .count()
                        == 1;

                    if is_unique {
                        Pair {
                            display: name.clone() + " ",
                            replacement: name.clone() + " ",
                        }
                    } else {
                        Pair {
                            display: name.clone(),
                            replacement: name.clone(),
                        }
                    }
                }
            })
            .collect();

        if matches.len() == 1 {
            unsafe {
                IS_BELL_RANGING = false;
                SHOWN_COMPLETIONS = false;
            }
            return Ok((start, matches));
        }

        unsafe {
            if !matches.is_empty() {
                if IS_BELL_RANGING && !SHOWN_COMPLETIONS {
                    println!(
                        "\n{}",
                        matches
                            .iter()
                            .map(|pair| pair.display.clone())
                            .collect::<Vec<_>>()
                            .join(" ")
                            .trim()
                    );

                    SHOWN_COMPLETIONS = true;

                    return Ok((
                        start,
                        vec![Pair {
                            display: word.to_string(),
                            replacement: word.to_string(),
                        }],
                    ));
                } else if SHOWN_COMPLETIONS {
                    return Ok((start, matches));
                } else {
                    IS_BELL_RANGING = true;
                    return Ok((start, Vec::new()));
                }
            } else {
                if IS_BELL_RANGING {
                    IS_BELL_RANGING = false;
                } else {
                    IS_BELL_RANGING = true;
                    return Ok((start, Vec::new()));
                }

                return Ok((
                    start,
                    vec![Pair {
                        display: word.to_string(),
                        replacement: word.to_string(),
                    }],
                ));
            }
        }
    }
}

fn find_longest_common_prefix(strings: &[String]) -> String {
    if strings.is_empty() {
        return String::new();
    }

    if strings.len() == 1 {
        return strings[0].clone();
    }

    let first = &strings[0];
    let mut prefix_len = first.len();

    for string in strings.iter().skip(1) {
        let mut i = 0;
        let max_check = std::cmp::min(prefix_len, string.len());

        while i < max_check && first.as_bytes()[i] == string.as_bytes()[i] {
            i += 1;
        }

        prefix_len = std::cmp::min(prefix_len, i);

        if prefix_len == 0 {
            break;
        }
    }

    first[..prefix_len].to_string()
}


#[allow(non_snake_case)]
fn find_files_in_path_PATH(word: &str) -> Vec<Pair> {
    let mut commands = Vec::new();
    if let Ok(paths) = env::var("PATH") {
        for path in env::split_paths(&paths) {
            for entry in WalkDir::new(path)
                .max_depth(1)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if let Ok(metadata) = entry.metadata() {
                    if metadata.is_file() && entry.file_type().is_file() {
                        if let Some(file_name) = entry.file_name().to_str() {
                            if file_name.starts_with(word) && file_name.trim() != word.trim() {
                                commands.push(Pair {
                                    display: file_name.to_string() + " ",
                                    replacement: file_name.to_string() + " ",
                                });
                            }
                        }
                    }
                }
            }
        }
    }
    commands
}

fn find_word_at_pos(line: &str, pos: usize) -> (usize, &str) {
    let mut start = pos;
    while start > 0 {
        start -= 1;
        if line.chars().nth(start).unwrap_or(' ') == ' ' {
            start += 1;
            break;
        }
    }
    (start, &line[start..pos])
}

#[allow(dead_code)]
#[derive(Clone)]
enum ParserState {
    Normal,
    SingleQuote,
    DoubleQuote,
    Escaped,
}

struct Redirect {
    file_descriptor: u32,
    file_path: String,
    append: bool,
}

fn ensure_parent_directory_exists(file_path: &str) -> io::Result<()> {
    if let Some(parent) = Path::new(file_path).parent() {
        if !parent.exists() {
            fs::create_dir_all(parent)?;
        }
    }
    Ok(())
}

fn tokenize(input: &str) -> std::result::Result<Vec<String>, String> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut state = ParserState::Normal;
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match state {
            ParserState::Normal => match c {
                ' ' | '\t' if !current_token.is_empty() => {
                    tokens.push(current_token);
                    current_token = String::new();
                }
                ' ' | '\t' => {}
                '\'' => state = ParserState::SingleQuote,
                '"' => state = ParserState::DoubleQuote,
                '\\' => {
                    if let Some(next_char) = chars.next() {
                        current_token.push(next_char);
                    } else {
                        return Err(String::from("Trailing backslash"));
                    }
                }
                _ => current_token.push(c),
            },
            ParserState::SingleQuote => match c {
                '\'' => {
                    state = ParserState::Normal;
                }
                _ => current_token.push(c),
            },
            ParserState::DoubleQuote => match c {
                '"' => {
                    state = ParserState::Normal;
                }
                '\\' => {
                    if let Some(next_char) = chars.next() {
                        match next_char {
                            '"' | '\\' | '$' | '`' => {
                                current_token.push(next_char);
                            }
                            _ => {
                                current_token.push('\\');
                                current_token.push(next_char);
                            }
                        }
                    } else {
                        return Err(String::from("Trailing backslash in double quotes"));
                    }
                }
                _ => current_token.push(c),
            },
            ParserState::Escaped => {
                state = ParserState::Normal;
            }
        }
    }

    match state {
        ParserState::SingleQuote => return Err(String::from("Unclosed single quote")),
        ParserState::DoubleQuote => return Err(String::from("Unclosed double quote")),
        _ => {}
    }

    if !current_token.is_empty() {
        tokens.push(current_token);
    }

    Ok(tokens)
}

#[derive(Debug)]
enum CommandResult {
    Success(String),
    Exit(i32),
    Error(String),
    Empty,
}

trait Command {
    fn execute(&self, args: &[&str]) -> CommandResult;
    fn name(&self) -> &str;
}

struct EchoCommand;
struct ExitCommand;
struct TypeCommand;
struct PwdCommand;
struct CdCommand;

impl Command for EchoCommand {
    fn execute(&self, args: &[&str]) -> CommandResult {
        CommandResult::Success(args.join(" "))
    }

    fn name(&self) -> &str {
        "echo"
    }
}

impl Command for ExitCommand {
    fn execute(&self, args: &[&str]) -> CommandResult {
        if args.is_empty() {
            return CommandResult::Exit(0);
        }

        if args.len() > 1 {
            return CommandResult::Error("exit: too many args".to_string());
        }

        match args[0].parse::<i32>() {
            Ok(code) => CommandResult::Exit(code),
            Err(_) => CommandResult::Error(format!("exit: {}: numeric argument required", args[0])),
        }
    }

    fn name(&self) -> &str {
        "exit"
    }
}

impl Command for TypeCommand {
    fn execute(&self, args: &[&str]) -> CommandResult {
        if args.is_empty() {
            return CommandResult::Error("type: not enough arguments".to_string());
        }

        let type_results = args
            .iter()
            .map(|&cmd| {
                if Shell::is_builtin(cmd) {
                    format!("{} is a shell builtin", cmd)
                } else if let Some(path) = Shell::find_in_path(cmd) {
                    format!("{} is {}", cmd, path)
                } else {
                    format!("{}: not found", cmd)
                }
            })
            .collect::<Vec<_>>()
            .join("\n");

        CommandResult::Success(type_results)
    }

    fn name(&self) -> &str {
        "type"
    }
}

impl Command for PwdCommand {
    fn execute(&self, args: &[&str]) -> CommandResult {
        if !args.is_empty() {
            return CommandResult::Error("pwd: too many args".to_string());
        }

        match env::current_dir() {
            Ok(dir) => CommandResult::Success(dir.display().to_string()),
            Err(err) => CommandResult::Error(format!("pwd: {}", err)),
        }
    }

    fn name(&self) -> &str {
        "pwd"
    }
}

impl Command for CdCommand {
    fn execute(&self, args: &[&str]) -> CommandResult {
        let user_home = match env::var("HOME") {
            Ok(home) => home,
            Err(_) => "/home".to_string(),
        };

        let target_path = if args.is_empty() {
            user_home
        } else if args.len() > 1 {
            return CommandResult::Error("cd: too many arguments".to_string());
        } else {
            args[0].to_string().replace("~", &user_home)
        };

        let path = Path::new(&target_path);
        match env::set_current_dir(path) {
            Ok(_) => CommandResult::Empty,
            Err(_) => {
                CommandResult::Error(format!("cd: {}: No such file or directory", target_path))
            }
        }
    }

    fn name(&self) -> &str {
        "cd"
    }
}

struct Shell {
    commands: Vec<Box<dyn Command>>,
}

impl Shell {
    fn new() -> Self {
        let commands: Vec<Box<dyn Command>> = vec![
            Box::new(EchoCommand),
            Box::new(ExitCommand),
            Box::new(TypeCommand),
            Box::new(PwdCommand),
            Box::new(CdCommand),
        ];

        Shell { commands }
    }

    fn is_builtin(name: &str) -> bool {
        matches!(name, "echo" | "exit" | "type" | "pwd" | "cd")
    }

    fn parse_redirects<'a>(&self, args: &[&'a str]) -> (Vec<&'a str>, Vec<Redirect>) {
        let mut command_args = Vec::new();
        let mut redirects = Vec::new();
        let mut i = 0;

        while i < args.len() {
            match args[i] {
                ">" | "1>" => {
                    if i + 1 < args.len() {
                        redirects.push(Redirect {
                            file_descriptor: 1,
                            file_path: args[i + 1].to_string(),
                            append: false,
                        });
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                ">>" | "1>>" => {
                    if i + 1 < args.len() {
                        redirects.push(Redirect {
                            file_descriptor: 1,
                            file_path: args[i + 1].to_string(),
                            append: true,
                        });
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "2>" => {
                    if i + 1 < args.len() {
                        redirects.push(Redirect {
                            file_descriptor: 2,
                            file_path: args[i + 1].to_string(),
                            append: false,
                        });
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                "2>>" => {
                    if i + 1 < args.len() {
                        redirects.push(Redirect {
                            file_descriptor: 2,
                            file_path: args[i + 1].to_string(),
                            append: true,
                        });
                        i += 2;
                    } else {
                        i += 1;
                    }
                }
                _ => {
                    command_args.push(args[i]);
                    i += 1;
                }
            }
        }

        (command_args, redirects)
    }

    fn find_in_path(command: &str) -> Option<String> {
        if let Ok(paths) = env::var("PATH") {
            for path in env::split_paths(&paths) {
                let candidate = path.join(command);
                if candidate.is_file() {
                    return Some(candidate.to_string_lossy().to_string());
                }
            }
        }
        None
    }

    fn get_builtin_command(&self, name: &str) -> Option<&Box<dyn Command>> {
        self.commands.iter().find(|cmd| cmd.name() == name)
    }

    fn get_builtin_commands_list(&self) -> Vec<String> {
        self.commands
            .iter()
            .map(|command| String::from(command.name()))
            .collect::<Vec<String>>()
    }

    fn parse_and_execute(&self, input: &str) -> CommandResult {
        let input = input.trim();
        if input.is_empty() {
            return CommandResult::Empty;
        }

        match tokenize(input) {
            Ok(tokens) if !tokens.is_empty() => {
                let command_name = &tokens[0];
                let args: Vec<&str> = tokens[1..].iter().map(|s| s.as_str()).collect();

                let (filtered_args, redirects) = self.parse_redirects(&args);

                for redirect in &redirects {
                    if let Err(e) = ensure_parent_directory_exists(&redirect.file_path) {
                        return CommandResult::Error(format!(
                            "Cannot create directory for {}: {}",
                            redirect.file_path, e
                        ));
                    }

                    if let Err(e) = fs::OpenOptions::new()
                        .write(true)
                        .create(true)
                        .open(&redirect.file_path)
                    {
                        return CommandResult::Error(format!(
                            "Cannot create file {}: {}",
                            redirect.file_path, e
                        ));
                    }
                }

                if let Some(command) = self.get_builtin_command(command_name) {
                    let result = command.execute(&filtered_args);

                    match result {
                        CommandResult::Success(mut output) => {
                            for redirect in &redirects {
                                if redirect.file_descriptor == 1 {
                                    if let Err(e) =
                                        ensure_parent_directory_exists(&redirect.file_path)
                                    {
                                        return CommandResult::Error(format!(
                                            "Cannot create directory for {}: {}",
                                            redirect.file_path, e
                                        ));
                                    }

                                    let file_result = if redirect.append {
                                        let file = fs::OpenOptions::new()
                                            .write(true)
                                            .append(true)
                                            .create(true)
                                            .open(&redirect.file_path);
                                        if file
                                            .as_ref()
                                            .expect("Append failed")
                                            .metadata()
                                            .expect("Append failed")
                                            .len()
                                            != 0
                                        {
                                            output = String::from("\n") + &output;
                                        }

                                        file
                                    } else {
                                        fs::OpenOptions::new()
                                            .write(true)
                                            .truncate(true)
                                            .create(true)
                                            .open(&redirect.file_path)
                                    };

                                    match file_result {
                                        Ok(mut file) => {
                                            if let Err(e) = file.write_all(output.as_bytes()) {
                                                return CommandResult::Error(format!(
                                                    "Failed to write to {}: {}",
                                                    redirect.file_path, e
                                                ));
                                            }
                                            return CommandResult::Success(String::new());
                                        }
                                        Err(e) => {
                                            return CommandResult::Error(format!(
                                                "Cannot open file {}: {}",
                                                redirect.file_path, e
                                            ));
                                        }
                                    }
                                }
                            }
                            CommandResult::Success(output)
                        }
                        CommandResult::Error(mut err_msg) => {
                            for redirect in &redirects {
                                if redirect.file_descriptor == 2 {
                                    if let Err(e) =
                                        ensure_parent_directory_exists(&redirect.file_path)
                                    {
                                        return CommandResult::Error(format!(
                                            "Cannot create directory for {}: {}",
                                            redirect.file_path, e
                                        ));
                                    }

                                    let file_result = if redirect.append {
                                        let file = fs::OpenOptions::new()
                                            .write(true)
                                            .append(true)
                                            .create(true)
                                            .open(&redirect.file_path);
                                        if file
                                            .as_ref()
                                            .expect("Append failed")
                                            .metadata()
                                            .expect("Append failed")
                                            .len()
                                            != 0
                                        {
                                            err_msg = String::from("\n") + &err_msg;
                                        }

                                        file
                                    } else {
                                        fs::OpenOptions::new()
                                            .write(true)
                                            .truncate(true)
                                            .create(true)
                                            .open(&redirect.file_path)
                                    };

                                    match file_result {
                                        Ok(mut file) => {
                                            if let Err(_) = file.write_all(err_msg.as_bytes()) {
                                                return CommandResult::Error(err_msg);
                                            }
                                            return CommandResult::Success(String::new());
                                        }
                                        Err(_) => {
                                            return CommandResult::Error(err_msg);
                                        }
                                    }
                                }
                            }
                            CommandResult::Error(err_msg)
                        }
                        _ => result,
                    }
                } else {
                    self.execute_external_command(command_name, &filtered_args, &redirects)
                }
            }
            Ok(_) => CommandResult::Empty,
            Err(err) => CommandResult::Error(err),
        }
    }

    fn execute_external_command(
        &self,
        command: &str,
        args: &[&str],
        redirects: &[Redirect],
    ) -> CommandResult {
        use std::fs::{OpenOptions};
        use std::io::{Read, Seek, SeekFrom};

        let mut command_process = process::Command::new(command);
        command_process.args(args);

        for redirect in redirects {
            match redirect.file_descriptor {
                1 | 2 => {
                    if let Err(e) = ensure_parent_directory_exists(&redirect.file_path) {
                        return CommandResult::Error(format!(
                            "Cannot create directory for {}: {}",
                            redirect.file_path, e
                        ));
                    }

                    if redirect.append {
                        let file_exists = Path::new(&redirect.file_path).exists();

                        if file_exists {
                            let mut existing_file = match OpenOptions::new()
                                .read(true)
                                .write(true)
                                .open(&redirect.file_path)
                            {
                                Ok(file) => file,
                                Err(e) => {
                                    return CommandResult::Error(format!(
                                        "Cannot open file {}: {}",
                                        redirect.file_path, e
                                    ));
                                }
                            };

                            let metadata = match existing_file.metadata() {
                                Ok(meta) => meta,
                                Err(e) => {
                                    return CommandResult::Error(format!(
                                        "Cannot read metadata for {}: {}",
                                        redirect.file_path, e
                                    ));
                                }
                            };

                            if metadata.len() > 0 {
                                let mut last_char = [0u8; 1];
                                if let Err(e) = existing_file.seek(SeekFrom::End(-1)) {
                                    return CommandResult::Error(format!(
                                        "Cannot seek in file {}: {}",
                                        redirect.file_path, e
                                    ));
                                }

                                if let Err(e) = existing_file.read_exact(&mut last_char) {
                                    return CommandResult::Error(format!(
                                        "Cannot read from file {}: {}",
                                        redirect.file_path, e
                                    ));
                                }

                                if last_char[0] != b'\n' {
                                    if let Err(e) = existing_file.write_all(b"\n") {
                                        return CommandResult::Error(format!(
                                            "Cannot write to file {}: {}",
                                            redirect.file_path, e
                                        ));
                                    }
                                }
                            }

                            drop(existing_file);
                        }
                    }

                    let file = if redirect.append {
                        OpenOptions::new()
                            .write(true)
                            .append(true)
                            .create(true)
                            .open(&redirect.file_path)
                    } else {
                        OpenOptions::new()
                            .write(true)
                            .truncate(true)
                            .create(true)
                            .open(&redirect.file_path)
                    };

                    match file {
                        Ok(file) => {
                            if redirect.file_descriptor == 1 {
                                command_process.stdout(file);
                            } else {
                                command_process.stderr(file);
                            }
                        }
                        Err(e) => {
                            return CommandResult::Error(format!(
                                "Cannot open file {}: {}",
                                redirect.file_path, e
                            ));
                        }
                    }
                }
                _ => {}
            }
        }

        match command_process.output() {
            Ok(output) => {
                if !output.stderr.is_empty() {
                    let error = String::from_utf8_lossy(&output.stderr)
                        .trim_end()
                        .to_string();
                    CommandResult::Error(error)
                } else {
                    CommandResult::Success(
                        String::from_utf8_lossy(&output.stdout)
                            .trim_end()
                            .to_string(),
                    )
                }
            }
            Err(e) => CommandResult::Error(format!(
                "{}: {}",
                command,
                e.to_string().replace(
                    "No such file or directory (os error 2)",
                    "command not found"
                )
            )),
        }
    }

    fn run(&self) -> std::result::Result<(), Box<dyn std::error::Error>> {
        let mut rl = Editor::new()?;

        rl.set_helper(Some(MyCompleter {
            commands: self.get_builtin_commands_list(),
        }));

        if let Err(_) = rl.load_history("./history.txt") {
            println!("No previous history.");
        }

        loop {
            let readline = rl.readline("$ ");
            match readline {
                Ok(line) => {
                    let _ = rl.add_history_entry(line.as_str());
                    match self.parse_and_execute(&line) {
                        CommandResult::Success(message) if !message.is_empty() => {
                            println!("{}", message)
                        }
                        CommandResult::Error(err) => eprintln!("{}", err),
                        CommandResult::Exit(code) => process::exit(code),
                        _ => {}
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    continue;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    eprintln!("Error: {:?}", err);
                    break;
                }
            }
        }

        match rl.save_history("./history.txt") {
            Ok(_) => Ok(()),
            Err(err) => Err(Box::new(err)),
        }
    }
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let shell = Shell::new();
    match shell.run() {
        Ok(_) => Ok(()),
        Err(err) => Err(err)
    }

}
