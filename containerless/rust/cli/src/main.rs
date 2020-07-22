mod k8s_shim;
mod error;

use clap::Clap;

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Clap)]
#[clap(name = "Containerless", version = "0.1", author = "Emily Herbert <emilyherbert@cs.umass.edu>, Arjun Guha <arjun@cs.umass.edu>")]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Status(Status),
    CreateFunction(CreateFunction),
    DeleteFunction(DeleteFunction),
    ListFunctions(ListFunctions),
}

/// Queries the status of Containerless.
#[derive(Clap)]
struct Status { }

/// Creates a function.
#[derive(Clap)]
struct CreateFunction {
    /// Print debug info
    #[clap(short)]
    name: String
}

/// Deletes a function.
#[derive(Clap)]
struct DeleteFunction {
    /// Print debug info
    #[clap(short)]
    name: String
}

/// Lists all functions.
#[derive(Clap)]
struct ListFunctions { }

fn main() {
    let opts: Opts = Opts::parse();

    // You can handle information about subcommands by requesting their matches by name
    // (as below), requesting just the name used, or both at the same time
    match opts.subcmd {
        SubCommand::Status(_) => {
            let status = k8s_shim::get_all().unwrap();
            println!("{}", status);
        },
        SubCommand::CreateFunction(t) => {
            println!("You called create function with {:?}!", t.name);
        },
        SubCommand::DeleteFunction(t) => {
            println!("You called create function with {:?}!", t.name);
        },
        SubCommand::ListFunctions(_) => {
            println!("You called list functions!");
        }
    }

    // more program logic goes here...
}