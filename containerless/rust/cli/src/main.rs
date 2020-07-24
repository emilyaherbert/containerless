mod shims;
mod error;

use shims::*;

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
    Deploy(Deploy),
    Status(Status),
    CreateFunction(CreateFunction),
    DeleteFunction(DeleteFunction),
    ListFunctions(ListFunctions),
}

/// Deploys the Containerless system.
#[derive(Clap)]
struct Deploy { }

/// Queries the status of Containerless.
#[derive(Clap)]
struct Status { }

/// Creates a function.
#[derive(Clap)]
struct CreateFunction {
    /// Name of the function to create
    #[clap(short)]
    name: String,
    // /// The file in the current directory
    //#[clap(short)]
    //filename: String
}

/// Deletes a function.
#[derive(Clap)]
struct DeleteFunction {
    /// Name of the function to delete
    #[clap(short)]
    name: String
}

/// Lists all functions.
#[derive(Clap)]
struct ListFunctions { }

fn main() {
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Deploy(_) => {
            let output = containerless_shim::deploy().unwrap();
            println!("{}", output);
        },
        SubCommand::Status(_) => {
            let status = k8s_shim::get_all().unwrap();
            println!("{}", status);
        },
        SubCommand::CreateFunction(t) => {
            let output = containerless_shim::create_function(&t.name).unwrap();
            println!("{}", output);
        },
        SubCommand::DeleteFunction(t) => {
            println!("You called create function with {:?}!", t.name);
        },
        SubCommand::ListFunctions(_) => {
            println!("You called list functions!");
        }
    }
}