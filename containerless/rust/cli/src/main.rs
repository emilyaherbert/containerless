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
    Status(Status),
    CreateFunction(CreateFunction),
    DeleteFunction(DeleteFunction),
    DescribeFunction(DescribeFunction),
    ListFunctions(ListFunctions),
    Invoke(Invoke)
}

/// Queries the status of Containerless.
#[derive(Clap)]
struct Status { }

/// Creates a function.
#[derive(Clap)]
struct CreateFunction {
    /// Name of the function to create
    #[clap(short)]
    name: String,
    /// The file in the current directory
    #[clap(short)]
    filename: String
}

/// Deletes a function.
#[derive(Clap)]
struct DeleteFunction {
    /// Name of the function to delete
    #[clap(short)]
    name: String
}

/// Deletes a function.
#[derive(Clap)]
struct DescribeFunction {
    /// Name of the function to describe
    #[clap(short)]
    name: String
}

/// Lists all functions.
#[derive(Clap)]
struct ListFunctions { }

/// Invokes a function.
#[derive(Clap)]
struct Invoke {
    /// Name of the function to describe
    #[clap(short)]
    name: String
}

#[tokio::main]
async fn main() {
    let opts: Opts = Opts::parse();
    let containerless_shim = containerless_shim::ContainerlessShim::new();

    match opts.subcmd {
        SubCommand::Status(_) => {
            let status = k8s_shim::get_all().unwrap();
            println!("{}", status);
        },
        SubCommand::CreateFunction(t) => {
            println!("{}", containerless_shim.create_function(&t.name, &t.filename).await.unwrap());
        },
        SubCommand::DeleteFunction(t) => { containerless_shim.delete_function(&t.name).await.unwrap(); }
        SubCommand::DescribeFunction(t) => {
            let output = containerless_shim.describe_function(&t.name).await.unwrap();
            println!("{}", output);
        },
        SubCommand::ListFunctions(_) => {
            let output = containerless_shim.list_functions().await.unwrap();
            println!("{}", output);
        },
        SubCommand::Invoke(t) => {
            let output = containerless_shim.invoke(&t.name).await.unwrap();
            println!("{}", output);
        }
    }

    //Ok(())
}