mod containerless_shim;
mod error;

use clap::Clap;

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Clap)]
#[clap(
    name = "Containerless",
    version = "0.1",
    author = "Emily Herbert <emilyherbert@cs.umass.edu>, Arjun Guha <arjun@cs.umass.edu>"
)]
struct Opts {
    #[clap(subcommand)]
    subcmd: SubCommand,
}

#[derive(Clap)]
enum SubCommand {
    Create(Create),
    Delete(Delete),
    RemoveContainers(RemoveContainers),
    RemoveTrace(RemoveTrace),
    Get(Get),
    List(List),
    Invoke(Invoke),
}

/// Creates a function.
#[derive(Clap)]
struct Create {
    /// Name of the function to create
    #[clap(short)]
    name: String,
    /// The file in the current directory
    #[clap(short)]
    filename: String,
}

/// Delete a function, removes its containers, and removes its trace.
#[derive(Clap)]
struct Delete {
    /// Name of the function to delete
    #[clap(short)]
    name: String,
}

/// Removes the containers for a function. For demo purposes only.
#[derive(Clap)]
struct RemoveContainers {
    /// Name of the function to shut down
    #[clap(short)]
    name: String,
}

/// Removes the compiled trace for a function. For demo purposes only.
#[derive(Clap)]
struct RemoveTrace {
    /// Name of the function to reset
    #[clap(short)]
    name: String,
}

/// Gets the body of a function.
#[derive(Clap)]
struct Get {
    /// Name of the function to get
    #[clap(short)]
    name: String,
}

/// Lists all functions.
#[derive(Clap)]
struct List {}

/// Invokes a function.
#[derive(Clap)]
struct Invoke {
    /// Name of the function to invoke
    #[clap(short)]
    name: String,
}

#[tokio::main]
async fn main() {
    let opts: Opts = Opts::parse();
    let containerless_shim = containerless_shim::ContainerlessShim::new();

    match opts.subcmd {
        SubCommand::Create(t) => {
            let output = containerless_shim
                .create_function(&t.name, &t.filename)
                .await
                .unwrap();
            println!("{}", output);
        }
        SubCommand::Delete(t) => {
            let output = containerless_shim.delete_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::RemoveContainers(t) => {
            let output = containerless_shim
                .shutdown_function_instances(&t.name)
                .await
                .unwrap();
            println!("{}", output);
        }
        SubCommand::RemoveTrace(t) => {
            let output = containerless_shim.reset_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::Get(t) => {
            let output = containerless_shim.get_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::List(_) => {
            let output = containerless_shim.list_functions().await.unwrap();
            println!("{}", output);
        }
        SubCommand::Invoke(t) => {
            let output = containerless_shim.invoke(&t.name).await.unwrap();
            println!("{}", output);
        }
    }
}
