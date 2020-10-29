use shared::containerless::controller;
use shared::containerless::dispatcher;

use clap::Clap;

/// This doc string acts as a help message when the user runs '--help'
/// as do all doc strings on fields
#[derive(Clap)]
#[clap(
    name = "Containerless",
    version = "0.1",
    author = "Emily Herbert <emilyherbert@cs.umass.edu>, Arjun Guha <a.guha@northeastern.edu>"
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
    DispatcherVersion(DispatcherVersion),
    Compile(Compile)
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

/// Delete a function, removes its containers, and removes its compiled trace.
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

/// Retrieves the current dispatcher version.
#[derive(Clap)]
struct DispatcherVersion {}

/// Compiles the decontainerized version for a funtion. For testing and demo
/// purposes only.
#[derive(Clap)]
struct Compile {
    /// Name of the function to compile
    #[clap(short)]
    name: String,
}

#[tokio::main]
async fn main() {
    let opts: Opts = Opts::parse();

    match opts.subcmd {
        SubCommand::Create(t) => {
            let output = controller::create_function(&t.name, &t.filename)
                .await
                .unwrap();
            println!("{}", output);
        }
        SubCommand::Delete(t) => {
            let output = controller::delete_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::RemoveContainers(t) => {
            let output = controller::shutdown_function_instances(&t.name)
                .await
                .unwrap();
            println!("{}", output);
        }
        SubCommand::RemoveTrace(t) => {
            let output = controller::reset_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::Get(t) => {
            let output = controller::get_function(&t.name).await.unwrap();
            println!("{}", output);
        }
        SubCommand::List(_) => {
            let output = controller::list_functions().await.unwrap();
            println!("{}", output);
        }
        SubCommand::Invoke(t) => {
            let output = dispatcher::invoke(&t.name).await.unwrap();
            println!("{}", output);
        },
        SubCommand::DispatcherVersion(_t) => {
            let output = controller::dispatcher_version().await.unwrap();
            println!("{}", output);
        },
        SubCommand::Compile(t) => {
            let output = dispatcher::compile(&t.name).await.unwrap();
            println!("{}", output);
        }
    }
}
