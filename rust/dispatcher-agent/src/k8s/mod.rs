//! API to Kubernetes
//!
//! This module provides a simple shim around the kube and k8s_openapi crates
//! to interact with Kubernetes. It has two main features:
//!
//! 1. It uses the builder pattern to construct requests, instead of the
//!    Option-heavy API that k8s_openapi has.
//! 2. It provides a Client that can interact with several different resource
//!    types, wrapping the per-type API that kube provides.
//!
//! NOTE: The API is woefully incomplete.
pub mod builder;
pub mod client;

pub use builder::*;
pub use client::Client;
