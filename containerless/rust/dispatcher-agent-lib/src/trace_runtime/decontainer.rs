use super::affine_ref::AffineBoxFactory;
use super::common::*;
use super::error::*;
use super::execution_context::*;
use super::type_dynamic::*;
use super::Containerless;
use std::convert::TryInto;
use std::str;

// An `async fn` produces a future that may or may not be sendable:
//
//    https://rust-lang.github.io/async-book/07_workarounds/04_send_approximation.html
//
// i.e., Send is inferred. The rest of this program requires
// run_decontainerized_function to produce a sendable future, but there is no
// way to ensure that  with an annotation on that function. The following code
// raises a type error if the future is not sendable. Having the type error
// locally within this file makes it easier to debug.
fn _statically_assert_sendable(
    func: Containerless, client: HttpClient, url_path: &str, body: &[u8],
) {
    fn _check_send<F>(_f: F)
    where
        F: std::future::Future + Send,
    {
    }
    _check_send(run_decontainerized_function(func, client, url_path, body));
}

pub async fn run_decontainerized_function(
    func: Containerless, client: HttpClient, url_path: &str, body: &[u8],
) -> Result<Response, Error> {
    let arena = Bump::new();
    let affine_factory = AffineBoxFactory::new();

    let request = affine_factory.make_box({
        let request = Dyn::object(&arena);
        request
            .set_field("path", Dyn::str(&arena, url_path))
            .unwrap();

        let body_json = serde_json::from_slice(&body)
            .map(|j| Dyn::from_json(&arena, j))
            .map_err(|err| Error::Json(err));
        let body_str = str::from_utf8(&body)
            .map(|s| Dyn::str(&arena, s))
            .map_err(|err| Error::String(err));
        let body = body_json.or(body_str).unwrap();

        request.set_field("body", body).unwrap();
        request
    });

    let mut ec = affine_factory.make_box(ExecutionContext::new());

    let mut pending_futures = Vec::new();
    pending_futures.push(Box::pin(PendingOp::initial().to_future2(&client)));
    while pending_futures.is_empty() == false {
        // Wait for a single asynchronous operation to complete.
        let (outcome_result, _, new_pending_futures) =
            futures::future::select_all(pending_futures).await;
        // Save the remaining operations in state.
        pending_futures = new_pending_futures;
        // Abort if the asynchronous operation failed.
        let (outcome, indicator, closure) = outcome_result?;
        // Build the arguments array from the result.
        affine_factory.begin_reads();
        let args = outcome.process(&arena, *request.read(), closure);
        // Run the serverless function.
        let _result = func(&arena, ec.read_mut(), Dyn::int(indicator), args)?;
        // Fetch newly constructed operations.
        let mut new_ops = Vec::new();
        std::mem::swap(&mut new_ops, &mut ec.read_mut().new_ops);
        affine_factory.end_reads();
        // Turn these operations into futures.
        pending_futures.extend(
            new_ops
                .into_iter()
                .map(|tuple| Box::pin(tuple.to_future2(&client))),
        );
    }

    affine_factory.begin_reads();

    match ec.read().response {
        None => {
            // The follow error is not due to optimistic trace compilation.
            // It will re-occur if we re-execution in JavaScript, thus we
            // don't bother doing so.
            return Ok(hyper::Response::builder()
                .status(500)
                .body(hyper::Body::from("No response set"))
                .unwrap());
        }
        Some(resp) => {
            let resp: Result<String, ()> = resp.try_into();
            if let Ok(body) = resp {
                return Ok(hyper::Response::new(hyper::Body::from(body)));
            }

            return Ok(hyper::Response::builder()
                .status(500)
                .body(hyper::Body::from(
                    "Could not convert response to string (... from inside Rust...)",
                ))
                .unwrap());
        }
    }
}
