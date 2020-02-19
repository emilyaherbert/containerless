use k8s_openapi::api::apps::v1::{ReplicaSet, ReplicaSetSpec};
use k8s_openapi::apimachinery::pkg::util::intstr::IntOrString;
use k8s_openapi::api::core::v1::{
    Container, ContainerPort, EnvVar, PodSpec, PodTemplateSpec, Service, ServicePort, ServiceSpec,
    Probe, HTTPGetAction
};
use k8s_openapi::apimachinery::pkg::apis::meta::v1::{LabelSelector, ObjectMeta};
use std::collections::BTreeMap;
use std::convert::Into;

pub struct ObjectMetaBuilder {
    meta: ObjectMeta,
}

pub struct ServicePortBuilder {
    service_port: ServicePort,
}

pub struct ServiceSpecBuilder {
    service_spec: ServiceSpec,
}

pub struct ServiceBuilder {
    service: Service,
}

pub struct ReplicaSetBuilder {
    replica_set: ReplicaSet,
}

pub struct ReplicaSetSpecBuilder {
    replica_set_spec: ReplicaSetSpec,
}

pub struct LabelSelectorBuilder {
    label_selector: LabelSelector,
}

pub struct PodSpecBuilder {
    pod_spec: PodSpec,
}

pub struct PodTemplateSpecBuilder {
    pod_template_spec: PodTemplateSpec,
}

pub struct ContainerBuilder {
    container: Container,
}

impl ObjectMetaBuilder {
    pub fn new() -> Self {
        return ObjectMetaBuilder {
            meta: ObjectMeta::default(),
        };
    }

    pub fn build(self) -> ObjectMeta {
        return self.meta;
    }

    pub fn name<S>(mut self, name: S) -> Self
    where
        S: Into<String>,
    {
        self.meta.name = Some(name.into());
        return self;
    }

    pub fn label<S1, S2>(mut self, key: S1, value: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        match &mut self.meta.labels {
            None => {
                let mut labels = BTreeMap::new();
                labels.insert(key.into(), value.into());
                self.meta.labels = Some(labels);
            }
            Some(labels) => {
                labels.insert(key.into(), value.into());
            }
        }
        return self;
    }
}

impl ServicePortBuilder {
    pub fn new() -> Self {
        let service_port = ServicePort::default();
        return ServicePortBuilder { service_port };
    }

    pub fn build(self) -> ServicePort {
        return self.service_port;
    }

    pub fn name(mut self, name: &str) -> Self {
        self.service_port.name = Some(name.to_string());
        return self;
    }

    pub fn port(mut self, port: i32) -> Self {
        self.service_port.port = port;
        return self;
    }
}

impl ServiceSpecBuilder {
    pub fn new() -> Self {
        let mut service_spec = ServiceSpec::default();
        service_spec.ports = Some(Vec::new());
        return ServiceSpecBuilder { service_spec };
    }

    pub fn build(self) -> ServiceSpec {
        return self.service_spec;
    }

    pub fn node_port(mut self) -> Self {
        self.service_spec.type_ = Some("NodePort".to_string());
        return self;
    }

    pub fn add_port(mut self, service_port: ServicePort) -> Self {
        match &mut self.service_spec.ports {
            None => panic!("ports array is None"),
            Some(ports) => {
                ports.push(service_port);
            }
        };
        return self;
    }

    pub fn selector<S1, S2>(mut self, key: S1, value: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        if let None = self.service_spec.selector {
            self.service_spec.selector = Some(BTreeMap::new());
        }
        let selector = self.service_spec.selector.as_mut().unwrap();
        selector.insert(key.into(), value.into());
        return self;
    }
}

impl ServiceBuilder {
    pub fn new() -> Self {
        let service = Service::default();
        return ServiceBuilder { service };
    }

    pub fn build(self) -> Service {
        return self.service;
    }

    pub fn metadata(mut self, meta: ObjectMeta) -> Self {
        self.service.metadata = Some(meta);
        return self;
    }

    pub fn spec(mut self, spec: ServiceSpec) -> Self {
        self.service.spec = Some(spec);
        return self;
    }
}

impl ReplicaSetBuilder {
    pub fn new() -> Self {
        let replica_set = ReplicaSet::default();
        return ReplicaSetBuilder { replica_set };
    }

    pub fn build(self) -> ReplicaSet {
        return self.replica_set;
    }

    pub fn metadata(mut self, meta: ObjectMeta) -> Self {
        self.replica_set.metadata = Some(meta);
        return self;
    }

    pub fn spec(mut self, spec: ReplicaSetSpec) -> Self {
        self.replica_set.spec = Some(spec);
        return self;
    }
}

impl ReplicaSetSpecBuilder {
    pub fn new() -> Self {
        let replica_set_spec = ReplicaSetSpec::default();
        return ReplicaSetSpecBuilder { replica_set_spec };
    }

    pub fn build(self) -> ReplicaSetSpec {
        return self.replica_set_spec;
    }

    pub fn replicas(mut self, n: i32) -> Self {
        self.replica_set_spec.replicas = Some(n);
        return self;
    }

    pub fn selector(mut self, s: LabelSelector) -> Self {
        self.replica_set_spec.selector = s;
        return self;
    }

    pub fn template(mut self, t: PodTemplateSpec) -> Self {
        self.replica_set_spec.template = Some(t);
        return self;
    }
}

impl LabelSelectorBuilder {
    pub fn new() -> Self {
        let label_selector = LabelSelector::default();
        return LabelSelectorBuilder { label_selector };
    }

    pub fn build(self) -> LabelSelector {
        return self.label_selector;
    }

    pub fn match_label<S1, S2>(mut self, key: S1, value: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        match &mut self.label_selector.match_labels {
            None => {
                let mut map = BTreeMap::new();
                map.insert(key.into(), value.into());
                self.label_selector.match_labels = Some(map);
            }
            Some(map) => {
                map.insert(key.into(), value.into());
            }
        }
        return self;
    }
}

impl PodTemplateSpecBuilder {
    pub fn new() -> Self {
        let pod_template_spec = PodTemplateSpec::default();
        return PodTemplateSpecBuilder { pod_template_spec };
    }

    pub fn build(self) -> PodTemplateSpec {
        return self.pod_template_spec;
    }

    pub fn metadata(mut self, meta: ObjectMeta) -> Self {
        self.pod_template_spec.metadata = Some(meta);
        return self;
    }

    pub fn spec(mut self, spec: PodSpec) -> Self {
        self.pod_template_spec.spec = Some(spec);
        return self;
    }
}

impl PodSpecBuilder {
    pub fn new() -> Self {
        let pod_spec = PodSpec::default();
        return PodSpecBuilder { pod_spec };
    }

    pub fn build(self) -> PodSpec {
        return self.pod_spec;
    }

    pub fn container(mut self, container: Container) -> Self {
        self.pod_spec.containers.push(container);
        return self;
    }
}

impl ContainerBuilder {
    pub fn new() -> Self {
        let container = Container::default();
        return ContainerBuilder { container };
    }

    pub fn build(self) -> Container {
        return self.container;
    }

    pub fn name<S>(mut self, name: S) -> Self
    where
        S: Into<String>,
    {
        self.container.name = name.into();
        return self;
    }

    pub fn image<S>(mut self, image: S) -> Self
    where
        S: Into<String>,
    {
        self.container.image = Some(image.into());
        return self;
    }

    pub fn always_pull(mut self) -> Self {
        self.container.image_pull_policy = Some("Always".to_string());
        return self;
    }

    pub fn port(mut self, container_port: ContainerPort) -> Self {
        match &mut self.container.ports {
            None => {
                let ports = vec![container_port];
                self.container.ports = Some(ports);
            }
            Some(vec) => {
                vec.push(container_port);
            }
        };
        return self;
    }

    pub fn expose_port<S>(self, name: S, port: i32) -> Self
    where
        S: Into<String>,
    {
        let mut container_port = ContainerPort::default();
        container_port.name = Some(name.into());
        container_port.container_port = port;
        return self.port(container_port);
    }

    pub fn env<S1, S2>(mut self, key: S1, value: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        if let None = self.container.env {
            self.container.env = Some(Vec::new());
        }
        let env = self.container.env.as_mut().unwrap();
        env.push(EnvVar {
            name: key.into(),
            value: Some(value.into()),
            value_from: None,
        });
        return self;
    }

    pub fn http_readiness_probe<S>(mut self, path: S, port: i32) -> Self
    where
        S: Into<String>,
    {
        if let None = self.container.readiness_probe {
            self.container.readiness_probe = Some(Probe::default());
        }
        let readiness_probe = self.container.readiness_probe.as_mut().unwrap();
        let mut http_get_action = HTTPGetAction::default();
        http_get_action.path = Some(path.into());
        http_get_action.port = IntOrString::Int(port);
        readiness_probe.http_get = Some(http_get_action);
        return self;
    }
}
