use serde::Deserialize;

#[derive(Debug, Deserialize, Clone)]
pub struct TestConfig {
    #[serde(default = "TestConfig::default_rate")]
    pub rate: u64,
    #[serde(default = "TestConfig::default_duration")]
    pub duration: u64,
    pub request: RequestConfig
}

impl TestConfig {
    pub fn from_string(s: &str) -> Self {
        serde_json::from_str(s).unwrap_or_else(|e| {
            panic!(
                "Error deserializing InvokerConfig. Error: {:?}. JSON string: {:?}",
                e, &s
            )
        })
    }

    fn default_rate() -> u64 {
        // 100 requests a second
        return 10;
    }

    fn default_duration() -> u64 {
        // 10 seconds
        return 10;
    }
}

#[derive(Debug, Deserialize, Clone)]
pub struct RequestConfig {
    pub url: String,
}

impl RequestConfig {
    
}