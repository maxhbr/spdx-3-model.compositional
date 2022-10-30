#![feature(core_intrinsics)]
use iri_string::types::IriStr;
use serde_derive::{Deserialize, Serialize};
use std::intrinsics::type_name;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

// #[derive(Debug, Deserialize, Serialize)] 
struct CreationInformation {

}

trait HasSpdxid {
    fn spdxid(&self) -> Box<IriStr>;
}

trait Identifyable {
    fn spdxid(&self) -> Box<IriStr>;
}

trait Elementic {
    fn name(&self) -> String;
    fn summary(&self) -> String;
    fn description(&self) -> String;
    fn comment(&self) -> String;
}

// #[derive(Debug, Deserialize, Serialize)] 
#[derive(Debug)]
struct ElementImpl {
    name: String,
    summary: String,
    description: String,
    comment: String,
}

impl Identifyable for ElementImpl {
    fn spdxid(&self) -> Box<IriStr> {
        let structname =  unsafe { type_name::<Self>() };
        let mut hasher = DefaultHasher::new();
        hasher.write(format!("{:?}", self).as_bytes());
        let hash = hasher.finish();
        let iri_str_raw = format!("urn:{}:{}", structname, hash);
        IriStr::new(iri_str_raw.as_str()).unwrap().into()
    }
}

impl Elementic for ElementImpl {
    fn name(&self) -> String { self.name.clone() }
    fn summary(&self) -> String { self.summary.clone() }
    fn description(&self) -> String { self.description.clone() }
    fn comment(&self) -> String { self.comment.clone() }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let e = ElementImpl {
            name: String::from("Element1"),
            summary: String::from("Summary1"),
            description: String::from("Description1"),
            comment: String::from("Comment1")
        };
        assert_eq!(e.name, "Element1".to_string());
    }
}
