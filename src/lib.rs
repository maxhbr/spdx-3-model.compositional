#![feature(core_intrinsics)]
use iri_string::types::IriStr;
use serde_derive::{Deserialize, Serialize};
use std::intrinsics::type_name;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use chrono::{DateTime, TimeZone, NaiveDateTime, Utc};

// #[derive(Debug, Deserialize, Serialize)] 
#[derive(Debug)]
struct CreationInformation {
    specVersion: String,
    profile: Vec<String>,
    created: DateTime<chrono::Utc>,
    dataLicense: String
    // createdBy
}
fn genCreationInformation() -> CreationInformation {
    CreationInformation {
        specVersion: String::from("3.0.0"),
        profile: vec![String::from("core")],
        created: Utc::now(),
        dataLicense: String::from("CC0")
    }
}

trait Createable {
    fn creationInfo(&self) -> *const CreationInformation;
}

fn identifierFromContent<T: core::fmt::Debug>(structvalue: T, structname: String) -> Box<IriStr> {
    let mut hasher = DefaultHasher::new();
    hasher.write(format!("{:?}", structvalue).as_bytes());
    let hash = hasher.finish();
    let iri_str_raw = format!("urn:{}:{}", structname, hash);
    IriStr::new(iri_str_raw.as_str()).unwrap().into()
}
trait Identifyable {
    fn spdxid(&self) -> Box<IriStr>;
}

trait Elementic : Identifyable + Createable {
    fn name(&self) -> String;
    fn summary(&self) -> String;
    fn description(&self) -> String;
    fn comment(&self) -> String;
}

// #[derive(Debug, Deserialize, Serialize)] 
#[derive(Debug)]
struct Element {
    creationInfo: CreationInformation,
    name: String,
    summary: String,
    description: String,
    comment: String
}

impl Identifyable for Element {
    fn spdxid(&self) -> Box<IriStr> {
        identifierFromContent(self, String::from("element"));
    }
}

impl Createable for Element {
    fn creationInfo(&self) -> *const CreationInformation {
        &(self.creationInfo)
    }
}

impl Elementic for Element {
    fn name(&self) -> String { self.name.clone() }
    fn summary(&self) -> String { self.summary.clone() }
    fn description(&self) -> String { self.description.clone() }
    fn comment(&self) -> String { self.comment.clone() }
}

struct Relationship {
    up: Element,
    relationshipType: String
    // from
    // to
}





#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let e = Element {
            creationInfo: genCreationInformation(),
            name: String::from("Element1"),
            summary: String::from("Summary1"),
            description: String::from("Description1"),
            comment: String::from("Comment1")
        };
        assert_eq!(e.name, "Element1".to_string());
    }
}
