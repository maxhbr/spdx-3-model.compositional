#![feature(core_intrinsics)]
use iri_string::types::IriStr;
use serde::{Serialize, Deserialize};
use serde_derive::{Deserialize, Serialize};
use std::intrinsics::type_name;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use chrono::{DateTime, TimeZone, NaiveDateTime, Utc};

#[derive(Debug, Clone, Deserialize, Serialize)] 
struct CreationInfo {
    specVersion: String,
    profile: Vec<String>,
    created: String,
    dataLicense: String
    // createdBy
}
fn genCreationInfo() -> CreationInfo {
    CreationInfo {
        specVersion: String::from("3.0.0"),
        profile: vec![String::from("core")],
        created: Utc::now().to_string(), // TODO: wrong format
        dataLicense: String::from("CC0")
    }
}

trait Createable {
    fn creationInfo(&self) -> *const CreationInfo;
}

fn identifierFromContent<T: core::fmt::Debug>(structvalue: T, structname: String) -> Box<IriStr> {
    let mut hasher = DefaultHasher::new();
    hasher.write(format!("{:?}", structvalue).as_bytes());
    let hash = hasher.finish();
    let iri_str_raw = format!("urn:spdx:{}:{}", structname, hash);
    IriStr::new(iri_str_raw.as_str()).unwrap().into()
}
trait Identifyable {
    fn spdxid(&self) -> Box<IriStr>;
}

#[derive(Debug, Clone, Deserialize, Serialize)] 
enum ExternalReferenceType {
    ALT_DOWNLOAD_LOCATION,
    SECURITY_ADVISORY,
    SECURITY_FIX,
    SECURITY_OTHER,
    OTHER
}
#[derive(Debug, Clone, Deserialize, Serialize)] 
struct ExternalReference {
    externalReferenceType: ExternalReferenceType,
    // locator: Box<IriStr>,
    contentType: String,
    comments: String
}

#[derive(Debug, Clone, Deserialize, Serialize)] 
enum ExternalIdentifierType {
    CPE22,
    CPE23,
    PURL,
    SWID,
    SWHID,
    GITOID,
    OTHER
}
#[derive(Debug, Clone, Deserialize, Serialize)] 
struct ExternalIdentifier {
    externalIdentifierType: ExternalIdentifierType,
    // identifier: Box<IriStr>,
    comments: String
}

#[derive(Debug, Clone, Deserialize, Serialize)] 
struct ElementInfo {
    name: String,
    summary: String,
    description: String,
    comment: String,
    // verifiedUsing: Vec<IntegrityMethod>,
    externalReferences: Vec<ExternalReference>,
    externalIdentifiers: Vec<ExternalIdentifier>
    // extensions: std::iter::Map<IriStr,dyn std::any::Any>

}
trait Element : Identifyable + Createable {
    fn getElementInfo(&self) -> &ElementInfo;
    fn name(&self) -> String { self.getElementInfo().name.clone() }
    fn summary(&self) -> String { self.getElementInfo().summary.clone() }
    fn description(&self) -> String { self.getElementInfo().description.clone() }
    fn comment(&self) -> String { self.getElementInfo().comment.clone() }
}

trait Artifact : Element {
    fn originatedBy(&self) -> Vec<Box<dyn Element>>;
}

#[derive(Debug, Deserialize, Serialize)] 
struct File {
    creationInfo: CreationInfo,
    elementInfo: ElementInfo,
    // originatedBy: 
    filePurpose: String,
    contentType: String
}
impl Identifyable for File {
    fn spdxid(&self) -> Box<IriStr> {
        identifierFromContent(self, String::from("file"))
    }
}
impl Createable for File {
    fn creationInfo(&self) -> *const CreationInfo { &self.creationInfo }
}
impl Element for File {
    fn getElementInfo(&self) -> &ElementInfo { &self.elementInfo }
}
impl Artifact for File {
    fn originatedBy(&self) -> Vec<Box<dyn Element>>{
        Vec::new()
    }
}

#[derive(Debug, Deserialize, Serialize)] 
struct Relationship {
    creationInfo: CreationInfo,
    elementInfo: ElementInfo,
    relationshipType: String,
    from: Box<SomeElement>,
    to: Vec<SomeElement>
}
impl Identifyable for Relationship {
    fn spdxid(&self) -> Box<IriStr> {
        identifierFromContent(self, String::from("relationship"))
    }
}
impl Createable for Relationship {
    fn creationInfo(&self) -> *const CreationInfo { &self.creationInfo }
}
impl Element for Relationship {
    fn getElementInfo(&self) -> &ElementInfo { &self.elementInfo }
}

trait Collection : Element {
    // fn namespaces: 
    // fn imports: 
    fn elements(&self) -> &Vec<SomeElement>;
    fn rootElements(&self) -> &Vec<SomeElement>;
}

#[derive(Debug, Deserialize, Serialize)] 
struct Bundle {
    creationInfo: CreationInfo,
    elementInfo: ElementInfo,
    context: Option<String>,
    elements: Vec<SomeElement>
}
impl Identifyable for Bundle {
    fn spdxid(&self) -> Box<IriStr> {
        identifierFromContent(self, String::from("bundle"))
    }
}
impl Createable for Bundle {
    fn creationInfo(&self) -> *const CreationInfo {
        &self.creationInfo
    }
}
impl Element for Bundle {
    fn getElementInfo(&self) -> &ElementInfo { &self.elementInfo }
}
impl Collection for Bundle {
    fn elements(&self) -> &Vec<SomeElement> { &self.elements }

    fn rootElements(&self) -> &Vec<SomeElement> {
        todo!()
    }
}

#[derive(Debug, Deserialize, Serialize)] 
enum SomeElement {
    File(File),
    Bundle(Bundle),
    Relationship(Relationship),
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let e = File {
            creationInfo: genCreationInfo(),
            elementInfo: ElementInfo {
                name: String::from("Element1"),
                summary: String::from("Summary1"),
                description: String::from("Description1"),
                comment: String::from("Comment1"),
                externalReferences: Vec::new(),
                externalIdentifiers: Vec::new(),
            },
            filePurpose: String::from("$FILE_PURPOSE"),
            contentType: String::from("$CONTENT_TYPE")
        };
        assert_eq!(e.name(), "Element1".to_string());
        assert_eq!(e.spdxid().to_string().starts_with("urn:spdx:file:"), true);
    }

    #[test]
    fn serialization_works() {
        let c = genCreationInfo();
        let f1 = File {
            creationInfo: c.clone(),
            elementInfo: ElementInfo {
                name: String::from("Element1"),
                summary: String::from("Summary1"),
                description: String::from("Description1"),
                comment: String::from("Comment1"),
                externalReferences: Vec::new(),
                externalIdentifiers: Vec::new(),
            },
            filePurpose: String::from("$FILE_PURPOSE"),
            contentType: String::from("$CONTENT_TYPE")
        };
        let f2 = File {
            creationInfo: c.clone(),
            elementInfo: ElementInfo {
                name: String::from("Element2"),
                summary: String::from("Summary2"),
                description: String::from("Description2"),
                comment: String::from("Comment2"),
                externalReferences: Vec::new(),
                externalIdentifiers: Vec::new(),
            },
            filePurpose: String::from("$FILE_PURPOSE"),
            contentType: String::from("$CONTENT_TYPE")
        };

        let b = Bundle {
            creationInfo: c.clone(),
            elementInfo: ElementInfo {
                name: String::from("Bundle"),
                summary: String::from("Bundle Summary1"),
                description: String::from("Bundle Description1"),
                comment: String::from("Bundle Comment1"),
                externalReferences: Vec::new(),
                externalIdentifiers: Vec::new(),
            },
            context: None,
            elements: vec![SomeElement::File(f1),SomeElement::File(f2)]
        };

        let serialized = serde_json::to_string_pretty(&b).unwrap();
        println!("serialized = {}", serialized);

        let deserialized: Bundle = serde_json::from_str(&serialized).unwrap();
        println!("deserialized = {:?}", deserialized);
    }
}
